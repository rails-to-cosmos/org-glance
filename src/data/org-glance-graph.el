;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'org)
(require 'org-id)

(require 'org-glance-core)
(require 'org-glance-utils)
(require 'org-glance-headline)

;; Hash and equality MUST canonicalize alike, else "foo" and "foo/" split.
(define-hash-table-test 'org-glance-graph:test
                        (lambda (a b) (f-equal? (file-truename a) (file-truename b)))
                        (lambda (a) (secure-hash 'sha1 (file-truename a))))

(defvar org-glance-graph:list (make-hash-table :test 'org-glance-graph:test)
  "Registered instances of `org-glance-graph' in current session.")

;; invariant 7: single user, no mutex / locking.
(cl-defstruct (org-glance-graph (:predicate org-glance-graph?)
                                   (:conc-name org-glance-graph:))
  (directory org-glance-directory :read-only t :type directory)
  ;; invariant 1: storage ordinal only -- never ordering, never in metadata.
  (seq 0 :type integer)
  (-paths nil)
  (-meta-cache nil)
  (-external-checked 0 :type number))

(defcustom org-glance-graph-segment-max-bytes (* 256 1024)
  "Soft cap, in bytes, on the open metadata segment before it is sealed.
Checked after each insert's whole batch, never split, so one may overshoot it."
  :group 'org-glance
  :type 'integer)

(defcustom org-glance-graph-compact-segment-count 4
  "Sealed-segment count that triggers automatic compaction.
A very large value disables it; then use \\[org-glance-graph-compact]."
  :group 'org-glance
  :type 'integer)

(cl-defstruct (org-glance-headline-metadata (:predicate org-glance-headline-metadata?)
                                               (:conc-name org-glance-headline-metadata:))
  (id nil :read-only t :type string)
  (state nil :read-only t :type string)
  (title nil :read-only t :type string)
  (tags nil :read-only t :type list)
  (hash nil :read-only t :type string)
  (schedule nil :read-only t :type string)
  (deadline nil :read-only t :type string)
  (priority nil :read-only t :type number)
  (linked? nil :read-only t :type boolean)
  (propertized? nil :read-only t :type boolean)
  (encrypted? nil :read-only t :type boolean)
  (relations nil :read-only t :type list)
  (links nil :read-only t :type list)
  (archived? nil :read-only t :type boolean)
  (commented? nil :read-only t :type boolean)
  (range nil :read-only t :type list))

(defconst org-glance-headline-metadata:fields
  ;; SLOT          JSON-KEY      FROM-HEADLINE                                                ENCODE       DECODE
  `((id            :id           ,#'org-glance-headline:id                                 nil          nil)
    (state         :state        ,#'org-glance-headline:state                              nil          nil)
    (title         :title        ,#'org-glance-headline:title                              nil          nil)
    (tags          :tags         ,#'org-glance-headline:tags                               strings-vector strings-list)
    (hash          :hash         :hash                                                     nil          nil)
    (schedule      :schedule     ,#'org-glance-headline:schedule                           nil          nil)
    (deadline      :deadline     ,#'org-glance-headline:deadline                           nil          nil)
    (priority      :priority     ,#'org-glance-headline:priority                           nil          nil)
    (linked?       :linked       :linked                                                   nil          bool)
    (propertized?  :propertized  :propertized                                              nil          bool)
    (encrypted?    :encrypted    :encrypted                                                nil          bool)
    (relations     :relations    :relations                                                edges-vector edges-list)
    (links         :links        :links                                                    strings-vector strings-list)
    (archived?     :archived     ,#'org-glance-headline:archived?                          nil          bool)
    (commented?    :commented    ,#'org-glance-headline:commented?                         nil          bool)
    (range         :range        :range                                                    strings-vector strings-list))
  "Single source of truth for the metadata projection's shape (invariant 4).
Drives `org-glance-headline:metadata', `serialize' and `deserialize'; a new
field is one row here plus one struct slot, checked at load.  FROM-HEADLINE is
a headline function or a keyword naming an `org-glance-headline--content-facts'
fact.  Row order is the JSON key order: append new fields at the end.")

(cl-defun org-glance-headline-metadata--check-fields (slots fields)
  "Signal unless FIELDS is a valid field table for struct SLOTS; else return t.
Checks slot ORDER, known FROM facts and vector ENCODE kinds (invariant 4).
SLOTS is `cl-struct-slot-info' minus its tag slot; runs at load."
  (let ((struct-slots (mapcar #'car slots))
        (table-slots (mapcar #'car fields)))
    (unless (equal struct-slots table-slots)
      (error "org-glance: metadata field table out of sync with the struct: %S vs %S"
             table-slots struct-slots))
    (cl-loop for (slot _json from encode) in fields
             for type = (plist-get (cddr (assq slot slots)) :type)
             do (when (and (keywordp from)
                           (not (memq from org-glance-headline--content-fact-keys)))
                  (error "org-glance: metadata field %S reads unknown content fact %S"
                         slot from))
                (when (and (eq type 'list) (not (memq encode '(strings-vector edges-vector))))
                  (error "org-glance: list-valued metadata field %S needs a vector ENCODE kind, got %S"
                         slot encode)))
    t))

(org-glance-headline-metadata--check-fields
 (cdr (cl-struct-slot-info 'org-glance-headline-metadata))
 org-glance-headline-metadata:fields)

(cl-defun org-glance-headline-metadata--encode (kind value)
  "Coerce VALUE for serialization per ENCODE kind KIND.
Lists become vectors, the only shape `json-serialize' writes as an array."
  (pcase kind
    ('strings-vector (apply #'vector (org-glance--strings value)))
    ('edges-vector (apply #'vector
                          (mapcar (lambda (e) (if (cdr e) (vector (car e) (cdr e))
                                             (vector (car e))))
                                  value)))
    (_ value)))

(cl-defun org-glance-headline-metadata--decode (kind value)
  "Coerce deserialized VALUE per DECODE kind KIND.
Edge kinds canonicalize to dash-slugs here (invariant 13)."
  (pcase kind
    ('bool (eq t value))                ; JSON false/null both read as nil
    ('strings-list (append value nil))
    ('edges-list (cl-loop for edge across (or value [])
                          for e = (append edge nil)
                          collect (cons (car e)
                                        (when (cadr e) (org-glance--kind-slug (cadr e))))))
    (_ value)))

(cl-defun org-glance-headline:metadata (headline)
  (cl-check-type headline org-glance-headline)
  (let ((facts (org-glance-headline--content-facts headline)))
    (apply #'make-org-glance-headline-metadata
           (cl-loop for (slot _json from) in org-glance-headline-metadata:fields
                    append (list (intern (concat ":" (symbol-name slot)))
                                 (if (keywordp from)
                                     (plist-get facts from)
                                   (funcall from headline)))))))

(cl-defun org-glance-headline:metadata* (obj)
  "Return OBJ's metadata, whether OBJ is metadata or a headline."
  (cl-typecase obj
    (org-glance-headline-metadata obj)
    (org-glance-headline (org-glance-headline:metadata obj))))

(cl-defun org-glance-headline-metadata:tag-strings (metadata)
  "Return METADATA's tags as distinct downcased strings (invariant 13).
Total over fresh symbol tags and deserialized strings, even in a vector."
  (delete-dups
   (mapcar #'org-glance--downcased-string
           (append (org-glance-headline-metadata:tags metadata) nil))))

(cl-defun org-glance-headline-metadata:relation-targets (metadata)
  "Return METADATA's distinct relation target ids, the `car' of each edge."
  (delete-dups (mapcar #'car (org-glance-headline-metadata:relations metadata))))

(cl-defun org-glance-headline-metadata:serialize* (obj)
  "Return OBJ as a record plist, serializing metadata; signal on anything else."
  (cl-typecase obj
    (org-glance-headline-metadata (org-glance-headline-metadata:serialize obj))
    (list obj)
    (t (error "Unable to determine object spec: %s" (prin1-to-string obj)))))

(cl-defun org-glance-headline-metadata:serialize (metadata)
  (cl-check-type metadata org-glance-headline-metadata)
  (cl-loop for (slot json _from encode) in org-glance-headline-metadata:fields
           for value = (cl-struct-slot-value 'org-glance-headline-metadata slot metadata)
           append (list json (org-glance-headline-metadata--encode encode value))))

(cl-defun org-glance-headline-metadata:deserialize (data)
  (cl-check-type data list)
  (apply #'make-org-glance-headline-metadata
         (cl-loop for (slot json _from _encode decode) in org-glance-headline-metadata:fields
                  append (list (intern (concat ":" (symbol-name slot)))
                               (org-glance-headline-metadata--decode decode (plist-get data json))))))

(cl-defun org-glance-headline-metadata:done? (metadata)
  "Non-nil if METADATA's state is a done keyword (per `org-done-keywords')."
  (cl-check-type metadata org-glance-headline-metadata)
  (not (null (member (org-glance-headline-metadata:state metadata) org-done-keywords))))

(cl-defun org-glance-headline-metadata:active? (metadata)
  "Non-nil if METADATA is not done, per its always-present `state'."
  (cl-check-type metadata org-glance-headline-metadata)
  (not (org-glance-headline-metadata:done? metadata)))

(cl-defun org-glance--done-keywords ()
  "Return `org-done-keywords', or derive it when unset outside an Org buffer.
The fallback reads the global `org-todo-keywords' in a scratch Org buffer.
Callers bind `org-done-keywords' to this around `done?'/`active?' checks; a
view's `:done-keywords' filter clause overrides it."
  (or org-done-keywords
      (with-temp-buffer
        (delay-mode-hooks (org-mode))
        org-done-keywords)))

(cl-defun org-glance-graph--path (graph key thunk)
  "Return GRAPH's path for KEY, computed once by THUNK and memoized.
Store paths depend only on the read-only `directory' slot."
  (or (plist-get (org-glance-graph:-paths graph) key)
      (let ((v (funcall thunk)))
        (setf (org-glance-graph:-paths graph)
              (plist-put (org-glance-graph:-paths graph) key v))
        v)))

(defvar org-glance-graph-after-open-functions nil
  "Abnormal hook run with GRAPH once it is freshly constructed and WAL-healed.
Runs at the end of `org-glance-graph', after caching the instance; side indexes
heal their own git conflicts here.  Errors are demoted (invariant 9).")

(cl-defun org-glance-graph (&optional (directory org-glance-directory))
  (cl-check-type directory string)
  (let* ((directory (-> directory (file-truename) (f-full)))
         (graph (gethash directory org-glance-graph:list)))
    (unless graph
      (setq graph (make-org-glance-graph :directory directory))
      (f-mkdir-full-path (org-glance-graph:data-path graph))
      (f-mkdir-full-path (org-glance-graph:meta-path graph))
      (f-touch (org-glance-graph:headline-meta-path graph))
      (org-glance-graph--ensure-gitattributes graph)    ; git union merge for *.jsonl
      (org-glance-graph--ensure-gitignore graph)         ; cache/ is per-machine
      (org-glance-graph--resolve-jsonl-conflicts graph)  ; heal markers a pre-driver sync left
      (org-glance-graph--migrate-maybe graph)      ; bootstrap MANIFEST / adopt legacy file
      (org-glance-graph--reconcile-manifest graph) ; rebuild a git-mangled MANIFEST
      (org-glance-graph--heal graph)               ; recover seal, derive seq, reap orphans
      (puthash directory graph org-glance-graph:list)
      (with-demoted-errors "org-glance: after-open hook: %S"
        (run-hook-with-args 'org-glance-graph-after-open-functions graph)))
    graph))

(cl-defun org-glance-graph:insert (graph meta)
  (declare (indent 1))
  (cl-check-type meta list)
  (cl-check-type (car meta) (or list org-glance-headline-metadata))
  (org-glance-graph--append graph meta))

;;; Segmented (LSM-lite) metadata store

(cl-defun org-glance-graph--open-segment-path (graph)
  "Return GRAPH's open append segment, which is `headline-meta-path'."
  (org-glance-graph:headline-meta-path graph))

(cl-defun org-glance-graph--manifest-path (graph)
  (org-glance-graph--path
   graph :manifest
   (lambda () (-> (f-join (org-glance-graph:meta-path graph) "MANIFEST") (file-truename)))))

(defconst org-glance-graph--segment-stem "seg-"
  "Prefix of a sealed segment's basename.
`--segment-path', `--segment-name-re' and `--ensure-gitattributes' use it.")

(cl-defun org-glance-graph--segment-path (graph gen)
  (-> (f-join (org-glance-graph:meta-path graph)
              (format "%s%010d.jsonl" org-glance-graph--segment-stem gen))
      (file-truename)))

(defconst org-glance-graph--segment-name-re
  (concat "\\`" (regexp-quote org-glance-graph--segment-stem)
          "\\([0-9]+\\)\\.jsonl\\'")
  "Matches a sealed segment basename; group 1 is its generation number.")

(cl-defun org-glance-graph--segment-generation (name)
  "Return the generation of sealed segment basename NAME, or nil."
  (when (string-match org-glance-graph--segment-name-re name)
    (string-to-number (match-string 1 name))))

(cl-defun org-glance-graph--sealed-segments (graph)
  "Return GRAPH's live sealed segment basenames, oldest first, per the MANIFEST.
An absent or unparseable MANIFEST reads as none."
  (let ((path (org-glance-graph--manifest-path graph)))
    (when (f-exists? path)
      (condition-case nil
          (-> (f-read-text path 'utf-8)
              (json-parse-string :object-type 'plist)
              (plist-get :segments)
              (append nil))
        (error nil)))))

(cl-defun org-glance-graph--write-manifest (graph segments)
  "Atomically write SEGMENTS, basenames oldest first, as GRAPH's MANIFEST.
This swap commits the live sealed-segment set (invariant 2)."
  (org-glance--atomic-write
   (org-glance-graph--manifest-path graph)
   (concat (json-serialize (list :version 2 :segments (apply #'vector segments))) "\n")))

(cl-defun org-glance-graph--live-segments (graph &optional newest-first)
  "Return GRAPH's existing live segment paths, oldest first and open last.
NEWEST-FIRST reverses the order.  Orphans outside the MANIFEST are omitted."
  (let* ((meta (org-glance-graph:meta-path graph))
         (sealed (cl-loop for name in (org-glance-graph--sealed-segments graph)
                          for p = (file-truename (f-join meta name))
                          when (f-exists? p) collect p))
         (open (org-glance-graph--open-segment-path graph))
         (all (append sealed (when (f-exists? open) (list open)))))
    (if newest-first (reverse all) all)))

(cl-defun org-glance-graph--segment-names (graph)
  "Return GRAPH's sealed segment basenames on disk, listed or not."
  (directory-files (org-glance-graph:meta-path graph) nil
                   org-glance-graph--segment-name-re))

(cl-defun org-glance-graph--next-generation (graph)
  "Return 1 + the highest sealed generation among GRAPH's segment filenames."
  (1+ (or (cl-loop for f in (org-glance-graph--segment-names graph)
                   maximize (org-glance-graph--segment-generation f))
          0)))

(cl-defun org-glance-graph--scan-file (graph path fn)
  "Call FN on each non-empty UTF-8 JSON record in GRAPH's segment PATH, top-down.
A parse error is ignored on the open segment's final line only (invariant 32)."
  (when (f-exists? path)
    (let ((tolerate-torn (string= path (org-glance-graph--open-segment-path graph))))
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents path))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (unless (string-empty-p line)
              (condition-case err
                  (funcall fn (json-parse-string line :object-type 'plist))
                (json-error
                 (unless (and tolerate-torn (= (line-end-position) (point-max)))
                   (signal (car err) (cdr err)))))))
          (forward-line 1))))))

(cl-defun org-glance-graph--scan-forward (graph fn)
  "Call FN on every record in GRAPH's live segments, oldest first, open last."
  (dolist (seg (org-glance-graph--live-segments graph))
    (org-glance-graph--scan-file graph seg fn)))

(cl-defun org-glance-graph--latest-records (graph)
  "Return (RECORDS . TOTAL): the latest record per id, first-sighting order.
RECORDS includes tombstones (invariant 1); TOTAL counts every raw record in
GRAPH's live segments."
  (let ((latest (make-hash-table :test 'equal))
        (order nil)
        (total 0))
    (org-glance-graph--scan-forward
     graph (lambda (record)
             (cl-incf total)
             (let ((id (plist-get record :id)))
               (unless (gethash id latest) (push id order))
               (puthash id record latest))))
    (cons (cl-loop for id in (nreverse order) collect (gethash id latest))
          total)))

;;; In-memory read cache

(cl-defun org-glance-graph--store-snapshot (graph)
  "Return GRAPH's store state as (OPEN-MTIME OPEN-SIZE SEALED-SEGMENTS).
Two snapshots compare with `equal'.  The open segment's mtime and size catch an
append or a reset; the sealed segment NAMES, read fresh from the MANIFEST, catch
a seal or a compaction on any clock (invariant 7)."
  (let ((oa (file-attributes (org-glance-graph--open-segment-path graph))))
    (list (and oa (file-attribute-modification-time oa))
          (and oa (file-attribute-size oa))
          (org-glance-graph--sealed-segments graph))))

(cl-defun org-glance-graph--invalidate-cache (graph)
  "Drop GRAPH's read cache; in-process mutations call this (invariant 7)."
  (setf (org-glance-graph:-meta-cache graph) nil))

(cl-defun org-glance-graph--patch-cache (graph records)
  "Fold just-appended RECORDS into GRAPH's read cache, then re-stamp its snapshot.
RECORDS are the plists `--append' wrote, in write order; a cold cache stays
cold.  An unknown id is pushed onto LIVE, which is newest-first; a live id is
replaced in place; a tombstone leaves LIVE and stays in BY-ID (invariant 30).
A re-added tombstoned id, its first-sighting slot gone, or any other doubt
invalidates the cache instead."
  (when-let* ((cache (org-glance-graph:-meta-cache graph)))
    (let ((by-id (plist-get cache :by-id))
          (live (plist-get cache :live))
          (bail nil))
      (cl-loop for record in records
               for id = (plist-get record :id)
               for prev = (gethash id by-id)
               until bail
               do (cond
                   ((and prev (plist-get prev :tombstone)
                         (not (plist-get record :tombstone)))
                    (setq bail t))
                   (t
                    (puthash id record by-id)
                    (cond
                     ((plist-get record :tombstone)
                      (setq live (cl-remove-if
                                  (lambda (m) (equal id (org-glance-headline-metadata:id m)))
                                  live)))
                     ((null prev)
                      (push (org-glance-headline-metadata:deserialize record) live))
                     (t
                      (let ((cell (cl-member id live :key #'org-glance-headline-metadata:id
                                             :test #'equal)))
                        (if cell
                            (setcar cell (org-glance-headline-metadata:deserialize record))
                          (setq bail t))))))))
      (if bail
          (org-glance-graph--invalidate-cache graph)
        (setf (org-glance-graph:-meta-cache graph)
              (list :snapshot (org-glance-graph--store-snapshot graph)
                    :by-id by-id
                    :live live))))))

(cl-defun org-glance-graph--ensure-cache (graph)
  "Return GRAPH's read cache, rebuilding it iff the store snapshot changed.
The cache is (:snapshot S :by-id HASH :live LIVE): BY-ID maps each id to its
latest record, tombstones included; LIVE is the deserialized live metadata,
newest first.  Pending external writes fold in first (`--fold-external-maybe'),
an external write moving a blob and never the WAL (invariant 33)."
  (org-glance-graph--fold-external-maybe graph)
  (let ((snap (org-glance-graph--store-snapshot graph))
        (cache (org-glance-graph:-meta-cache graph)))
    (unless (and cache (equal (plist-get cache :snapshot) snap))
      (let ((records (car (org-glance-graph--latest-records graph)))
            (by-id (make-hash-table :test 'equal))
            (live nil))
        (dolist (record records)
          (puthash (plist-get record :id) record by-id))
        (setq live (nreverse
                    (cl-loop for record in records
                             unless (plist-get record :tombstone)
                             collect (org-glance-headline-metadata:deserialize record))))
        (setf cache (list :snapshot snap :by-id by-id :live live)
              (org-glance-graph:-meta-cache graph) cache)))
    cache))

(cl-defun org-glance-graph--max-seq (graph)
  "Return the highest `seq' in GRAPH's records, or 0 when none has one."
  (let ((mx 0))
    (org-glance-graph--scan-forward
     graph (lambda (r) (let ((s (plist-get r :seq)))
                         (when (and (integerp s) (> s mx)) (setq mx s)))))
    mx))

(cl-defun org-glance-graph--ensure-newline-terminated (path)
  "Drop PATH's newline-less final line, rewriting PATH only when there is one."
  (let ((size (org-glance--file-size path)))
    (when (> size 0)
      (let ((last-byte (with-temp-buffer
                         (org-glance--insert-bytes path (1- size) size)
                         (char-after (point-min)))))
        (unless (eql last-byte ?\n)
          (with-temp-buffer
            (org-glance--insert-bytes path)
            (goto-char (point-max))
            (if (search-backward "\n" nil t)
                (delete-region (1+ (point)) (point-max))
              (erase-buffer))
            (let ((coding-system-for-write 'no-conversion))
              (write-region (point-min) (point-max) path nil 'silent))))))))

(defvar org-glance-graph-before-append-functions nil
  "Abnormal hook run with GRAPH and SPECS just before SPECS append to the WAL.
SPECS are metadata structs and tombstone plists; the read cache still shows the
pre-append state.  Errors are demoted (invariant 9).")

(defvar org-glance-graph-after-append-functions nil
  "Abnormal hook run with GRAPH and SPECS after SPECS reach the WAL.
Side indexes update here when they need the graph's new read-cache state.")

(cl-defun org-glance-graph--append (graph specs)
  "Append SPECS to GRAPH's open segment, then maybe seal and compact.
SPECS are metadata structs or bare plists; each gets a fresh monotonic `seq'."
  (with-demoted-errors "org-glance: before-append hook: %S"
    (run-hook-with-args 'org-glance-graph-before-append-functions graph specs))
  (let ((open (org-glance-graph--open-segment-path graph))
        (written nil))
    (org-glance-graph--ensure-newline-terminated open)
    (cl-loop for spec in specs
             for record = (plist-put (copy-sequence (org-glance-headline-metadata:serialize* spec))
                                     :seq (cl-incf (org-glance-graph:seq graph)))
             collect record into records
             collect (json-serialize record) into jsons
             finally (progn (f-append-text (concat (s-join "\n" jsons) "\n") 'utf-8 open)
                            (setq written records)))
    ;; Seal/compact first: compaction REWRITES records; no patch expresses that.
    (org-glance-graph--maybe-seal graph)
    (org-glance-graph--maybe-compact graph)
    (org-glance-graph--patch-cache graph written)
    (with-demoted-errors "org-glance: after-append hook: %S"
      (run-hook-with-args 'org-glance-graph-after-append-functions graph specs))))

(cl-defun org-glance-graph--maybe-seal (graph)
  (let ((open (org-glance-graph--open-segment-path graph)))
    (when (> (org-glance--file-size open) org-glance-graph-segment-max-bytes)
      (org-glance-graph--seal graph))))

(cl-defun org-glance-graph--seal (graph)
  "Seal GRAPH's full open segment into an immutable seg-<gen>.
The MANIFEST swap commits; a crash before it leaves an unlisted seg-<gen> that
`--heal' adopts.  Leaves the read cache to its caller, `--append'."
  (let* ((open (org-glance-graph--open-segment-path graph))
         (sealed (org-glance-graph--segment-path graph (org-glance-graph--next-generation graph))))
    (rename-file open sealed)           ; atomic: a complete file becomes immutable
    (f-touch open)                      ; fresh empty open segment (bumps the signal mtime)
    (org-glance-graph--write-manifest
     graph (append (org-glance-graph--sealed-segments graph)
                   (list (file-name-nondirectory sealed))))))

(cl-defun org-glance-graph--maybe-compact (graph)
  (when (>= (length (org-glance-graph--sealed-segments graph))
            org-glance-graph-compact-segment-count)
    (org-glance-graph:compact graph)))

(cl-defun org-glance-graph:store-path (graph)
  "Return GRAPH's hidden store root, dot-prefixed to keep `org-agenda' out."
  (cl-check-type graph org-glance-graph)
  (org-glance-graph--path
   graph :store
   (lambda () (-> (f-join (org-glance-graph:directory graph) ".org-glance") (file-truename)))))

(cl-defun org-glance-graph:data-path (graph)
  (cl-check-type graph org-glance-graph)
  (org-glance-graph--path
   graph :data
   (lambda () (-> (f-join (org-glance-graph:store-path graph) "data") (file-truename)))))

(cl-defun org-glance-graph:meta-path (graph)
  (cl-check-type graph org-glance-graph)
  (org-glance-graph--path
   graph :meta
   (lambda () (-> (f-join (org-glance-graph:store-path graph) "meta") (file-truename)))))

(cl-defun org-glance-graph:config-file (graph name)
  "Return the path of GRAPH's synced config sidecar NAME, which may not exist.
User-authored state lives under `config/', derived caches under `cache/'."
  (cl-check-type graph org-glance-graph)
  (f-join (org-glance-graph:store-path graph) "config" name))

(cl-defun org-glance-graph:cache-path (graph)
  "Return GRAPH's `cache/' directory, deletable whole (invariant 5)."
  (cl-check-type graph org-glance-graph)
  (f-join (org-glance-graph:store-path graph) "cache"))

(cl-defun org-glance-graph:cache-read (graph name)
  "Read GRAPH's derived-cache sidecar NAME, or nil when absent.
Public sidecar API with `org-glance-graph:cache-write' (invariant 23)."
  (let ((path (org-glance-graph:cache-file graph name)))
    (when (f-exists? path) (org-glance--read-eld path))))

(cl-defun org-glance-graph:cache-write (graph name value)
  "Persist VALUE as GRAPH's derived-cache sidecar NAME; return VALUE."
  (org-glance--write-eld (org-glance-graph:cache-file graph name) value)
  value)

(cl-defun org-glance-graph:cache-file (graph name)
  "Return the path of GRAPH's derived-cache sidecar NAME under `cache/'.
The directory is per-machine and git-ignored (`--ensure-gitignore')."
  (cl-check-type graph org-glance-graph)
  (f-join (org-glance-graph:cache-path graph) name))

(cl-defun org-glance-graph:headline-meta-path (graph)
  (cl-check-type graph org-glance-graph)
  (org-glance-graph--path
   graph :headline-meta
   (lambda () (-> (f-join (org-glance-graph:meta-path graph) "headlines.jsonl") (file-truename)))))

(cl-defun org-glance-graph:headline-data-path (graph id)
  "Return the directory of ID's data blob in GRAPH; signal on an unsafe ID.
Ids longer than two characters shard by their first two."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  ;; invariant 6: path-check with `error' -- `cl-assert' can be compiled out.
  (when (or (string-empty-p id) (string-match-p "/" id) (string-match-p "\\.\\." id))
    (error "Unsafe ORG_GLANCE_ID for content-addressable path: %S" id))
  (let ((data (org-glance-graph:data-path graph)))
    (file-truename
     (if (> (length id) 2)
         (f-join data (substring id 0 2) (substring id 2))
       (f-join data id)))))

(cl-defun org-glance-graph:content-path (graph id)
  "Return the path of ID's content blob in GRAPH's data store."
  (f-join (org-glance-graph:headline-data-path graph id) "data.org"))

(cl-defun org-glance-graph:put-content (graph headline)
  "Persist HEADLINE's contents atomically under GRAPH's store, keyed by its id.
Return the file path, or nil if HEADLINE has no id."
  (cl-check-type graph org-glance-graph)
  (cl-check-type headline org-glance-headline)
  (when-let* ((id (org-glance-headline:id headline)))
    (let ((dir (org-glance-graph:headline-data-path graph id))
          (path (org-glance-graph:content-path graph id)))
      (f-mkdir-full-path dir)
      (org-glance--atomic-write path (org-glance-headline:contents headline))
      path)))

(cl-defun org-glance-graph:get-content (graph id)
  "Return ID's stored contents in GRAPH, even if tombstoned, or nil if none."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (let ((path (org-glance-graph:content-path graph id)))
    (when (f-exists? path)
      (f-read-text path 'utf-8))))

(cl-defun org-glance-graph:make-id (graph)
  (cl-check-type graph org-glance-graph)
  (cl-loop while t
           for id = (org-id-uuid)
           for data-path = (org-glance-graph:headline-data-path graph id)
           unless (f-exists? data-path)
           return (prog1 id (f-mkdir-full-path data-path))))

(cl-defun org-glance-graph:add (graph &rest headlines)
  "Add HEADLINES, each a headline or pre-built metadata, to GRAPH; return GRAPH.
Full headlines also persist their contents (invariant 5)."
  (cl-check-type graph org-glance-graph)
  (when headlines
    ;; invariant 5: metadata FIRST, so a projection error writes nothing.
    (let ((specs (mapcar #'org-glance-headline:metadata* headlines)))
      (dolist (headline headlines)
        (when (org-glance-headline? headline)
          (org-glance-graph:put-content graph headline)))
      (org-glance-graph:insert graph specs)))
  graph)

(cl-defun org-glance-graph:get-headline (graph id)
  "Return the most recent metadata for ID in GRAPH, from the read cache.
Return `tombstone' if ID was deleted, or nil if unknown (invariant 30)."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (let ((record (gethash id (plist-get (org-glance-graph--ensure-cache graph) :by-id))))
    (cond ((null record) nil)
          ((plist-get record :tombstone) 'tombstone)
          (t (org-glance-headline-metadata:deserialize record)))))

(cl-defun org-glance-graph:live-meta (graph id)
  "Return GRAPH's metadata for ID, or nil when unknown or tombstoned.
The read-only collapse of `org-glance-graph:get-headline' (invariant 30)."
  (let ((meta (org-glance-graph:get-headline graph id)))
    (and (org-glance-headline-metadata? meta) meta)))

(cl-defun org-glance-graph:headline (graph id)
  "Return the live `org-glance-headline' stored for ID in GRAPH, or nil.
Parses the stored contents; nil for an unknown or tombstoned ID."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (when (org-glance-graph:live-meta graph id)
    (-some-> (org-glance-graph:get-content graph id)
      (org-glance-headline--from-string))))

(cl-defun org-glance-graph--tombstone-spec (graph id)
  "Return the record deleting ID in GRAPH, or nil if it is unknown or deleted.
The guard `org-glance-graph:delete' shares with the fold (invariant 30)."
  (unless (memq (org-glance-graph:get-headline graph id) '(nil tombstone))
    (list :id id :tombstone t)))

(cl-defun org-glance-graph:delete (graph id)
  "Append a tombstone for ID to GRAPH unless ID is absent or already deleted."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (when-let* ((spec (org-glance-graph--tombstone-spec graph id)))
    (org-glance-graph:insert graph (list spec))))

(cl-defun org-glance-graph:headlines (graph)
  "Return all live headline metadata in GRAPH, latest record per id.
Ordered by first insertion.  The list is fresh, so callers may sort it
destructively; the read-only structs are shared with the cache."
  (cl-check-type graph org-glance-graph)
  (reverse (plist-get (org-glance-graph--ensure-cache graph) :live)))

(cl-defun org-glance-graph--distinct (graph extract &optional ids)
  "Return the sorted distinct strings EXTRACT yields over GRAPH's live metadata.
EXTRACT returns a list per metadata; IDS, when non-nil, limits the fold."
  (cl-check-type graph org-glance-graph)
  (org-glance--sorted-distinct
   (cl-loop for meta in (org-glance-graph--metas graph ids)
            append (funcall extract meta))))

(cl-defun org-glance-graph:tags (graph)
  "Return the sorted distinct tags across GRAPH's live headlines."
  (org-glance-graph--distinct graph #'org-glance-headline-metadata:tag-strings))

(cl-defun org-glance-graph:states (graph)
  "Return the sorted distinct non-empty todo states of GRAPH's live headlines."
  (org-glance-graph--distinct
   graph (lambda (meta)
           (let ((state (org-glance-headline-metadata:state meta)))
             (when (org-glance--present-string? state) (list state))))))

(defconst org-glance--org-repeater-re "[.+]\\+?0*[1-9][0-9]*[hdwmy]"
  "Matches a nonzero org repeater cookie, such as +1d, ++1w or .+2m.")

(cl-defun org-glance-headline-metadata:repeated? (metadata)
  "Non-nil when METADATA's schedule or deadline carries a repeater cookie.
Repeaters on body-only timestamps are invisible here."
  (cl-check-type metadata org-glance-headline-metadata)
  (cl-some (lambda (ts) (and ts (string-match-p org-glance--org-repeater-re ts)))
           (list (org-glance-headline-metadata:schedule metadata)
                 (org-glance-headline-metadata:deadline metadata))))

(cl-defun org-glance-graph:occurrences-path (graph id)
  "Return the directory of ID's occurrence snapshots in GRAPH, maybe absent.
Holds one immutable `<STAMP>.org' per completed repetition (invariant 5)."
  (f-join (org-glance-graph:headline-data-path graph id) "occurrences"))

(cl-defun org-glance-graph:occurrences (graph id)
  "Return ID's occurrence snapshots in GRAPH as (STAMP . PATH), newest first.
The filenames are the index; STAMP sorts lexically."
  (let ((dir (org-glance-graph:occurrences-path graph id)))
    (when (f-exists? dir)
      (sort (mapcar (lambda (f) (cons (f-base f) f)) (f-files dir))
            (lambda (a b) (string> (car a) (car b)))))))

(cl-defun org-glance-graph--metas (graph &optional ids)
  "Return GRAPH's live metadata, or just that of IDS, skipping unknown ones."
  (if ids
      (delq nil (mapcar (lambda (id) (org-glance-graph:live-meta graph id)) ids))
    (org-glance-graph:headlines graph)))

(cl-defun org-glance-graph:edge-kinds (graph &optional ids)
  "Return the sorted distinct relation kinds across GRAPH's live headlines.
With IDS, restrict the fold to those headlines."
  (org-glance-graph--distinct
   graph (lambda (meta) (-keep #'cdr (org-glance-headline-metadata:relations meta)))
   ids))

(cl-defun org-glance-graph:title-or-id (graph id)
  "Return ID's headline title in GRAPH, or ID itself when the headline is gone."
  (if-let* ((meta (org-glance-graph:live-meta graph id)))
      (org-glance-headline-metadata:title meta)
    id))

(defconst org-glance-graph--reindex-batch 500
  "Headlines per metadata-append batch during `org-glance-graph:reindex'.")

(cl-defun org-glance-graph:reindex (graph)
  "Re-derive metadata for every live headline in GRAPH from its stored blob.
Blobs are only read (invariant 5); records append in batches, compacting once
at the end.  Return the number of headlines re-indexed."
  (cl-check-type graph org-glance-graph)
  (let* ((metas (org-glance-graph:headlines graph))
         (reporter (and metas (make-progress-reporter "org-glance: re-indexing... " 0 (length metas))))
         (org-glance-graph-compact-segment-count most-positive-fixnum)
         (batch nil) (fill 0) (n 0))
    (cl-flet ((flush ()
                (when batch
                  (org-glance-graph:insert graph (nreverse batch))
                  (setq batch nil fill 0))))
      (cl-loop for meta in metas
               for i from 1
               for id = (org-glance-headline-metadata:id meta)
               for contents = (org-glance-graph:get-content graph id)
               when contents
               do (push (org-glance-headline:metadata*
                         (org-glance-headline--from-string contents))
                        batch)
                  (cl-incf n)
                  (when (= org-glance-graph--reindex-batch (cl-incf fill)) (flush))
               end
               do (when reporter (progress-reporter-update reporter i))
               finally (flush)))
    (org-glance-graph:compact graph)
    (when reporter (progress-reporter-done reporter))
    n))

;;; External writers: `meta/EXTERNAL.jsonl' -- invariants 33-34, hazards H2-H3

(defconst org-glance-graph--external-name "EXTERNAL.jsonl"
  "Basename of the `meta/' file listing ids an external writer moved or deleted.
Its line format is frozen with glance's `Data.Org.External' (invariant 33).")

(defconst org-glance-graph--external-generation-stem "EXTERNAL-"
  "Prefix of a rotated notification generation's basename.
`--external-generation-path' and `--external-generation-re' build on it.")

(defconst org-glance-graph--external-generation-re
  (concat "\\`" (regexp-quote org-glance-graph--external-generation-stem)
          "\\([0-9]+\\)\\.jsonl\\'")
  "Match a rotated notification generation; group 1 is its number.")

(cl-defun org-glance-graph:external-path (graph)
  "Path of GRAPH's external-write notification file (may not exist)."
  (cl-check-type graph org-glance-graph)
  (org-glance-graph--path
   graph :external
   (lambda () (f-join (org-glance-graph:meta-path graph)
                 org-glance-graph--external-name))))

(cl-defun org-glance-graph--external-cursor-path (path)
  "Return the path of notification file PATH's cursor, named off PATH."
  (concat (file-name-sans-extension path) ".cursor"))

(cl-defun org-glance-graph--external-generation-path (graph gen)
  "Return the path of GRAPH's rotated notification generation GEN.
GEN is zero-padded, so name order is generation order."
  (f-join (org-glance-graph:meta-path graph)
          (format "%s%010d.jsonl" org-glance-graph--external-generation-stem gen)))

(defconst org-glance-graph--external-window-bytes 4096
  "Bytes before a cursor's offset that the pending check hashes.
It keeps the poll's cost flat in file size; free to shrink, constrained upward
by invariant 34.")

(cl-defun org-glance-graph--external-digest (beg end)
  "Return the SHA-1 of the current unibyte buffer's bytes from BEG to END.
Every cursor digest is minted and verified here."
  (secure-hash 'sha1 (current-buffer) beg end))

(cl-defun org-glance-graph--external-window-beg (offset)
  "Return the first byte of the window a cursor at OFFSET is verified over.
The fold's minting and the poll's reading must both ask here."
  (max 0 (- offset org-glance-graph--external-window-bytes)))

(cl-defun org-glance-graph--external-digests (offset)
  "Return the digest plist a cursor at OFFSET records over the current buffer.
The buffer holds the notification file whole and unibyte.  `:window' hashes
from `--external-window-beg' OFFSET to OFFSET, `:prefix' from 0 to OFFSET.  The
range clamps, so a caller must refuse an offset past the end first."
  (let* ((end (min (+ (point-min) offset) (point-max)))
         (beg (min (+ (point-min) (org-glance-graph--external-window-beg offset))
                   end)))
    (list :window (org-glance-graph--external-digest beg end)
          :prefix (org-glance-graph--external-digest (point-min) end))))

(cl-defun org-glance-graph--external-bytes (path fn &optional refused)
  "Call FN in a unibyte buffer holding notification file PATH; return its value.
Return REFUSED without calling FN when PATH cannot be read whole: absent,
unreadable, a directory or a dangling symlink."
  (with-temp-buffer
    (if (org-glance--insert-bytes path)
        (funcall fn)
      refused)))

(cl-defun org-glance-graph--external-cursor (path)
  "Return notification file PATH's cursor plist (invariant 34), or nil.
The plist holds `:offset', `:window' and `:prefix'.  Nil when the cursor is
absent, unreadable, garbled, short of two digests or at a negative offset."
  (let ((cursor (org-glance-graph--external-cursor-path path)))
    (pcase (condition-case nil
               (split-string (s-trim (f-read-text cursor 'utf-8)) nil t)
             (file-error nil))
      (`(,offset ,window ,prefix)
       (let ((n (string-to-number offset)))
         (and (integerp n) (>= n 0)
              (list :offset n :window window :prefix prefix))))
      (_ nil))))

(cl-defun org-glance-graph--external-window (path offset)
  "Return the digest of notification file PATH's window up to OFFSET, or nil.
Reads only the window.  Nil when the read comes up short or the byte before
OFFSET is not a newline, an assertion kept off the fold path (invariant 34)."
  (let ((beg (org-glance-graph--external-window-beg offset)))
    (with-temp-buffer
      (org-glance--insert-bytes path beg offset)
      (and (= (- (point-max) (point-min)) (- offset beg))
           (or (= offset 0) (eq ?\n (char-before (point-max))))
           (org-glance-graph--external-digest (point-min) (point-max))))))

(cl-defun org-glance-graph--external-folded-in
    (path &optional (cursor (org-glance-graph--external-cursor path)))
  "Return how many bytes of notification file PATH are folded, per CURSOR.
The current buffer holds PATH whole and unibyte; CURSOR defaults to PATH's own.
Return 0 unless the offset lies within the buffer and both recorded digests
match `--external-digests' there (invariant 34)."
  (or (and cursor
           (let ((offset (plist-get cursor :offset)))
             (and (<= offset (- (point-max) (point-min)))
                  (let ((have (org-glance-graph--external-digests offset)))
                    (and (string= (plist-get cursor :window)
                                  (plist-get have :window))
                         (string= (plist-get cursor :prefix)
                                  (plist-get have :prefix))
                         offset)))))
      0))

(cl-defun org-glance-graph--external-folded (path)
  "Read notification file PATH and return how many of its bytes are folded.
An unreadable file has folded 0.  `glance''s interop harness calls this; no
production path does."
  (org-glance-graph--external-bytes
   path (lambda () (org-glance-graph--external-folded-in path)) 0))

(cl-defun org-glance-graph--external-drained?
    (path &optional (cursor (org-glance-graph--external-cursor path)))
  "Non-nil when notification file PATH is folded to its end, judged by a window.
CURSOR defaults to PATH's own.  Requires the offset to be PATH's size, a
newline before it and the window to hash as recorded; with no cursor, an empty
PATH.  Bounded, so nothing destructive may ask it (invariant 34)."
  (if (null cursor)
      (= 0 (org-glance--file-size path))
    (let ((offset (plist-get cursor :offset)))
      (and (= offset (org-glance--file-size path))
           (equal (plist-get cursor :window)
                  (org-glance-graph--external-window path offset))))))

(cl-defun org-glance-graph--external-folded-whole? (path)
  "Non-nil when every byte of notification file PATH is folded, exactly.
Rotation asks only this, retiring what it blesses and re-folding the rest.
Size and verified prefix come from one read, with no newline assertion; a
failed read answers nil."
  (org-glance-graph--external-bytes
   path
   (lambda ()
     (= (- (point-max) (point-min))
        (org-glance-graph--external-folded-in path)))
   nil))

(cl-defun org-glance-graph--external-refold (path)
  "Drop notification file PATH's cursor, so the next fold takes PATH whole.
Re-folding is idempotent (invariant 33); an absent cursor is no error."
  (ignore-errors (f-delete (org-glance-graph--external-cursor-path path))))

(cl-defun org-glance-graph--external-generation (name)
  "Return the generation number of rotated notification basename NAME, or nil."
  (when (string-match org-glance-graph--external-generation-re name)
    (string-to-number (match-string 1 name))))

(cl-defun org-glance-graph--external-generations (graph)
  "Return GRAPH's rotated notification generation basenames, oldest first."
  (directory-files (org-glance-graph:meta-path graph) nil
                   org-glance-graph--external-generation-re))

(cl-defun org-glance-graph--external-sources (graph)
  "Return GRAPH's notification files, generations oldest first, the live last.
The order drains each generation before the live file.  A spent generation
under `meta/spent/' is never listed."
  (let ((meta (org-glance-graph:meta-path graph)))
    (append (mapcar (lambda (name) (f-join meta name))
                    (org-glance-graph--external-generations graph))
            (list (org-glance-graph:external-path graph)))))

(cl-defun org-glance-graph--external-spent-path (graph &optional name)
  "Return GRAPH's spent-generation directory `meta/spent/', or NAME inside it.
Beside the generations, so retirement is one atomic rename (invariant 34)."
  (let ((dir (f-join (org-glance-graph:meta-path graph) "spent")))
    (if name (f-join dir name) dir)))

(cl-defun org-glance-graph--external-move (from to)
  "Rename FROM to TO, refusing to clobber; return non-nil when it moved.
Any `file-error' (FROM gone, TO present, unwritable) leaves both in place."
  (condition-case nil
      (progn (rename-file from to nil) t)
    (file-error nil)))

(cl-defun org-glance-graph--external-retire (graph name)
  "Move spent generation NAME out of GRAPH's live meta dir into `spent/'.
Moves the cursor first, then the file (invariant 34); nothing prunes
`spent/'.  Return non-nil when the file moved."
  (let ((meta (org-glance-graph:meta-path graph))
        (spent (org-glance-graph--external-spent-path graph)))
    (f-mkdir-full-path spent)
    (org-glance-graph--external-move
     (org-glance-graph--external-cursor-path (f-join meta name))
     (org-glance-graph--external-cursor-path (f-join spent name)))
    (org-glance-graph--external-move (f-join meta name) (f-join spent name))))

(cl-defun org-glance-graph:clear-spent-external (&optional (graph (org-glance-ensure-init)))
  "Delete GRAPH's spent notification generations; return how many files went.
Interactively, confirm first.  Nothing else prunes `meta/spent/'.  To re-fold a
suspect generation instead, move its `.jsonl' alone back into `meta/'."
  (interactive)
  (let* ((dir (org-glance-graph--external-spent-path graph))
         (files (when (f-directory? dir)
                  (directory-files dir nil directory-files-no-dot-files-regexp)))
         (n (length files)))
    (if (and files
             (or (not (called-interactively-p 'any))
                 (y-or-n-p (format "Delete %d spent notification file(s) under %s? "
                                   n dir))))
        (progn (f-delete dir t)
               (when (called-interactively-p 'any)
                 (message "org-glance: cleared %d spent notification file%s"
                          n (if (= n 1) "" "s")))
               n)
      0)))

(cl-defun org-glance-graph--set-external-cursor (path offset digests)
  "Record OFFSET, hashed as DIGESTS, as the folded prefix of file PATH.
Writes `OFFSET WINDOW PREFIX' atomically, the layout `--external-cursor' reads.
DIGESTS must come from the fold's own read (`--external-tail'); this hashes
nothing and is never guarded with `max' (invariant 34)."
  (org-glance--atomic-write
   (org-glance-graph--external-cursor-path path)
   (format "%d %s %s\n" offset
           (plist-get digests :window) (plist-get digests :prefix))))

(cl-defun org-glance-graph--external-tail
    (path &optional (cursor (org-glance-graph--external-cursor path)))
  "Return notification file PATH's unfolded bytes as (TEXT END DIGESTS), or nil.
CURSOR defaults to PATH's own.  END is PATH's size as this one read saw it, and
DIGESTS come from the same buffer.  Nothing owed returns (\"\" END), a failed
read nil; rotation tells the two apart."
  (org-glance-graph--external-bytes
   path
   (lambda ()
     (let ((size (- (point-max) (point-min)))
           (from (org-glance-graph--external-folded-in path cursor)))
       (if (>= from size)
           (list "" size)
         (list (decode-coding-string
                (buffer-substring-no-properties (+ (point-min) from) (point-max))
                'utf-8)
               size
               (org-glance-graph--external-digests size)))))
   nil))

(cl-defun org-glance-graph--external-survey (graph)
  "Return one (PATH DRAINED TAIL) reading per notification source of GRAPH.
DRAINED is `--external-drained?''s bounded answer; TAIL is `--external-tail''s,
read only when DRAINED is nil.  Poll and fold share it (invariant 34)."
  (mapcar (lambda (path)
            (let* ((cursor (org-glance-graph--external-cursor path))
                   (drained (org-glance-graph--external-drained? path cursor)))
              (list path drained
                    (unless drained
                      (org-glance-graph--external-tail path cursor)))))
          (org-glance-graph--external-sources graph)))

(cl-defun org-glance-graph--external-took (survey path)
  "Return the offset a fold over SURVEY takes PATH's cursor to, or nil.
Nil unless SURVEY read PATH whole and found bytes owed."
  (let ((tail (nth 2 (assoc path survey))))
    (and tail (not (string-empty-p (car tail))) (cadr tail))))

(cl-defun org-glance-graph--read-external (graph &optional survey)
  "Read GRAPH's pending notification bytes as a plist of ids and new cursors.
SURVEY defaults to a fresh `--external-survey'.  Each id gets one `:entries'
cons (ID . KIND), KIND `edit' or `tombstone', placed at its first sighting and
carrying its last (invariant 33); unparseable lines are skipped.  `:marks'
holds, per source with bytes, the (PATH OFFSET DIGESTS) its cursor moves to."
  (let ((kinds (make-hash-table :test 'equal))
        order marks)
    (pcase-dolist (`(,path ,_drained ,tail)
                   (or survey (org-glance-graph--external-survey graph)))
      (let ((text (car tail)))
        (when (and text (not (string-empty-p text)))
          (push (cons path (cdr tail)) marks)
          (dolist (line (split-string text "\n" t))
            (condition-case nil
                (let ((object (json-parse-string line :object-type 'plist)))
                  (when-let* ((id (plist-get object :id)))
                    ;; `(eq t ...)': JSON false and null read as :false and :null
                    (let ((kind (if (eq t (plist-get object :tombstone))
                                    'tombstone
                                  'edit)))
                      (unless (gethash id kinds) (push id order))
                      (puthash id kind kinds))))
              (json-error nil))))))
    (list :entries (mapcar (lambda (id) (cons id (gethash id kinds)))
                           (nreverse order))
          :marks (nreverse marks))))

(cl-defun org-glance-graph--external-pending-p (graph &optional survey)
  "Non-nil when any of GRAPH's notification sources is not drained.
SURVEY defaults to a fresh `--external-survey'."
  (cl-notevery #'cadr (or survey (org-glance-graph--external-survey graph))))

(defcustom org-glance-graph-external-max-bytes (* 1024 1024)
  "Size in bytes at which `meta/EXTERNAL.jsonl' rotates to a generation.
It bounds what one fold hashes and what a lost cursor re-folds; the idle poll's
cost is independent of it."
  :group 'org-glance
  :type 'integer)

(cl-defun org-glance-graph--rotate-external-maybe (graph survey)
  "Rotate GRAPH's notification file to a generation once it is worth doing.
SURVEY is the fold's own reading; rotation fires once this fold took the live
file to `org-glance-graph-external-max-bytes' or more (`--external-took').
Every existing generation but the newest retires if SURVEY read it whole or
`--external-folded-whole?' holds, else re-folds.  The cursor, then the file,
move to the next generation (invariant 34)."
  (let* ((path (org-glance-graph:external-path graph))
         (drained (org-glance-graph--external-took survey path)))
    (when (and drained (>= drained org-glance-graph-external-max-bytes))
      (let* ((meta (org-glance-graph:meta-path graph))
             (generations (org-glance-graph--external-generations graph))
             (next (1+ (apply #'max 0 (mapcar #'org-glance-graph--external-generation
                                              generations))))
             (born (org-glance-graph--external-generation-path graph next)))
        (dolist (name (butlast generations))
          (let ((old (f-join meta name)))
            (if (or (nth 2 (assoc old survey))
                    (org-glance-graph--external-folded-whole? old))
                (org-glance-graph--external-retire graph name)
              (org-glance-graph--external-refold old))))
        (let ((cursor (org-glance-graph--external-cursor-path path)))
          (when (f-exists? cursor)
            (rename-file cursor (org-glance-graph--external-cursor-path born) t)))
        (rename-file path born t)))))

;; Reached by name: `org-glance-tag-config' requires THIS module.
(declare-function org-glance-tag-config:cycle-for-filter "org-glance-tag-config")
(declare-function org-glance-tag-config:cycle->keywords-or "org-glance-tag-config")

(cl-defun org-glance-graph--reparse-blob (graph meta contents)
  "Parse CONTENTS as META's headline in GRAPH, with its tag's todo cycle in scope.
Mirrors `org-glance-material:sync'; else a custom state folds into the title."
  (let ((org-todo-keywords
         (if (fboundp 'org-glance-tag-config:cycle-for-filter)
             (org-glance-tag-config:cycle->keywords-or
              (org-glance-tag-config:cycle-for-filter
               graph (list :tags (append (org-glance-headline-metadata:tags meta) nil)))
              org-todo-keywords)
           org-todo-keywords)))
    (org-glance-headline--from-string contents)))

(defcustom org-glance-graph-external-poll-seconds 1.0
  "Seconds between two checks for pending external writes on the read path.
One check per graph per interval, whatever the read volume; 0 checks each read."
  :group 'org-glance
  :type 'number)

(defvar org-glance-graph--folding-external nil
  "Non-nil while a fold runs, so its own reads start no other.
Bound by `org-glance-graph:refresh-external' itself.")

(cl-defun org-glance-graph--fold-external-maybe (graph)
  "Fold GRAPH's pending external writes, at most once per poll interval.
Every read calls this (`--ensure-cache'); the interval is
`org-glance-graph-external-poll-seconds'.  A failed fold is messaged and leaves
the cursor: `condition-case', never `with-demoted-errors', which lets errors
through under `debug-on-error' (invariant 33)."
  (unless org-glance-graph--folding-external
    (let ((now (float-time)))
      (when (>= (- now (org-glance-graph:-external-checked graph))
                org-glance-graph-external-poll-seconds)
        ;; stamped BEFORE the fold: the reads it makes must not re-check either
        (setf (org-glance-graph:-external-checked graph) now)
        (let ((survey (org-glance-graph--external-survey graph)))
          (when (org-glance-graph--external-pending-p graph survey)
            (condition-case err
                (org-glance-graph:refresh-external graph survey)
              (error (message "org-glance: refresh-external skipped: %S" err)))))))))

(cl-defun org-glance-graph:refresh-external (&optional (graph (org-glance-ensure-init))
                                                       survey)
  "Fold the entries an external writer moved back into GRAPH's metadata.
GRAPH defaults to the session's; `--fold-external-maybe' runs this throttled on
reads.  SURVEY is the poll's reading, made here when nil.  A WRITE re-derives
its id from the blob on disk, ingesting an unknown id and never resurrecting a
tombstoned one (invariant 30); a DELETE appends the tombstone
`org-glance-graph:delete' would.  Unusable notes are skipped with a message and
still spent.  Records land in one append, cursors move after (invariant 33),
then the file may rotate.  Return the number of entries refreshed."
  (interactive)
  (let* ((org-glance-graph--folding-external t)   ; its own reads: no nested fold
         (survey (or survey (org-glance-graph--external-survey graph)))
         (read (org-glance-graph--read-external graph survey))
         (entries (plist-get read :entries))
         (specs nil)
         (skipped 0))
    (pcase-dolist (`(,id . ,kind) entries)
      (let (spec reason)
        (if (eq kind 'tombstone)
            (setq spec (org-glance-graph--tombstone-spec graph id)
                  reason "unknown or deleted")
          ;; invariant 30; an unknown id parses first to pick its todo cycle.
          (if (eq (org-glance-graph:get-headline graph id) 'tombstone)
              (setq spec nil reason "already deleted")
            (let* ((contents (org-glance-graph:get-content graph id))
                   (basis (or (org-glance-graph:live-meta graph id)
                              (and contents
                                   (ignore-errors
                                     (org-glance-headline:metadata*
                                      (org-glance-headline--from-string contents))))))
                   (record (and contents basis
                                (org-glance-graph--reparse-blob graph basis contents))))
              (setq spec (and record (org-glance-headline:metadata* record))
                    reason (cond ((not contents) "no stored blob")
                                  ((not basis) "the blob did not parse")
                                  (t "unknown or deleted"))))))
        (if spec
            (push spec specs)
          (cl-incf skipped)
          (message "org-glance: refresh-external skips %s (%s)" id reason))))
    (when specs
      (org-glance-graph:insert graph (nreverse specs)))
    ;; BYTES, never entries: an all-unparseable file would be re-read forever.
    (pcase-dolist (`(,path ,offset ,digests) (plist-get read :marks))
      (org-glance-graph--set-external-cursor path offset digests))
    (org-glance-graph--rotate-external-maybe graph survey)
    (let ((n (- (length entries) skipped)))
      (when (called-interactively-p 'any)
        (message "org-glance: refreshed %d external entr%s%s"
                 n (if (= n 1) "y" "ies")
                 (if (> skipped 0) (format ", skipped %d" skipped) "")))
      n)))

(cl-defun org-glance-graph:reset-external
    (&optional (graph (org-glance-ensure-init)))
  "Drop every notification cursor in GRAPH, then fold the family from byte zero.
Recovers writes a past fold spent without ingesting.  Re-folding a known id is a
no-op; a folded delete whose id was since re-created deletes it again.  Spent
generations are not reached.  Return the count `refresh-external' folded."
  (interactive)
  (dolist (path (org-glance-graph--external-sources graph))
    (org-glance-graph--external-refold path))
  (let ((n (org-glance-graph:refresh-external graph)))
    (when (called-interactively-p 'any)
      (message "org-glance: reset external, refolded %d entr%s"
               n (if (= n 1) "y" "ies")))
    n))

;;; Store bootstrap / recovery / compaction

(cl-defun org-glance-graph--write-if-absent (path content)
  "Write CONTENT to PATH unless PATH exists, sparing hand edits (invariant 8)."
  (unless (f-exists? path)
    (f-write-text content 'utf-8 path)))

(cl-defun org-glance-graph--ensure-gitattributes (graph)
  "Write the built-in `merge=union' driver for GRAPH's WAL files, if absent.
Names only the open segment and `--segment-stem'`*.jsonl', the resolver's
allowlist (invariant 8); a `*.jsonl' glob would reach the notification family."
  (org-glance-graph--write-if-absent
   (f-join (org-glance-graph:meta-path graph) ".gitattributes")
   (mapconcat (lambda (glob) (format "%s merge=union\n" glob))
              (list (file-name-nondirectory
                     (org-glance-graph--open-segment-path graph))
                    (concat org-glance-graph--segment-stem "*.jsonl"))
              "")))

(defconst org-glance-graph--gitignore-lines
  '("cache/" "meta/EXTERNAL*.jsonl" "meta/EXTERNAL*.cursor" "meta/spent/")
  "Store-relative paths `--ensure-gitignore' keeps out of git.
`cache/' and the notification family: the live file, rotated generations, their
cursors and `meta/spent/' (invariant 34).")

(cl-defun org-glance-graph--ensure-gitignore (graph)
  "Git-ignore GRAPH's per-machine files, appending any line the store lacks.
Each missing line of `org-glance-graph--gitignore-lines' joins the end; a line
removed by hand returns at the next open.  Ignoring never untracks: a committed
family needs `git rm --cached' by hand (invariant 34, hazard H3)."
  (let* ((path (f-join (org-glance-graph:store-path graph) ".gitignore"))
         (text (if (f-exists? path) (f-read-text path 'utf-8) ""))
         (have (split-string text "\n" t "[ \t\r]+"))
         (missing (--remove (member it have) org-glance-graph--gitignore-lines)))
    (when missing
      (org-glance--atomic-write
       path
       (concat text
               (if (or (string-empty-p text) (s-ends-with? "\n" text)) "" "\n")
               (mapconcat #'identity missing "\n") "\n")))))

(cl-defun org-glance-graph--manifest-broken? (text)
  "Non-nil when MANIFEST TEXT cannot be trusted as the live segment set.
Broken is nil, blank, conflict-marked, unparseable or lacking `:segments'."
  (or (null text)
      (string-empty-p (string-trim text))
      (org-glance--conflict-marked? text)
      (condition-case nil
          (not (vectorp (plist-get (json-parse-string text :object-type 'plist)
                                   :segments)))
        (error t))))

(cl-defun org-glance-graph--reconcile-manifest (graph)
  "Rebuild GRAPH's MANIFEST from the on-disk segments when it is broken.
A valid MANIFEST stays byte-stable (invariant 3).  The rebuild lists every
non-empty seg-*.jsonl, oldest first.  Runs before `--heal', which would reap a
synced-in segment still unlisted."
  (let* ((path (org-glance-graph--manifest-path graph))
         (text (when (f-exists? path) (f-read-text path 'utf-8))))
    (when (org-glance-graph--manifest-broken? text)
      (let ((meta (org-glance-graph:meta-path graph)))
        (org-glance-graph--write-manifest
         graph
         (sort (cl-loop for name in (org-glance-graph--segment-names graph)
                        when (> (org-glance--file-size (f-join meta name)) 0)
                        collect name)
               #'string<))))))

(cl-defun org-glance-graph--conflicted-jsonl-files (graph)
  "Return the paths of GRAPH's WAL segments that carry git conflict markers.
An allowlist by name, the open segment and `seg-<gen>.jsonl', since this runs
before `--reconcile-manifest' (invariant 8)."
  (let ((meta (org-glance-graph:meta-path graph))
        (open (file-name-nondirectory (org-glance-graph--open-segment-path graph))))
    (cl-loop for name in (directory-files meta nil "\\.jsonl\\'")
             when (or (string= name open)
                      (org-glance-graph--segment-generation name))
             when (org-glance--conflict-marked?
                   (f-read-text (f-join meta name) 'utf-8))
             collect (f-join meta name))))

(cl-defun org-glance-graph--union-resolve-file (path)
  "Strip git conflict markers from PATH by union merge; return blocks resolved.
Both sides' lines survive, so last-wins reading collapses duplicate ids."
  (let* ((text (f-read-text path 'utf-8))
         (blocks (cl-count-if (lambda (l) (string-prefix-p "<<<<<<<" l))
                              (split-string text "\n"))))
    (f-write-text (org-glance--strip-conflict-markers text) 'utf-8 path)
    blocks))

(cl-defun org-glance-graph--resolve-jsonl-conflicts (graph)
  "Resolve git conflict markers in GRAPH's WAL segments by union merge.
Per `org-glance-conflict-resolution', `ask' prompts, `union' acts, nil signals."
  (when-let* ((files (org-glance-graph--conflicted-jsonl-files graph)))
    (let ((names (mapconcat #'file-name-nondirectory files ", ")))
      (org-glance--resolve-conflict
       names
       (lambda ()
         (let ((blocks (cl-loop for path in files
                                sum (org-glance-graph--union-resolve-file path))))
           (message "org-glance: union-resolved %d git conflict block(s) in %s"
                    blocks names)))))))

(cl-defun org-glance-graph--migrate-maybe (graph)
  "Write GRAPH's initial empty MANIFEST if it has none.
This adopts `headlines.jsonl' in place as the open segment; idempotent."
  (unless (f-exists? (org-glance-graph--manifest-path graph))
    (org-glance-graph--write-manifest graph nil)))

(cl-defun org-glance-graph--gc-orphans (graph)
  "Delete GRAPH's stale *.tmp.* files and any seg-*.jsonl the MANIFEST omits."
  (let ((meta (org-glance-graph:meta-path graph))
        (live (org-glance-graph--sealed-segments graph)))
    (dolist (f (directory-files meta nil nil t))
      (when (or (string-match-p "\\.tmp\\." f)
                (and (org-glance-graph--segment-generation f) (not (member f live))))
        (ignore-errors (f-delete (f-join meta f)))))))

(cl-defun org-glance-graph--open-empty? (graph)
  "Non-nil when GRAPH's open segment is empty or absent."
  (= 0 (org-glance--file-size (org-glance-graph--open-segment-path graph))))

(cl-defun org-glance-graph--segment-seqs (graph name)
  "Return the `seq' ordinals recorded in GRAPH's sealed segment basename NAME."
  (let (seqs)
    (org-glance-graph--scan-file
     graph (file-truename (f-join (org-glance-graph:meta-path graph) name))
     (lambda (r) (when-let* ((s (plist-get r :seq))) (push s seqs))))
    seqs))

(cl-defun org-glance-graph--heal (graph)
  "Recover GRAPH from an interrupted seal and re-derive session state.
With an empty open segment, adopt each unlisted seg-* newer than every listed
one whose `seq' ordinals no listed segment holds; compaction debris shares
them (invariant 1).  Reap other orphans; idempotent."
  (let* ((listed (org-glance-graph--sealed-segments graph))
         (listed-max (or (cl-loop for n in listed
                                  maximize (org-glance-graph--segment-generation n))
                         0))
         (open-empty (org-glance-graph--open-empty? graph))
         (adopt (and open-empty
                     (cl-loop for f in (org-glance-graph--segment-names graph)
                              when (and (not (member f listed))
                                        (> (org-glance-graph--segment-generation f) listed-max))
                              collect f))))
    (when adopt
      (let ((listed-seqs (make-hash-table :test 'eql)))
        (dolist (name listed)
          (dolist (s (org-glance-graph--segment-seqs graph name))
            (puthash s t listed-seqs)))
        (setq adopt (cl-loop for f in adopt
                             unless (cl-some (lambda (s) (gethash s listed-seqs))
                                             (org-glance-graph--segment-seqs graph f))
                             collect f))))
    (when adopt
      (org-glance-graph--write-manifest graph (append listed (sort adopt #'string<)))))
  (org-glance-graph--ensure-newline-terminated (org-glance-graph--open-segment-path graph))
  (setf (org-glance-graph:seq graph) (org-glance-graph--max-seq graph))
  (org-glance-graph--gc-orphans graph)
  (org-glance-graph--invalidate-cache graph))

(cl-defun org-glance-graph:compact (graph)
  "Merge all of GRAPH's segments into one, dropping superseded and dead records.
Also GC the blobs of deleted ids.  The MANIFEST swap commits before the open
segment empties (invariant 2); a compact store is left alone.  Return the live
record count."
  (cl-check-type graph org-glance-graph)
  (let* ((sealed-names (org-glance-graph--sealed-segments graph))
         (open (org-glance-graph--open-segment-path graph))
         (open-empty (org-glance-graph--open-empty? graph))
         (fold (org-glance-graph--latest-records graph))
         (total (cdr fold)))
    (let (emit dead-ids)
      (dolist (record (car fold))
        (if (plist-get record :tombstone)
            (push (plist-get record :id) dead-ids) ; globally dead -> drop the id, GC its blob
          (push record emit)))
      (setq emit (nreverse emit))
      (unless (and (<= (length sealed-names) 1) open-empty
                   (null dead-ids) (= total (length emit)))
        (let (new-names)
          (when emit
            (let* ((gen (org-glance-graph--next-generation graph))
                   (newseg (org-glance-graph--segment-path graph gen)))
              (org-glance--atomic-write
               newseg (concat (s-join "\n" (mapcar #'json-serialize emit)) "\n") nil)
              (setq new-names (list (file-name-nondirectory newseg)))))
          ;; invariant 2: commit BEFORE truncating the open segment.
          (org-glance-graph--write-manifest graph new-names)
          (org-glance--atomic-write open "")))
      (dolist (id dead-ids)
        (let ((dir (ignore-errors (org-glance-graph:headline-data-path graph id))))
          (when (and dir (f-exists? dir)) (ignore-errors (f-delete dir t)))))
      (org-glance-graph--gc-orphans graph)
      (org-glance-graph--invalidate-cache graph)
      (length emit))))

(cl-defun org-glance-graph:capture-buffer (&optional (buffer (current-buffer)))
  "Return a list of `org-glance-headline' parsed from BUFFER."
  (cl-check-type buffer buffer)
  (with-current-buffer buffer
    (cl-loop for element in (org-element-map (org-element-parse-buffer 'headline) 'headline #'identity)
             collect (org-glance-headline--from-element element))))

(defun org-glance-graph--creation-stamp ()
  "Return now as an inactive org timestamp, glance's `[YYYY-MM-DD Dow HH:MM]'."
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

(cl-defun org-glance-graph:capture (graph &optional (buffer (current-buffer)))
  "Ingest BUFFER into GRAPH and return GRAPH.
Stamp every headline lacking one with a fresh ORG_GLANCE_ID, likewise
ORG_GLANCE_CREATION_TIME, then add them all to GRAPH."
  (cl-check-type graph org-glance-graph)
  (with-current-buffer buffer
    (org-with-wide-buffer
     (org-map-entries
      (lambda ()
        ;; Marker: the first put drifts point, so nil pom reads the NEXT entry.
        (let ((heading (point-marker)))
          (unless (org-entry-get heading "ORG_GLANCE_ID")
            (org-entry-put heading "ORG_GLANCE_ID" (org-glance-graph:make-id graph)))
          (unless (org-entry-get heading "ORG_GLANCE_CREATION_TIME")
            (org-entry-put heading "ORG_GLANCE_CREATION_TIME"
                           (org-glance-graph--creation-stamp)))
          (set-marker heading nil))))))
  (apply #'org-glance-graph:add graph (org-glance-graph:capture-buffer buffer)))

(provide 'org-glance-graph)
