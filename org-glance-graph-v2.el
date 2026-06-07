;; -*- lexical-binding: t -*-

(require 'cl-macs)
(require 'dash)
(require 'f)
(require 'org)
(require 'org-id)

(require 'org-glance-utils)
(require 'org-glance-headline-v2)

(defcustom org-glance-directory org-directory
  "Main location for all Org mode content managed by `org-glance`."
  :group 'org-glance
  :type 'directory)

;; NB: the hash must canonicalize exactly like the equality does -- hashing the
;; raw string would make equal keys ("foo" vs "foo/") land in different buckets.
(define-hash-table-test 'org-glance-graph-v2:test
                        (lambda (a b) (f-equal? (file-truename a) (file-truename b)))
                        (lambda (a) (secure-hash 'sha1 (file-truename a))))

(defvar org-glance-graph-v2:list (make-hash-table :test 'org-glance-graph-v2:test)
  "Registered instances of `org-glance-graph-v2' in current session.")

;; Single-user assumption: no mutex / locking (see MIGRATION-PLAN.md, decision 3).
(cl-defstruct (org-glance-graph-v2 (:predicate org-glance-graph-v2?)
                                   (:conc-name org-glance-graph-v2:))
  (directory org-glance-directory :read-only t :type directory)
  ;; Cached store-global monotonic record ordinal; re-derived from disk at open
  ;; (`--max-seq'), `cl-incf'-ed per appended record.  Storage ordinal only --
  ;; never part of headline metadata.
  (seq 0 :type integer))

(defcustom org-glance-graph-v2-segment-max-bytes (* 256 1024)
  "Soft maximum size, in bytes, of the open v2 metadata segment before it is
sealed into an immutable segment.  Checked after each insert's whole batch is
appended, so a record/batch is never split; a single oversized batch may push a
segment past this bound (the cap is soft)."
  :group 'org-glance
  :type 'integer)

(defcustom org-glance-graph-v2-compact-segment-count 4
  "Compact the v2 metadata store automatically once this many sealed segments
accumulate.  Set to a very large value to effectively disable auto-compaction
\(then use \\[org-glance-graph-compact])."
  :group 'org-glance
  :type 'integer)

(cl-defstruct (org-glance-headline-metadata-v2 (:predicate org-glance-headline-metadata-v2?)
                                               (:conc-name org-glance-headline-metadata-v2:))
  (id nil :read-only t :type string)
  (state nil :read-only t :type string)
  (title nil :read-only t :type string)
  (tags nil :read-only t :type list)
  (hash nil :read-only t :type string)
  (schedule nil :read-only t :type string)
  (deadline nil :read-only t :type string)
  (priority nil :read-only t :type number)
  ;; Content-derived projection flags (for view filters). Absent (nil) on records
  ;; written before these fields existed -- `M-x org-glance-reindex' backfills them.
  (linked? nil :read-only t :type boolean)
  (propertized? nil :read-only t :type boolean)
  (encrypted? nil :read-only t :type boolean))

(defconst org-glance-headline-metadata-v2:fields
  ;; SLOT          JSON-KEY      FROM-HEADLINE                                                ENCODE       DECODE
  `((id            :id           ,#'org-glance-headline-v2:id                                 nil          nil)
    (state         :state        ,#'org-glance-headline-v2:state                              nil          nil)
    (title         :title        ,#'org-glance-headline-v2:title                              nil          nil)
    (tags          :tags         ,#'org-glance-headline-v2:tags                               tags-vector  nil)
    (hash          :hash         ,#'org-glance-headline-v2:hash                               nil          nil)
    (schedule      :schedule     ,#'org-glance-headline-v2:schedule                           nil          nil)
    (deadline      :deadline     ,#'org-glance-headline-v2:deadline                           nil          nil)
    (priority      :priority     ,#'org-glance-headline-v2:priority                           nil          nil)
    (linked?       :linked       ,(lambda (h) (and (org-glance-headline-v2:links h) t))       nil          bool)
    (propertized?  :propertized  ,(lambda (h) (and (org-glance-headline-v2:properties h) t))  nil          bool)
    (encrypted?    :encrypted    ,(lambda (h) (and (org-glance-headline-v2:encrypted? h) t))  nil          bool))
  "The single source of truth for the metadata projection's shape.
Drives the `org-glance-headline-v2:metadata' constructor, `serialize' and
`deserialize' together, so the four can never drift (a hand-written
`deserialize' line was silently forgettable -- the field then read as
always-nil).  Adding a projection field = one row here + one struct slot
\(checked against this table at load).  Row order IS the serialized JSON key
order -- a byte-stability contract with the on-disk store; append new fields
at the end.  SCHEDULE/DEADLINE come back as raw strings (or nil) from the
headline methods, so they are JSON-serializable as-is.")

;; Load-time guard: the struct and the table must list the same slots, in order.
(let ((struct-slots (mapcar #'car (cdr (cl-struct-slot-info 'org-glance-headline-metadata-v2))))
      (table-slots (mapcar #'car org-glance-headline-metadata-v2:fields)))
  (unless (equal struct-slots table-slots)
    (error "org-glance: metadata field table out of sync with the struct: %S vs %S"
           table-slots struct-slots)))

(cl-defun org-glance-headline-metadata-v2:--encode (kind value)
  "Serialize-side coercion for a field of ENCODE kind KIND."
  (pcase kind
    ('tags-vector (->> value (mapcar (-partial #'format "%s")) (apply #'vector)))
    (_ value)))

(cl-defun org-glance-headline-metadata-v2:--decode (kind value)
  "Deserialize-side coercion for a field of DECODE kind KIND."
  (pcase kind
    ('bool (eq t value))                ; JSON false/null both read as nil
    (_ value)))

(cl-defun org-glance-headline-v2:metadata (headline)
  (cl-check-type headline org-glance-headline-v2)
  (apply #'make-org-glance-headline-metadata-v2
         (cl-loop for (slot _json from) in org-glance-headline-metadata-v2:fields
                  append (list (intern (concat ":" (symbol-name slot)))
                               (funcall from headline)))))

(cl-defun org-glance-headline-v2:metadata* (obj)
  "Generic variant of `org-glance-headline-v2:metadata'."
  (cl-typecase obj
    (org-glance-headline-metadata-v2 obj)
    (org-glance-headline-v2 (org-glance-headline-v2:metadata obj))))

(cl-defun org-glance-headline-metadata-v2:serialize* (obj)
  "Generic variant of `org-glance-headline-metadata-v2:serialize'."
  (cl-typecase obj
    (org-glance-headline-metadata-v2 (org-glance-headline-metadata-v2:serialize obj))
    (list obj)
    (t (error "Unable to determine object spec: %s" (prin1-to-string obj)))))

(cl-defun org-glance-headline-metadata-v2:serialize (metadata)
  (cl-check-type metadata org-glance-headline-metadata-v2)
  (cl-loop for (slot json _from encode) in org-glance-headline-metadata-v2:fields
           for value = (cl-struct-slot-value 'org-glance-headline-metadata-v2 slot metadata)
           append (list json (org-glance-headline-metadata-v2:--encode encode value))))

(cl-defun org-glance-headline-metadata-v2:deserialize (data)
  (cl-check-type data list)
  (apply #'make-org-glance-headline-metadata-v2
         (cl-loop for (slot json _from _encode decode) in org-glance-headline-metadata-v2:fields
                  append (list (intern (concat ":" (symbol-name slot)))
                               (org-glance-headline-metadata-v2:--decode decode (plist-get data json))))))

(cl-defun org-glance-headline-metadata-v2:done? (metadata)
  "Non-nil if METADATA's state is a done keyword (per `org-done-keywords')."
  (cl-check-type metadata org-glance-headline-metadata-v2)
  (not (null (member (org-glance-headline-metadata-v2:state metadata) org-done-keywords))))

(cl-defun org-glance-headline-metadata-v2:active? (metadata)
  "Non-nil if METADATA is not done.  Derived from `state' (always present), so
it works on records written before any later schema additions."
  (cl-check-type metadata org-glance-headline-metadata-v2)
  (not (org-glance-headline-metadata-v2:done? metadata)))

(cl-defun org-glance--done-keywords ()
  "The \"done\" todo keywords to use, reusing Org's own `org-done-keywords'.
That variable is buffer-local and unset outside Org buffers, so `done?'
misbehaves when called from a command/minibuffer context: fall back to deriving
the set from the global `org-todo-keywords' in a scratch Org buffer.  Callers
bind `org-done-keywords' to this around a batch of `done?'/`active?' checks so
the result is deterministic.  An overview can still override it per view via a
`:done-keywords' filter clause."
  (or org-done-keywords
      (with-temp-buffer
        (delay-mode-hooks (org-mode))
        org-done-keywords)))

(cl-defun org-glance-graph-v2 (&optional (directory org-glance-directory))
  (cl-check-type directory string)
  (let* ((directory (-> directory (file-truename) (f-full)))
         (graph (gethash directory org-glance-graph-v2:list)))
    (unless graph
      (setq graph (make-org-glance-graph-v2 :directory directory))
      (f-mkdir-full-path (org-glance-graph-v2:data-path graph))
      (f-mkdir-full-path (org-glance-graph-v2:meta-path graph))
      (f-touch (org-glance-graph-v2:headline-meta-path graph))
      (org-glance-graph-v2:--migrate-maybe graph) ; bootstrap MANIFEST / adopt legacy file
      (org-glance-graph-v2:--heal graph)           ; recover seal, derive seq, reap orphans
      (puthash directory graph org-glance-graph-v2:list))
    graph))

(cl-defun org-glance-graph-v2:insert (graph meta)
  (declare (indent 1))
  (cl-check-type meta list)
  (cl-check-type (car meta) (or list org-glance-headline-metadata-v2))
  (org-glance-graph-v2:--append graph meta))

;;; Segmented (LSM-lite) metadata store
;;
;; `meta/headlines.jsonl' is the OPEN append segment; once it crosses
;; `org-glance-graph-v2-segment-max-bytes' it is sealed (atomic rename) into an
;; immutable `seg-NNNNNNNNNN.jsonl' and a fresh open segment takes over.  A
;; one-line JSON MANIFEST lists the live sealed segments (oldest-first); readers
;; merge segments, latest-per-id wins.  Each record carries a store-global
;; monotonic `seq' ordinal.  Every mutation is crash-safe via temp-then-rename;
;; the MANIFEST rename is the sole commit point.  See MIGRATION-PLAN.md Phase 4.

(cl-defun org-glance-graph-v2:--open-segment-path (graph)
  "The open append segment -- identical to `headline-meta-path' (the store-change
signal for the overview cache, and the migration-adoption target)."
  (org-glance-graph-v2:headline-meta-path graph))

(cl-defun org-glance-graph-v2:--manifest-path (graph)
  (-> (f-join (org-glance-graph-v2:meta-path graph) "MANIFEST") (file-truename)))

(cl-defun org-glance-graph-v2:--segment-path (graph gen)
  (-> (f-join (org-glance-graph-v2:meta-path graph) (format "seg-%010d.jsonl" gen))
      (file-truename)))

(cl-defun org-glance-graph-v2:--tmp-path (graph base)
  (make-temp-name (f-join (org-glance-graph-v2:meta-path graph) (concat base ".tmp."))))

(defconst org-glance-graph-v2:--segment-name-re "\\`seg-\\([0-9]+\\)\\.jsonl\\'"
  "Matches a sealed segment basename; group 1 is its generation number.")

(cl-defun org-glance-graph-v2:--segment-generation (name)
  "Generation number of sealed segment basename NAME, or nil if not a segment."
  (when (string-match org-glance-graph-v2:--segment-name-re name)
    (string-to-number (match-string 1 name))))

(cl-defun org-glance-graph-v2:--sealed-segments (graph)
  "Basenames of the live sealed segments, oldest-first, per the MANIFEST.
An absent or unparseable MANIFEST (impossible after construction -- the swap is
an atomic rename) reads as the empty set."
  (let ((path (org-glance-graph-v2:--manifest-path graph)))
    (when (f-exists? path)
      (condition-case nil
          (append (plist-get (json-parse-string (f-read-text path 'utf-8) :object-type 'plist)
                             :segments)
                  nil)
        (error nil)))))

(cl-defun org-glance-graph-v2:--write-manifest (graph segments)
  "Atomically persist SEGMENTS (sealed basenames, oldest-first) as the MANIFEST.
The temp-then-rename is the sole commit/swap point for the live segment set."
  (let ((tmp (org-glance-graph-v2:--tmp-path graph "MANIFEST"))
        (path (org-glance-graph-v2:--manifest-path graph)))
    (f-write-text (concat (json-serialize (list :version 2 :segments (apply #'vector segments)))
                          "\n")
                  'utf-8 tmp)
    (rename-file tmp path t)))

(cl-defun org-glance-graph-v2:--live-segments (graph &optional newest-first)
  "Existing absolute paths of the live segments, oldest->newest (open last);
reversed (open first) when NEWEST-FIRST.  Orphans (on disk, not in the MANIFEST)
are invisible."
  (let* ((meta (org-glance-graph-v2:meta-path graph))
         (sealed (cl-loop for name in (org-glance-graph-v2:--sealed-segments graph)
                          for p = (file-truename (f-join meta name))
                          when (f-exists? p) collect p))
         (open (org-glance-graph-v2:--open-segment-path graph))
         (all (append sealed (when (f-exists? open) (list open)))))
    (if newest-first (reverse all) all)))

(cl-defun org-glance-graph-v2:--next-generation (graph)
  "1 + the highest seg-NNN generation on disk (filenames are the authority)."
  (1+ (or (cl-loop for f in (directory-files (org-glance-graph-v2:meta-path graph) nil
                                             org-glance-graph-v2:--segment-name-re)
                   maximize (org-glance-graph-v2:--segment-generation f))
          0)))

(cl-defun org-glance-graph-v2:--scan-file (graph path fn)
  "Call FN on each non-empty UTF-8 JSON record in GRAPH's segment PATH, top-down.
The scanner owns the torn-line policy: only the OPEN segment can legitimately
have a torn (newline-less) final line -- a crash can only tear the last append
-- so a parse error there is ignored; elsewhere it re-signals (corruption)."
  (when (f-exists? path)
    (let ((tolerate-torn (string= path (org-glance-graph-v2:--open-segment-path graph))))
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

(cl-defun org-glance-graph-v2:--scan-forward (graph fn)
  "Call FN on every record across live segments, oldest->newest (open last)."
  (dolist (seg (org-glance-graph-v2:--live-segments graph))
    (org-glance-graph-v2:--scan-file graph seg fn)))

(cl-defun org-glance-graph-v2:--latest-records (graph)
  "Cons of (RECORDS . TOTAL): the latest record per id, tombstones included, in
original insertion (first-sighting) order across all live segments; TOTAL is the
raw record count before the latest-per-id fold."
  (let ((latest (make-hash-table :test 'equal))
        (order nil)
        (total 0))
    (org-glance-graph-v2:--scan-forward
     graph (lambda (record)
             (cl-incf total)
             (let ((id (plist-get record :id)))
               (unless (gethash id latest) (push id order))
               (puthash id record latest))))
    (cons (cl-loop for id in (nreverse order) collect (gethash id latest))
          total)))

(cl-defun org-glance-graph-v2:--max-seq (graph)
  "Highest `seq' ordinal across live records, or 0 (legacy records lack `seq')."
  (let ((mx 0))
    (org-glance-graph-v2:--scan-forward
     graph (lambda (r) (let ((s (plist-get r :seq)))
                         (when (and (integerp s) (> s mx)) (setq mx s)))))
    mx))

(cl-defun org-glance-graph-v2:--ensure-newline-terminated (path)
  "Drop a trailing partial line in PATH (crash mid-append) so future appends stay
clean.  Cheap: only rewrites when PATH's final byte is not a newline."
  (let ((size (or (f-size path) 0)))
    (when (> size 0)
      (let ((last-byte (with-temp-buffer
                         (set-buffer-multibyte nil)
                         (insert-file-contents-literally path nil (1- size) size)
                         (char-after (point-min)))))
        (unless (eql last-byte ?\n)
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally path)
            (goto-char (point-max))
            (if (search-backward "\n" nil t)
                (delete-region (1+ (point)) (point-max))
              (erase-buffer))
            (let ((coding-system-for-write 'no-conversion))
              (write-region (point-min) (point-max) path nil 'silent))))))))

(cl-defun org-glance-graph-v2:--append (graph specs)
  "Append SPECS (metadata structs or bare plists) to the open segment, stamping a
fresh monotonic `seq' on each, then maybe seal and compact."
  (let ((open (org-glance-graph-v2:--open-segment-path graph)))
    (org-glance-graph-v2:--ensure-newline-terminated open)
    (cl-loop for spec in specs
             for record = (plist-put (copy-sequence (org-glance-headline-metadata-v2:serialize* spec))
                                     :seq (cl-incf (org-glance-graph-v2:seq graph)))
             collect (json-serialize record) into jsons
             finally (f-append-text (concat (s-join "\n" jsons) "\n") 'utf-8 open)))
  (org-glance-graph-v2:--maybe-seal graph)
  (org-glance-graph-v2:--maybe-compact graph))

(cl-defun org-glance-graph-v2:--maybe-seal (graph)
  (let ((open (org-glance-graph-v2:--open-segment-path graph)))
    (when (> (or (f-size open) 0) org-glance-graph-v2-segment-max-bytes)
      (org-glance-graph-v2:--seal graph))))

(cl-defun org-glance-graph-v2:--seal (graph)
  "Seal the full open segment into an immutable seg-<gen>.
Commit = the MANIFEST swap.  A crash before it leaves seg-<gen> durable but
unlisted with an empty open -- `--heal' adopts it on the next open."
  (let* ((open (org-glance-graph-v2:--open-segment-path graph))
         (sealed (org-glance-graph-v2:--segment-path graph (org-glance-graph-v2:--next-generation graph))))
    (rename-file open sealed)           ; atomic: a complete file becomes immutable
    (f-touch open)                      ; fresh empty open segment (bumps the signal mtime)
    (org-glance-graph-v2:--write-manifest
     graph (append (org-glance-graph-v2:--sealed-segments graph)
                   (list (file-name-nondirectory sealed))))))

(cl-defun org-glance-graph-v2:--maybe-compact (graph)
  (when (>= (length (org-glance-graph-v2:--sealed-segments graph))
            org-glance-graph-v2-compact-segment-count)
    (org-glance-graph-v2:compact graph)))

(cl-defun org-glance-graph-v2:--find-latest (graph id)
  "Newest record for ID across segments (newest-first); `tombstone'/nil aware."
  (cl-loop for seg in (org-glance-graph-v2:--live-segments graph t)
           for hit = (let (found)
                       (org-glance-graph-v2:--scan-file
                        graph seg
                        (lambda (r) (when (string= (plist-get r :id) id) (setq found r))))
                       found)
           when hit return (if (plist-get hit :tombstone)
                               'tombstone
                             (org-glance-headline-metadata-v2:deserialize hit))))

(cl-defun org-glance-graph-v2:store-path (graph)
  "Hidden per-directory store root for the graph.
Dot-prefixed so v1 tag discovery (`org-glance--list-directories', which
matches `^[[:word:]]+') and `org-agenda' ignore it during coexistence."
  (cl-check-type graph org-glance-graph-v2)
  (-> (f-join (org-glance-graph-v2:directory graph) ".org-glance")
      (file-truename)))

(cl-defun org-glance-graph-v2:data-path (graph)
  (cl-check-type graph org-glance-graph-v2)
  (-> (f-join (org-glance-graph-v2:store-path graph) "data")
      (file-truename)))

(cl-defun org-glance-graph-v2:meta-path (graph)
  (cl-check-type graph org-glance-graph-v2)
  (-> (f-join (org-glance-graph-v2:store-path graph) "meta")
      (file-truename)))

(cl-defun org-glance-graph-v2:headline-meta-path (graph)
  (cl-check-type graph org-glance-graph-v2)
  (-> (f-join (org-glance-graph-v2:meta-path graph) "headlines.jsonl")
      (file-truename)))

(cl-defun org-glance-graph-v2:headline-data-path (graph id)
  "Content-addressable directory for ID's data blob within GRAPH.
Long ids (e.g. UUIDs) are sharded by their first two characters."
  (cl-check-type graph org-glance-graph-v2)
  (cl-check-type id string)
  (cl-assert (not (string-empty-p id)))
  ;; Path-safety: ID feeds straight into `f-join'/`substring', so reject path
  ;; separators and traversal -- a hand-edited ORG_GLANCE_ID must not escape the
  ;; store (auto-generated UUIDs and tag-hash ids are already safe). Use `error',
  ;; not `cl-assert': the latter can be optimized out, and its
  ;; `cl-assertion-failed' is not a portable `error' subtype across Emacs versions.
  (when (or (string-match-p "/" id) (string-match-p "\\.\\." id))
    (error "Unsafe ORG_GLANCE_ID for content-addressable path: %S" id))
  (let ((data (org-glance-graph-v2:data-path graph)))
    (file-truename
     (if (> (length id) 2)
         (f-join data (substring id 0 2) (substring id 2))
       (f-join data id)))))

(cl-defun org-glance-graph-v2:put-content (graph headline)
  "Persist HEADLINE's contents under GRAPH's data store, keyed by its id.
Return the file path, or nil if HEADLINE has no id."
  (cl-check-type graph org-glance-graph-v2)
  (cl-check-type headline org-glance-headline-v2)
  (when-let ((id (org-glance-headline-v2:id headline)))
    (let* ((dir (org-glance-graph-v2:headline-data-path graph id))
           (path (f-join dir "data.org")))
      (f-mkdir-full-path dir)
      (f-write-text (org-glance-headline-v2:contents headline) 'utf-8 path)
      path)))

(cl-defun org-glance-graph-v2:get-content (graph id)
  "Return the stored contents string for ID in GRAPH, or nil if none.
Low-level: returns the blob regardless of tombstone state."
  (cl-check-type graph org-glance-graph-v2)
  (cl-check-type id string)
  (let ((path (f-join (org-glance-graph-v2:headline-data-path graph id) "data.org")))
    (when (f-exists? path)
      (f-read-text path 'utf-8))))

(cl-defun org-glance-graph-v2:make-id (graph)
  (cl-check-type graph org-glance-graph-v2)
  (cl-loop while t
           for id = (org-id-uuid)
           for data-path = (org-glance-graph-v2:headline-data-path graph id)
           unless (f-exists? data-path)
           return (prog1 id (f-mkdir-full-path data-path))))

;; (cl-defun org-glance-headline-metadata-v2:data-path (metadata)
;;   (cl-check-type metadata org-glance-headline-metadata-v2)
;;   (f-join (org-glance-graph-v2:headline-data-path (org-glance-headline-metadata-v2:graph metadata) (org-glance-headline-metadata-v2:id metadata)) "data.org"))

(cl-defun org-glance-graph-v2:add (graph &rest headlines)
  "Add HEADLINES to GRAPH and return GRAPH.
Each element may be an `org-glance-headline-v2' or pre-built metadata.  Full
headlines also have their contents persisted to the data store."
  (cl-check-type graph org-glance-graph-v2)
  (when headlines
    ;; Compute metadata FIRST: if a projection field errors, nothing is written
    ;; (no blob-saved-but-metadata-missing half-state).
    (let ((specs (mapcar #'org-glance-headline-v2:metadata* headlines)))
      (dolist (headline headlines)
        (when (org-glance-headline-v2? headline)
          (org-glance-graph-v2:put-content graph headline)))
      (org-glance-graph-v2:insert graph specs)))
  graph)

(cl-defun org-glance-graph-v2:get-headline (graph id)
  "Return the most recent metadata for ID in GRAPH.
Return the symbol `tombstone' if ID was deleted, or nil if unknown."
  (cl-check-type graph org-glance-graph-v2)
  (cl-check-type id string)
  (org-glance-graph-v2:--find-latest graph id))

(cl-defun org-glance-graph-v2:headline (graph id)
  "Return the full live `org-glance-headline-v2' stored for ID, or nil.
Reconstructs the headline from its persisted contents; returns nil for
unknown or tombstoned ids."
  (cl-check-type graph org-glance-graph-v2)
  (cl-check-type id string)
  (when (org-glance-headline-metadata-v2? (org-glance-graph-v2:get-headline graph id))
    (-some-> (org-glance-graph-v2:get-content graph id)
      (org-glance-headline-v2--from-string))))

(cl-defun org-glance-graph-v2:delete (graph id)
  "Append a tombstone for ID unless it is already absent or deleted."
  (cl-check-type graph org-glance-graph-v2)
  (cl-check-type id string)
  (let ((meta (org-glance-graph-v2:get-headline graph id)))
    (unless (memq meta '(nil tombstone))
      (org-glance-graph-v2:insert graph (list (list :id id :tombstone t))))))

(cl-defun org-glance-graph-v2:headlines (graph)
  "Return all live (non-tombstoned) headline metadata in GRAPH.
The latest record per id wins; original insertion order is preserved across all
segments (first-sighting in the oldest->newest scan == earliest insertion)."
  (cl-check-type graph org-glance-graph-v2)
  ;; NB: do not name this loop variable `it' -- `cl-loop' binds `it'
  ;; anaphorically to the `unless' condition value, shadowing it.
  (cl-loop for record in (car (org-glance-graph-v2:--latest-records graph))
           unless (plist-get record :tombstone)
           collect (org-glance-headline-metadata-v2:deserialize record)))

(cl-defun org-glance-graph-v2:reindex (graph)
  "Re-derive metadata for every live headline in GRAPH from its stored content,
appending fresh records so newly-added projection fields get populated.
Return the number of headlines re-indexed."
  (cl-check-type graph org-glance-graph-v2)
  (let* ((metas (org-glance-graph-v2:headlines graph))
         (reporter (and metas (make-progress-reporter "org-glance: re-indexing... " 0 (length metas))))
         (n 0))
    (cl-loop for meta in metas
             for i from 1
             for id = (org-glance-headline-metadata-v2:id meta)
             for contents = (org-glance-graph-v2:get-content graph id)
             do (when contents
                  (org-glance-graph-v2:add graph (org-glance-headline-v2--from-string contents))
                  (cl-incf n))
             do (when reporter (progress-reporter-update reporter i)))
    (when reporter (progress-reporter-done reporter))
    n))

;;; Store bootstrap / recovery / compaction

(cl-defun org-glance-graph-v2:--migrate-maybe (graph)
  "Bootstrap the segmented layout.  If no MANIFEST exists, adopt the present
`headlines.jsonl' in place as the open segment by writing an initial MANIFEST --
no bytes moved.  Idempotent; a no-op once a MANIFEST is present (also covers the
brand-new empty store, whose open segment the constructor just touched)."
  (unless (f-exists? (org-glance-graph-v2:--manifest-path graph))
    (org-glance-graph-v2:--write-manifest graph nil)))

(cl-defun org-glance-graph-v2:--gc-orphans (graph)
  "Delete stale *.tmp.* files and any seg-*.jsonl not referenced by the MANIFEST."
  (let ((meta (org-glance-graph-v2:meta-path graph))
        (live (org-glance-graph-v2:--sealed-segments graph)))
    (dolist (f (directory-files meta nil nil t))
      (when (or (string-match-p "\\.tmp\\." f)
                (and (org-glance-graph-v2:--segment-generation f) (not (member f live))))
        (ignore-errors (f-delete (f-join meta f)))))))

(cl-defun org-glance-graph-v2:--heal (graph)
  "Recover from an interrupted seal and re-derive session state.  Idempotent.
An unlisted seg-* whose generation exceeds every listed one, alongside an EMPTY
open segment, is a seal whose MANIFEST commit was lost -- adopt it.  Crashed
COMPACTION debris must NOT be adopted: it is told apart because compaction
copies records (it never re-stamps `seq'), so its output shares `seq' ordinals
with the listed segments, whereas a genuinely sealed open segment only holds
ordinals no listed segment has.  Non-adopted orphans are reaped."
  (let* ((meta (org-glance-graph-v2:meta-path graph))
         (listed (org-glance-graph-v2:--sealed-segments graph))
         (open (org-glance-graph-v2:--open-segment-path graph))
         (listed-max (or (cl-loop for n in listed
                                  maximize (org-glance-graph-v2:--segment-generation n))
                         0))
         (open-empty (= 0 (or (f-size open) 0)))
         (adopt (and open-empty
                     (cl-loop for f in (directory-files meta nil org-glance-graph-v2:--segment-name-re)
                              when (and (not (member f listed))
                                        (> (org-glance-graph-v2:--segment-generation f) listed-max))
                              collect f))))
    (when adopt
      ;; Disqualify compaction debris: any candidate sharing a `seq' with a
      ;; listed segment is a copy, not a seal.
      (let ((listed-seqs (make-hash-table :test 'eql)))
        (dolist (name listed)
          (org-glance-graph-v2:--scan-file
           graph (file-truename (f-join meta name))
           (lambda (r) (when-let ((s (plist-get r :seq))) (puthash s t listed-seqs)))))
        (setq adopt (cl-loop for f in adopt
                             for overlap = nil
                             do (org-glance-graph-v2:--scan-file
                                 graph (file-truename (f-join meta f))
                                 (lambda (r) (when-let ((s (plist-get r :seq)))
                                               (when (gethash s listed-seqs) (setq overlap t)))))
                             unless overlap collect f))))
    (when adopt
      (org-glance-graph-v2:--write-manifest graph (append listed (sort adopt #'string<)))))
  (org-glance-graph-v2:--ensure-newline-terminated (org-glance-graph-v2:--open-segment-path graph))
  (setf (org-glance-graph-v2:seq graph) (org-glance-graph-v2:--max-seq graph))
  (org-glance-graph-v2:--gc-orphans graph))

(cl-defun org-glance-graph-v2:compact (graph)
  "Merge all segments (sealed + open) into one, dropping superseded and dead
records, and GC the content blobs of fully-deleted ids.  Folding the open
segment in keeps the `:headlines' insertion-order contract intact (an id whose
older record lived in a sealed segment must not be \"re-sighted\" later), and
replacing it bumps the store-change signal -- compaction changes observable
reads (a dropped tombstone turns `tombstone' into nil), so caches must
invalidate.  Commit = the MANIFEST swap.  A no-op on an already-compact store.
Return the live record count.  See MIGRATION-PLAN.md Phase 4."
  (cl-check-type graph org-glance-graph-v2)
  (let* ((sealed-names (org-glance-graph-v2:--sealed-segments graph))
         (open (org-glance-graph-v2:--open-segment-path graph))
         (open-empty (= 0 (or (f-size open) 0)))
         (fold (org-glance-graph-v2:--latest-records graph))
         (total (cdr fold)))
    (let (emit dead-ids)
      (dolist (record (car fold))
        (if (plist-get record :tombstone)
            (push (plist-get record :id) dead-ids) ; globally dead -> drop the id, GC its blob
          (push record emit)))
      (setq emit (nreverse emit))
      ;; No-op guard: a single sealed segment of exactly the live records and an
      ;; empty open segment is already compact -- write nothing.
      (unless (and (<= (length sealed-names) 1) open-empty
                   (null dead-ids) (= total (length emit)))
        ;; 1. New compacted segment (orphan until the MANIFEST lists it).
        (let (new-names)
          (when emit
            (let* ((gen (org-glance-graph-v2:--next-generation graph))
                   (newseg (org-glance-graph-v2:--segment-path graph gen))
                   (tmp (org-glance-graph-v2:--tmp-path graph (format "seg-%010d.jsonl" gen))))
              (f-write-text (concat (s-join "\n" (mapcar #'json-serialize emit)) "\n") 'utf-8 tmp)
              (rename-file tmp newseg)
              (setq new-names (list (file-name-nondirectory newseg)))))
          ;; 2. Commit BEFORE touching the open segment: a crash before this swap
          ;;    leaves the old MANIFEST, old segments AND the intact open (with
          ;;    any tombstones whose only copy lives there) fully live -- the
          ;;    merged segment is a reapable orphan.  Truncating the open first
          ;;    would let a crash destroy a tombstone's only copy while an old
          ;;    listed segment still holds the headline live (resurrection).
          (org-glance-graph-v2:--write-manifest graph new-names)
          ;; 3. Fresh empty open segment (its content was folded in; the new
          ;;    mtime is the store-change signal for this compaction).  A crash
          ;;    between the swap and this point leaves duplicate-but-identical
          ;;    records in the open, which the next scan/compaction absorbs.
          (let ((tmp (org-glance-graph-v2:--tmp-path graph "headlines.jsonl")))
            (f-write-text "" 'utf-8 tmp)
            (rename-file tmp open t))))
      ;; Content GC for fully-dead ids (idempotent), then reclaim orphan files.
      (dolist (id dead-ids)
        (let ((dir (ignore-errors (org-glance-graph-v2:headline-data-path graph id))))
          (when (and dir (f-exists? dir)) (ignore-errors (f-delete dir t)))))
      (org-glance-graph-v2:--gc-orphans graph)
      (length emit))))

;; (cl-defun org-glance-graph-v2:add-relation (graph relation &rest entities)
;;   (cl-check-type graph org-glance-graph-v2)
;;   (cl-check-type relation symbol)
;;   (cl-loop with ids = (->> entities
;;                            (mapcar #'org-glance-headline-metadata-v2:id*)
;;                            (-non-nil))
;;            for id in ids
;;            collect (--> (org-glance-graph-v2:get-headline graph id)
;;                         (org-glance-headline-metadata-v2:serialize it)
;;                         (plist-put it :relations (list relation (vconcat (plist-get it :relations) (apply #'vector (remove id ids))))))
;;            into spec
;;            finally do (apply #'org-glance-graph-v2:insert graph spec)))

;; (let* ((graph (org-glance-graph-v2 "/tmp/glance"))
;;        (foo (org-glance-headline-v2--from-lines "* foo :a:" "- [[http://10.17.2.107:3002/overview/activity/timeline][Web UI]]"))
;;        (bar (org-glance-headline-v2--from-lines "* bar :b:" "123"))
;;        (foo* (org-glance-headline-metadata-v2 graph foo))
;;        (bar* (org-glance-headline-metadata-v2 graph bar)))
;;   (org-glance-graph-v2:add graph foo* bar*)
;;   (org-glance-graph-v2:add-relation graph 'neighbors foo* bar*))

;; (let* ((graph (org-glance-graph-v2 "/tmp/glance"))
;;        (meta (org-glance-graph-v2:get-headline graph "575cd2ec-8184-4d75-8f15-526dd9e76c8b"))
;;        ;; (id (org-glance-headline-metadata-v2:id meta))
;;        )
;;   ;; (org-glance-graph-v2:delete graph id)
;;   ;; (org-glance-headline-metadata-v2:relations meta)
;;   meta
;;   )

(cl-defun org-glance-graph-v2:capture-buffer (&optional (buffer (current-buffer)))
  "Return a list of `org-glance-headline-v2' parsed from BUFFER."
  (cl-check-type buffer buffer)
  (with-current-buffer buffer
    (cl-loop for element in (org-element-map (org-element-parse-buffer 'headline) 'headline #'identity)
             collect (org-glance-headline-v2--from-element element))))

(cl-defun org-glance-graph-v2:capture (graph &optional (buffer (current-buffer)))
  "Ingest BUFFER into GRAPH.
Assign a fresh ORG_GLANCE_ID -- unique within GRAPH's namespace, via
`org-glance-graph-v2:make-id' -- to every headline that lacks one, then add
them all to GRAPH.  Return GRAPH."
  (cl-check-type graph org-glance-graph-v2)
  (with-current-buffer buffer
    (org-with-wide-buffer
     (org-map-entries
      (lambda ()
        (unless (org-entry-get nil "ORG_GLANCE_ID")
          (org-entry-put nil "ORG_GLANCE_ID" (org-glance-graph-v2:make-id graph)))))))
  (apply #'org-glance-graph-v2:add graph (org-glance-graph-v2:capture-buffer buffer)))

(provide 'org-glance-graph-v2)
