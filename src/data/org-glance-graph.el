;; -*- lexical-binding: t -*-

(require 'cl-macs)
(require 'dash)
(require 'f)
(require 'org)
(require 'org-id)

(require 'org-glance-utils)
(require 'org-glance-headline)

(defcustom org-glance-directory org-directory
  "Main location for all Org mode content managed by `org-glance`."
  :group 'org-glance
  :type 'directory)

;; NB: the hash must canonicalize exactly like the equality does -- hashing the
;; raw string would make equal keys ("foo" vs "foo/") land in different buckets.
(define-hash-table-test 'org-glance-graph:test
                        (lambda (a b) (f-equal? (file-truename a) (file-truename b)))
                        (lambda (a) (secure-hash 'sha1 (file-truename a))))

(defvar org-glance-graph:list (make-hash-table :test 'org-glance-graph:test)
  "Registered instances of `org-glance-graph' in current session.")

;; Single-user assumption: no mutex / locking (see docs/archive/MIGRATION-PLAN.md, decision 3).
(cl-defstruct (org-glance-graph (:predicate org-glance-graph?)
                                   (:conc-name org-glance-graph:))
  (directory org-glance-directory :read-only t :type directory)
  ;; Cached store-global monotonic record ordinal; re-derived from disk at open
  ;; (`--max-seq'), `cl-incf'-ed per appended record.  Storage ordinal only --
  ;; never part of headline metadata.
  (seq 0 :type integer)
  ;; Resolved store paths, memoized (pure functions of the read-only `directory';
  ;; see `org-glance-graph--path').  Avoids a `file-truename' per read.
  (-paths nil)
  ;; In-memory read cache: nil (cold), else a plist
  ;; (:snapshot S :by-id HASH :live LIVE-STRUCTS).
  ;; Valid while the store SNAPSHOT (`--store-snapshot') is unchanged; cleared
  ;; in-process by every mutation (`--invalidate-cache').  See `--ensure-cache'.
  (-meta-cache nil))

(defcustom org-glance-graph-segment-max-bytes (* 256 1024)
  "Soft maximum size, in bytes, of the open metadata segment before it is
sealed into an immutable segment.  Checked after each insert's whole batch is
appended, so a record/batch is never split; a single oversized batch may push a
segment past this bound (the cap is soft)."
  :group 'org-glance
  :type 'integer)

(defcustom org-glance-graph-compact-segment-count 4
  "Compact the metadata store automatically once this many sealed segments
accumulate.  Set to a very large value to effectively disable auto-compaction
\(then use \\[org-glance-graph-compact])."
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
  ;; Content-derived projection flags (for view filters). Absent (nil) on records
  ;; written before these fields existed -- `M-x org-glance-reindex' backfills them.
  (linked? nil :read-only t :type boolean)
  (propertized? nil :read-only t :type boolean)
  (encrypted? nil :read-only t :type boolean))

(defconst org-glance-headline-metadata:fields
  ;; SLOT          JSON-KEY      FROM-HEADLINE                                                ENCODE       DECODE
  `((id            :id           ,#'org-glance-headline:id                                 nil          nil)
    (state         :state        ,#'org-glance-headline:state                              nil          nil)
    (title         :title        ,#'org-glance-headline:title                              nil          nil)
    (tags          :tags         ,#'org-glance-headline:tags                               tags-vector  tags-list)
    (hash          :hash         ,#'org-glance-headline:hash                               nil          nil)
    (schedule      :schedule     ,#'org-glance-headline:schedule                           nil          nil)
    (deadline      :deadline     ,#'org-glance-headline:deadline                           nil          nil)
    (priority      :priority     ,#'org-glance-headline:priority                           nil          nil)
    (linked?       :linked       ,(lambda (h) (and (org-glance-headline:links h) t))       nil          bool)
    (propertized?  :propertized  ,(lambda (h) (and (org-glance-headline:properties h) t))  nil          bool)
    (encrypted?    :encrypted    ,(lambda (h) (and (org-glance-headline:encrypted? h) t))  nil          bool))
  "The single source of truth for the metadata projection's shape.
Drives the `org-glance-headline:metadata' constructor, `serialize' and
`deserialize' together, so the four can never drift (a hand-written
`deserialize' line was silently forgettable -- the field then read as
always-nil).  Adding a projection field = one row here + one struct slot
\(checked against this table at load).  Row order IS the serialized JSON key
order -- a byte-stability contract with the on-disk store; append new fields
at the end.  SCHEDULE/DEADLINE come back as raw strings (or nil) from the
headline methods, so they are JSON-serializable as-is.")

;; Load-time guard: the struct and the table must list the same slots, in order.
(let ((struct-slots (mapcar #'car (cdr (cl-struct-slot-info 'org-glance-headline-metadata))))
      (table-slots (mapcar #'car org-glance-headline-metadata:fields)))
  (unless (equal struct-slots table-slots)
    (error "org-glance: metadata field table out of sync with the struct: %S vs %S"
           table-slots struct-slots)))

(cl-defun org-glance-headline-metadata--encode (kind value)
  "Serialize-side coercion for a field of ENCODE kind KIND."
  (pcase kind
    ('tags-vector (->> value (mapcar (-partial #'format "%s")) (apply #'vector)))
    (_ value)))

(cl-defun org-glance-headline-metadata--decode (kind value)
  "Deserialize-side coercion for a field of DECODE kind KIND."
  (pcase kind
    ('bool (eq t value))                ; JSON false/null both read as nil
    ('tags-list (append value nil))
    (_ value)))

(cl-defun org-glance-headline:metadata (headline)
  (cl-check-type headline org-glance-headline)
  ;; The four content-derived fields (hash/linked?/propertized?/encrypted?) each
  ;; reparse the same blob via their own thunk; compute them in ONE `org-mode'
  ;; pass (`--content-facts') -- one reparse per metadata build instead of four
  ;; (matters on save/reindex/bulk).  The rest are cheap struct-slot reads.
  (let ((facts (org-glance-headline--content-facts headline)))
    (apply #'make-org-glance-headline-metadata
           (cl-loop for (slot _json from) in org-glance-headline-metadata:fields
                    append (list (intern (concat ":" (symbol-name slot)))
                                 (pcase slot
                                   ('hash         (plist-get facts :hash))
                                   ('linked?      (plist-get facts :linked))
                                   ('propertized? (plist-get facts :propertized))
                                   ('encrypted?   (plist-get facts :encrypted))
                                   (_             (funcall from headline))))))))

(cl-defun org-glance-headline:metadata* (obj)
  "Generic variant of `org-glance-headline:metadata'."
  (cl-typecase obj
    (org-glance-headline-metadata obj)
    (org-glance-headline (org-glance-headline:metadata obj))))

(cl-defun org-glance-headline-metadata:serialize* (obj)
  "Generic variant of `org-glance-headline-metadata:serialize'."
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
  "Non-nil if METADATA is not done.  Derived from `state' (always present), so
it works on records written before any later schema additions."
  (cl-check-type metadata org-glance-headline-metadata)
  (not (org-glance-headline-metadata:done? metadata)))

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

(cl-defun org-glance-graph--path (graph key thunk)
  "Return GRAPH's resolved path for KEY, computing it via THUNK once and memoizing.
Store paths are pure functions of the read-only `directory' slot, so they never
change for a graph -- caching them removes a `file-truename' from every read's
snapshot stat (see `--store-snapshot')."
  (or (plist-get (org-glance-graph:-paths graph) key)
      (let ((v (funcall thunk)))
        (setf (org-glance-graph:-paths graph)
              (plist-put (org-glance-graph:-paths graph) key v))
        v)))

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
      (org-glance-graph--resolve-jsonl-conflicts graph)  ; heal markers a pre-driver sync left
      (org-glance-graph--migrate-maybe graph)      ; bootstrap MANIFEST / adopt legacy file
      (org-glance-graph--reconcile-manifest graph) ; rebuild a git-mangled MANIFEST
      (org-glance-graph--heal graph)               ; recover seal, derive seq, reap orphans
      (puthash directory graph org-glance-graph:list))
    graph))

(cl-defun org-glance-graph:insert (graph meta)
  (declare (indent 1))
  (cl-check-type meta list)
  (cl-check-type (car meta) (or list org-glance-headline-metadata))
  (org-glance-graph--append graph meta))

;;; Segmented (LSM-lite) metadata store
;;
;; `meta/headlines.jsonl' is the OPEN append segment; once it crosses
;; `org-glance-graph-segment-max-bytes' it is sealed (atomic rename) into an
;; immutable `seg-NNNNNNNNNN.jsonl' and a fresh open segment takes over.  A
;; one-line JSON MANIFEST lists the live sealed segments (oldest-first); readers
;; merge segments, latest-per-id wins.  Each record carries a store-global
;; monotonic `seq' ordinal.  Every mutation is crash-safe via temp-then-rename;
;; the MANIFEST rename is the sole commit point.  See docs/archive/MIGRATION-PLAN.md Phase 4.

(cl-defun org-glance-graph--open-segment-path (graph)
  "The open append segment -- identical to `headline-meta-path' (the store-change
signal for the overview cache, and the migration-adoption target)."
  (org-glance-graph:headline-meta-path graph))

(cl-defun org-glance-graph--manifest-path (graph)
  (org-glance-graph--path
   graph :manifest
   (lambda () (-> (f-join (org-glance-graph:meta-path graph) "MANIFEST") (file-truename)))))

(cl-defun org-glance-graph--segment-path (graph gen)
  (-> (f-join (org-glance-graph:meta-path graph) (format "seg-%010d.jsonl" gen))
      (file-truename)))

(cl-defun org-glance-graph--tmp-path (graph base &optional dir)
  "A unique temp path under DIR (default the meta dir) sharing BASE's `.tmp.' name.
The single naming convention every temp-then-rename commit uses."
  (make-temp-name (f-join (or dir (org-glance-graph:meta-path graph)) (concat base ".tmp."))))

(cl-defun org-glance-graph--atomic-write (graph path content &optional (overwrite t))
  "Write CONTENT to a temp file in PATH's dir, then rename it over PATH.
Crash-safe: the single temp-then-rename commit the MANIFEST/seal/compact paths
use.  OVERWRITE is passed to `rename-file' (nil requires PATH not pre-exist --
the orphan-segment no-clobber case)."
  (let ((tmp (org-glance-graph--tmp-path
              graph (file-name-nondirectory path) (file-name-directory path))))
    (f-write-text content 'utf-8 tmp)
    (rename-file tmp path overwrite)))

(defconst org-glance-graph--segment-name-re "\\`seg-\\([0-9]+\\)\\.jsonl\\'"
  "Matches a sealed segment basename; group 1 is its generation number.")

(cl-defun org-glance-graph--segment-generation (name)
  "Generation number of sealed segment basename NAME, or nil if not a segment."
  (when (string-match org-glance-graph--segment-name-re name)
    (string-to-number (match-string 1 name))))

(cl-defun org-glance-graph--sealed-segments (graph)
  "Basenames of the live sealed segments, oldest-first, per the MANIFEST.
An absent or unparseable MANIFEST (impossible after construction -- the swap is
an atomic rename) reads as the empty set."
  (let ((path (org-glance-graph--manifest-path graph)))
    (when (f-exists? path)
      (condition-case nil
          (-> (f-read-text path 'utf-8)
              (json-parse-string :object-type 'plist)
              (plist-get :segments)
              (append nil))
        (error nil)))))

(cl-defun org-glance-graph--write-manifest (graph segments)
  "Atomically persist SEGMENTS (sealed basenames, oldest-first) as the MANIFEST.
The temp-then-rename is the sole commit/swap point for the live segment set."
  (org-glance-graph--atomic-write
   graph (org-glance-graph--manifest-path graph)
   (concat (json-serialize (list :version 2 :segments (apply #'vector segments))) "\n")))

(cl-defun org-glance-graph--live-segments (graph &optional newest-first)
  "Existing absolute paths of the live segments, oldest->newest (open last);
reversed (open first) when NEWEST-FIRST.  Orphans (on disk, not in the MANIFEST)
are invisible."
  (let* ((meta (org-glance-graph:meta-path graph))
         (sealed (cl-loop for name in (org-glance-graph--sealed-segments graph)
                          for p = (file-truename (f-join meta name))
                          when (f-exists? p) collect p))
         (open (org-glance-graph--open-segment-path graph))
         (all (append sealed (when (f-exists? open) (list open)))))
    (if newest-first (reverse all) all)))

(cl-defun org-glance-graph--next-generation (graph)
  "1 + the highest seg-NNN generation on disk (filenames are the authority)."
  (1+ (or (cl-loop for f in (directory-files (org-glance-graph:meta-path graph) nil
                                             org-glance-graph--segment-name-re)
                   maximize (org-glance-graph--segment-generation f))
          0)))

(cl-defun org-glance-graph--scan-file (graph path fn)
  "Call FN on each non-empty UTF-8 JSON record in GRAPH's segment PATH, top-down.
The scanner owns the torn-line policy: only the OPEN segment can legitimately
have a torn (newline-less) final line -- a crash can only tear the last append
-- so a parse error there is ignored; elsewhere it re-signals (corruption)."
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
  "Call FN on every record across live segments, oldest->newest (open last)."
  (dolist (seg (org-glance-graph--live-segments graph))
    (org-glance-graph--scan-file graph seg fn)))

(cl-defun org-glance-graph--latest-records (graph)
  "Cons of (RECORDS . TOTAL): the latest record per id, tombstones included, in
original insertion (first-sighting) order across all live segments; TOTAL is the
raw record count before the latest-per-id fold."
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
;;
;; `--latest-records' re-reads and re-parses every record in every live segment.
;; Every hot read (`:headlines'/`:get-headline'/`:tags'/`:states') went through
;; it, so listing N headlines and then opening one re-scanned the whole store
;; twice.  `--ensure-cache' folds the store ONCE per store-version and serves all
;; of them: `:headlines' from the cached live list, `:get-headline' as an O(1)
;; hash lookup.
;;
;; Coherence has two independent guards, so a stale row is impossible:
;;   * cross-process -- the cache carries the store SNAPSHOT it was built under
;;     (open-segment mtime+size, plus the live sealed-segment NAME list); a read
;;     re-stats and rebuilds on any change, catching another Emacs writing to the
;;     same store.  Size catches a same-second append (the file grows); the
;;     segment names catch a seal/compaction independent of mtime granularity.
;;   * in-process -- every mutation (`--append'/`compact'/`--heal') calls
;;     `--invalidate-cache', so our OWN writes never depend on mtime granularity.

(cl-defun org-glance-graph--store-snapshot (graph)
  "A value identifying the store's current state for cache validity.
List of (OPEN-MTIME OPEN-SIZE SEALED-SEGMENTS); `equal' compares two snapshots.
The open segment's mtime+size catch an append (size strictly grows) and the open
reset; the live sealed-segment NAME list catches a seal or a compaction.

The segment NAMES are used -- not the MANIFEST's mtime -- so detection never
depends on filesystem mtime resolution.  This closes a resurrection hole an
adversarial review found: a compaction rewrites the open EMPTY (size back to 0)
and swaps the live set, so on a coarse-granularity clock (FAT / SMB / old ext) a
same-second external compaction by another Emacs could otherwise repeat a purely
mtime-based snapshot (OPEN-MTIME, 0, MANIFEST-MTIME) while dropping a tombstone,
serving a deleted headline as live.  Segment names provably change on every seal
and every compaction (each mints a new `seg-<gen>'), independent of any clock.
`--sealed-segments' reads the MANIFEST fresh (never memoized), so this dimension
is always current."
  (let ((oa (file-attributes (org-glance-graph--open-segment-path graph))))
    (list (and oa (file-attribute-modification-time oa))
          (and oa (file-attribute-size oa))
          (org-glance-graph--sealed-segments graph))))

(cl-defun org-glance-graph--invalidate-cache (graph)
  "Drop GRAPH's in-memory read cache.  Called by every mutation so an in-process
write is reflected immediately, independent of filesystem mtime resolution."
  (setf (org-glance-graph:-meta-cache graph) nil))

(cl-defun org-glance-graph--ensure-cache (graph)
  "Return GRAPH's read cache, rebuilding it iff the store snapshot changed.
The cache is a plist (:snapshot S :by-id HASH :live LIVE).  The latest record
per id (tombstones included, in insertion order) feeds both BY-ID (id -> record,
backing the O(1) `:get-headline') and LIVE (the deserialized non-tombstoned
metadata in the same order, exactly what `:headlines' returns)."
  (let ((snap (org-glance-graph--store-snapshot graph))
        (cache (org-glance-graph:-meta-cache graph)))
    (unless (and cache (equal (plist-get cache :snapshot) snap))
      (let ((records (car (org-glance-graph--latest-records graph)))
            (by-id (make-hash-table :test 'equal))
            (live nil))
        (dolist (record records)
          (puthash (plist-get record :id) record by-id))
        (setq live (cl-loop for record in records
                            unless (plist-get record :tombstone)
                            collect (org-glance-headline-metadata:deserialize record)))
        (setf cache (list :snapshot snap :by-id by-id :live live)
              (org-glance-graph:-meta-cache graph) cache)))
    cache))

(cl-defun org-glance-graph--max-seq (graph)
  "Highest `seq' ordinal across live records, or 0 (legacy records lack `seq')."
  (let ((mx 0))
    (org-glance-graph--scan-forward
     graph (lambda (r) (let ((s (plist-get r :seq)))
                         (when (and (integerp s) (> s mx)) (setq mx s)))))
    mx))

(cl-defun org-glance-graph--ensure-newline-terminated (path)
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

(defvar org-glance-graph-before-append-functions nil
  "Abnormal hook run just BEFORE SPECS are appended to a graph's WAL.
Each function is called with (GRAPH SPECS) -- the metadata structs and tombstone
plists about to be appended.  The read cache still shows the PRE-append state,
so a tombstone's tags are resolvable via `org-glance-graph:get-headline'.  For
side indexes such as tag metrics; errors are demoted so a failing hook never
breaks the append.")

(cl-defun org-glance-graph--append (graph specs)
  "Append SPECS (metadata structs or bare plists) to the open segment, stamping a
fresh monotonic `seq' on each, then maybe seal and compact."
  ;; Side indexes (tag metrics) fire here, while the cache still reflects the
  ;; pre-append state so a tombstone's tags resolve.  Demoted: a side index must
  ;; never break a save.
  (with-demoted-errors "org-glance: before-append hook: %S"
    (run-hook-with-args 'org-glance-graph-before-append-functions graph specs))
  (let ((open (org-glance-graph--open-segment-path graph)))
    (org-glance-graph--ensure-newline-terminated open)
    (cl-loop for spec in specs
             for record = (plist-put (copy-sequence (org-glance-headline-metadata:serialize* spec))
                                     :seq (cl-incf (org-glance-graph:seq graph)))
             collect (json-serialize record) into jsons
             finally (f-append-text (concat (s-join "\n" jsons) "\n") 'utf-8 open)))
  (org-glance-graph--maybe-seal graph)
  (org-glance-graph--maybe-compact graph)
  (org-glance-graph--invalidate-cache graph))

(cl-defun org-glance-graph--maybe-seal (graph)
  (let ((open (org-glance-graph--open-segment-path graph)))
    (when (> (or (f-size open) 0) org-glance-graph-segment-max-bytes)
      (org-glance-graph--seal graph))))

(cl-defun org-glance-graph--seal (graph)
  "Seal the full open segment into an immutable seg-<gen>.
Commit = the MANIFEST swap.  A crash before it leaves seg-<gen> durable but
unlisted with an empty open -- `--heal' adopts it on the next open.
Does NOT itself drop the read cache: its only caller is `--maybe-seal' (within
`--append', which invalidates).  A future direct caller should invalidate too,
though `--store-snapshot's segment-name dimension would catch the new seg-<gen>
on the next read regardless."
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
  "Hidden per-directory store root for the graph.
Dot-prefixed so `org-agenda' (and legacy v1 tag discovery, which matched
`^[[:word:]]+') ignore it."
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

(cl-defun org-glance-graph:headline-meta-path (graph)
  (cl-check-type graph org-glance-graph)
  (org-glance-graph--path
   graph :headline-meta
   (lambda () (-> (f-join (org-glance-graph:meta-path graph) "headlines.jsonl") (file-truename)))))

(cl-defun org-glance-graph:headline-data-path (graph id)
  "Content-addressable directory for ID's data blob within GRAPH.
Long ids (e.g. UUIDs) are sharded by their first two characters."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (cl-assert (not (string-empty-p id)))
  ;; Path-safety: ID feeds straight into `f-join'/`substring', so reject path
  ;; separators and traversal -- a hand-edited ORG_GLANCE_ID must not escape the
  ;; store (auto-generated UUIDs and tag-hash ids are already safe). Use `error',
  ;; not `cl-assert': the latter can be optimized out, and its
  ;; `cl-assertion-failed' is not a portable `error' subtype across Emacs versions.
  (when (or (string-match-p "/" id) (string-match-p "\\.\\." id))
    (error "Unsafe ORG_GLANCE_ID for content-addressable path: %S" id))
  (let ((data (org-glance-graph:data-path graph)))
    (file-truename
     (if (> (length id) 2)
         (f-join data (substring id 0 2) (substring id 2))
       (f-join data id)))))

(cl-defun org-glance-graph:content-path (graph id)
  "Path to ID's stored content blob under GRAPH's data store."
  (f-join (org-glance-graph:headline-data-path graph id) "data.org"))

(cl-defun org-glance-graph:put-content (graph headline)
  "Persist HEADLINE's contents under GRAPH's data store, keyed by its id.
Return the file path, or nil if HEADLINE has no id.
The blob is written atomically -- to a temp file in the same dir, then renamed
over `data.org' -- so a crash or ENOSPC mid-write can never truncate or corrupt
an existing blob (the same temp-then-rename commit the MANIFEST/seal/compact
paths use)."
  (cl-check-type graph org-glance-graph)
  (cl-check-type headline org-glance-headline)
  (when-let ((id (org-glance-headline:id headline)))
    (let ((dir (org-glance-graph:headline-data-path graph id))
          (path (org-glance-graph:content-path graph id)))
      (f-mkdir-full-path dir)
      (org-glance-graph--atomic-write graph path (org-glance-headline:contents headline))
      path)))

(cl-defun org-glance-graph:get-content (graph id)
  "Return the stored contents string for ID in GRAPH, or nil if none.
Low-level: returns the blob regardless of tombstone state."
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
  "Add HEADLINES to GRAPH and return GRAPH.
Each element may be an `org-glance-headline' or pre-built metadata.  Full
headlines also have their contents persisted to the data store."
  (cl-check-type graph org-glance-graph)
  (when headlines
    ;; Compute metadata FIRST: if a projection field errors, nothing is written
    ;; (no blob-saved-but-metadata-missing half-state).
    (let ((specs (mapcar #'org-glance-headline:metadata* headlines)))
      (dolist (headline headlines)
        (when (org-glance-headline? headline)
          (org-glance-graph:put-content graph headline)))
      (org-glance-graph:insert graph specs)))
  graph)

(cl-defun org-glance-graph:get-headline (graph id)
  "Return the most recent metadata for ID in GRAPH.
Return the symbol `tombstone' if ID was deleted, or nil if unknown.
O(1): a hash lookup into the cached latest-record-per-id set (`--ensure-cache'),
which agrees with the old newest-first segment scan -- the cache keeps the last
record written for each id, tombstones included."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (let ((record (gethash id (plist-get (org-glance-graph--ensure-cache graph) :by-id))))
    (cond ((null record) nil)
          ((plist-get record :tombstone) 'tombstone)
          (t (org-glance-headline-metadata:deserialize record)))))

(cl-defun org-glance-graph:headline (graph id)
  "Return the full live `org-glance-headline' stored for ID, or nil.
Reconstructs the headline from its persisted contents; returns nil for
unknown or tombstoned ids."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (when (org-glance-headline-metadata? (org-glance-graph:get-headline graph id))
    (-some-> (org-glance-graph:get-content graph id)
      (org-glance-headline--from-string))))

(cl-defun org-glance-graph:delete (graph id)
  "Append a tombstone for ID unless it is already absent or deleted."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (let ((meta (org-glance-graph:get-headline graph id)))
    (unless (memq meta '(nil tombstone))
      (org-glance-graph:insert graph (list (list :id id :tombstone t))))))

(cl-defun org-glance-graph:headlines (graph)
  "Return all live (non-tombstoned) headline metadata in GRAPH.
The latest record per id wins; original insertion order is preserved across all
segments (first-sighting in the oldest->newest scan == earliest insertion).
Served from the in-memory cache (`--ensure-cache').  Returns a FRESH list each
call (a shallow copy of the cached live list), so a caller may sort/nreverse the
result without corrupting the cache; the metadata structs are shared but
immutable (`:read-only' slots)."
  (cl-check-type graph org-glance-graph)
  (copy-sequence (plist-get (org-glance-graph--ensure-cache graph) :live)))

(cl-defun org-glance-graph--sorted-distinct (strings)
  "Return STRINGS de-duplicated and sorted with `string<'."
  (sort (-distinct strings)               ; hash-backed O(N) dedup (was O(N^2))
        #'string<))

(cl-defun org-glance-graph:tags (graph)
  "Distinct tags across GRAPH's live headlines, sorted."
  (cl-check-type graph org-glance-graph)
  (org-glance-graph--sorted-distinct
   (cl-loop for meta in (org-glance-graph:headlines graph)
            append (mapcar (lambda (x) (format "%s" x))
                           (append (org-glance-headline-metadata:tags meta) nil)))))

(cl-defun org-glance-graph:states (graph)
  "Distinct non-empty todo states across GRAPH's live headlines, sorted."
  (cl-check-type graph org-glance-graph)
  (org-glance-graph--sorted-distinct
   (cl-loop for meta in (org-glance-graph:headlines graph)
            for state = (org-glance-headline-metadata:state meta)
            when (org-glance--present-string? state)
            collect state)))

(cl-defun org-glance-graph:reindex (graph)
  "Re-derive metadata for every live headline in GRAPH from its stored content,
appending fresh records so newly-added projection fields get populated.
Return the number of headlines re-indexed."
  (cl-check-type graph org-glance-graph)
  (let* ((metas (org-glance-graph:headlines graph))
         (reporter (and metas (make-progress-reporter "org-glance: re-indexing... " 0 (length metas))))
         (n 0))
    (cl-loop for meta in metas
             for i from 1
             for id = (org-glance-headline-metadata:id meta)
             for contents = (org-glance-graph:get-content graph id)
             do (when contents
                  (org-glance-graph:add graph (org-glance-headline--from-string contents))
                  (cl-incf n))
             do (when reporter (progress-reporter-update reporter i)))
    (when reporter (progress-reporter-done reporter))
    n))

;;; Store bootstrap / recovery / compaction

(cl-defun org-glance-graph--ensure-gitattributes (graph)
  "Write the `merge=union' git driver for GRAPH's *.jsonl files, if absent.
Concurrent appends to `headlines.jsonl' on two machines conflict when the store
dir is synced via git; the built-in `union' driver keeps every line from both
sides, and the positional last-wins reader (`--latest-records') resolves any
duplicate id.  Write-if-absent and idempotent: never clobber an existing (maybe
hand-edited) file.  `union' is built in, so no `git config' is needed."
  (let ((path (f-join (org-glance-graph:meta-path graph) ".gitattributes")))
    (unless (f-exists? path)
      (f-write-text "*.jsonl merge=union\n" 'utf-8 path))))

(cl-defun org-glance-graph--manifest-broken? (text)
  "Non-nil when MANIFEST TEXT cannot be trusted as the live segment set.
Broken means: no content (nil or blank), git conflict markers (a line opening
with `<<<<<<<', `=======', or `>>>>>>>'), unparseable JSON, or a parse lacking
a `:segments' vector.  MANIFEST is not `*.jsonl', so git's default merge leaves
conflict markers rather than a union -- the reader must notice and rebuild."
  (or (null text)
      (string-empty-p (string-trim text))
      (org-glance--conflict-marked? text)
      (condition-case nil
          (not (vectorp (plist-get (json-parse-string text :object-type 'plist)
                                   :segments)))
        (error t))))

(cl-defun org-glance-graph--reconcile-manifest (graph)
  "Rebuild GRAPH's MANIFEST from the on-disk segments when it is unusable.
A git sync can leave the MANIFEST conflict-marked or otherwise mangled (see
`--manifest-broken?').  A valid MANIFEST is left byte-for-byte untouched (the
byte-stability contract, and to not fight compaction).  A broken one is rebuilt
by listing every non-empty on-disk seg-*.jsonl (excluding the open segment and
temp files), oldest-first by generation, through `--write-manifest' so the
canonical format is identical.  Run before `--heal' so a segment sealed on
another machine and synced in is adopted here, never reaped as an orphan."
  (let* ((path (org-glance-graph--manifest-path graph))
         (text (when (f-exists? path) (f-read-text path 'utf-8))))
    (when (org-glance-graph--manifest-broken? text)
      (let ((meta (org-glance-graph:meta-path graph)))
        (org-glance-graph--write-manifest
         graph
         (sort (cl-loop for name in (directory-files
                                     meta nil org-glance-graph--segment-name-re)
                        when (> (or (f-size (f-join meta name)) 0) 0)
                        collect name)
               #'string<))))))

(cl-defun org-glance-graph--conflicted-jsonl-files (graph)
  "List of GRAPH meta/*.jsonl paths that carry git conflict markers.
A store synced before the `union' driver was in place can arrive with markers
already written into a segment; the JSONL reader would choke on those lines."
  (let ((meta (org-glance-graph:meta-path graph)))
    (cl-loop for name in (directory-files meta nil "\\.jsonl\\'")
             for path = (f-join meta name)
             when (org-glance--conflict-marked? (f-read-text path 'utf-8))
             collect path)))

(cl-defun org-glance-graph--union-resolve-file (path)
  "Strip git conflict markers from PATH by union merge; return blocks resolved.
Keeps both sides' content lines via `org-glance--strip-conflict-markers' --
exactly what git's built-in `union' driver produces, so the positional last-wins
reader collapses any duplicate id.  Blocks = the `<<<<<<<' count."
  (let* ((text (f-read-text path 'utf-8))
         (blocks (cl-count-if (lambda (l) (string-prefix-p "<<<<<<<" l))
                              (split-string text "\n"))))
    (f-write-text (org-glance--strip-conflict-markers text) 'utf-8 path)
    blocks))

(cl-defun org-glance-graph--resolve-jsonl-conflicts (graph)
  "Resolve git conflict markers in GRAPH's meta/*.jsonl by union merge.
The `union' driver stops future conflicts, but a store synced before it was in
place can carry markers a JSONL reader cannot parse.  Gated by the shared
`org-glance--resolve-conflict' (see `org-glance-conflict-resolution'): `ask'
prompts before rewriting, `union' resolves silently, nil (or a declined prompt)
errors so the store never loads with markers in place."
  (when-let ((files (org-glance-graph--conflicted-jsonl-files graph)))
    (let ((names (mapconcat #'file-name-nondirectory files ", ")))
      (org-glance--resolve-conflict
       names
       (lambda ()
         (let ((blocks (cl-loop for path in files
                                sum (org-glance-graph--union-resolve-file path))))
           (message "org-glance: union-resolved %d git conflict block(s) in %s"
                    blocks names)))))))

(cl-defun org-glance-graph--migrate-maybe (graph)
  "Bootstrap the segmented layout.  If no MANIFEST exists, adopt the present
`headlines.jsonl' in place as the open segment by writing an initial MANIFEST --
no bytes moved.  Idempotent; a no-op once a MANIFEST is present (also covers the
brand-new empty store, whose open segment the constructor just touched)."
  (unless (f-exists? (org-glance-graph--manifest-path graph))
    (org-glance-graph--write-manifest graph nil)))

(cl-defun org-glance-graph--gc-orphans (graph)
  "Delete stale *.tmp.* files and any seg-*.jsonl not referenced by the MANIFEST."
  (let ((meta (org-glance-graph:meta-path graph))
        (live (org-glance-graph--sealed-segments graph)))
    (dolist (f (directory-files meta nil nil t))
      (when (or (string-match-p "\\.tmp\\." f)
                (and (org-glance-graph--segment-generation f) (not (member f live))))
        (ignore-errors (f-delete (f-join meta f)))))))

(cl-defun org-glance-graph--open-empty? (graph)
  "Non-nil when GRAPH's open segment is empty (an absent file reads as empty)."
  (= 0 (or (f-size (org-glance-graph--open-segment-path graph)) 0)))

(cl-defun org-glance-graph--segment-seqs (graph name)
  "List of the `seq' ordinals recorded in sealed segment basename NAME."
  (let (seqs)
    (org-glance-graph--scan-file
     graph (file-truename (f-join (org-glance-graph:meta-path graph) name))
     (lambda (r) (when-let ((s (plist-get r :seq))) (push s seqs))))
    seqs))

(cl-defun org-glance-graph--heal (graph)
  "Recover from an interrupted seal and re-derive session state.  Idempotent.
An unlisted seg-* whose generation exceeds every listed one, alongside an EMPTY
open segment, is a seal whose MANIFEST commit was lost -- adopt it.  Crashed
COMPACTION debris must NOT be adopted: it is told apart because compaction
copies records (it never re-stamps `seq'), so its output shares `seq' ordinals
with the listed segments, whereas a genuinely sealed open segment only holds
ordinals no listed segment has.  Non-adopted orphans are reaped."
  (let* ((meta (org-glance-graph:meta-path graph))
         (listed (org-glance-graph--sealed-segments graph))
         (listed-max (or (cl-loop for n in listed
                                  maximize (org-glance-graph--segment-generation n))
                         0))
         (open-empty (org-glance-graph--open-empty? graph))
         (adopt (and open-empty
                     (cl-loop for f in (directory-files meta nil org-glance-graph--segment-name-re)
                              when (and (not (member f listed))
                                        (> (org-glance-graph--segment-generation f) listed-max))
                              collect f))))
    (when adopt
      ;; Disqualify compaction debris: any candidate sharing a `seq' with a
      ;; listed segment is a copy, not a seal.
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
  "Merge all segments (sealed + open) into one, dropping superseded and dead
records, and GC the content blobs of fully-deleted ids.  Folding the open
segment in keeps the `:headlines' insertion-order contract intact (an id whose
older record lived in a sealed segment must not be \"re-sighted\" later), and
replacing it bumps the store-change signal -- compaction changes observable
reads (a dropped tombstone turns `tombstone' into nil), so caches must
invalidate.  Commit = the MANIFEST swap.  A no-op on an already-compact store.
Return the live record count.  See docs/archive/MIGRATION-PLAN.md Phase 4."
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
      ;; No-op guard: a single sealed segment of exactly the live records and an
      ;; empty open segment is already compact -- write nothing.
      (unless (and (<= (length sealed-names) 1) open-empty
                   (null dead-ids) (= total (length emit)))
        ;; 1. New compacted segment (orphan until the MANIFEST lists it).
        (let (new-names)
          (when emit
            (let* ((gen (org-glance-graph--next-generation graph))
                   (newseg (org-glance-graph--segment-path graph gen)))
              (org-glance-graph--atomic-write
               graph newseg (concat (s-join "\n" (mapcar #'json-serialize emit)) "\n") nil)
              (setq new-names (list (file-name-nondirectory newseg)))))
          ;; 2. Commit BEFORE touching the open segment: a crash before this swap
          ;;    leaves the old MANIFEST, old segments AND the intact open (with
          ;;    any tombstones whose only copy lives there) fully live -- the
          ;;    merged segment is a reapable orphan.  Truncating the open first
          ;;    would let a crash destroy a tombstone's only copy while an old
          ;;    listed segment still holds the headline live (resurrection).
          (org-glance-graph--write-manifest graph new-names)
          ;; 3. Fresh empty open segment (its content was folded in; the new
          ;;    mtime is the store-change signal for this compaction).  A crash
          ;;    between the swap and this point leaves duplicate-but-identical
          ;;    records in the open, which the next scan/compaction absorbs.
          (org-glance-graph--atomic-write graph open "")))
      ;; Content GC for fully-dead ids (idempotent), then reclaim orphan files.
      (dolist (id dead-ids)
        (let ((dir (ignore-errors (org-glance-graph:headline-data-path graph id))))
          (when (and dir (f-exists? dir)) (ignore-errors (f-delete dir t)))))
      (org-glance-graph--gc-orphans graph)
      (org-glance-graph--invalidate-cache graph)
      (length emit))))

;; (cl-defun org-glance-graph:add-relation (graph relation &rest entities)
;;   (cl-check-type graph org-glance-graph)
;;   (cl-check-type relation symbol)
;;   (cl-loop with ids = (->> entities
;;                            (mapcar #'org-glance-headline-metadata:id*)
;;                            (-non-nil))
;;            for id in ids
;;            collect (--> (org-glance-graph:get-headline graph id)
;;                         (org-glance-headline-metadata:serialize it)
;;                         (plist-put it :relations (list relation (vconcat (plist-get it :relations) (apply #'vector (remove id ids))))))
;;            into spec
;;            finally do (apply #'org-glance-graph:insert graph spec)))

;; (let* ((graph (org-glance-graph "/tmp/glance"))
;;        (foo (org-glance-headline--from-lines "* foo :a:" "- [[http://10.17.2.107:3002/overview/activity/timeline][Web UI]]"))
;;        (bar (org-glance-headline--from-lines "* bar :b:" "123"))
;;        (foo* (org-glance-headline-metadata graph foo))
;;        (bar* (org-glance-headline-metadata graph bar)))
;;   (org-glance-graph:add graph foo* bar*)
;;   (org-glance-graph:add-relation graph 'neighbors foo* bar*))

;; (let* ((graph (org-glance-graph "/tmp/glance"))
;;        (meta (org-glance-graph:get-headline graph "575cd2ec-8184-4d75-8f15-526dd9e76c8b"))
;;        ;; (id (org-glance-headline-metadata:id meta))
;;        )
;;   ;; (org-glance-graph:delete graph id)
;;   ;; (org-glance-headline-metadata:relations meta)
;;   meta
;;   )

(cl-defun org-glance-graph:capture-buffer (&optional (buffer (current-buffer)))
  "Return a list of `org-glance-headline' parsed from BUFFER."
  (cl-check-type buffer buffer)
  (with-current-buffer buffer
    (cl-loop for element in (org-element-map (org-element-parse-buffer 'headline) 'headline #'identity)
             collect (org-glance-headline--from-element element))))

(cl-defun org-glance-graph:capture (graph &optional (buffer (current-buffer)))
  "Ingest BUFFER into GRAPH.
Assign a fresh ORG_GLANCE_ID -- unique within GRAPH's namespace, via
`org-glance-graph:make-id' -- to every headline that lacks one, then add
them all to GRAPH.  Return GRAPH."
  (cl-check-type graph org-glance-graph)
  (with-current-buffer buffer
    (org-with-wide-buffer
     (org-map-entries
      (lambda ()
        (unless (org-entry-get nil "ORG_GLANCE_ID")
          (org-entry-put nil "ORG_GLANCE_ID" (org-glance-graph:make-id graph)))))))
  (apply #'org-glance-graph:add graph (org-glance-graph:capture-buffer buffer)))

(provide 'org-glance-graph)
