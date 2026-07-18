;;; org-glance-property-index.el --- derived per-headline property index -*- lexical-binding: t; -*-

;;; Commentary:
;; A PURE, rebuildable cache of each live headline's org properties, keyed by id
;; and invalidated by content hash.  It backs the table's custom-column values,
;; the column-add key discovery (a completing-read over the properties the
;; filtered headlines actually carry), and extract -- so those read properties
;; without re-parsing the content blob every time.
;;
;; Two property notions are indexed, both filled in ONE blob parse:
;;   :drawer  the org `:PROPERTIES:' drawer (`node-properties'), shown as columns;
;;   :body    org-glance's body `KEY: value' pairs (`properties'), read by extract.
;;
;; Persisted at `<store>/config/property-index.eld' (alist id -> (HASH :drawer AL
;; :body AL)).  Correctness never depends on it: a cold / stale / git-conflicted
;; entry falls back to an O(N) blob parse, and `org-glance-reindex' drops it.

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'org-glance-utils)
(require 'org-glance-headline)
(require 'org-glance-graph)

(defvar org-glance-property-index--cache (make-hash-table :test #'equal)
  "In-session memo: store-path -> hash-table id->(HASH :drawer AL :body AL).
Avoids re-reading the sidecar per cell; our own writes keep it current, and the
per-id content-hash check catches any change made in another process.")

(defconst org-glance-property-index--noise-keys
  '("ORG_GLANCE_ID" "ORG_GLANCE_HASH" "ORG_GLANCE_PROJECT_DIR" "CATEGORY")
  "Drawer keys hidden from column-candidate discovery: org-glance bookkeeping
and org's synthesised CATEGORY (which `node-properties' always injects).")

(cl-defun org-glance-property-index--file (graph)
  "Path of GRAPH's property-index sidecar (may not exist)."
  (f-join (org-glance-graph:store-path graph) "config" "property-index.eld"))

(cl-defun org-glance-property-index--table (graph)
  "GRAPH's index as a live hash-table id->entry (loaded from disk on first use)."
  (let ((key (org-glance-graph:store-path graph)))
    (or (gethash key org-glance-property-index--cache)
        (let ((h (make-hash-table :test #'equal)))
          (dolist (cell (org-glance--read-eld (org-glance-property-index--file graph)))
            (when (consp cell) (puthash (car cell) (cdr cell) h)))
          (puthash key h org-glance-property-index--cache)))))

(cl-defun org-glance-property-index--flush (graph)
  "Persist GRAPH's in-session index to disk (alist form), atomically."
  (when-let ((h (gethash (org-glance-graph:store-path graph)
                         org-glance-property-index--cache)))
    (org-glance--write-eld
     (org-glance-property-index--file graph)
     (cl-loop for id being the hash-keys of h using (hash-values e)
              collect (cons id e)))))

(cl-defun org-glance-property-index--entry (graph id)
  "ID's index entry (HASH :drawer AL :body AL), refreshed on a content-hash miss.
Mutates the in-session table; a cold/stale/absent id costs ONE blob parse (both
alists at once).  Does not flush -- a batch op (`:ensure' / `:keys') does."
  (let* ((h (org-glance-property-index--table graph))
         (meta (org-glance-graph:get-headline graph id))
         (hash (and (org-glance-headline-metadata? meta)
                    (org-glance-headline-metadata:hash meta)))
         (entry (gethash id h)))
    (if (and entry hash (equal (car entry) hash))
        entry
      (let* ((headline (ignore-errors (org-glance-graph:headline graph id)))
             (new (list hash
                        :drawer (and headline
                                     (ignore-errors (org-glance-headline:node-properties headline)))
                        :body   (and headline
                                     (ignore-errors (org-glance-headline:properties headline))))))
        (puthash id new h)
        new))))

(cl-defun org-glance-property-index:drawer (graph id)
  "Alist of ID's drawer properties (UPCASE keys) in GRAPH, via the index."
  (plist-get (cdr (org-glance-property-index--entry graph id)) :drawer))

(cl-defun org-glance-property-index:body (graph id)
  "Alist of ID's body `KEY: value' pairs in GRAPH, via the index."
  (plist-get (cdr (org-glance-property-index--entry graph id)) :body))

(cl-defun org-glance-property-index:property (graph id property)
  "Value of ID's drawer PROPERTY in GRAPH (case-insensitive), via the index."
  (alist-get (upcase (string-trim property))
             (org-glance-property-index:drawer graph id) nil nil #'string=))

(cl-defun org-glance-property-index:ensure (graph ids)
  "Refresh the index for IDS (parse the stale/absent ones) and flush to disk."
  (dolist (id ids) (org-glance-property-index--entry graph id))
  (org-glance-property-index--flush graph))

(cl-defun org-glance-property-index:keys (graph ids)
  "Sorted union of drawer-property KEYS across IDS, refreshing + flushing.
The candidate set for the table's `C-u +' column prompt: every drawer key the
headlines matching the current filter actually carry."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (id ids)
      (dolist (kv (org-glance-property-index:drawer graph id))
        (unless (member (car kv) org-glance-property-index--noise-keys)
          (puthash (car kv) t seen))))          ; drop bookkeeping + CATEGORY
    (org-glance-property-index--flush graph)
    (sort (hash-table-keys seen) #'string<)))

(cl-defun org-glance-property-index:clear (graph)
  "Drop GRAPH's property index (in-session memo + on-disk file); rebuilds lazily."
  (remhash (org-glance-graph:store-path graph) org-glance-property-index--cache)
  (let ((f (org-glance-property-index--file graph)))
    (when (f-exists? f) (f-delete f))))

(provide 'org-glance-property-index)
;;; org-glance-property-index.el ends here
