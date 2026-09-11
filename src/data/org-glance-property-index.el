;;; org-glance-property-index.el --- derived per-headline property index -*- lexical-binding: t; -*-

;; Invariant 5: a pure derived cache -- hash-guarded reads, O(N) blob fallback.

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'org-glance-utils)
(require 'org-glance-headline)
(require 'org-glance-graph)

(defvar org-glance-property-index--cache (make-hash-table :test #'equal)
  "In-session memo: store-path -> hash-table id->(HASH :drawer AL :body AL).")

(defvar org-glance-property-index--dirty (make-hash-table :test #'equal)
  "Set of store-paths whose memo re-parsed a blob since the last flush.")

(cl-defun org-glance-property-index--candidate-key? (key)
  "Non-nil if drawer KEY is a user-authored column candidate (invariant 16)."
  (not (or (string-prefix-p "ORG_GLANCE_" key)
           (equal key "CATEGORY"))))

(cl-defun org-glance-property-index--file (graph)
  "Path of GRAPH's property-index sidecar (may not exist)."
  (org-glance-graph:cache-file graph "property-index.eld"))

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
  (when-let* ((h (gethash (org-glance-graph:store-path graph)
                         org-glance-property-index--cache)))
    (org-glance--write-eld
     (org-glance-property-index--file graph)
     (cl-loop for id being the hash-keys of h using (hash-values e)
              collect (cons id e)))))

(cl-defun org-glance-property-index--flush-if-dirty (graph)
  "Persist GRAPH's index only if a blob was re-parsed since the last flush."
  (let ((key (org-glance-graph:store-path graph)))
    (when (gethash key org-glance-property-index--dirty)
      (org-glance-property-index--flush graph)
      (remhash key org-glance-property-index--dirty))))

(cl-defun org-glance-property-index--entry (graph id)
  "Return GRAPH's entry (HASH :drawer AL :body AL) for ID, parsed on a miss.
A hash miss costs one blob parse, memoized unless ID is gone; never flushes."
  (let* ((h (org-glance-property-index--table graph))
         (meta (org-glance-graph:live-meta graph id))
         (hash (and meta (org-glance-headline-metadata:hash meta)))
         (entry (gethash id h)))
    (if (and entry hash (equal (car entry) hash))
        entry
      (let* ((headline (ignore-errors (org-glance-graph:headline graph id)))
             (new (list hash
                        :drawer (ignore-errors (org-glance-headline:node-properties headline))
                        :body   (ignore-errors (org-glance-headline:properties headline)))))
        (when headline                  ; skip gone ids -- do not cache or persist
          (puthash id new h)
          (puthash (org-glance-graph:store-path graph) t
                   org-glance-property-index--dirty))
        new))))

(cl-defun org-glance-property-index--field (graph id key)
  "Value of ID's index field KEY (`:drawer' or `:body') in GRAPH."
  (plist-get (cdr (org-glance-property-index--entry graph id)) key))

(cl-defun org-glance-property-index:drawer (graph id)
  "Alist of ID's drawer properties (UPCASE keys) in GRAPH, via the index."
  (org-glance-property-index--field graph id :drawer))

(cl-defun org-glance-property-index:body (graph id)
  "Alist of ID's body `KEY: value' pairs in GRAPH, via the index."
  (org-glance-property-index--field graph id :body))

(cl-defun org-glance-property-index:property (graph id property)
  "Value of ID's drawer PROPERTY in GRAPH (case-insensitive), via the index."
  (alist-get (org-glance--property-key property)
             (org-glance-property-index:drawer graph id) nil nil #'string=))

(cl-defun org-glance-property-index:ensure (graph ids)
  "Refresh GRAPH's index for IDS (parse stale/absent ones); flush if changed."
  (dolist (id ids) (org-glance-property-index--entry graph id))
  (org-glance-property-index--flush-if-dirty graph))

(cl-defun org-glance-property-index:keys (graph ids)
  "Return the sorted union of candidate drawer keys across IDS in GRAPH.
These feed the table's `C-c +' column prompt; nothing is written to disk."
  (org-glance--sorted-distinct
   (cl-loop for id in ids
            append (cl-loop for kv in (org-glance-property-index:drawer graph id)
                            when (org-glance-property-index--candidate-key? (car kv))
                            collect (car kv)))))

(cl-defun org-glance-property-index--prune-legacy (graph)
  "Delete GRAPH's sidecar from its legacy config location, on graph open."
  (let ((legacy (org-glance-graph:config-file graph "property-index.eld")))
    (when (f-exists? legacy) (f-delete legacy))))
(add-hook 'org-glance-graph-after-open-functions
          #'org-glance-property-index--prune-legacy)

(cl-defun org-glance-property-index:clear (graph)
  "Drop GRAPH's property index (in-session memo + on-disk file); rebuilds lazily."
  (let ((key (org-glance-graph:store-path graph)))
    (remhash key org-glance-property-index--cache)
    (remhash key org-glance-property-index--dirty))
  (let ((f (org-glance-property-index--file graph)))
    (when (f-exists? f) (f-delete f))))

(provide 'org-glance-property-index)
