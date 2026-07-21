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
;; Persisted at `<store>/cache/property-index.eld' (alist id -> (HASH :drawer AL
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

(defvar org-glance-property-index--dirty (make-hash-table :test #'equal)
  "Set of store-paths whose memo re-parsed a blob since the last flush.
`--flush-if-dirty' writes only these, so a no-op refresh persists nothing.")

(cl-defun org-glance-property-index--candidate-key? (key)
  "Non-nil if drawer KEY is a genuine column candidate.
Hides org-glance bookkeeping (`ORG_GLANCE_*') and org's synthesised CATEGORY
\(which `node-properties' always injects), leaving user-authored drawer keys."
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
  (when-let ((h (gethash (org-glance-graph:store-path graph)
                         org-glance-property-index--cache)))
    (org-glance--write-eld
     (org-glance-property-index--file graph)
     (cl-loop for id being the hash-keys of h using (hash-values e)
              collect (cons id e)))))

(cl-defun org-glance-property-index--flush-if-dirty (graph)
  "Persist GRAPH's index only if a blob was re-parsed since the last flush.
Table renders warm the memo via the columns' value-fns; this then writes at most
once per render, and nothing at all when every id was already hash-valid."
  (let ((key (org-glance-graph:store-path graph)))
    (when (gethash key org-glance-property-index--dirty)
      (org-glance-property-index--flush graph)
      (remhash key org-glance-property-index--dirty))))

(cl-defun org-glance-property-index--entry (graph id)
  "ID's index entry (HASH :drawer AL :body AL), refreshed on a content-hash miss.
Mutates the in-session table; a cold/stale/absent id costs ONE blob parse (both
alists at once).  A gone id is not cached.  Does not flush -- a batch op
\(`:ensure' / `:keys') or a table render (`--flush-if-dirty') persists."
  (let* ((h (org-glance-property-index--table graph))
         (meta (org-glance-graph:get-headline graph id))
         (hash (and (org-glance-headline-metadata? meta)
                    (org-glance-headline-metadata:hash meta)))
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
  "Refresh the index for IDS (parse the stale/absent ones) and flush if changed."
  (dolist (id ids) (org-glance-property-index--entry graph id))
  (org-glance-property-index--flush-if-dirty graph))

(cl-defun org-glance-property-index:keys (graph ids)
  "Sorted union of candidate drawer KEYS across IDS.
The candidate set for the table's `C-u +' column prompt: every user-authored
drawer key the headlines matching the current filter carry (bookkeeping and
CATEGORY excluded).  Read-only w.r.t. disk -- a following render persists."
  (org-glance--sorted-distinct
   (cl-loop for id in ids
            append (cl-loop for kv in (org-glance-property-index:drawer graph id)
                            when (org-glance-property-index--candidate-key? (car kv))
                            collect (car kv)))))

(cl-defun org-glance-property-index--prune-legacy (graph)
  "Delete the pre-cache/-split location of this module's sidecar, if present.
`org-glance-graph-after-open-functions' hook; the module owns its filename."
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
;;; org-glance-property-index.el ends here
