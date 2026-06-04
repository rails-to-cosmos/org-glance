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

(define-hash-table-test 'org-glance-graph-v2:test
                        (lambda (a b) (f-equal? (file-truename a) (file-truename b)))
                        (lambda (a) (secure-hash 'sha1 a)))

(defvar org-glance-graph-v2:list (make-hash-table :test 'org-glance-graph-v2:test)
  "Registered instances of `org-glance-graph-v2' in current session.")

;; Single-user assumption: no mutex / locking (see MIGRATION-PLAN.md, decision 3).
(cl-defstruct (org-glance-graph-v2 (:predicate org-glance-graph-v2?)
                                   (:conc-name org-glance-graph-v2:))
  (directory org-glance-directory :read-only t :type directory))

(cl-defstruct (org-glance-headline-metadata-v2 (:predicate org-glance-headline-metadata-v2?)
                                               (:conc-name org-glance-headline-metadata-v2:))
  (id nil :read-only t :type string)
  (state nil :read-only t :type string)
  (title nil :read-only t :type string)
  (tags nil :read-only t :type list)
  (hash nil :read-only t :type string)
  (schedule nil :read-only t :type string)
  (deadline nil :read-only t :type string)
  (priority nil :read-only t :type number))

(cl-defun org-glance-headline-v2:metadata (headline)
  (cl-check-type headline org-glance-headline-v2)
  ;; SCHEDULE/DEADLINE come back as raw strings (or nil) from the headline
  ;; methods, so they are JSON-serializable as-is.
  (make-org-glance-headline-metadata-v2
   :id (org-glance-headline-v2:id headline)
   :state (org-glance-headline-v2:state headline)
   :title (org-glance-headline-v2:title headline)
   :tags (org-glance-headline-v2:tags headline)
   :hash (org-glance-headline-v2:hash headline)
   :schedule (org-glance-headline-v2:schedule headline)
   :deadline (org-glance-headline-v2:deadline headline)
   :priority (org-glance-headline-v2:priority headline)))

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
  (list :id (org-glance-headline-metadata-v2:id metadata)
        :state (org-glance-headline-metadata-v2:state metadata)
        :title (org-glance-headline-metadata-v2:title metadata)
        :tags (->> (org-glance-headline-metadata-v2:tags metadata)
                   (mapcar (-partial #'format "%s"))
                   (apply #'vector))
        :hash (org-glance-headline-metadata-v2:hash metadata)
        :schedule (org-glance-headline-metadata-v2:schedule metadata)
        :deadline (org-glance-headline-metadata-v2:deadline metadata)
        :priority (org-glance-headline-metadata-v2:priority metadata)))

(cl-defun org-glance-headline-metadata-v2:deserialize (data)
  (cl-check-type data list)
  (make-org-glance-headline-metadata-v2 :id (plist-get data :id)
                                        :state (plist-get data :state)
                                        :title (plist-get data :title)
                                        :tags (plist-get data :tags)
                                        :hash (plist-get data :hash)
                                        :schedule (plist-get data :schedule)
                                        :deadline (plist-get data :deadline)
                                        :priority (plist-get data :priority)))

(cl-defun org-glance-graph-v2 (&optional (directory org-glance-directory))
  (cl-check-type directory string)
  (let* ((directory (-> directory (file-truename) (f-full)))
         (graph (gethash directory org-glance-graph-v2:list)))
    (unless graph
      (setq graph (make-org-glance-graph-v2 :directory directory))
      (f-mkdir-full-path (org-glance-graph-v2:data-path graph))
      (f-mkdir-full-path (org-glance-graph-v2:meta-path graph))
      (f-touch (org-glance-graph-v2:headline-meta-path graph))
      (puthash directory graph org-glance-graph-v2:list))
    graph))

(cl-defun org-glance-graph-v2:insert (graph meta)
  (declare (indent 1))
  (cl-check-type meta list)
  (cl-check-type (car meta) (or list org-glance-headline-metadata-v2))
  (cl-loop for spec in meta
           collect (-> spec
                       (org-glance-headline-metadata-v2:serialize*)
                       (json-serialize))
           into result
           finally (f-append-text (concat (s-join "\n" result) "\n") 'utf-8 (org-glance-graph-v2:headline-meta-path graph))))

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
    (dolist (headline headlines)
      (when (org-glance-headline-v2? headline)
        (org-glance-graph-v2:put-content graph headline)))
    (org-glance-graph-v2:insert graph (mapcar #'org-glance-headline-v2:metadata* headlines)))
  graph)

(cl-defun org-glance-graph-v2:get-headline (graph id)
  "Return the most recent metadata for ID in GRAPH.
Return the symbol `tombstone' if ID was deleted, or nil if unknown."
  (cl-check-type graph org-glance-graph-v2)
  (cl-check-type id string)
  (org-glance-jsonl:iterate (org-glance-graph-v2:headline-meta-path graph)
    (when (string= (plist-get it :id) id)
      (if (plist-get it :tombstone)
          'tombstone
        (org-glance-headline-metadata-v2:deserialize it)))))

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
The latest record per id wins; original insertion order is preserved."
  (cl-check-type graph org-glance-graph-v2)
  (let ((latest (make-hash-table :test 'equal))
        (order nil)
        (path (org-glance-graph-v2:headline-meta-path graph)))
    (when (f-exists? path)
      (with-temp-buffer
        ;; Force utf-8 to match the writer, independent of locale autodetection.
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents path))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (unless (string-empty-p line)
              (let* ((it (json-parse-string line :object-type 'plist))
                     (id (plist-get it :id)))
                (unless (gethash id latest) (push id order))
                (puthash id it latest))))
          (forward-line 1))))
    ;; NB: do not name this loop variable `it' -- `cl-loop' binds `it'
    ;; anaphorically to the `unless' condition value, shadowing it.
    (cl-loop for id in (nreverse order)
             for record = (gethash id latest)
             unless (plist-get record :tombstone)
             collect (org-glance-headline-metadata-v2:deserialize record))))

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
