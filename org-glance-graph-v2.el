;; -*- lexical-binding: t -*-

(require 'cl-macs)
(require 'f)
(require 'org)

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

(cl-defstruct (org-glance-graph-v2 (:predicate org-glance-graph-v2?)
                                   (:conc-name org-glance-graph-v2:))
  (mutex (make-mutex) :read-only t :type mutex)
  (directory org-glance-directory :read-only t :type directory))

(cl-defun org-glance-graph-v2:merge (&rest graphs)
  )

(cl-defstruct (org-glance-headline-metadata-v2 (:predicate org-glance-headline-metadata-v2?)
                                               (:conc-name org-glance-headline-metadata-v2:))
  (state nil :read-only t :type string)
  (title nil :read-only t :type string)
  (tags nil :read-only t :type list)
  (hash nil :read-only t :type string)
  (schedule nil :read-only t :type string)
  (deadline nil :read-only t :type string)
  (priority nil :read-only t :type number))

(cl-defun org-glance-headline-v2:metadata (headline)
  (cl-check-type headline org-glance-headline-v2)
  (make-org-glance-headline-metadata-v2
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
  (list :state (org-glance-headline-metadata-v2:state metadata)
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
  (make-org-glance-headline-metadata-v2 :state (plist-get data :state)
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

(cl-defmacro org-glance-graph-v2:lock (graph &rest forms)
  "Modify GRAPH data with FORMS."
  (declare (indent 1))
  `(with-mutex (org-glance-graph-v2:mutex ,graph)
     ,@forms))

(cl-defun org-glance-graph-v2:insert (graph meta)
  (declare (indent 1))
  (cl-check-type meta list)
  (cl-check-type (car meta) (or list org-glance-headline-metadata-v2))
  (org-glance-graph-v2:lock graph
    (cl-loop for spec in meta
             collect (-> spec
                         (org-glance-headline-metadata-v2:serialize*)
                         (json-serialize))
             into result
             finally (f-append-text (concat (s-join "\n" result) "\n") `utf-8 (org-glance-graph-v2:headline-meta-path graph)))))

(cl-defun org-glance-graph-v2:data-path (graph)
  (cl-check-type graph org-glance-graph-v2)
  (-> (f-join (org-glance-graph-v2:directory graph) "data")
      (file-truename)))

(cl-defun org-glance-graph-v2:meta-path (graph)
  (cl-check-type graph org-glance-graph-v2)
  (-> graph
      (org-glance-graph-v2:directory)
      (f-join "meta")
      (file-truename)))

(cl-defun org-glance-graph-v2:headline-meta-path (graph)
  (cl-check-type graph org-glance-graph-v2)
  (-> graph
      (org-glance-graph-v2:meta-path)
      (f-join "headlines.jsonl")
      (file-truename)))

(cl-defun org-glance-graph-v2:headline-data-path (graph id)
  "TODO move to metadata?"
  (cl-check-type graph org-glance-graph-v2)
  (cl-check-type id string)
  (-> (f-join (org-glance-graph-v2:data-path graph) (substring id 0 2) (substring id 2))
      (file-truename)))

(cl-defun org-glance-graph-v2:make-id (graph)
  (cl-check-type graph org-glance-graph-v2)
  (org-glance-graph-v2:lock graph
    (cl-loop while t
             for id = (org-id-uuid)
             for data-path = (org-glance-graph-v2:headline-data-path graph id)
             unless (f-exists? data-path)
             return (prog1 id (f-mkdir-full-path data-path)))))

;; (cl-defun org-glance-headline-metadata-v2:data-path (metadata)
;;   (cl-check-type metadata org-glance-headline-metadata-v2)
;;   (f-join (org-glance-graph-v2:headline-data-path (org-glance-headline-metadata-v2:graph metadata) (org-glance-headline-metadata-v2:id metadata)) "data.org"))

(cl-defun org-glance-graph-v2:add (graph &rest headlines)
  "Add HEADLINES to GRAPH. TODO return a new graph."
  (cl-check-type graph org-glance-graph-v2)
  (cl-loop for headline in headlines
           collect (org-glance-headline-v2:metadata* headline) into specs
           finally (org-glance-graph-v2:insert graph specs)))

(cl-defun org-glance-graph-v2:get-headline (graph id)
  (cl-check-type graph org-glance-graph-v2)
  (cl-check-type id string)
  (org-glance-jsonl:iterate (org-glance-graph-v2:headline-meta-path graph)
    (when (string= (plist-get it :id) id)
      (if (plist-get it :tombstone)
          'tombstone
        (org-glance-headline-metadata-v2:deserialize graph it)))))

(cl-defun org-glance-graph-v2:delete (graph id)
  (cl-check-type graph org-glance-graph-v2)
  (cl-check-type id string)
  (let ((meta (org-glance-graph-v2:get-headline graph id)))
    (unless (org-glance-headline-metadata-v2:tombstone meta)
      (org-glance-graph-v2:insert graph (list :id id :tombstone t)))))

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
  (interactive)
  (cl-check-type buffer buffer)
  (cl-loop with elements = (org-element-map (org-element-parse-buffer 'headline) 'headline #'identity)
           for element in elements
           collect (org-glance-headline-v2--from-element element)))

(provide 'org-glance-graph-v2)
