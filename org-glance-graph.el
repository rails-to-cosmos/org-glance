;; -*- lexical-binding: t -*-

(require 'cl-macs)
(require 'f)
(require 'org)

(require 'org-glance-headline1)

(defcustom org-glance-directory org-directory
  "Main location for all Org mode content managed by `org-glance`."
  :group 'org-glance
  :type 'directory)

(define-hash-table-test 'org-glance-graph:test
                        (lambda (a b) (f-equal? (file-truename a) (file-truename b)))
                        (lambda (a) (secure-hash 'sha1 a)))

(defvar org-glance-graph:list (make-hash-table :test 'org-glance-graph:test)
  "Registered instances of `org-glance-graph' in current session.")

(cl-defstruct (org-glance-graph (:predicate org-glance-graph?)
                                (:conc-name org-glance-graph:))
  (mutex (make-mutex) :read-only t :type mutex)
  (directory org-glance-directory :read-only t :type directory))

(cl-defstruct (org-glance-headline1-metadata (:predicate org-glance-headline1-metadata?)
                                             (:conc-name org-glance-headline1-metadata:))
  (graph nil :read-only t :type org-glance-graph)
  (id nil :read-only t :type string)
  (state nil :read-only t :type string)
  (title nil :read-only t :type string)
  (tags nil :read-only t :type list)
  (hash nil :read-only t :type string)
  (schedule nil :read-only t :type string)
  (deadline nil :read-only t :type string)
  (priority nil :read-only t :type number)
  (relations nil :read-only t :type list)
  (tombstone nil :read-only t :type boolean))

(cl-defun org-glance-headline1-metadata (graph headline)
  (cl-check-type graph org-glance-graph)
  (cl-check-type headline org-glance-headline1)
  (make-org-glance-headline1-metadata
   :graph graph
   :id (or (org-glance-headline1:id headline)
           (org-glance-graph:make-id graph))
   :state (org-glance-headline1:state headline)
   :title (org-glance-headline1:title headline)
   :tags (org-glance-headline1:tags headline)
   :hash (org-glance-headline1:hash headline)
   :schedule (org-glance-headline1:schedule headline)
   :deadline (org-glance-headline1:deadline headline)
   :priority (org-glance-headline1:priority headline)
   :relations nil
   :tombstone nil))

(cl-defmacro org-glance-jsonl:iterate (file &rest forms)
  (declare (indent 1))
  `(with-temp-buffer
     (cl-flet ((process-line () (let ((it (json-parse-string (buffer-substring-no-properties (line-beginning-position) (line-end-position))
                                                             :object-type 'plist)))
                                  ,@forms)))
       (cl-loop with result = nil
                with chunk-size = 4096
                with file-size = (or (file-attribute-size (file-attributes ,file)) 0)
                with disk-read-count = 0
                for upper-bound downfrom file-size downto 0 by chunk-size
                for lower-bound = (max 0 (- upper-bound chunk-size))
                do (progn (cl-incf disk-read-count)
                          (goto-char (point-min))
                          (insert-file-contents-literally ,file nil lower-bound upper-bound)
                          (goto-char (point-max))
                          (skip-chars-backward "\n")
                          (beginning-of-line)
                          (while (not (or (bobp) (setq result (process-line))))
                            (forward-line -1)
                            (beginning-of-line))
                          (delete-region (line-end-position) (point-max)))
                finally return (or result (condition-case nil
                                              (process-line)
                                            (json-end-of-file nil)))))))

(cl-defun org-glance-headline1-metadata:serialize (metadata)
  (cl-check-type metadata org-glance-headline1-metadata)
  (list :id (org-glance-headline1-metadata:id metadata)
        :state (org-glance-headline1-metadata:state metadata)
        :title (org-glance-headline1-metadata:title metadata)
        :tags (org-glance-headline1-metadata:tags metadata)
        :hash (org-glance-headline1-metadata:hash metadata)
        :schedule (org-glance-headline1-metadata:schedule metadata)
        :deadline (org-glance-headline1-metadata:deadline metadata)
        :priority (org-glance-headline1-metadata:priority metadata)
        :relations (org-glance-headline1-metadata:relations metadata)
        :tombstone (org-glance-headline1-metadata:tombstone metadata)))

(cl-defun org-glance-headline1-metadata:deserialize (graph data)
  (cl-check-type graph org-glance-graph)
  (cl-check-type data list)
  (make-org-glance-headline1-metadata :graph graph
                                      :id (plist-get data :id)
                                      :state (plist-get data :state)
                                      :title (plist-get data :title)
                                      :tags (plist-get data :tags)
                                      :hash (plist-get data :hash)
                                      :schedule (plist-get data :schedule)
                                      :deadline (plist-get data :deadline)
                                      :priority (plist-get data :priority)
                                      :relations (plist-get data :relations)
                                      :tombstone (plist-get data :tombstone)))

(cl-defun org-glance-graph (&optional (directory org-glance-directory))
  (cl-check-type directory string)
  (let ((graph (cl-gethash directory org-glance-graph:list)))
    (unless graph
      (setq graph (make-org-glance-graph :directory directory))
      (f-mkdir-full-path (org-glance-graph:data-path graph))
      (f-mkdir-full-path (org-glance-graph:meta-path graph))
      (f-touch (f-join (org-glance-graph:meta-path graph) "headlines.jsonl"))
      (cl-puthash directory graph org-glance-graph:list))
    graph))

(cl-defmacro org-glance-graph:lock (graph &rest forms)
  "Modify GRAPH data with FORMS."
  (declare (indent 1))
  `(with-mutex (org-glance-graph:mutex ,graph)
     ,@forms))

(cl-defun org-glance-graph:modify (graph &rest specs)
  (cl-check-type specs list)
  (cl-check-type (car specs) (or list org-glance-headline1-metadata))
  (org-glance-graph:lock graph
    (cl-loop for spec in specs
             collect (json-serialize (cl-typecase spec
                                       (org-glance-headline1-metadata (org-glance-headline1-metadata:serialize spec))
                                       (t spec)))
             into result
             finally (f-append-text (concat (s-join "\n" result) "\n") `utf-8 (f-join (org-glance-graph:meta-path graph) "headlines.jsonl")))))

(cl-defun org-glance-graph:headline-data-path (graph id)
  "TODO move to metadata."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (f-join (org-glance-graph:data-path graph) (substring id 0 2) (substring id 2)))

(cl-defun org-glance-graph:data-path (graph)
  (cl-check-type graph org-glance-graph)
  (f-join (org-glance-graph:directory graph) "data"))

(cl-defun org-glance-graph:meta-path (graph)
  (cl-check-type graph org-glance-graph)
  (f-join (org-glance-graph:directory graph) "meta"))

(cl-defun org-glance-graph:make-id (graph)
  (cl-check-type graph org-glance-graph)
  (org-glance-graph:lock graph
    (cl-loop while t
             for id = (org-id-uuid)
             for data-path = (org-glance-graph:headline-data-path graph id)
             unless (f-exists? data-path)
             return (prog1 id (f-mkdir-full-path data-path)))))

(cl-defun org-glance-headline1-metadata:data-path (metadata)
  (cl-check-type metadata org-glance-headline1-metadata)
  (f-join (org-glance-graph:headline-data-path (org-glance-headline1-metadata:graph metadata) (org-glance-headline1-metadata:id metadata)) "data.org"))

(cl-defun org-glance-graph:add-headline (graph metadata)
  "Set headline METADATA in GRAPH. TODO return a new graph."
  (cl-check-type graph org-glance-graph)
  (cl-check-type metadata org-glance-headline1-metadata)
  (cl-assert (equal graph (org-glance-headline1-metadata:graph metadata)))
  (org-glance-graph:modify graph metadata))

(cl-defun org-glance-graph:get-headline (graph id)
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (org-glance-jsonl:iterate (f-join (org-glance-graph:meta-path graph) "headlines.jsonl")
    (when (string= (plist-get it :id) id)
      (org-glance-headline1-metadata:deserialize graph it))))

(cl-defun org-glance-graph:remove-headline (graph id)
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (let ((meta (org-glance-graph:get-headline graph id)))
    (unless (org-glance-headline1-metadata:tombstone meta)
      (org-glance-graph:modify graph (list :id id :tombstone t)))))

(cl-defun org-glance-graph:add-relation (graph relation a-id b-id)
  (cl-check-type graph org-glance-graph)
  (cl-check-type relation symbol)
  (cl-check-type a-id string)
  (cl-check-type b-id string)
  (let* ((a (org-glance-graph:get-headline graph a-id))
         (a-relations (org-glance-headline1-metadata:relations a))
         (b (org-glance-graph:get-headline graph b-id))
         (b-relations (org-glance-headline1-metadata:relations b)))
    (org-glance-graph:set-headline graph :id a-id :relations (cl-pushnew b-id a-relations))
    (org-glance-graph:set-headline graph :id b-id :relations (cl-pushnew a-id b-relations))))

;; (let* ((graph (org-glance-graph "/tmp/glance"))
;;        (foo (org-glance-headline1--from-lines "* foo" "- [[http://10.17.2.107:3002/overview/activity/timeline][Web UI]]"))
;;        (bar (org-glance-headline1--from-lines "* bar" "123"))
;;        (foo-meta (org-glance-headline1-metadata graph foo))
;;        (bar-meta (org-glance-headline1-metadata graph bar))
;;        ;; (foo-id (org-glance-graph:set-headline graph foo))
;;        ;; (bar-id (org-glance-graph:set-headline graph bar :id foo-id))
;;        )
;;   ;; (org-glance-graph:add-relation graph "parent" foo-id bar-id)

;;   (org-glance-graph:add-headline graph foo-meta)
;;   (org-glance-graph:add-headline graph bar-meta)

;;   ;; (org-glance-graph:add-relation graph 'parent
;;   ;;                                (org-glance-headline1-metadata:id foo-meta)
;;   ;;                                (org-glance-headline1-metadata:id bar-meta))
;;   )

;; (let* ((graph (org-glance-graph "/tmp/glance"))
;;        (meta (org-glance-graph:get-headline graph "9fad118e-ff95-41e5-8560-b3d5b162ed43"))
;;        ;; (id (org-glance-headline1-metadata:id meta))
;;        )
;;   ;; (org-glance-graph:remove-headline graph id)
;;   (org-glance-headline1-metadata:state meta)
;;   )

(provide 'org-glance-graph)
