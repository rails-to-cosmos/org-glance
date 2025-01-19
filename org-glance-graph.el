;; -*- lexical-binding: t -*-

(require 'cl-macs)
(require 'f)
(require 'org)

(require 'org-glance-headline1)

(defcustom org-glance-directory org-directory
  "Main location for all Org mode content managed by `org-glance`."
  :group 'org-glance
  :type 'directory)

(cl-defstruct (org-glance-graph (:predicate org-glance-graph?)
                                (:conc-name org-glance-graph:))
  (mutex (make-mutex) :read-only t :type mutex)
  (directory org-glance-directory :read-only t :type directory))

(cl-defstruct (org-glance-headline1-metadata (:predicate org-glance-headline1-metadata?)
                                             (:conc-name org-glance-headline1-metadata:))
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

(cl-defun org-glance-headline1-metadata:serialize (metadata graph &key (id (org-glance-headline1-metadata:id metadata)))
  (cl-check-type metadata org-glance-headline1-metadata)
  (cl-check-type id string)
  (cl-check-type graph org-glance-graph)
  (->> (list :id id
             :title (org-glance-headline1-metadata:title metadata)
             :hash (org-glance-headline1-metadata:hash metadata)
             :relations (org-glance-headline1-metadata:relations metadata))
       (json-serialize)
       (format "%s\n")))

(cl-defun org-glance-graph (&optional (directory org-glance-directory))
  (cl-check-type directory string)
  (let ((graph (make-org-glance-graph :directory directory)))
    (f-mkdir-full-path (org-glance-graph:data-path graph))
    (f-mkdir-full-path (org-glance-graph:meta-path graph))
    graph))

(cl-defmacro org-glance-graph:modify (graph &rest forms)
  "Modify GRAPH data with FORMS."
  (declare (indent 1))
  `(with-mutex (org-glance-graph:mutex ,graph)
     ,@forms))

(cl-defun org-glance-graph:headline-data-directory (graph id)
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (f-join (org-glance-graph:data-path graph) (substring id 0 2) (substring id 2)))

(cl-defun org-glance-graph:data-directory (graph)
  (cl-check-type graph org-glance-graph)
  (f-join (org-glance-graph:directory graph) "data"))

(cl-defun org-glance-graph:meta-directory (graph)
  (cl-check-type graph org-glance-graph)
  (f-join (org-glance-graph:directory graph) "meta"))

(cl-defun org-glance-graph:make-id (graph)
  (cl-check-type graph org-glance-graph)
  (org-glance-graph:modify graph
    (cl-loop while t
             for id = (org-id-uuid)
             for data-directory = (org-glance-graph:headline-data-directory graph id)
             unless (f-exists? data-directory)
             return (prog1 id (f-mkdir-full-path data-directory)))))

(cl-defun org-glance-graph:add-headline-metadata (graph metadata)
  "Set headline METADATA in GRAPH."
  (cl-check-type graph org-glance-graph)
  (cl-check-type metadata org-glance-headline1-metadata)
  (let ((id (or (org-glance-headline1-metadata:id metadata)
                (org-glance-graph:make-id graph)))
        (data (org-glance-headline1-metadata:serialize metadata graph :id id)))
    (org-glance-graph:modify graph
      (f-append-text data `utf-8 (f-join (org-glance-graph:meta-directory graph) "headlines.jsonl"))
      (with-temp-file (f-join (org-glance-graph:headline-data-directory graph id) "data.org")
        (insert (org-glance-headline1:contents headline))))
    id))

(cl-defun org-glance-graph:remove-headline-metadata (graph id)
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  )

(cl-defun org-glance-graph:get-headline-metadata (graph id)
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  )

(cl-defun org-glance-graph:add-relation (graph relation a-id b-id)
  (cl-check-type graph org-glance-graph)
  (cl-check-type relation string)
  (cl-check-type a-id string)
  (cl-check-type b-id string)
  (let* ((a (org-glance-graph:get-headline-metadata graph a-id))
         (a-relations (org-glance-headline1-metadata:relations a))
         (b (org-glance-graph:get-headline-metadata graph b-id))
         (b-relations (org-glance-headline1-metadata:relations b)))
    (org-glance-graph:set-headline-metadata graph :id a-id :relations (cl-pushnew b-id a-relations))
    (org-glance-graph:set-headline-metadata graph :id b-id :relations (cl-pushnew a-id b-relations))))

(cl-defmacro org-glance-jsonl:--iter-lines (file &rest forms)
  (declare (indent 1))
  `(with-temp-buffer
     (cl-flet ((process-line () (let ((it (json-parse-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)) :object-type 'plist)))
                                  ,@forms)))
       (cl-loop with chunk-size = 4096
                with file-size = (file-attribute-size (file-attributes ,file))
                with disk-read-count = 0
                for upper-bound downfrom file-size downto 0 by chunk-size
                for lower-bound = (max 0 (- upper-bound chunk-size))
                do (progn (incf disk-read-count)
                          (goto-char (point-min))
                          (insert-file-contents-literally ,file nil lower-bound upper-bound)
                          (goto-char (point-max))
                          (skip-chars-backward "\n")
                          (beginning-of-line)
                          (while (not (bobp))
                            (process-line)
                            (delete-line)
                            (forward-line -1)
                            (beginning-of-line)))
                finally (process-line)
                (message "disk read count: %d, chunk size: %d" disk-read-count chunk-size)))))

;; (let* ((graph (org-glance-graph "/tmp/glance"))
;;        (foo (org-glance-headline1--from-lines "* foo" "- [[http://10.17.2.107:3002/overview/activity/timeline][Web UI]]"))
;;        (bar (org-glance-headline1--from-lines "* bar" "123"))
;;        (foo-id (org-glance-graph:set-headline graph foo))
;;        (bar-id (org-glance-graph:set-headline graph bar
;;                                               :id foo-id)))
;;   (org-glance-graph:add-relation graph "parent" foo-id bar-id))

(provide 'org-glance-graph)
