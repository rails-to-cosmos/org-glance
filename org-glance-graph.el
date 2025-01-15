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

(cl-defmacro org-glance-graph:mutate (graph &rest forms)
  (declare (indent 1))
  `(progn
     (cl-check-type ,graph org-glance-graph)
     (mutex-lock (org-glance-graph:mutex ,graph))
     (unwind-protect ,@forms
       (mutex-unlock (org-glance-graph:mutex ,graph)))))

(cl-defun org-glance-graph (&optional (directory org-glance-directory))
  (make-org-glance-graph :directory directory))

(cl-defun org-glance-graph:id-path (graph id)
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (f-join (org-glance-graph:directory graph) "data" (substring id 0 2) (substring id 2)))

(cl-defun org-glance-graph:make-id (graph)
  (cl-check-type graph org-glance-graph)
  (org-glance-graph:mutate graph
    (cl-loop while t
             for id = (org-id-uuid)
             for id-path = (org-glance-graph:id-path graph id)
             unless (f-exists? id-path)
             return (progn (f-mkdir-full-path id-path) id))))

(cl-defun org-glance-graph:add-headline (graph headline)
  (cl-check-type graph org-glance-graph)
  (cl-check-type headline org-glance-headline1)

  (let ((id (org-glance-graph:make-id graph)))
    (with-temp-file (f-join (org-glance-graph:id-path graph id) "headline.org")
      (insert (org-glance-headline1:contents headline)))
    id))

(cl-defun org-glance-graph:remove-headline (graph id)
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  )

(cl-defun org-glance-graph:get-headline (graph id)
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  )

;; (let ((graph (org-glance-graph "/tmp"))
;;       (headline (org-glance-headline1--from-lines "* dagster" "- [[http://10.17.2.107:3002/overview/activity/timeline][Web UI]]")))
;;   (org-glance-graph:add-headline graph headline))

(provide 'org-glance-graph)
