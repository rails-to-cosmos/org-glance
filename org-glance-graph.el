;; -*- lexical-binding: t -*-

(require 'cl-macs)
(require 'f)

(require 'org-glance-headline1)

(defcustom org-glance-directory org-directory
  "Main location for all Org mode content managed by `org-glance`."
  :group 'org-glance
  :type 'directory)

(cl-defstruct (org-glance-graph (:predicate org-glance-graph?)
                                (:conc-name org-glance-graph:))
  (directory org-glance-directory :read-only t :type directory))

(cl-defun org-glance-graph (&optional (directory org-glance-directory))
  (make-org-glance-graph :directory directory))

(cl-defun org-glance-graph:generate-id (graph)
  (cl-check-type graph org-glance-graph)
  )

(cl-defun org-glance-graph:add-headline (graph headline)
  (cl-check-type graph org-glance-graph)
  (cl-check-type headline org-glance-headline1)

  (let ((id (org-glance-graph:generate-id graph)))
    ))

(cl-defun org-glance-graph:remove-headline (graph id)
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  )

(cl-defun org-glance-graph:get-headline (graph id)
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  )

(org-glance-graph)

(provide 'org-glance-graph)
