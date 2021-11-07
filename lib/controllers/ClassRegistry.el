(require 'org-glance-module)

(org-glance:require
  lib.utils.helpers
  lib.models.Class)

(cl-defun org-glance-class-registry:create ()
  "Hash table (id->class) that lists all registered classes."
  (make-hash-table))

(cl-defun org-glance-register-class (class-id)
  "Create CLASS as instance of `org-glance-class'.
Register it in `org-glance-class-registry'."
  (org-glance:log-debug "Create class \"%s\"" class-id)

  (let ((class (org-glance-class :id class-id)))
    (puthash class-id class org-glance-class-registry))

  ;; (org-glance:log-debug "Metastore exists?")
  ;; (unless (f-exists? (org-glance-view:metastore-location (org-glance:get-class class)))
  ;;   (org-glance:log-debug "Create metastore")
  ;;   (org-glance-metastore:create (org-glance-view:metastore-location (org-glance:get-class class))))

  ;; (org-glance:log-debug "Overview exists?")
  ;; (unless (f-exists? (org-glance-overview:location class))
  ;;   (org-glance:log-debug "Create overview")
  ;;   (org-glance-overview:create class))

  )

(cl-defun org-glance-class-registry:update (registry location)
  (cl-loop
     for directory in (-org-glance:list-directories location)
     for class-id = (intern directory)
     unless (gethash class-id registry nil)
     do (let ((class (org-glance-class :id class-id)))
          (puthash class-id class registry))))

;; (defun org-glance:get-class (class)
;;   "Get CLASS from registry."
;;   (gethash class org-glance-class-registry))

(org-glance:provide)
