(require 'org-glance-module)

(cl-defun org-glance-class-registry.create ()
  "Hash table (id->class) that lists all registered classes."
  (make-hash-table))

(cl-defun org-glance-class-registry.add-class (class-id)
  "Create CLASS as instance of `org-glance-class'.
Register it in `org-glance-class-registry'."
  (org-glance:log-debug "Create class \"%s\"" class-id)

  (let ((class (org-glance-class :id class-id)))
    (puthash class-id class org-glance-class-registry)))

(cl-defun org-glance-class-registry.save (registry location)
  (cl-loop
     for directory in (org-glance-list-directories location)
     for class-id = (intern directory)
     unless (gethash class-id registry nil)
     do (let ((class (org-glance-class :id class-id)))
          (puthash class-id class registry))))

(org-glance:provide)
