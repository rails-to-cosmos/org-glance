;; -*- lexical-binding: t; -*-

(require 'cl-generic)
(require 'org-glance-helpers)
(require 'org-glance-types)
(require 'org-glance-log)

(org-glance-class org-glance-partition ()
    ((dimension :type string :initarg :dimension)
     (value     :type string :initarg :value)
     (location  :type OptionalDirectory :initarg :location)))

(cl-defgeneric org-glance-partition:create (&rest args)
  "Create `org-glance-partition' object.")

(cl-defmethod org-glance-partition:create ((dimension string) (value string))
  "Create `org-glance-partition' object by DIMENSION and VALUE."
  (org-glance-partition :dimension (downcase dimension) :value (downcase value)))

(org-glance-declare org-glance-partition:representation :: Partition -> string)
(defun org-glance-partition:representation (partition)
  (downcase (format "%s=%s" (org-glance? partition :dimension) (org-glance? partition :value))))

(org-glance-declare org-glance-partition:path :: Partition -> OptionalDirectory)
(defun org-glance-partition:path (partition)
  (f-join (org-glance? partition :dimension) (org-glance? partition :value)))

(provide 'org-glance-partition)
