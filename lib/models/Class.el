(require 'org-glance-module)

(org-glance:require
  eieio)

(defclass org-glance-class ()
  ((id :initarg :id
       :type symbol)))

(org-glance:provide)
