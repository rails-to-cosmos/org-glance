(require 'org-glance-module)

(org-glance:require
  eieio)

(defclass org-glance-class ()
  ((id
    :initarg :id
    :type symbol)
   (capture-template
    :initarg :capture-template
    :initform "* %?"
    :type string))
  "Class symbol for `org-glance-headline'.")

(cl-defmethod initialize-instance :after ((m org-glance-class) &rest _)
  "Constructor for `org-glance-class'."
  (unless (slot-boundp m 'id)
    (error "Unable to initialize `org-glance-class': ID should be bound")))

(org-glance:provide)
