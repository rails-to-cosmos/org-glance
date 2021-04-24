(require 'load-relative)

(defclass org-glance-transient-variable (transient-variable)
  ((default
       :initarg :default
       :initform nil)))

(cl-defmethod transient-init-value ((obj org-glance-transient-variable))
  "Override transient value initialization."
  (let ((variable (oref obj variable))
        (default (oref obj default)))
    (oset obj variable variable)
    (oset obj value (or (eval variable) default))))

(cl-defmethod transient-infix-set ((obj org-glance-transient-variable) value)
  "Override setter."
  (oset obj value value)
  (set (oref obj variable) value))

(cl-defmethod transient-format-description ((obj org-glance-transient-variable))
  "Override description format."
  (or (oref obj description)
      (oref obj variable)))

(cl-defmethod transient-format-value ((obj org-glance-transient-variable))
  "Override value format."
  (propertize (oref obj value) 'face 'transient-inactive-value))


(defvar org-glance-transient--view "all")

(defclass org-glance-transient-variable:view (org-glance-transient-variable) ())

(cl-defmethod transient-infix-read ((obj org-glance-transient-variable:view))
  (oset obj value (symbol-name (org-glance-read-view-id))))

(cl-defmethod transient-format-value ((obj org-glance-transient-variable:view))
  (let* ((val (or (oref obj value) (oref obj default)))
         (val-pretty (propertize val 'face 'transient-argument)))
    (format "(%s)" val-pretty)))

(transient-define-infix org-glance-act.view ()
  :class 'org-glance-transient-variable:view
  :variable 'org-glance-transient--view
  :reader 'org-glance-read-view-id
  :default "all")

(provide-me)
