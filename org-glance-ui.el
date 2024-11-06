(require 'transient)

(defclass org-glance-transient-variable (transient-variable)
  ((default :initarg :default :initform nil)))

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

(transient-define-prefix org-glance-form-action ()
  "Perform action on selected view/headlines"
  ["Overview"
   [("a" "Agenda" org-glance-overview:agenda*)
    ("o" "Overview" org-glance-overview)]]
  ["Actions"
   [;; ("+" "Capture headline" org-glance-capture)
    ;; ("k" "Kill headline" org-glance-headline-remove)

    ("e" "Extract property" org-glance:extract)
    ("j" "Open link" org-glance:open)
    ("m" "Materialize headline" org-glance:materialize)]]
  (interactive)
  (org-glance-init)
  (transient-setup 'org-glance-form-action))

(provide 'org-glance-ui)
