(require 'transient)
(require 'load-relative)

(defvar org-glance-transient--view "all")

(defclass org-glance-transient-variable (transient-variable)
  ((default     :initarg :default     :initform nil)))

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

(defclass org-glance-transient-variable:view (org-glance-transient-variable)
  ())

(cl-defmethod transient-infix-read ((obj org-glance-transient-variable:view))
  (oset obj value (symbol-name (org-glance-read-view))))

(cl-defmethod transient-format-value ((obj org-glance-transient-variable:view))
  (let* ((val (or (oref obj value) (oref obj default)))
         (val-pretty (propertize val 'face 'transient-argument)))
    (format "(%s)" val-pretty)))

(transient-define-infix org-glance-act.view ()
  :class 'org-glance-transient-variable:view
  :variable 'org-glance-transient--view
  :reader 'org-glance-read-view
  :default "all")

(transient-define-prefix org-glance-act ()
  "In Glance-View buffer, perform action on selected view"
  ["Analytics"

   [("A" "Agenda" org-glance-view-agenda)
    ("D" "Dashboard" org-glance-show-report)]

   [("-v" "View" org-glance-act.view)]]

  ["Actions"
   [("U" "Update" org-glance-view-update)
    ("V" "Visit" org-glance-view-visit)]]

  ["Headlines"
   ;; [("c" "Capture" org-glance-action-extract-property)]
   [("e" "Extract" org-glance-action-extract-property)]
   [("j" "Jump" org-glance-action-open)]
   [("i" "Insert" org-glance-action-insert)]
   [("m" "Materialize" org-glance-action-materialize)]
   [("v" "Visit" org-glance-action-visit)]])

(provide-me)
