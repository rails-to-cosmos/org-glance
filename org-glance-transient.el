(eval-and-compile
  (require 'transient)
  (require 'eieio-core))

(require 'org-glance-views)

;; Global glance transient state

(defvar org-glance-transient--current-view nil)

;; Base infix class

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

(defun org-glance-read-scope ()
  (completing-read
   "Scope: "
   '(agenda
     agenda-with-archives
     file)))

(defvar org-glance-transient--scope "agenda")

(defclass org-glance-transient-variable:scope (org-glance-transient-variable)
  ())

(cl-defmethod transient-infix-read ((obj org-glance-transient-variable:scope))
  (oset obj value (org-glance-read-scope)))

(cl-defmethod transient-format-value ((obj org-glance-transient-variable:scope))
  (let* ((val (or (oref obj value) (oref obj default)))
         (val-pretty (propertize val 'face 'transient-argument)))
    (format "(%s)" val-pretty)))

(transient-define-infix org-glance-act.scope ()
  :class 'org-glance-transient-variable:scope
  :variable 'org-glance-transient--scope
  :reader 'org-glance-read-scope
  :default "false")

(transient-define-prefix org-glance-act ()
  "In Glance-View buffer, perform action on selected view"
  ;; ["Arguments"
  ;;  ("-s" "Scope" org-glance-act.scope)]
  ["Views"
   [("E" "Export" org-glance-export-view)]
   [("R" "Report" org-glance-show-report)]]
  ["Headlines"
   [("c" "Capture" org-glance-action-extract-property)]
   [("e" "Extract" org-glance-action-extract-property)]
   [("j" "Jump" org-glance-action-open)]
   [("m" "Materialize" org-glance-action-materialize)]
   [("v" "Visit" org-glance-action-visit)]])

(provide-me)
