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

;; (defclass org-glance-transient-variable:view (org-glance-transient-variable)
;;   ())

;; (cl-defmethod transient-infix-read ((obj org-glance-transient-variable:view))
;;   (oset obj value (org-glance-read-view)))

;; (cl-defmethod transient-format-value ((obj org-glance-transient-variable:view))
;;   (let* ((val (or (oref obj value) (oref obj default)))
;;          (val-pretty (propertize val 'face 'transient-argument)))
;;     (format "(%s)" val-pretty)))

;; (transient-define-infix org-glance-act.current-view ()
;;   :class 'org-glance-transient-variable:view
;;   :variable 'org-glance-transient--current-view
;;   :reader 'org-glance-read-view
;;   :default "false")

(transient-define-prefix org-glance-act ()
  "In Glance-View buffer, perform action on selected view"
  ;; ["Arguments"
  ;;  ("-r" "Reread view" org-glance-act.current-view)]
  ["View"
   [("E" "Export" org-glance-export-view)]
   [("R" "Reread" org-glance-reread-view)]]
  ["Headline"
   [("e" "Extract" org-glance-action-extract-property)]
   [("j" "Jump" org-glance-action-open)]
   [("m" "Materialize" org-glance-action-materialize)]
   [("v" "Visit" org-glance-action-visit)]])

(provide-me)
