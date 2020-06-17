(require 'transient)
(require 'eieio-core)

;; Base infix class

(defclass org-glance--variable (transient-variable) ())

(cl-defmethod transient-init-value ((obj org-glance--variable))
  (let ((variable (format (oref obj variable))))
    (oset obj variable variable)
    (oset obj value (eval (read (format variable))))))

(cl-defmethod transient-infix-set ((obj org-glance--variable) value)
  (let ((variable (oref obj variable)))
    (oset obj value value)
    (set (make-local-variable (intern (oref obj variable))) value)
    (unless (or value transient--prefix)
      (message "Unset %s" variable))))

(cl-defmethod transient-format-description ((obj org-glance--variable))
  (or (oref obj description)
      (oref obj variable)))

(cl-defmethod transient-format-value ((obj org-glance--variable))
  (propertize (oref obj value) 'face 'transient-inactive-value))

;; Base class for view specifying

(defclass org-glance--variable:view (org-glance--variable)
  ((default     :initarg :default     :initform nil)))

(cl-defmethod transient-infix-read ((obj org-glance--variable:view))
  (oset obj value (org-glance-read-view "View: ")))

(cl-defmethod transient-format-value ((obj org-glance--variable:view))
  (let* ((val (or (oref obj value) (oref obj default)))
         (val-pretty (propertize val 'face 'transient-argument)))
    (format "(%s)" val-pretty)))

(transient-define-infix org-glance-act.current-view ()
  :class 'org-glance--variable:view
  :variable "--og-local--current-view"
  :reader 'org-glance-read-view
  :default "--all")

;;;###autoload (autoload 'org-glance-act "org-glance" nil t)
(transient-define-prefix org-glance-act (view)
  "In Glance-View buffer, perform action on selected view"
  ["Arguments"
   ("-r" "Rebuild database file" "--reread")
   ("-v" "Specify view" org-glance-act.current-view)]
  ["Actions"
   [("j" "Jump"        org-glance-action-open)]
   [("m" "Materialize" org-glance-action-materialize)]
   [("v" "Visit"       org-glance-action-visit)]])

(provide-me)
