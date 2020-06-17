(require 'transient)
(require 'eieio-core)

(defclass org-glance--variable (transient-variable) ())

(cl-defmethod transient-init-value ((obj org-glance--variable))
  (let ((variable (format (oref obj variable))))
    (oset obj variable variable)))

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

(defclass org-glance--variable:view (org-glance--variable)
  nil)

(cl-defmethod transient-infix-read ((obj org-glance--variable:view))
  (oset obj value (org-glance-read-view "View: ")))

(cl-defmethod transient-format-value ((obj org-glance--variable:view))
  (concat "(" (propertize (or (oref obj value) "--all") 'face 'transient-argument) ")"))

(transient-define-infix org-glance-act.current-view ()
  :class 'org-glance--variable:view
  :variable "--og-transient--current-view"
  :reader 'org-glance-read-view)

;;;###autoload (autoload 'org-glance-act "org-glance" nil t)
(transient-define-prefix org-glance-act (view)
  "In Glance-View buffer, perform action on selected view"
  ["Arguments"
   ("-r" "Rebuild database file" "--reread")
   ("-v" "Current view" org-glance-act.current-view)]
  ["Actions"
   [("j" "Jump"        org-glance-action-open)]
   [("m" "Materialize" org-glance-action-materialize)]
   [("v" "Visit"       org-glance-action-visit)]
   [("d" "Decrypt"     org-glance-action-decrypt)]])

(provide-me)
