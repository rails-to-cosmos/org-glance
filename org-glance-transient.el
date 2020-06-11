(require 'transient)
(require 'eieio-core)

(defclass org-glance--variable (transient-variable)
  ((scope       :initarg :scope)))

(defclass org-glance--variable:view (org-glance--variable)
  ((choices     :initarg :choices)
   (fallback    :initarg :fallback    :initform nil)
   (default     :initarg :default     :initform nil)))

(cl-defmethod transient-infix-read ((obj org-glance--variable:view))
  (let ((choices (oref obj choices)))
    (when (functionp choices)
      (setq choices (funcall choices)))
    (if-let ((value (oref obj value)))
        (cadr (member value choices))
      (car choices))))

(cl-defmethod transient-format-value ((obj org-glance--variable:view))
  (let* ((variable (oref obj variable))
         (choices (oref obj choices)))
      "Hello"))

;;;###autoload (autoload 'org-glance-act "org-glance" nil t)
(transient-define-prefix org-glance-act (view)
  "In Glance-View buffer, perform action on selected view"
  [ ;; "Arguments"
   ("-r" "Reread database from scope" "--reread")
   ;; (org-glance-arg:--view)
   ("V" org-glance-act.current-view)
   ]
  ["Actions"
   [("j" "Jump"        org-glance-action-open)]
   [("m" "Materialize" org-glance-action-materialize)]
   [("v" "Visit"       org-glance-action-visit)]
   [("d" "Decrypt"     org-glance-action-decrypt)]]
  ;; (interactive (list (org-glance-read-view "Read view: ")))
  ;; (transient-setup 'org-glance-act nil nil :scope view)
  )

;; (transient-define-argument org-glance-arg:--view ()
;;   :description "View"
;;   :class 'transient-option
;;   :shortarg "-v"
;;   :argument "--view="
;;   :choices org-glance-views)

(transient-define-infix org-glance-act.current-view ()
  :class 'org-glance--variable:view
  :scope 'magit--read-branch-scope
  :variable "act.current-view"
  :choices '("Book" "Bookmark")         ;org-glance-views
  :default "all")

(provide-me)
