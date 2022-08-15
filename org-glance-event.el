;; -*- lexical-binding: t; -*-

(cl-defstruct (org-glance-event
                (:constructor org-glance-event--create)
                (:copier org-glance-event--copy))
  (offset (float-time) :type float :read-only t)
  (type nil :type symbol :read-only t)
  (state nil :type cl-struct :read-only t))

(cl-defun org-glance-event:less-p (lhs rhs)
  "Return `t' if LHS has less offset than RHS.
Return `nil' otherwise."
  (< (org-glance-event-offset lhs)
     (org-glance-event-offset rhs)))

(cl-defun org-glance-event:PUT (state)
  (org-glance-event--create :type 'PUT :state state))

(cl-defun org-glance-event:PUT-p (event)
  (eq 'PUT (org-glance-event-type event)))

(cl-defun org-glance-event:RM (state)
  (org-glance-event--create :type 'RM :state state))

(cl-defun org-glance-event:RM-p (event)
  (eq 'RM (org-glance-event-type event)))

(provide 'org-glance-event)
