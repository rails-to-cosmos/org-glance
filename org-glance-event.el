;; -*- lexical-binding: t; -*-

(require 'org-glance-helpers)

(org-glance-class org-glance-event ()
    ((offset :type float :initarg :offset :initform (float-time))))

(cl-defun org-glance-event:less-p (lhs rhs)
  "Return `t' if LHS has less offset than RHS.
Return `nil' otherwise."
  (< (org-glance-> lhs :offset)
     (org-glance-> rhs :offset)))

(provide 'org-glance-event)
