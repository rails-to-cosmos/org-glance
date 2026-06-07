;; -*- lexical-binding: t -*-
;; `org-glance-tag' data model

(require 'org)
(require 's)
(require 'cl-lib)

(cl-defun org-glance-tag? (tag)
  "Return t if TAG is a downcased, non-nil symbol."
  (and (symbolp tag)
       (not (null tag))
       (string= (symbol-name tag) (downcase (symbol-name tag)))))

(cl-deftype org-glance-tag () '(satisfies org-glance-tag?))

(cl-defun org-glance-tag:to-string (tag)
  (cl-check-type tag org-glance-tag)

  (symbol-name tag))

(provide 'org-glance-tag)
