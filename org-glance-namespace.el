;; -*- lexical-binding: t -*-

(require 'cl-lib)

(cl-defun org-glance--valid-directory? (dir)
  "Check if DIR is an existing, readable, and writable directory."
  (and (stringp dir)
       (file-directory-p dir)
       (file-readable-p dir)
       (file-writable-p dir)))

(cl-deftype org-glance-namespace ()
  '(satisfies org-glance--valid-directory?))

(provide 'org-glance-namespace)
