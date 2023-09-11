(require 'org-glance-module)

(defvar org-glance:classes (make-hash-table)
  "Hash table (id->view) that lists all registered classes.")

(defun org-glance:get-class (class)
  (gethash class org-glance:classes))

(org-glance:provide)
