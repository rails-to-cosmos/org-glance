;; `org-glance-tag' data model

(require 'org)
(require 's)

(cl-defun org-glance-tag:exists? (tag tags) ;; -> bool
  (gethash tag tags))

(cl-defun org-glance-tag:remove (tag tags)
  (remhash tag tags))

(cl-defun org-glance-tag:to-string (tag)
  (symbol-name tag))

(cl-defun org-glance-tag:file-name (tag)
  (org-glance-tag:to-string tag))

(cl-defun org-glance-tag:from-string (value)
  (intern (s-downcase value)))

(cl-defun org-glance-tag:read (value)
  (cl-typecase value
    (symbol (intern (downcase (symbol-name value))))
    (string (org-glance-tag:from-string value))
    (t (user-error "Unable to convert value \"%v\" to `org-glance-tag'"))))

(cl-defun org-glance-tag:from-headline-at-point ()
  (mapcar #'org-glance-tag:from-string (org-get-tags)))

(cl-defun org-glance-tag:register (tag tags)
  (puthash tag t tags)
  tag)

(cl-defun org-glance-tag:id* (tag)
  (format "%s-%s" tag (md5 (s-concat (prin1-to-string (current-time)) (buffer-string)))))

(provide 'org-glance-tag)
