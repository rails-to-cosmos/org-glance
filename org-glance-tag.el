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

;; TODO refactor is needed for all the filters
(cl-defun org-glance-tag:filter (tag) ;; -> callable
  #'(lambda (headline)
      (when (-contains? (mapcar #'downcase (org-element-property :tags headline)) (symbol-name tag))
        headline)))

(cl-defun org-glance-tag:register (tag tags)
  (puthash tag t tags)
  tag)

(cl-defun org-glance-tag:id* (&optional (tag (org-glance-tags:completing-read)))
  (substring-no-properties
   (format "%s-%s-%s"
           tag
           (s-join "-" (mapcar #'number-to-string (current-time)))
           (secure-hash 'md5 (buffer-string)))))

(provide 'org-glance-tag)
