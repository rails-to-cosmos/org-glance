(require 'org-glance-module)

(cl-defun org-glance-headline-at-point ()
  "Create headline from `org-element' at point.
`org-glance-headline' is an `org-element' of type `org-data'
with properties andd `org-element' of type `headline' in contents."
  (save-excursion
    (when (org-glance-ensure-at-heading)
      (save-restriction
        (org-narrow-to-subtree)
        (let* ((ast (org-element-parse-buffer))
               (headline (car (org-element-contents ast))))

          (org-element-put-property ast :title (with-temp-buffer
                                                 (insert (or (org-element-property :TITLE headline)
                                                             (org-element-property :raw-value headline)
                                                             ""))
                                                 (org-glance-replace-links-with-titles)
                                                 (buffer-string)))

          (org-element-put-property ast :tags (--map (intern (downcase it)) (org-element-property :tags headline)))

          ;; no mutation restrictions on complete ast
          (org-element-put-property headline :level 1)

          ast)))))

(cl-defun org-glance-headline-title (headline)
  (org-element-property :title headline))

(cl-defun org-glance-headline-tags (headline)
  (org-element-property :tags headline))

(cl-defun org-glance-headline-contents (headline)
  (s-trim (org-element-interpret-data headline)))

(org-glance:provide)
