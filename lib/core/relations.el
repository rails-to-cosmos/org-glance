(require 'org-glance-module)

(cl-defun org-glance-headline:add-log-note (note &optional (headline (org-glance-headline:at-point)))
  (save-window-excursion
    (org-glance-headline:visit (org-glance-metastore:get-headline-by-id (org-glance-headline:id headline)))
    (goto-char (org-log-beginning t))
    (insert note "\n")))

(cl-defun org-glance-link:visit ()
  (error "Not implemented yet."))

(cl-defun org-glance:add-relation (&optional (headline (org-glance-metastore:choose-headline))
                                     (relation "Related to")
                                     (bidirectional t))
  (interactive)
  (let* ((todo-state (org-glance-headline:state headline))
         (todo-label (if (string-empty-p todo-state) "" (format " *%s*" todo-state)))
         (title (s-replace-regexp (format "^%s\\W*" todo-state) "" (org-glance-headline:title headline)))
         (id (org-glance-headline:id headline))
         (view-ids (s-join ", " (org-glance-headline:view-ids headline)))
         (now (format-time-string (org-time-stamp-format 'long 'inactive) (current-time))))
    (org-glance-headline:add-log-note
     (org-glance:f** "- ${relation}${todo-label} =${view-ids}= [[org-glance-visit:${id}][${title}]] on ${now}")))

  (when bidirectional
    (save-window-excursion
      (org-glance-headline:visit (org-glance-metastore:get-headline-by-id (org-glance-headline:id headline)))
      (org-glance:add-relation (org-glance-headline:at-point) "Referred from" nil))))

(org-glance-module-provide)
