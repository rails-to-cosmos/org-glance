(require 'org-glance-module)

(cl-defun org-glance-headline:add-log-note (note &optional (headline (org-glance-headline:at-point)))
  (org-glance-headline:narrow headline
    (goto-char (org-log-beginning t))
    (insert note "\n")))

(cl-defun org-glance:add-relation (&optional
                                     (source (org-glance-headline:at-point))
                                     (relation "Related to")
                                     (target (org-glance-metastore:choose-headline)))
  (interactive)
  (let* ((target-id (org-glance-headline:id target))
         (target-state (org-glance-headline:state target))
         (target-label (if (string-empty-p target-state) "" (format " *%s*" target-state)))
         (target-title (s-replace-regexp (format "^%s\\W*" target-state) "" (org-glance-headline:format target)))
         (target-views (s-join ", " (org-glance-headline:view-ids target)))
         (now (format-time-string (org-time-stamp-format 'long 'inactive) (current-time))))
    (org-glance-headline:add-log-note
     (org-glance:f**
      "- ${relation}${target-label} =${target-views}= [[org-glance-visit:${target-id}][${target-title}]] on ${now}")
     source)))

(org-glance-module-provide)
