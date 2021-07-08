(require 'org-glance-module)

(cl-defun org-glance-headline:choose ()
  (let ((headlines
         (cl-loop for vid in (org-glance-view:ids)
            append (cl-loop for headline in (org-glance-view:headlines vid)
                      collect (cons ;; duplication of format*
                               (format "[%s] %s" vid (org-glance-headline:format headline))
                               (org-element-put-property headline :ORG_GLANCE_VIEW_ID vid))))))
    (alist-get (org-completing-read "Headline: " headlines) headlines nil nil #'string=)))

(cl-defun org-glance-headline:add-log-note (note)
  (save-window-excursion
    (org-glance-headline:visit (org-glance-headline:at-point))
    (goto-char (org-log-beginning t))
    (insert note "\n")))

(cl-defun org-glance-link:visit ()
  (error "Not implemented yet."))

(cl-defun org-glance:add-relation (&optional (headline (org-glance-headline:choose))
                                     (relation "Related to")
                                     (bidirectional t))
  (interactive)
  (let* ((todo-state (org-glance-headline:state headline))
         (todo-label (if (string-empty-p todo-state) "" (format " *%s*" todo-state)))
         (title (s-replace-regexp (format "^%s\\W*" todo-state) "" (org-glance-headline:title headline)))
         (id (org-glance-headline:id headline))
         (view-id (org-glance-headline:view-id headline))
         (now (format-time-string (org-time-stamp-format 'long 'inactive) org-log-note-effective-time)))
    (org-glance-headline:add-log-note
     (org-glance:f** "- ${relation}${todo-label} =${view-id}= [[org-glance-visit:${id}][${title}]] on ${now}")))

  (when bidirectional
    (let* ((source-view-ids (org-glance-headline:view-ids))
           (source-headline (org-element-put-property (org-glance-headline:at-point)
                                                      :ORG_GLANCE_VIEW_ID
                                                      (s-join ", " (mapcar #'symbol-name source-view-ids)))))
      (save-window-excursion
        (org-glance-headline:visit headline)
        (org-glance:add-relation source-headline "Referred from" nil)))))

(org-glance-module-provide)
