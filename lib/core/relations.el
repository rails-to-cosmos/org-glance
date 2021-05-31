(require 'org-glance-module)

;; (plist-get '(:a 1 :b 2) :a)

(cl-defun org-glance-headline:choose ()
  (let ((headlines
         (cl-loop for vid in (org-glance-view:ids)
            append (cl-loop for hl in (org-glance-view:headlines vid)
                      collect (cons  ;; duplication of format*
                               (format "[%s] %s" vid (org-glance-headline:format hl))
                               (org-element-put-property hl :ORG_GLANCE_VIEW_ID vid))))))
    (alist-get (org-completing-read "Headline: " headlines) headlines nil nil #'string=)))

(cl-defun org-glance-headline:add-log-note (note)
  (org-glance-headline:goto-beginning-of-nearest-headline)
  (goto-char (org-log-beginning t))
  (insert (format "%s\n" note)))

(s-replace-regexp "^TODO\\W+" "" "TODO Hello there")

(cl-defun org-glance:add-relation (&optional (hl (org-glance-headline:choose))
                                     (relstate "Related to")
                                     (bidirectional t))
  (interactive)

  (let* ((todo-state (org-glance-headline:state hl))
         (todo-label (if todo-state (format " *%s*" todo-state) ""))
         (title (s-replace-regexp (format "^%s\\W*" todo-state) "" (org-glance-headline:title hl)))
         (id (org-glance-headline:id hl))
         (view-id (org-glance-headline:view-id hl))
         (now (format-time-string
	       (org-time-stamp-format 'long 'inactive)
	       org-log-note-effective-time)))
    (org-glance-headline:add-log-note
     (format "- %s%s =%s= [[elisp:(org-glance-headline:visit \"%s\")][%s]] on %s"
             relstate
             todo-label
             view-id
             id
             title
             now)))


  (when bidirectional
    (let* ((source-view-ids (org-glance-headline:view-ids))
           (source-headline (org-element-put-property (org-glance-headline:at-point)
                                                      :ORG_GLANCE_VIEW_ID
                                                      (s-join ", " (mapcar #'symbol-name source-view-ids)))))
      (save-window-excursion
        (org-glance-headline:visit (org-glance-headline:id hl))
        (org-glance:add-relation source-headline "Referred by" nil)))))

(org-glance-module-provide)
