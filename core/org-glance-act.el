(cl-defun org-glance-act (headline &optional action)
  (if action
      (funcall action headline)
    (let ((file (org-element-property :file headline))
          (point (org-element-property :begin headline)))
      (find-file file)
      (goto-char point)
      (org-show-context))))

(defun og-act--open-org-link (headline)
  (let ((title (org-element-property :raw-value headline)))
    (if-let (search (string-match org-any-link-re title))
        (let ((link (substring title (match-beginning 0) (match-end 0)))
              (org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
          (org-open-link-from-string link))
      (user-error "Link not found: %s" title))))

(provide 'org-glance-act)
