(cl-defun org-glance-act (headline &optional action)
  (if action
      (funcall action headline)
    (og-act--visit-headline headline)))

(defun og-act--visit-headline (headline)
  (let* ((file (org-element-property :file headline))
         (point (org-element-property :begin headline))
         (file-buffer (get-file-buffer file)))

    (if (file-exists-p file)
        (find-file file)
      (user-error "File not found: %s" file))

    (goto-char point)

    (if (condition-case nil
            (s-contains? (org-element-property :raw-value (org-element-at-point))
                         (org-element-property :raw-value headline))
          (error nil))
        (org-show-context 'org-goto)
      (unless file-buffer
        (kill-buffer))
      (user-error "Cache file is outdated"))))

(defun og-act--open-org-link (headline)
  (let* ((file (org-element-property :file headline))
         (file-buffer (get-file-buffer file))
         (org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
    (og-act--visit-headline headline)
    (org-open-at-point)
    (if file-buffer
        (bury-buffer file-buffer)
      (kill-buffer (get-file-buffer file)))))

(provide 'org-glance-act)
