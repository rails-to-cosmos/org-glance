(defvar og-pm-cache-file "~/.emacs.d/org-glance/passwords.el")

(defun org-glance-password-manager-visit (&optional reread)
  (interactive "P")
  (when reread
    (delete-file og-pm-cache-file))
  (let ((org-glance-prompt "Visit secure data: ")
        (org-glance-cache og-pm-cache-file)
        (org-glance-fallback (lambda (x) (user-error "Entry not found.")))
        (org-glance-title-property :TITLE)
        (org-glance-filter (lambda (headline)
                             (-contains? (org-element-property :tags headline) "Password"))))
    (org-glance 'agenda-with-archives)))

(provide 'org-glance-password-manager)
