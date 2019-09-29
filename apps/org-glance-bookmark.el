(defvar og-bmkp-cache-file "~/.emacs.d/org-glance/bookmarks.el")
(defvar og-bmkp-filter (lambda (headline)
                         (and (s-matches? org-bracket-link-regexp (org-element-property :raw-value headline))
                              (-contains? (org-element-property :tags headline) "Bookmark"))))

;;;###autoload
(defun org-glance-bookmark-jump (&optional reread)
  (interactive "P")
  (when reread
    (delete-file og-bmkp-cache-file))
  (let ((org-glance-prompt "Jump to bookmark: ")
        (org-glance-cache og-bmkp-cache-file)
        (org-glance-fallback (lambda (x) (user-error "Bookmark not found.")))
        (org-glance-title-property :TITLE)
        (org-glance-action #'og-act--open-org-link)
        (org-glance-filter og-bmkp-filter))
    (org-glance 'agenda-with-archives)))

;;;###autoload
(defun org-glance-bookmark-visit (&optional reread)
  (interactive "P")
  (when reread
    (delete-file og-bmkp-cache-file))
  (let ((org-glance-prompt "Visit bookmark: ")
        (org-glance-cache og-bmkp-cache-file)
        (org-glance-fallback (lambda (x) (user-error "Bookmark not found.")))
        (org-glance-title-property :TITLE)
        (org-glance-filter og-bmkp-filter))
    (org-glance 'agenda-with-archives)))

(provide 'org-glance-bookmark)
