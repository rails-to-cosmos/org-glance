(defvar og-bmkp-cache-file "~/.emacs.d/org-glance/bookmarks.el")

;;;###autoload
(defun org-glance-bookmark-jump ()
  (interactive)
  (let ((org-glance-prompt "Jump to bookmark: ")
        (org-glance-cache og-bmkp-cache-file)
        (org-glance-fallback (lambda (x) (user-error "Bookmark not found.")))
        (org-glance-title-property :TITLE)
        (org-glance-action #'og-act--open-org-link)
        (org-glance-filter (lambda (headline)
                             (and (s-matches? org-bracket-link-regexp (org-element-property :raw-value headline))
                                  (-contains? (org-element-property :tags headline) "Bookmark")))))
    (org-glance 'agenda-with-archives)))

;;;###autoload
(defun org-glance-bookmark-visit ()
  (interactive)
  (let ((org-glance-prompt "Visit bookmark: ")
        (org-glance-cache og-bmkp-cache-file)
        (org-glance-fallback (lambda (x) (user-error "Bookmark not found.")))
        (org-glance-title-property :TITLE)
        (org-glance-filter (lambda (headline)
                             (and (s-matches? org-bracket-link-regexp (org-element-property :raw-value headline))
                                  (-contains? (org-element-property :tags headline) "Bookmark")))))
    (org-glance 'agenda-with-archives)))

;;;###autoload
(defun org-glance-bookmark-reread ()
  (interactive)
  (delete-file og-bmkp-cache-file)
  (org-glance-bookmark-jump))

(provide 'org-glance-bookmark)
