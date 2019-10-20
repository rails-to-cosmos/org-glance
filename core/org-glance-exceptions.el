(define-error 'org-glance-cache-outdated
  "Cache file is outdated"
  'user-error)

(defun org-glance-cache-outdated (format &rest args)
  (signal 'org-glance-cache-outdated
          (list (apply #'format-message format args))))

(provide 'org-glance-exceptions)
