(require 'org-glance-module)

(define-error 'org-glance-source-file-corrupted
    "Source file corrupted, please reread" 'user-error)

(cl-defun org-glance-source-file-corrupted (format &rest args)
  (signal 'org-glance-source-file-corrupted (list (apply #'format-message format args))))

(define-error 'org-glance-properties-corrupted
    "Materialized view properties corrupted, please reread" 'user-error)

(cl-defun org-glance-properties-corrupted (format &rest args)
  (signal 'org-glance-properties-corrupted (list (apply #'format-message format args))))

(org-glance-module-provide)
