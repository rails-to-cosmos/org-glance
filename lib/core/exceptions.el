(require 'org-glance-module)

(define-error 'org-glance-source-file-corrupted
    "Source file corrupted, please reread" 'user-error)

(cl-defun org-glance-source-file-corrupted (format &rest args)
  (signal 'org-glance-source-file-corrupted (list (apply #'format-message format args))))

(define-error 'org-glance-properties-corrupted
    "Materialized view properties corrupted, please reread" 'user-error)

(cl-defun org-glance-properties-corrupted (format &rest args)
  (signal 'org-glance-properties-corrupted (list (apply #'format-message format args))))

(define-error 'org-glance-db-outdated
    "Material view database is outdated" 'user-error)

(defun org-glance-db-outdated (format &rest args)
  "Raise `org-glance-db-outdated' exception formatted with FORMAT ARGS."
  (signal 'org-glance-db-outdated (list (apply #'format-message format args))))

(define-error 'org-glance-exception:headline-not-found
    "Headline not found" 'user-error)

(defun org-glance-exception:headline-not-found (format &rest args)
  "Raise `org-glance-exception:headline-not-found' exception formatted with FORMAT ARGS."
  (signal 'org-glance-exception:headline-not-found (list (apply #'format-message format args))))

(defun org-glance-exception:view-not-found (format &rest args)
  "Raise `org-glance-exception:view-not-found' exception formatted with FORMAT ARGS."
  (signal 'org-glance-exception:view-not-found (list (apply #'format-message format args))))

(org-glance-module-provide)
