(require 'org-glance-module)

(cl-defmacro org-glance:define-exception (name message &optional (parent 'user-error))
  `(progn
     (define-error (quote ,name) ,message (quote ,parent))
     (cl-defun ,name (format &rest args)
       (signal (quote ,name) (list (apply #'format-message format args))))))

(org-glance:define-exception org-glance-exception:source-file-corrupted "Source file corrupted, please reread")
(org-glance:define-exception org-glance-exception:properties-corrupted "Headline metadata corrupted, please reread")
(org-glance:define-exception org-glance-exception:metastore-outdated "Metastore is outdated, please rebuild")
(org-glance:define-exception org-glance-exception:headline-not-found "Headline not found")

(org-glance:provide)
