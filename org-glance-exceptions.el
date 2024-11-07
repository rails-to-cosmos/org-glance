(cl-defmacro org-glance-exception-define (name message &optional (parent 'user-error))
  `(progn
     (define-error (quote ,name) ,message (quote ,parent))
     (cl-defun ,name (format &rest args)
       (signal (quote ,name) (list (apply #'format format args))))))

(org-glance-exception-define SOURCE-CORRUPTED "Headline source corrupted, please reread")
(org-glance-exception-define PROPERTIES-CORRUPTED "Headline metadata corrupted, please reread")
(org-glance-exception-define METASTORE-OUTDATED "Metastore is outdated, please rebuild")
(org-glance-exception-define HEADLINE-NOT-FOUND "Headline not found")
(org-glance-exception-define CLASS-NOT-FOUND "Class not found")

(provide 'org-glance-exceptions)
