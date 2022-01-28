(require 'org-glance-module)

(cl-defmacro org-glance-exception-define (name message &optional (parent 'user-error))
  (declare (indent 1))
  `(progn
     (define-error (quote ,name) ,message (quote ,parent))
     (cl-defun ,name (format &rest args)
       (signal (quote ,name) (list (apply #'format format args))))))

(org-glance-exception-define org-glance-exception:SOURCE-CORRUPTED
  "Headline source corrupted, please reread")

(org-glance-exception-define org-glance-exception:PROPERTIES-CORRUPTED
  "Headline metadata corrupted, please reread")

(org-glance-exception-define org-glance-exception:METASTORE-OUTDATED
  "Metastore is outdated, please rebuild")

(org-glance-exception-define org-glance-exception:HEADLINE-NOT-FOUND
  "Headline not found")

(org-glance-exception-define org-glance-exception:CLASS-NOT-FOUND
  "Class not found")

(org-glance:provide)
