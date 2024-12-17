;; -*- lexical-binding: t -*-

(cl-defmacro org-glance-exception:define (name message &optional (parent 'user-error))
  (declare (indent 1))
  `(progn
     (define-error (quote ,name) ,message (quote ,parent))
     (cl-defun ,name (format &rest args)
       (signal (quote ,name) (list (apply #'format format args))))))

(provide 'org-glance-exception)
