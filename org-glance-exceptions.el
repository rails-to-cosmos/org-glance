;; -*- lexical-binding: t -*-

(cl-defmacro org-glance-exception:define (name message &optional (parent 'user-error))
  `(progn
     (define-error (quote ,name) ,message (quote ,parent))
     (cl-defun ,name (format &rest args)
       (signal (quote ,name) (list (apply #'format format args))))))


(org-glance-exception:define org-glance-exception:corrupted-properties "Headline metadata corrupted, please reread")
(org-glance-exception:define org-glance-exception:outdated-metadata "Metadata is outdated, please rebuild")
(org-glance-exception:define org-glance-exception:headline-not-found "Headline not found")
(org-glance-exception:define org-glance-exception:tag-not-found "Tag not found")

(provide 'org-glance-exceptions)
