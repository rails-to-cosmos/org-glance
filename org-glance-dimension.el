(require 'a)
(require 's)
(require 'org-glance-helpers)

(org-glance-class org-glance-dimension nil
    ((name :type symbol
           :initarg :name)
     (form :type list
           :initarg :form)))

(cl-defun org-glance-dimension:partitions (dimension headline)
  (cl-check-type dimension org-glance-dimension)
  (cl-check-type headline (or org-glance-headline org-glance-headline-header))

  (cons (org-glance- dimension :name)
        (let ((result (eval (org-glance- dimension :form) (a-list 'headline headline))))
          (cond ((atom result) (list result))
                (t result)))))

(cl-defun org-glance-dimension:predicates (dimension headline)
  (cl-check-type dimension org-glance-dimension)
  (cl-check-type headline (or org-glance-headline org-glance-headline-header))

  (cl-destructuring-bind (name &rest partitions) (org-glance-dimension:partitions dimension headline)
    (cl-loop for partition in partitions
       when (not (string-empty-p (s-trim (format "%s" partition))))
       collect (cl-typecase partition
                 (symbol `(member (quote ,partition) ,name))
                 (t `(member ,partition ,name))))))

(cl-defun org-glance-dimension:context (headline dimensions)
  (cl-check-type headline (or org-glance-headline org-glance-headline-header))
  (cl-check-type dimensions list)

  (cl-loop for dimension in dimensions
     collect (org-glance-dimension:partitions dimension headline)))

(cl-defun org-glance-dimension:validate (predicate headline dimensions)
  (cl-check-type predicate list)
  (cl-check-type dimensions list)
  (cl-check-type headline (or org-glance-headline org-glance-headline-header))

  (car (eval predicate (org-glance-dimension:context headline dimensions))))

(provide 'org-glance-dimension)
