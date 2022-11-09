(require 'a)
(require 's)
(require 'org-glance-helpers)
(require 'org-glance-types)
(require 'org-glance-log)

(org-glance-class org-glance-dimension nil
    ((name :type symbol :initarg :name)
     (form :type list   :initarg :form)))

(org-glance-class org-glance-derivation nil
    ((dimension :type string :initarg :dimension)
     (value :type string :initarg :value)))

(cl-defun org-glance-derivation:representation (derivation)
  (cl-check-type derivation org-glance-derivation)

  (downcase (format "%s=%s"
                    (org-glance- derivation :dimension)
                    (org-glance- derivation :value))))

(cl-defun org-glance-derivation:filename (derivation)
  (cl-check-type derivation org-glance-derivation)

  (f-join (org-glance- derivation :dimension)
          (concat (org-glance- derivation :value) ".org")))

(cl-defun org-glance-derivation:from-string (s)
  (cl-check-type s string)

  (cl-destructuring-bind (dimension value)
      (--> s
           (downcase it)
           (s-split-up-to "=" it 2))
    (org-glance-derivation :dimension dimension
                           :value value)))

(cl-defun org-glance-derivation:from-key-value (k v)
  (org-glance-derivation:from-string (downcase (format "%s=%s" k v))))

(cl-defun org-glance-dimension:apply (dimension headline)
  (cl-check-type dimension org-glance-dimension)
  (cl-check-type headline org-glance-headline-header)

  (let ((result (eval (org-glance- dimension :form) (a-list 'headline headline))))
    (--map (thread-last it
             (format "%s")
             (downcase)
             (s-replace-regexp "[[:blank:][:punct:]]+" "-")
             (s-replace-regexp "[[:cntrl:]]+" "")
             (s-replace-regexp "[[:nonascii:]]+" "*"))
           (cl-typecase result
             (atom (list result))
             (otherwise result)))))

(cl-defun org-glance-dimension:partitions (dimension headline)
  (cl-check-type dimension org-glance-dimension)
  (cl-check-type headline (or org-glance-headline org-glance-headline-header))

  (cons (org-glance- dimension :name) (org-glance-dimension:apply dimension headline)))

(cl-defun org-glance-dimension:make-predicate (dimension value)
  (let ((name (org-glance- dimension :name)))
    (cl-typecase value
      (symbol `(member (quote ,value) ,name))
      (t `(member ,value ,name)))))

(cl-defun org-glance-dimension:predicates (dimension headline)
  (cl-check-type dimension org-glance-dimension)
  (cl-check-type headline (or org-glance-headline org-glance-headline-header))

  (cl-destructuring-bind (name &rest partitions) (org-glance-dimension:partitions dimension headline)
    (cl-loop for partition in partitions
       when (not (string-empty-p (s-trim (format "%s" partition))))
       collect (org-glance-dimension:make-predicate dimension partition))))

(cl-defun org-glance-dimension:context (headline dimensions)
  (cl-check-type headline (or org-glance-headline org-glance-headline-header))
  (cl-check-type dimensions (org-glance-type:list-of org-glance-dimension))

  (cl-loop for dimension in dimensions
     collect (org-glance-dimension:partitions dimension headline)))

(cl-defun org-glance-dimension:validate (predicate headline dimensions)
  (cl-check-type predicate list)
  (cl-check-type dimensions (org-glance-type:list-of org-glance-dimension))
  (cl-check-type headline (or org-glance-headline org-glance-headline-header))

  (let ((result (eval predicate (org-glance-dimension:context headline dimensions))))
    (if (null result)
        nil ;; validation failed
      (format "%s" (car result)) ;; return evaluation result
      )))

(provide 'org-glance-dimension)
