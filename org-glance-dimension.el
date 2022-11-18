(require 'a)
(require 's)
(require 'org-glance-helpers)
(require 'org-glance-types)
(require 'org-glance-log)

(org-glance-class org-glance-dimension nil
    ((name :type symbol :initarg :name)
     (form :type list   :initarg :form)))

(org-glance-class org-glance-partition nil
    ((dimension :type string :initarg :dimension)
     (value     :type string :initarg :value)))

(cl-defun org-glance-partition:representation (partition)
  (cl-check-type partition org-glance-partition)

  (downcase (format "%s=%s"
                    (org-glance? partition :dimension)
                    (org-glance? partition :value))))

(cl-defun org-glance-partition:path (partition)
  (cl-check-type partition org-glance-partition)

  (f-join (org-glance? partition :dimension)
          (org-glance? partition :value)))

(cl-defun org-glance-partition:from-string (s)
  (cl-check-type s string)

  (cl-destructuring-bind (dimension value)
      (--> s
           (downcase it)
           (s-split-up-to "=" it 2))
    (org-glance-partition :dimension dimension
                           :value value)))

(cl-defun org-glance-partition:from-key-value (k v)
  (org-glance-partition:from-string (downcase (format "%s=%s" k v))))

(cl-defun org-glance-dimension:apply (dimension headline)
  (cl-check-type dimension org-glance-dimension)
  (cl-check-type headline org-glance-headline-header)

  (let ((result (eval (org-glance? dimension :form) (a-list 'headline headline))))
    (--map (thread-last it
             (format "%s")
             (downcase)
             (replace-regexp-in-string "[[:blank:][:punct:]]+" "-")
             (replace-regexp-in-string "[[:cntrl:]]+" "")
             (replace-regexp-in-string "[[:nonascii:]]+" "*"))
           (cl-typecase result
             (atom (list result))
             (otherwise result)))))

(cl-defun org-glance-dimension:partitions (dimension headline)
  (cl-check-type dimension org-glance-dimension)
  (cl-check-type headline (or org-glance-headline org-glance-headline-header))

  (cons (org-glance? dimension :name) (org-glance-dimension:apply dimension headline)))

(cl-defun org-glance-dimension:make-predicate (dimension value)
  (let ((name (org-glance? dimension :name)))
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
  (cl-check-type dimensions (org-glance-list-of org-glance-dimension))

  (cl-loop for dimension in dimensions
     collect (org-glance-dimension:partitions dimension headline)))

(org-glance-fun org-glance-dimension:validate ((predicate :: list)
                                               (headline :: (or Headline HeadlineHeader))
                                               (dimensions :: (ListOf Dimension))) -> (Optional string)
  (pcase (eval predicate (org-glance-dimension:context headline dimensions))
    ((pred (null)) nil)
    (result (format "%s" (car result)))))

(provide 'org-glance-dimension)
