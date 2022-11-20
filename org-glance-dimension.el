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

(org-glance-declare org-glance-partition:representation :: Partition -> string)
(cl-defun org-glance-partition:representation (partition)
  (downcase (format "%s=%s" (org-glance? partition :dimension) (org-glance? partition :value))))

(org-glance-declare org-glance-partition:path :: Partition -> OptionalDirectory)
(cl-defun org-glance-partition:path (partition)
  (f-join (org-glance? partition :dimension) (org-glance? partition :value)))

(org-glance-declare org-glance-partition:from-string :: string -> Partition)
(cl-defun org-glance-partition:from-string (str)
  (cl-destructuring-bind (dimension value)
      (--> (downcase str) (s-split-up-to "=" it 2))
    (org-glance-partition :dimension dimension :value value)))

(org-glance-declare org-glance-partition:from-key-value :: string -> string -> Partition)
(cl-defun org-glance-partition:from-key-value (key value)
  (org-glance-partition:from-string (downcase (format "%s=%s" key value))))

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

(org-glance-declare org-glance-dimension:validate :: list -> (or Headline HeadlineHeader) -> (ListOf Dimension) -> (Optional string))
(defun org-glance-dimension:validate (predicate headline dimensions)
  (pcase (eval predicate (org-glance-dimension:context headline dimensions))
    ((pred (null)) nil)
    (result (format "%s" (car result)))))

(provide 'org-glance-dimension)
