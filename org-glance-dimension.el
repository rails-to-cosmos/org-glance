(require 'a)
(require 's)

(require 'org-glance-helpers)
(require 'org-glance-types)
(require 'org-glance-log)

(org-glance-class org-glance-dimension nil
    ((name :type symbol :initarg :name)
     (form :type list   :initarg :form)))

(org-glance-declare org-glance-dimension:apply :: Dimension -> HeadlineHeader -> list)
(defun org-glance-dimension:apply (dimension headline)
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

(org-glance-declare org-glance-dimension:split :: Dimension -> (or Headline HeadlineHeader) -> cons)
(defun org-glance-dimension:split (dimension headline)
  (cons (org-glance? dimension :name) (org-glance-dimension:apply dimension headline)))

(defun org-glance-dimension:make-predicate (dimension value)
  (let ((name (org-glance? dimension :name)))
    (cl-typecase value
      (symbol `(member (quote ,value) ,name))
      (t `(member ,value ,name)))))

(org-glance-declare org-glance-dimension:predicates :: Dimension -> (or Headline HeadlineHeader) -> list)
(defun org-glance-dimension:predicates (dimension headline)
  (cl-destructuring-bind (name &rest partitions) (org-glance-dimension:split dimension headline)
    (cl-loop for partition in partitions
       when (not (string-empty-p (s-trim (format "%s" partition))))
       collect (org-glance-dimension:make-predicate dimension partition))))

(org-glance-declare org-glance-dimension:context :: (or Headline HeadlineHeader) -> (ListOf Dimension) -> t)
(defun org-glance-dimension:context (headline dimensions)
  (cl-loop for dimension in dimensions
     collect (org-glance-dimension:split dimension headline)))

(org-glance-declare org-glance-dimension:validate :: list -> (or Headline HeadlineHeader) -> (ListOf Dimension) -> (Optional string))
(defun org-glance-dimension:validate (predicate headline dimensions)
  (pcase (eval predicate (org-glance-dimension:context headline dimensions))
    ((pred (null)) nil)
    (result (format "%s" (car result)))))

(provide 'org-glance-dimension)
