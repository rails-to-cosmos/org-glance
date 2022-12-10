;; -*- lexical-binding: t; -*-

(require 'org-glance-helpers)
(require 'org-glance-types)
(require 'org-glance-log)

(org-glance-class Partition ()
    ((dimension :type String :initarg :dimension)
     (value     :type String :initarg :value)))

(org-glance-declare org-glance-partition:representation :: Partition -> string)
(defun org-glance-partition:representation (partition)
  (downcase (format "%s=%s" (org-glance? partition :dimension) (org-glance? partition :value))))

(org-glance-declare org-glance-partition:path :: Partition -> OptionalDirectory)
(defun org-glance-partition:path (partition)
  (f-join (org-glance? partition :dimension) (org-glance? partition :value)))

(org-glance-declare org-glance-partition:from-string :: string -> Partition)
(defun org-glance-partition:from-string (str)
  (cl-destructuring-bind (dimension value)
      (--> (downcase str) (s-split-up-to "=" it 2))
    (org-glance-partition :dimension dimension :value value)))

(org-glance-declare org-glance-partition:from-key-value :: string -> string -> Partition)
(defun org-glance-partition:from-key-value (key value)
  (org-glance-partition:from-string (downcase (format "%s=%s" key value))))

(provide 'org-glance-partition)
