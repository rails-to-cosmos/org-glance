;; -*- lexical-binding: t -*-
;; `org-glance-tag' and `org-glance-tag-info' data models

(require 'org)
(require 's)
(require 'cl-lib)

(require 'org-glance-exception)
(require 'org-glance-namespace)

(org-glance-exception:define org-glance-tag-!-not-found
  "Tag not found")

(defun org-glance--valid-tag? (tag)
  "Return t if TAG is a downcased, non-nil symbol."
  (and (symbolp tag)
       (not (null tag))
       (string= (symbol-name tag) (downcase (symbol-name tag)))))

(cl-deftype org-glance-tag ()
  "Type representing a downcased, non-nil symbol."
  '(satisfies org-glance--valid-tag?))

(cl-defstruct (org-glance-tag-info (:type vector))
  (tag nil :read-only t :type org-glance-tag)
  (namespace nil :read-only t :type org-glance-namespace))

(cl-defun org-glance-tag-info (&key tag namespace)
  "Create an `org-glance-tag-info` struct with validated NAMESPACE.
NAMESPACE should be a string representing an existing readable and writable directory."
  (cl-check-type tag org-glance-tag)
  (cl-check-type namespace org-glance-namespace)

  (make-org-glance-tag-info :tag tag :namespace namespace))

(cl-defun org-glance-tag:exists? (tag tags) ;; -> bool
  (cl-check-type tag org-glance-tag)
  (cl-check-type tags hash-table)

  (gethash tag tags))

(cl-defun org-glance-tag:remove (tag tags)
  (cl-check-type tag org-glance-tag)
  (cl-check-type tags hash-table)

  (remhash tag tags))

(cl-defun org-glance-tag:to-string (tag)
  (cl-check-type tag org-glance-tag)

  (symbol-name tag))

(cl-defun org-glance-tag:from-string (value)
  (cl-check-type value string)

  (intern (s-downcase value)))

(cl-defun org-glance-tag:read (value)
  (cl-typecase value
    (symbol (intern (downcase (symbol-name value))))
    (string (org-glance-tag:from-string value))
    (t (error "Unable to convert value \"%v\" to `org-glance-tag'"))))

(cl-defun org-glance-tag:from-headline-at-point ()
  (mapcar #'org-glance-tag:from-string (org-get-tags)))

(cl-defun org-glance-tag:register (tag tags &key namespace)
  (cl-check-type tag org-glance-tag)
  (cl-check-type namespace org-glance-namespace)
  (cl-check-type tags hash-table)

  (let ((tag-info (org-glance-tag-info :tag tag :namespace namespace)))
    (puthash tag tag-info tags))

  tag)

(cl-defun org-glance-tag:generate-id (tag)
  (cl-check-type tag org-glance-tag)
  (format "%s-%s" tag (md5 (s-concat (prin1-to-string (current-time)) (buffer-string)))))

(provide 'org-glance-tag)
