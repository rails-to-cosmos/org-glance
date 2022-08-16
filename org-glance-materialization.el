;; -*- lexical-binding: t; -*-

(require 'eieio)
(require 'org-glance-helpers)

(eieio-declare-slots :view :predicate)

(defclass org-glance-materialization nil
  ((view
    :type org-glance-view
    :initarg :view
    :reader org-glance-materialization:view
    :documentation "Backlink to source of materialization.")
   (location
    :type org-glance-file
    :initarg :location
    :reader org-glance-materialization:location
    :documentation "Location where materialization persists.")))

(cl-deftype org-glance-materialization-list ()
  '(satisfies org-glance-materialization-list-p))

(cl-defun org-glance-materialization-list-p (list)
  (cl-every #'org-glance-materialization-p list))

(cl-defmethod org-glance-materialization:header ((materialization org-glance-materialization))
  "Generate header for MATERIALIZATION."
  (format
   "#  -*- mode: org; mode: org-glance-material -*-

#+STORE: %s
#+PREDICATE: %s

"
   (org-glance-> materialization :view :store :location)
   (org-glance-> materialization :view :predicate)))

(provide 'org-glance-materialization)
