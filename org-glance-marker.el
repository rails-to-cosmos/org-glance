;; -*- lexical-binding: t; -*-

(require 'org-glance-types)

(org-glance-class org-glance-marker nil
    ((hash :type string :initarg :hash)
     (position :type number :initarg :position)
     (changed? :type boolean :initarg :changed? :initform nil)
     (removed? :type boolean :initarg :removed? :initform nil)))

(provide 'org-glance-marker)
