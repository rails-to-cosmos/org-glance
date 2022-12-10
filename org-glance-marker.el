;; -*- lexical-binding: t; -*-

(require 'org-glance-types)

(org-glance-class Marker nil
    ((hash :type String :initarg :hash)
     (position :type Number :initarg :position)
     (changed? :type Boolean :initarg :changed? :initform nil)
     (removed? :type Boolean :initarg :removed? :initform nil)))

(provide 'org-glance-marker)
