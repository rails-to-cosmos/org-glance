;; -*- lexical-binding: t; -*-

(require 'type-break)
(require 'org-glance-offset)
(require 'org-glance-helpers)

(org-glance-class org-glance-event ()
    ((offset :type org-glance-offset
             :initarg :offset
             :initform (org-glance-offset:current))))

(provide 'org-glance-event)
