;; -*- lexical-binding: t; -*-

(require 'f)
(require 'cl-macs)
(require 'nadvice)

(cl-deftype org-glance-optional-file ()
  '(satisfies (lambda (location) (or (not (f-exists-p location))
                                (and (f-readable-p location)
                                     (f-ext-p location "org"))))))

(cl-defun org-glance:list-of-p (tp thing)
  (and (listp thing) (cl-every (lambda (x) (cl-typep x tp)) thing)))

(cl-deftype org-glance:list-of (tp)
  `(satisfies (lambda (thing) (org-glance:list-of-p (quote ,tp) thing))))

(cl-deftype org-glance-hash () 'string)

(provide 'org-glance-types)
