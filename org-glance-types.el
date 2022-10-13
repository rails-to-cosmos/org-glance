;; -*- lexical-binding: t; -*-

(require 'f)
(require 'cl-macs)
(require 'nadvice)

(cl-deftype org-glance-file ()
  '(satisfies (lambda (location) (or (not (f-exists-p location))
                                (and (f-readable-p location)
                                     (f-ext-p location "org"))))))

(cl-deftype org-glance-directory ()
  '(satisfies (lambda (location) (and (f-dir-p location)
                                 (f-readable-p location)))))

(cl-defun org-glance:list-of-p (tp thing)
  (and (listp thing) (cl-every (lambda (x) (cl-typep x tp)) thing)))

(cl-deftype org-glance:list-of (tp)
  `(satisfies (lambda (thing) (org-glance:list-of-p (quote ,tp) thing))))

(provide 'org-glance-types)
