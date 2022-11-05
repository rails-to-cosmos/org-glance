;; -*- lexical-binding: t; -*-

(require 'f)
(require 'cl-macs)
(require 'nadvice)

(cl-deftype org-glance-type:optional-org-file ()
  '(satisfies (lambda (location) (or (not (f-exists-p location))
                                (and (f-readable-p location)
                                     (f-ext-p location "org"))))))

(cl-defun org-glance-type:list-of-p (tp thing)
  (and (listp thing) (cl-every (lambda (x) (cl-typep x tp)) thing)))

(cl-deftype org-glance-type:list-of (tp)
  `(satisfies (lambda (thing) (org-glance-type:list-of-p (quote ,tp) thing))))

(cl-deftype org-glance-type:hash () 'string)

(cl-deftype org-glance-type:offset nil 'time)

(cl-deftype org-glance-type:world-location ()
  '(satisfies (lambda (location)
                (and (f-absolute? location)
                     (f-exists? location)
                     (f-directory? location)
                     (f-readable? location)
                     (f-exists? (f-join location "org-glance-world.md"))))))

(cl-deftype org-glance-scope ()
  (org-glance-type:list-of org-glance-type:optional-org-file))

(provide 'org-glance-types)
