;; -*- lexical-binding: t; -*-

(require 'f)
(require 'cl-macs)
(require 'nadvice)

(cl-deftype org-glance-type:bounded-by (val)
  `(satisfies (lambda (thing)
                (and (cl-typep thing 'number)
                     (>= thing 0)
                     (< thing ,val)))))

(cl-deftype org-glance-type:index-of (seq)
  `(satisfies (lambda (thing)
                (cl-typep thing '(org-glance-type:bounded-by ,(seq-length seq))))))

(cl-deftype org-glance-type:list-of (tp)
  `(satisfies (lambda (thing)
                (and (listp thing)
                     (cl-every (lambda (x) (cl-typep x (quote ,tp))) thing)))))

(cl-deftype org-glance-type:optional (tp)
  `(satisfies (lambda (thing)
                (or (null thing)
                    (cl-typep thing (quote ,tp))))))

(cl-deftype org-glance-type:directory ()
  `(satisfies (lambda (location) (and (f-readable? location)
                                 (f-directory? location)))))

(cl-deftype org-glance-type:optional-directory ()
  `(satisfies (lambda (location) (or (not (f-exists? location))
                                (and (f-readable? location)
                                     (f-directory? location))))))

(cl-deftype org-glance-type:optional-org-file ()
  `(satisfies (lambda (location) (or (not (f-exists-p location))
                                (and (f-readable-p location)
                                     (f-ext-p location "org"))))))

(cl-deftype org-glance-type:hash () 'string)

(cl-deftype org-glance-type:offset () 'time)

(cl-deftype org-glance-type:world-location ()
  '(satisfies (lambda (location)
                (and (f-absolute? location)
                     (f-exists? location)
                     (f-directory? location)
                     (f-readable? location)
                     (f-exists? (f-join location "world.md"))))))

(cl-deftype org-glance-type:scope ()
  '(org-glance-type:list-of org-glance-type:optional-org-file))

(provide 'org-glance-types)
