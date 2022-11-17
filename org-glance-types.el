;; -*- lexical-binding: t; -*-

(require 'f)
(require 'cl-macs)
(require 'nadvice)

(cl-defmacro org-glance-fun (name args _ return-type &rest body)
  (declare (indent 4))
  (cl-loop
     for (arg _ type) in args
     collect arg into cl-args
     collect (list arg type) into cl-types
     finally return (append `(cl-defun ,name)
                            (list cl-args)
                            (cl-loop for (arg type) in cl-types
                               collect `(cl-check-type ,arg ,(pcase (format "%s" type)
                                                               ((and type (guard (<= 65 (aref type 0) 90))) (read (format "org-glance-type:%s" (let ((case-fold-search nil))
                                                                                                                                                 (downcase (s-replace-regexp "\\([a-z]\\)\\([A-Z]\\)" "\\1-\\2" type))))))
                                                               (otherwise type))))
                            (list `(cl-the ,(pcase (format "%s" return-type)
                                              ((and return-type (guard (<= 65 (aref return-type 0) 90))) (read (format "org-glance-type:%s" (let ((case-fold-search nil))
                                                                                                                                              (downcase (s-replace-regexp "\\([a-z]\\)\\([A-Z]\\)" "\\1-\\2" return-type))))))
                                              (otherwise return-type))
                                     (progn ,@body))))))


;; (cl-macroexpand '(org-glance-fun hello ((a :: Hash)) -> HashSum
;;                   (+ 1 a)))

;; (org-glance-fun hello ((a :: Hash)) -> number
;;   (+ 1 a))

;; (cl-macroexpand '(org-glance-fun:type number))
;; (cl-check-type "a" (org-glance-fun:type Hash))

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

(cl-deftype org-glance-type:readable-file ()
  `(satisfies (lambda (location) (and (f-readable? location)
                                 (f-file? location)))))

(cl-deftype org-glance-type:readable-directory ()
  `(satisfies (lambda (location) (and (f-readable? location)
                                 (f-directory? location)))))

(cl-deftype org-glance-type:optional-directory ()
  `(satisfies (lambda (location) (or (not (f-exists? location))
                                (and (f-readable? location)
                                     (f-directory? location))))))

(cl-deftype org-glance-type:optional-file ()
  `(satisfies (lambda (location) (or (not (f-exists-p location))
                                (and (f-readable-p location)
                                     (f-file-p location))))))

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
