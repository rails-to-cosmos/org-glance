;; -*- lexical-binding: t; -*-

(require 'f)
(require 'cl-macs)

(require 'nadvice)
(defalias 's-replace-regexp 'replace-regexp-in-string)

(cl-defun lance-sub (type)
  "Substitute lance's type declarations with full elisp declarations."
  (pcase type
    ((and T
          (cl-struct symbol)
          (guard (<= 65 (aref (symbol-name T) 0) 90)))
     (read (format "org-glance-%s"
                   (let ((case-fold-search nil))
                     (downcase (s-replace-regexp "\\([a-z]\\)\\([A-Z]\\)" "\\1-\\2" (symbol-name T)))))))
    ((and T (cl-struct list)) (-map #'lance-sub T))
    (_ type)))

(cl-defmacro lance-dec (name _ &rest types)
  "Declare TYPES for function NAME."
  (let ((symbol (lance-sub name)))
    `(progn
       ,(when-let (arg-types (-butlast types))
          `(advice-add (quote ,symbol)
                       :before (lambda (&rest args)
                                 ,(cl-loop for type in arg-types
                                     for i from 0
                                     when (not (eq '-> type))
                                     collect `(cl-check-type (nth ,(/ i 2) args) ,(lance-sub type))
                                     into typechecks
                                     finally return (append '(progn) typechecks)))))
       (advice-add (quote ,symbol)
                   :filter-return (lambda (result)
                                    ,(when-let (return-type (car (last types)))
                                       `(cl-the ,(lance-sub return-type) result)))))))

(cl-defmacro lance-def (name &rest body)
  (declare (indent defun))
  `(cl-defun ,(lance-sub name) ,@body))

(cl-defmacro lance (name &rest args)
  `(,(lance-sub name) ,@args))

(cl-defmacro org-glance-fun (name args _ return-type &rest body)
  (declare (indent 4))
  (cl-loop
     for (arg _ type) in args
     collect arg into cl-args
     collect (list arg type) into cl-types
     finally return (append `(cl-defun ,name)
                            (list cl-args)
                            (cl-loop for (arg type) in cl-types
                               collect `(cl-check-type ,arg ,(lance-sub type)))
                            (list `(cl-the ,(lance-sub return-type)
                                     (progn ,@body))))))

;; (cl-macroexpand '(org-glance-fun hello ((a :: (Optional Hash))) -> HashSum
;;                   (+ 1 a)))

;; (org-glance-fun hello ((a :: Hash)) -> number
;;   (+ 1 a))

;; (cl-macroexpand '(org-glance-fun:type number))
;; (cl-check-type "a" (org-glance-fun:type Hash))

(cl-deftype org-glance-bounded-by (val)
  `(satisfies (lambda (thing)
                (and (cl-typep thing 'number)
                     (>= thing 0)
                     (< thing ,val)))))

(cl-deftype org-glance-index-of (seq)
  `(satisfies (lambda (thing)
                (cl-typep thing '(org-glance-bounded-by ,(seq-length seq))))))

(cl-deftype org-glance-list-of (tp)
  `(satisfies (lambda (thing)
                (and (listp thing)
                     (cl-every (lambda (x) (cl-typep x (quote ,tp))) thing)))))

(cl-deftype org-glance-optional (tp)
  `(satisfies (lambda (thing)
                (or (null thing)
                    (cl-typep thing (quote ,tp))))))

(cl-deftype org-glance-readable-file ()
  `(satisfies (lambda (location) (and (f-readable? location)
                                 (f-file? location)))))

(cl-deftype org-glance-readable-directory ()
  `(satisfies (lambda (location) (and (f-readable? location)
                                 (f-directory? location)))))

(cl-deftype org-glance-optional-directory ()
  `(satisfies (lambda (location) (or (not (f-exists? location))
                                (and (f-readable? location)
                                     (f-directory? location))))))

(cl-deftype org-glance-optional-file ()
  `(satisfies (lambda (location) (or (not (f-exists-p location))
                                (and (f-readable-p location)
                                     (f-file-p location))))))

(cl-deftype org-glance-optional-org-file ()
  `(satisfies (lambda (location) (or (not (f-exists-p location))
                                (and (f-readable-p location)
                                     (f-ext-p location "org"))))))

(cl-deftype org-glance-hash () 'string)

(cl-deftype org-glance-offset () 'time)

(cl-deftype org-glance-world-location ()
  '(satisfies (lambda (location)
                (and (f-absolute? location)
                     (f-exists? location)
                     (f-directory? location)
                     (f-readable? location)
                     (f-exists? (f-join location "world.md"))))))

(cl-deftype org-glance-scope ()
  '(org-glance-list-of org-glance-optional-org-file))

(provide 'org-glance-types)
