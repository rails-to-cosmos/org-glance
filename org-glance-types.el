;; -*- lexical-binding: t; -*-

(require 'f)
(require 'cl-macs)
(require 'nadvice)

(defalias 's-replace-regexp 'replace-regexp-in-string)

;;;###autoload
(defun org-glance-subst-type (type)
  "Substitute lance's type declarations with full elisp declarations."
  (pcase type
    ((and T (cl-struct symbol) (guard (<= 65 (aref (symbol-name T) 0) 90)))
     (read (format "org-glance-%s"
                   (let ((case-fold-search nil))
                     (downcase (s-replace-regexp "\\([a-z]\\)\\([A-Z]\\)" "\\1-\\2" (symbol-name T)))))))
    ((and T (cl-struct list)) (-map #'org-glance-subst-type T))
    (_ type)))

;;;###autoload
(cl-defmacro org-glance-declare (name _ &rest types)
  "Declare TYPES for function NAME."
  `(progn
     ,(when-let (arg-types (-butlast types))
        `(advice-add (quote ,name)
                     :before (lambda (&rest args)
                               ,(cl-loop for type in arg-types
                                   for i from 0
                                   when (not (eq '-> type))
                                   collect `(cl-check-type (nth ,(/ i 2) args) ,(org-glance-subst-type type))
                                   into typechecks
                                   finally return (append '(progn) typechecks)))))
     (advice-add (quote ,name)
                 :filter-return (lambda (result)
                                  ,(when-let (return-type (car (last types)))
                                     `(cl-the ,(org-glance-subst-type return-type) result))))))

;;;###autoload
(defmacro org-glance-class (name superclasses slots &rest options-and-doc)
  "`defclass' wrapper that avoids compile-time slot declaration warnings."
  (declare (indent 3))
  (org-glance-subst-type
   `(progn
      (eieio-declare-slots ,@(mapcar (lambda (slot) (intern (format ":%s" (car slot)))) slots))
      (defclass ,name ,superclasses ,slots ,@options-and-doc))))

;;;###autoload
(defmacro org-glance-type (name arglist &rest body)
  "Define NAME as a new data type."
  (declare (debug cl-defmacro) (doc-string 3) (indent 2))
  (org-glance-subst-type `(cl-deftype ,name ,arglist ,@body)))

(org-glance-type Boolean () 'boolean)
(org-glance-type Cons () 'cons)
(org-glance-type Hash () 'string)
(org-glance-type HashTable () 'hash-table)
(org-glance-type Number () 'number)
(org-glance-type Offset () 'time)
(org-glance-type String () 'string)

(org-glance-type Scope () '(ListOf OptionalFile))

(org-glance-type ListOf (tp)
  `(satisfies (lambda (thing)
                (and (listp thing)
                     (cl-every (lambda (x) (cl-typep x (quote ,tp))) thing)))))

(org-glance-type Optional (tp)
  `(satisfies (lambda (thing)
                (or (null thing)
                    (cl-typep thing (quote ,tp))))))

(org-glance-type ReadableFile ()
  `(satisfies (lambda (location) (and (f-readable? location) (f-file? location)))))

(org-glance-type ReadableDirectory ()
  `(satisfies (lambda (location) (and (f-readable? location) (f-directory? location)))))

(org-glance-type OptionalDirectory ()
  `(satisfies (lambda (location) (or (not (f-exists? location)) (and (f-readable? location) (f-directory? location))))))

(org-glance-type OptionalFile ()
  `(satisfies (lambda (location) (or (not (f-exists-p location)) (and (f-readable-p location) (f-file-p location))))))

(org-glance-type org-glance-bounded-by (val)
  `(satisfies (lambda (thing)
                (and (cl-typep thing 'number)
                     (>= thing 0)
                     (< thing ,val)))))

(org-glance-type org-glance-index-of (seq)
  `(satisfies (lambda (thing)
                (cl-typep thing '(org-glance-bounded-by ,(seq-length seq))))))

(org-glance-type org-glance-world-location ()
  '(satisfies (lambda (location)
                (and (f-absolute? location)
                     (f-exists? location)
                     (f-directory? location)
                     (f-readable? location)
                     (f-exists? (f-join location "world.md"))))))

(provide 'org-glance-types)
