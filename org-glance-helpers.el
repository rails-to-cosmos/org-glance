;; -*- lexical-binding: t; -*-

(require 'cl-macs)
(require 'dash)
(require 'eieio)
(require 'thunk)

(defmacro org-glance? (object &rest slots)
  "Get mutable pointers from SLOTS recursively starting from OBJECT.

Example: (org-glance? view :world :location)"
  (declare (indent 1))
  (cl-reduce
   (lambda (acc slot)
     (let ((s-slot (format "%s" slot)))
       (cond
         ;; convert :-prefixed symbols to slot names
         ((s-starts-with? ":" s-slot) `(slot-value ,acc ,slot))
         ;; convert other symbols to array indices
         ((and (s-starts-with? "[" s-slot)
               (s-ends-with? "]" s-slot))
          (let ((idx (read (substring s-slot 1 (- (length s-slot) 1)))))
            `(aref ,acc ,idx)))
         (t (user-error "Unknown slot reference: %s" slot)))))
   slots
   :initial-value object))

(defmacro org-glance! (object &rest slots)
  (let ((get-slots (--take-while (not (eq it :=)) slots))
        (set-slots (cdr (--drop-while (not (eq it :=)) slots))))
    (pcase set-slots
      (`() (user-error "Set slots should contain more than 1 element"))
      (`(,value) `(setf (org-glance? ,object ,@get-slots) ,value))
      (value `(setf (org-glance? ,object ,@get-slots) (org-glance? ,@value))))))

(defmacro org-glance++ (object &rest slots)
  `(cl-incf (org-glance? ,object ,@slots)))

(defmacro org-glance-- (object &rest slots)
  `(cl-decf (org-glance? ,object ,@slots)))

(defmacro org-glance-class (name superclasses slots &rest options-and-doc)
  "`defclass' wrapper that avoids compile-time slot declaration warnings."
  (declare (indent 3))
  `(progn
     (eieio-declare-slots ,@(mapcar (lambda (slot) (intern (format ":%s" (car slot)))) slots))
     (defclass ,name ,superclasses ,slots ,@options-and-doc)))

(cl-defmacro org-glance:with-temp-file (file &rest forms)
  (declare (indent 1))
  `(progn
     (mkdir (file-name-directory ,file) t)
     (with-temp-file ,file
       (org-mode)
       ,@forms)))

(cl-defmacro org-glance:with-file-overwrite (file &rest forms)
  (declare (indent 1))
  `(org-glance:with-temp-file ,file
     (insert-file-contents ,file)
     ,@forms))

(cl-defmacro org-glance:with-temp-buffer (&rest forms)
  `(with-temp-buffer
     (org-mode)
     ,@forms))

(cl-defmacro org-glance-memc (var bodyform handler)
  "Memory consumption report."
  (declare (indent 2))
  `(cl-flet ((memc () (list
                       cons-cells-consed
                       floats-consed
                       vector-cells-consed
                       symbols-consed
                       string-chars-consed
                       intervals-consed
                       strings-consed)))
     (let ((initial-memory-consumption (memc))
           final-memory-consumption)
       (prog1 ,bodyform
         (let ((,var (--map (- (car it) (cdr it)) (-zip (memc) initial-memory-consumption))))
           ,handler)))))

(provide 'org-glance-helpers)
