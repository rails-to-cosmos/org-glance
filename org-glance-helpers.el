;; -*- lexical-binding: t; -*-

(require 'cl-macs)
(require 'dash)
(require 'eieio)
(require 'thunk)

(defmacro org-glance- (object &rest slots)
  "Get mutable pointers from SLOTS recursively starting from OBJECT.

Example: (org-glance- view :world :location)"
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
          `(aref ,acc ,(intern (substring s-slot 1 (- (length s-slot) 1)))))
         (t (user-error "Unknown slot reference: %s" slot)))))
   slots
   :initial-value object))

(defmacro org-glance-class (name superclasses slots &rest options-and-doc)
  "`defclass' wrapper that avoids compile-time slot declaration warnings."
  (declare (indent 3))
  `(progn
     (eieio-declare-slots ,@(mapcar (lambda (slot) (intern (format ":%s" (car slot)))) slots))
     (defclass ,name ,superclasses ,slots ,@options-and-doc)))

(cl-defmacro org-glance--with-temp-file (file &rest forms)
  (declare (indent 1))
  `(progn
     (mkdir (file-name-directory ,file) t)
     (with-temp-file ,file
       (org-mode)
       ,@forms)))

(cl-defun org-glance:append-to-file (string file)
  (cond ((and (f-exists-p file) (f-readable-p file)) (append-to-file (concat "\n" string) nil file))
        ((not (f-exists-p file)) (append-to-file string nil file))
        (t (user-error "Unable to write file %s" file))))

(cl-defmacro org-glance:with-temp-buffer (&rest forms)
  `(with-temp-buffer
     (org-mode)
     ,@forms))

(cl-defun org-glance:convert-links-to-titles (ast)
  "Replace links with its titles in AST."
  (cl-loop for link in (org-element-map ast 'link #'identity)
     do (org-element-set-element link (or (-some->> link
                                            org-element-contents
                                            org-element-interpret-data)
                                          (org-element-property :raw-link link)))
     finally return ast))

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

(cl-defmacro org-glance-comment (&rest _)
  (declare (indent 0))
  t)

(cl-defun org-glance:binary-search (vec
                                    v
                                    &key
                                      (l 0)
                                      (r (1- (length vec)))
                                      (key #'(lambda (vec idx) (aref vec idx))))
  (declare (indent 2))
  (thunk-let* ((m (/ (+ l r 1) 2))
               (mv (funcall key vec m))
               (lv (funcall key vec l))
               (rv (funcall key vec r)))
    (cond ((= 0 (length vec)) -1)
          ((< v lv) -1)
          ((= v lv) l)
          ((>= v rv) r)
          ((>= v mv) (org-glance:binary-search vec v :l m :r r :key key))
          (t (org-glance:binary-search vec v :l l :r (1- m) :key key)))))

;; (eval-when-compile
;;   (cl-assert (= -1 (org-glance:binary-search [] 10)))

;;   (cl-assert (= -1 (org-glance:binary-search [119] 10)))
;;   (cl-assert (= 0 (org-glance:binary-search [119] 1000)))

;;   (cl-assert (= -1 (org-glance:binary-search [119 211] 0)))
;;   (cl-assert (= 0 (org-glance:binary-search [119 211] 119)))
;;   (cl-assert (= 0 (org-glance:binary-search [119 211] 120)))
;;   (cl-assert (= 1 (org-glance:binary-search [119 211] 211)))
;;   (cl-assert (= 1 (org-glance:binary-search [119 211] 1000)))

;;   (cl-assert (= -1 (org-glance:binary-search [119 211 300] 100)))
;;   (cl-assert (= 0 (org-glance:binary-search [119 211 300] 120)))
;;   (cl-assert (= 1 (org-glance:binary-search [119 211 300] 211)))
;;   (cl-assert (= 1 (org-glance:binary-search [119 211 300] 250)))
;;   (cl-assert (= 2 (org-glance:binary-search [119 211 300] 300)))
;;   (cl-assert (= 2 (org-glance:binary-search [119 211 300] 1000))))

(provide 'org-glance-helpers)
