;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'cl-macs)

(defmacro org-glance-class (name superclasses slots &rest options-and-doc)
  "`defclass' wrapper that avoids compile-time slot declaration warnings."
  (declare (indent 3))
  `(progn
     (eieio-declare-slots ,@(mapcar (lambda (slot) (intern (format ":%s" (car slot)))) slots))
     (defclass ,name ,superclasses ,slots ,@options-and-doc)))

(defmacro org-glance-> (object &rest slots)
  "Get mutable pointers from SLOTS recursively starting from OBJECT.

Example: (org-glance-> mew :view :store :location)"
  (declare (indent 1))
  (cl-reduce
   (lambda (acc slot)
     `(slot-value ,acc ,slot))
   slots
   :initial-value object))

(cl-defun org-glance--ensure-at-headline ()
  "Ensure point is at heading.
Return t if it is or raise `user-error' otherwise."
  (or (org-at-heading-p)
      (progn
        (org-back-to-heading-or-point-min)
        (org-at-heading-p))))

(cl-defmacro org-glance--with-headline-at-point (&rest forms)
  "Execute FORMS only if point is at heading."
  (declare (indent 0))
  `(save-excursion
     (when (org-glance--ensure-at-headline)
       (save-restriction
         (save-excursion
           (save-match-data
             (org-with-limited-levels
              (narrow-to-region
               (save-excursion (org-back-to-heading t) (point))
               (save-excursion (org-end-of-subtree t t))))))
         ,@forms))))

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

(cl-defun org-glance--links-to-titles (ast)
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

(cl-defun org-glance-debug (&rest args)
  (let ((buffer (get-buffer-create "*org-glance-debug*")))
    (with-current-buffer buffer
      (delete-region (point-min) (point-max))
      (cl-loop for arg in args
         collect (prin1-to-string arg t) into result
         finally (insert (s-join "\n" result))))))

(provide 'org-glance-helpers)
