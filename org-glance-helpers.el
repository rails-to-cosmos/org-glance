;; -*- lexical-binding: t; -*-

(require 'dash)

(cl-defun org-glance--links-to-titles (ast)
  "Replace links with its titles in AST."
  (cl-loop for link in (org-element-map ast 'link #'identity)
     do (org-element-set-element link (or (-some->> link
                                            org-element-contents
                                            org-element-interpret-data)
                                          (org-element-property :raw-link link)))
     finally return ast))

(cl-defun org-glance--ensure-at-heading ()
  "Ensure point is at heading.
Return t if it is or raise `user-error' otherwise."
  (or (org-at-heading-p)
      (progn
        (org-back-to-heading-or-point-min)
        (org-at-heading-p))))

(cl-defmacro org-glance--with-heading-at-point (&rest forms)
  "Execute FORMS only if point is at heading."
  (declare (indent 0))
  `(save-excursion
     (when (org-glance--ensure-at-heading)
       (save-restriction
         (org-narrow-to-subtree)
         ,@forms))))

(cl-defmacro org-glance--with-temp-file (file &rest forms)
  (declare (indent 1))
  `(progn
     (mkdir (file-name-directory ,file) t)
     (with-temp-file ,file
       (org-mode)
       ,@forms)))

(cl-defmacro org-glance--with-temp-buffer (&rest forms)
  `(with-temp-buffer
     (org-mode)
     ,@forms))

(cl-defmacro org-glance--doalist (list &rest body)
  (declare (indent 1))
  `(dolist (it ,list)
     (let-alist it ,@body)))

(cl-defmacro org-glance--mapalist (list &rest body)
  (declare (indent 1))
  `(cl-loop for it in ,list
      collect (let-alist it ,@body)))

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
