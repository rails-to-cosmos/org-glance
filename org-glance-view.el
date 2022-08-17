;; -*- lexical-binding: t; -*-

(require 'f)
(require 'eieio)
(require 'org-glance-materialization)
(require 'org-glance-types)

(declare-function f-mkdir-full-path 'f)

(org-glance-class org-glance-view nil
  ((store
    :type org-glance-store
    :initarg :store
    :reader org-glance-view:store
    :documentation "Original `org-glance-store' instance.")
   ;; available TODO states etc
   (predicate
    :type org-glance-predicate
    :initarg :predicate
    :reader org-glance-view:predicate
    :documentation "Predicate that takes one argument of type
    `org-glance-headline' and is guaranteed to be `t' for each
    headline per `org-glance-view' instance.")
   (materializations
    :type hash-table
    :initarg :materializations
    :initform (make-hash-table :test #'equal)
    :documentation "Keep track of materializations."
    :reader org-glance-view:materializations)))

(cl-defun org-glance-view:materialization (view location)
  (let ((true-location (file-truename location)))
    (or (gethash true-location (slot-value view :materializations))
        (progn (f-mkdir-full-path (file-name-directory true-location))
               (let ((materialization (org-glance-materialization :view view :location true-location)))
                 (org-glance--with-temp-file true-location
                   (insert (org-glance-materialization:header materialization))
                   (cl-dolist (headline (org-glance-store:headlines (org-glance-view:store view)))
                     (when (funcall (org-glance-view:predicate view) headline)
                       (org-glance-headline-insert
                        (org-glance-store:get
                         (org-glance-view:store view)
                         (org-glance-headline:hash headline))))))
                 (puthash true-location materialization (slot-value view :materializations)))))))

(provide 'org-glance-view)
