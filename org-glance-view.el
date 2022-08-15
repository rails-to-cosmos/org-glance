;; -*- lexical-binding: t; -*-

(require 'f)
(require 'eieio)
(require 'org-glance-materialization)

(declare-function f-mkdir-full-path 'f)
(eieio-declare-slots :materializations)
(eieio-declare-slots :store)

(defclass org-glance-view nil
  ((store
    :type org-glance-store
    :initarg :store
    :reader org-glance-view:store
    :documentation "Original `org-glance-store' instance.")
   (predicate
    :type function
    :initarg :predicate
    :reader org-glance-view:predicate
    :documentation "Predicate that takes one argument of type
    `org-glance-headline' and is guaranteed to be `t' for each
    headline per `org-glance-view' instance.")
   (materializations
    :type org-glance-materialization-list
    :initarg :materializations
    :initform nil
    :documentation "Keep track of materializations."
    :reader org-glance-view:materializations)))

(cl-defmethod org-glance-view:materialize ((view org-glance-view) (location string))
  (f-mkdir-full-path (file-name-directory location))
  (let ((materialization (org-glance-materialization :view view :location location)))
    (org-glance--with-temp-file location
      (insert (org-glance-materialization:header materialization))
      (cl-dolist (headline (org-glance-store:headlines (org-glance-view:store view)))
        (when (funcall (org-glance-view:predicate view) headline)
          (org-glance-headline-insert
           (org-glance-store:get
            (org-glance-view:store view)
            (org-glance-headline:hash headline))))))
    (cl-pushnew materialization (slot-value view :materializations))))

(provide 'org-glance-view)
