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
     (type
      :type string
      :initarg :type
      :initform "Declaration that transforms into predicate of
      one argument: `org-glance-headline'. View is guaranteed to
      contain only headlines for which predicate returns non-nil
      value.")
     (materializations
      :type hash-table
      :initarg :materializations
      :initform (make-hash-table :test #'equal)
      :documentation "Keep track of materializations.")))

(cl-defun org-glance-view:filter (view headline)
  "Decide if HEADLINE should be a part of VIEW."
  (let ((type (org-glance-> view :type)))
    (member (downcase type) (org-glance-headline-class headline))))

(cl-defun org-glance-view:materialize (view location)
  "Materialize VIEW to LOCATION."
  (let ((true-location (file-truename location)))
    (or (gethash true-location (org-glance-> view :materializations))
        (progn (f-mkdir-full-path (file-name-directory true-location))
               (let ((materialization (org-glance-materialization :view view
                                                                  :location true-location
                                                                  :offset (org-glance-store:offset (org-glance-> view :store)))))
                 (org-glance--with-temp-file true-location
                   (insert (org-glance-materialization:header materialization))
                   (cl-dolist (headline (org-glance-store:headlines (org-glance-view:store view)))
                     (when (org-glance-view:filter view headline)
                       (org-glance-headline-insert
                        (org-glance-store:get
                         (org-glance-view:store view)
                         (org-glance-headline:hash headline))))))
                 (puthash true-location materialization (org-glance-> view :materializations)))))))

(provide 'org-glance-view)
