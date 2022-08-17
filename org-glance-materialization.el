;; -*- lexical-binding: t; -*-

(require 'eieio)
(require 'org-glance-helpers)



;; (macroexpand '(org-glance-class org-glance-materialization nil ((view :type string) (location :type string))))

(org-glance-class org-glance-materialization nil
    ((view
      :type org-glance-view
      :initarg :view
      :reader org-glance-materialization:view
      :documentation "Backlink to source of materialization.")
     (location
      :type org-glance-file
      :initarg :location
      :reader org-glance-materialization:location
      :documentation "Location where materialization persists.")))

(cl-defun org-glance-materialization:header (materialization)
  "Generate header for MATERIALIZATION."
  (s-join "\n"
          (list "#  -*- mode: org; mode: org-glance-material -*-"
                ""
                (format "#+STORE: %s" (org-glance-> materialization :view :store :location))
                (format "#+PREDICATE: %s" (org-glance-> materialization :view :predicate))
                ""
                "")))

(cl-defun org-glance-materialization:get-property (property &optional (mapper #'identity))
  (funcall mapper (save-excursion
                    (goto-char (point-min))
                    (search-forward (format "#+%s: " property))
                    (buffer-substring-no-properties (point) (line-end-position)))))

(cl-defun org-glance-materialization:get-buffer-store ()
  "Get `org-glance-store' instance associated with current buffer."
  (let ((location (org-glance-materialization:get-property "STORE")))
    (org-glance-store:read location)))

(cl-defun org-glance-materialization:get-buffer-view ()
  "Get `org-glance-view' instance associated with current buffer."
  (let ((store (org-glance-materialization:get-buffer-store))
        (predicate (org-glance-materialization:get-property "PREDICATE" #'intern)))
    (org-glance-store:view store predicate)))

(cl-defun org-glance-materialization:get-buffer-materialization ()
  "Get `org-glance-materialization' instance associated with current buffer."
  (let ((view (org-glance-materialization:get-buffer-view)))
    (org-glance-view:materialization view (buffer-file-name))))

(provide 'org-glance-materialization)
