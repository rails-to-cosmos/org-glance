;; -*- lexical-binding: t; -*-

(require 'eieio)
(require 'org-glance-helpers)
(require 'org-glance-headline)
(require 'org-glance-marker)

(defalias 'org-glance-buffer-materialization 'org-glance-materialization:get-buffer-materialization)

(defvar org-glance-materializations (make-hash-table :test #'equal))

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
      :documentation "Location where materialization persists.")
     (changes
      :type list
      :initarg :changes
      :initform nil
      :reader org-glance-materialization:changes
      :documentation "Set of changed markers.")))

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
    (or (gethash (cons predicate store) org-glance-views)
        (puthash (cons predicate store) (org-glance-store:view store predicate) org-glance-views))))

(cl-defun org-glance-materialization:get-buffer-materialization ()
  "Get `org-glance-materialization' instance associated with current buffer."
  (let ((filename (file-truename (buffer-file-name))))
    (or (gethash filename org-glance-materializations)
        (let ((view (org-glance-materialization:get-buffer-view)))
          (puthash filename (org-glance-view:materialize view filename) org-glance-materializations)))))

(cl-defmacro org-glance-materialization:pop-changes (spec &rest forms)
  "Loop over changed markers in current buffer binding each marker to VAL and executing BODY.

\(fn (VAR MATERIALIZATION) BODY...)"
  ;; TODO lock, possible data loss
  (declare (indent 1) (debug ((symbolp form &optional form) forms)))
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (= 2 (length spec))
    (signal 'wrong-number-of-arguments (list '(2 . 2) (length spec))))
  `(cl-loop
      with materialization = ,(cadr spec)
      while (org-glance-> materialization :changes)
      for marker = (pop (org-glance-> materialization :changes))
      when (org-glance-marker:live-p marker)
      do (org-glance-marker:with-current-buffer marker
           (let ((,(car spec) marker))
             ,@forms))))

(cl-defun org-glance-materialization:commit (materialization)
  (let ((store (org-glance-> materialization :view :store)))
    (org-glance-materialization:pop-changes (marker materialization)
      (let ((headline (org-glance-marker:headline marker)))
        (org-glance-store:put store headline)
        (setf (org-glance-> marker :state :committed) t
              (org-glance-> marker :state :changed) nil
              (org-glance-> marker :hash) (org-glance-headline:hash headline))
        (org-glance-marker:redisplay marker)))
    (org-glance-store:flush store)))

(provide 'org-glance-materialization)
