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
      :documentation "Backlink to source of materialization.")
     (location
      :type org-glance-file
      :initarg :location
      :documentation "Location where materialization persists.")
     (changes
      :type list
      :initarg :changes
      :initform nil
      :documentation "Set of changed markers.")
     (offset
      :type float
      :initarg :offset)))

;; TODO implement material offsets
;; On commit check if our offset is latest
;;  Latest -- proceed, update offset and header
;;  Not latest -- rebase, last write wins or user diff
;; On material mode check if our offset is latest, update offset and header

(cl-defun org-glance-materialization:header (materialization)
  "Generate header for MATERIALIZATION."
  (s-join "\n"
          (list "#  -*- mode: org; mode: org-glance-material -*-"
                ""
                (format "#+TYPE: %s :: %s"
                        (org-glance-> materialization :view :store :location)
                        (org-glance-> materialization :view :type))
                (format "#+OFFSET: %s"
                        (org-glance-> materialization :offset))
                ""
                "")))

(cl-defun org-glance-materialization:get-property (property)
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
        (progn
          (search-forward (format "#+%s: " property))
          (buffer-substring-no-properties (point) (line-end-position)))
      (search-failed nil))))

(cl-defun org-glance-materialization:set-property (property value)
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
        (progn
          (search-forward (format "#+%s: " property))
          (delete-region (point) (line-end-position))
          (insert (prin1-to-string value)))
      (search-failed nil))))

(cl-defun org-glance-materialization:get-buffer-store ()
  "Get `org-glance-store' associated with current buffer."
  (thread-last (org-glance-materialization:get-property "TYPE")
    (s-split " :: ")
    cl-first
    org-glance-store:read))

(cl-defun org-glance-materialization:get-buffer-view ()
  "Get `org-glance-view' associated with current buffer."
  (let* ((store (org-glance-materialization:get-buffer-store))
         (type (thread-last (org-glance-materialization:get-property "TYPE")
                 (s-split " :: ")
                 cl-second)))
    (org-glance-store:view store type)))

(cl-defun org-glance-materialization:get-buffer-materialization ()
  "Get `org-glance-materialization' instance associated with current buffer."
  (let ((filename (file-truename (buffer-file-name))))
    (or (gethash filename org-glance-materializations)
        (let ((view (org-glance-materialization:get-buffer-view)))
          (puthash filename (org-glance-view:materialize view filename) org-glance-materializations)))))

(cl-defmacro org-glance-materialization:map-changes (spec &rest forms)
  "Pop changed markers one by one from current buffer binding each marker to VAL and executing BODY.

\(fn (VAR MATERIALIZATION) BODY...)"
  ;; TODO lock, possible data loss
  (declare (indent 1) (debug ((symbolp form &optional form) forms)))
  (unless (consp spec) (signal 'wrong-type-argument (list 'consp spec)))
  (unless (= 2 (length spec)) (signal 'wrong-number-of-arguments (list '(2 . 2) (length spec))))
  `(cl-loop
      with materialization = ,(cadr spec)
      while (org-glance-> materialization :changes)
      for marker = (pop (org-glance-> materialization :changes))
      when (org-glance-marker:live-p marker)
      collect (org-glance-marker:with-current-buffer marker
                (let ((,(car spec) marker))
                  ,@forms))))

(cl-defun org-glance-materialization:commit (materialization)
  (let ((store (org-glance-> materialization :view :store)))
    (org-glance-materialization:map-changes (marker materialization)
      (let ((headline (org-glance-marker:headline marker)))
        (org-glance-store:put store headline)
        (setf (org-glance-> marker :state :committed) t
              (org-glance-> marker :state :changed) nil
              (org-glance-> marker :hash) (org-glance-headline:hash headline))
        (org-glance-marker:redisplay marker)))
    (let ((offset (org-glance-store:flush store)))
      (org-glance-materialization:set-property "OFFSET" offset))))

(provide 'org-glance-materialization)