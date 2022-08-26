;; -*- lexical-binding: t; -*-

(require 'eieio)
(require 'org-glance-helpers)
(require 'org-glance-headline)
(require 'org-glance-marker)

(defalias 'org-glance-buffer-materialisation 'org-glance-materialisation:get-buffer-materialisation)

(defvar org-glance-materialisations (make-hash-table :test #'equal))

(org-glance-class org-glance-materialisation nil
    ((view
      :type org-glance-view
      :initarg :view
      :documentation "Backlink to source of materialisation.")
     (location
      :type org-glance-file
      :initarg :location
      :documentation "Location where materialisation persists.")
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

(cl-defun org-glance-materialisation:header (materialisation)
  "Generate header for MATERIALISATION."
  (s-join "\n"
          (list "#  -*- mode: org; mode: org-glance-material -*-"
                ""
                (format "#+TYPE: %s :: %s"
                        (org-glance-> materialisation :view :store :location)
                        (org-glance-> materialisation :view :type))
                (format "#+OFFSET: %s"
                        (org-glance-> materialisation :offset))
                ""
                "")))

(cl-defun org-glance-materialisation:get-property (property)
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
        (progn
          (search-forward (format "#+%s: " property))
          (buffer-substring-no-properties (point) (line-end-position)))
      (search-failed nil))))

(cl-defun org-glance-materialisation:set-property (property value)
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
        (progn
          (search-forward (format "#+%s: " property))
          (delete-region (point) (line-end-position))
          (insert (prin1-to-string value)))
      (search-failed nil))))

(cl-defun org-glance-materialisation:get-buffer-store ()
  "Get `org-glance-store' associated with current buffer."
  (thread-last (org-glance-materialisation:get-property "TYPE")
    (s-split " :: ")
    cl-first
    org-glance-store:read))

(cl-defun org-glance-materialisation:get-buffer-view ()
  "Get `org-glance-view' associated with current buffer."
  (let* ((store (org-glance-materialisation:get-buffer-store))
         (type (thread-last (org-glance-materialisation:get-property "TYPE")
                 (s-split " :: ")
                 cl-second)))
    (org-glance-store:view store type)))

(cl-defun org-glance-materialisation:get-buffer-materialisation ()
  "Get `org-glance-materialisation' instance associated with current buffer."
  (let ((filename (file-truename (buffer-file-name))))
    (or (gethash filename org-glance-materialisations)
        (let ((view (org-glance-materialisation:get-buffer-view)))
          (puthash filename (org-glance-view:materialise view filename) org-glance-materialisations)))))

(cl-defmacro org-glance-materialisation:pop-changes (spec &rest forms)
  "Pop changed markers one by one from current buffer binding each marker to VAL and executing BODY.

\(fn (VAR MATERIALISATION) BODY...)"
  ;; TODO lock, possible data loss
  (declare (indent 1) (debug ((symbolp form &optional form) forms)))
  (unless (consp spec) (signal 'wrong-type-argument (list 'consp spec)))
  (unless (= 2 (length spec)) (signal 'wrong-number-of-arguments (list '(2 . 2) (length spec))))
  `(cl-loop
      with materialisation = ,(cadr spec)
      while (org-glance-> materialisation :changes)
      for marker = (pop (org-glance-> materialisation :changes))
      when (org-glance-marker:live-p marker)
      collect (org-glance-marker:with-current-buffer marker
                (let ((,(car spec) marker))
                  ,@forms))))

(cl-defun org-glance-materialisation:commit (materialisation)
  (let ((store (org-glance-> materialisation :view :store)))
    (org-glance-materialisation:pop-changes (marker materialisation)
      (let ((headline (org-glance-marker:headline marker)))
        (org-glance-store:put store headline)
        (org-glance:append-to-file
         (format "%s %s" (org-glance-> marker :hash) (org-glance-headline:hash headline))
         (org-glance-store:/ store "CHANGELOG"))
        (setf (org-glance-> marker :state :committed) t
              (org-glance-> marker :state :changed) nil
              (org-glance-> marker :hash) (org-glance-headline:hash headline))
        (org-glance-marker:redisplay marker)))
    (let ((offset (org-glance-store:flush store)))
      (org-glance-materialisation:set-property "OFFSET" offset)
      (setf (org-glance-> materialisation :offset) offset))))

(provide 'org-glance-materialisation)
