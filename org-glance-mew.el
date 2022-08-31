;; -*- lexical-binding: t; -*-

(require 'eieio)
(require 'org-glance-helpers)
(require 'org-glance-headline)
(require 'org-glance-marker)

(defalias 'org-glance-buffer:mew 'org-glance-mew:get-buffer-mew)

(defvar org-glance-mews (make-hash-table :test #'equal))

(org-glance-class org-glance-mew nil
    ((view
      :type org-glance-view
      :initarg :view
      :documentation "Backlink to source of mew.")
     (location
      :type org-glance-file
      :initarg :location
      :documentation "Location where mew persists.")
     (changes
      :type list
      :initarg :changes
      :initform nil
      :documentation "List of currently changed markers.")
     (offset
      :type float
      :initarg :offset))
  "Materialised viEW.")

;; TODO implement material offsets
;; On commit check if our offset is latest
;;  Latest -- proceed, update offset and header
;;  Not latest -- rebase, last write wins or user diff
;; On material mode check if our offset is latest, update offset and header

(cl-defun org-glance-mew:header (mew)
  "Generate header for MEW."
  (s-join "\n"
          (list "#  -*- mode: org; mode: org-glance-material -*-"
                ""
                (format "#+TYPE: %s :: %s"
                        (org-glance-> mew :view :store :location)
                        (org-glance-> mew :view :type))
                (format "#+OFFSET: %s"
                        (org-glance-> mew :offset))
                ""
                "")))

(cl-defun org-glance-mew:get-property (property)
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
        (progn
          (search-forward (format "#+%s: " property))
          (buffer-substring-no-properties (point) (line-end-position)))
      (search-failed nil))))

(cl-defun org-glance-mew:set-property (property value)
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
        (progn
          (search-forward (format "#+%s: " property))
          (delete-region (point) (line-end-position))
          (insert (prin1-to-string value)))
      (search-failed nil))))

(cl-defun org-glance-mew:get-buffer-store ()
  "Get `org-glance-store' associated with current buffer."
  (thread-last (org-glance-mew:get-property "TYPE")
    (s-split " :: ")
    cl-first
    org-glance-store:read))

(cl-defun org-glance-mew:get-buffer-view ()
  "Get `org-glance-view' associated with current buffer."
  (let* ((store (org-glance-mew:get-buffer-store))
         (type (thread-last (org-glance-mew:get-property "TYPE")
                 (s-split " :: ")
                 cl-second)))
    (org-glance-store:view store type)))

(defalias 'org-glance-buffer:mew 'org-glance-mew:get-buffer-mew)

(cl-defun org-glance-mew:get-buffer-mew ()
  "Get `org-glance-mew' instance associated with current buffer."
  (let ((filename (file-truename (buffer-file-name))))
    (or (gethash filename org-glance-mews)
        (let ((view (org-glance-mew:get-buffer-view)))
          (puthash filename (org-glance-view:materialise view filename) org-glance-mews)))))

(cl-defmacro org-glance-mew:pop-changes (spec &rest forms)
  "Pop changed markers one by one from current buffer binding each marker to VAL and executing BODY.

\(fn (VAR MEW) BODY...)"
  ;; TODO lock, possible data loss
  (declare (indent 1) (debug ((symbolp form &optional form) forms)))
  (unless (consp spec) (signal 'wrong-type-argument (list 'consp spec)))
  (unless (= 2 (length spec)) (signal 'wrong-number-of-arguments (list '(2 . 2) (length spec))))
  `(cl-loop
      with mew = ,(cadr spec)
      while (org-glance-> mew :changes)
      for marker = (pop (org-glance-> mew :changes))
      when (org-glance-marker:live-p marker)
      collect (org-glance-marker:with-current-buffer marker
                (let ((,(car spec) marker))
                  ,@forms))))

(cl-defun org-glance-mew:fetch (mew)
  (let ((store (org-glance-> mew :view :store)))
    (unless (= (org-glance-store:offset store) (org-glance-> mew :offset))
      (let ((events (--take-while (> (org-glance-> it :offset) (org-glance-> mew :offset))
                                  (org-glance-store:events store))))
        ;; (pp events)
        ))))

(cl-defun org-glance-mew:commit (mew)
  (let ((store (org-glance-> mew :view :store)))
    (unless (= (org-glance-store:offset store) (org-glance-> mew :offset))
      ;; merge new changes

      ;; (when (y-or-n-p "Your changes could be reflected only if you pull changes from store. Proceed?")

      ;;   )
      (user-error "Halt")
      )
    ;; TODO take lock
    (org-glance-mew:pop-changes (marker mew)
      (let* ((headline (org-glance-marker:headline marker))
             (offset (org-glance-store:put store headline))
             (old-hash (org-glance-> marker :hash))
             (new-hash (org-glance-> headline :hash)))
        (org-glance-store:remove store old-hash)
        ;; (org-glance:append-to-file (format "%s %s %s"
        ;;                                    offset
        ;;                                    (org-glance-> marker :hash)
        ;;                                    (org-glance-> headline :hash))
        ;;                            (org-glance-store:/ store "CHANGELOG"))
        (setf (org-glance-> marker :state :committed) t
              (org-glance-> marker :state :changed) nil
              (org-glance-> marker :offset) offset
              (org-glance-> marker :hash) new-hash)
        (org-glance-marker:redisplay marker)))
    (let ((offset (org-glance-store:flush store)))
      (org-glance-mew:set-property "OFFSET" offset)
      (setf (org-glance-> mew :offset) offset))))

(provide 'org-glance-mew)
