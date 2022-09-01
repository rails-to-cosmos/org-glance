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
     (markers
      :type hash-table
      :initarg :markers
      :initform (make-hash-table :test #'equal))
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

(cl-defun org-glance-mew:create-marker (mew hash beg end buf)
  (let ((marker (org-glance-marker
                 :hash hash
                 :beg beg
                 :end end ;; end of headline in narrowed buffer
                 :buffer buf
                 :state (org-glance-marker-state
                         ;; FIXME Getting full headline is unneccessary
                         :corrupted (null (org-glance-store:in (org-glance-> mew :view :store) hash))))))
    (puthash hash marker (org-glance-> mew :markers))
    (with-current-buffer buf
      (add-text-properties beg end (list :marker marker))
      (org-glance-marker:redisplay marker))
    marker))

(cl-defun org-glance-mew:delete-marker (mew hash)
  (when-let (marker (gethash hash (org-glance-> mew :markers)))
    (with-current-buffer (org-glance-> marker :buffer)
      (remove-text-properties (org-glance-> marker :beg)
                              (org-glance-> marker :end)
                              (list :marker marker)))
    (remhash hash (org-glance-> mew :markers))))

(cl-defun org-glance-mew:update-headline (mew old-hash new-hash)
  (pcase (gethash old-hash (org-glance-> mew :markers))
    ((and marker (guard (not (null marker)))) ;; marker exists, let's go and update headline
     ;; assume that all marker positions are consistent by material-mode hooks
     (save-excursion
       (goto-char (org-glance-> marker :beg))
       (org-glance--with-headline-at-point
         (delete-region (point-min) (point-max))
         (org-glance-headline-insert (org-glance-store:get (org-glance-> mew :view :store) new-hash))
         (org-glance-mew:create-marker mew
                                       new-hash
                                       (point-min)
                                       (point-max)
                                       (current-buffer))
         (org-glance-mew:delete-marker mew old-hash))))
    (otherwise nil)))

(cl-defun org-glance-mew:fetch (mew)
  (let ((store (org-glance-> mew :view :store)))
    (unless (= (org-glance-store:offset store) (org-glance-> mew :offset))
      (dolist (event (--take-while (> (org-glance-> it :offset)
                                      (org-glance-> mew :offset))
                                   (org-glance-store:events store)))
        (cl-typecase event
          (org-glance-event:UPDATE
           (org-glance-mew:update-headline mew (org-glance-> event :hash) (org-glance-> event :headline :hash))
           (org-glance-mew:set-offset mew (org-glance-> event :offset)))
          (otherwise (user-error "Not implemented yet")))))))

(defun org-glance-fetch ()
  (interactive)
  (org-glance-mew:fetch (org-glance-mew:get-buffer-mew)))

(cl-defun org-glance-mew:set-offset (mew offset)
  (declare (indent 1))
  (org-glance-mew:set-property "OFFSET" offset)
  (setf (org-glance-> mew :offset) offset))

(cl-defun org-glance-mew:commit (mew)
  (let ((store (org-glance-> mew :view :store)))
    (unless (= (org-glance-store:offset store) (org-glance-> mew :offset))
      ;; merge new changes

      (when (y-or-n-p "New changes appeared. Fetch?")
        (org-glance-mew:fetch mew)))

    ;; TODO take lock
    (org-glance-mew:pop-changes (marker mew)
      (let* ((headline (org-glance-marker:headline marker))
             (old-hash (org-glance-> marker :hash))
             (new-hash (org-glance-> headline :hash)))
        (let ((offset (org-glance-store:update store old-hash headline)))
          (setf (org-glance-> marker :state :committed) t
                (org-glance-> marker :state :changed) nil
                (org-glance-> marker :offset) offset
                (org-glance-> marker :hash) new-hash)
          (org-glance-marker:redisplay marker))
        ;; (org-glance:append-to-file (format "%s %s %s"
        ;;                                    offset
        ;;                                    (org-glance-> marker :hash)
        ;;                                    (org-glance-> headline :hash))
        ;;                            (org-glance-store:/ store "CHANGELOG"))
        ))
    (org-glance-mew:set-offset mew
      (org-glance-store:flush store))))

(provide 'org-glance-mew)
