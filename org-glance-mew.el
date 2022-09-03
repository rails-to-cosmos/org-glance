;; -*- lexical-binding: t; -*-

(require 'eieio)
(require 'org-macs)
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
      :initform (make-hash-table :test #'equal)
      :documentation "Hash to marker.")
     (changes
      :type list
      :initarg :changes
      :initform nil
      :documentation "List of changed markers.")
     (offset
      :type time
      :initarg :offset))
  "Materialized viEW.")

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
  (when (buffer-file-name)
    (let ((filename (file-truename (buffer-file-name))))
      (or (gethash filename org-glance-mews)
          (let ((view (org-glance-mew:get-buffer-view)))
            (puthash filename (org-glance-view:materialize view filename) org-glance-mews))))))

(cl-defmacro org-glance-mew:pop-changes (spec &rest forms)
  "Pop changed markers one by one from current buffer binding each marker to VAR and executing FORMS.

\(fn (VAR MEW) FORMS...)"
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

(cl-defun org-glance-mew:create-marker (mew hash)
  ;; assume we in headline buffer
  (let ((marker (org-glance-marker
                 :hash hash
                 :beg (point-min)
                 :end (point-max) ;; end of headline in narrowed buffer
                 :buffer (get-file-buffer (org-glance-> mew :location))
                 :state (org-glance-marker-state
                         ;; FIXME Getting full headline is unneccessary
                         :corrupted (null (org-glance-store:in (org-glance-> mew :view :store) hash))))))
    (puthash hash marker (org-glance-> mew :markers))
    (add-text-properties (point-min) (point-max) (list :marker marker))
    (org-glance-marker:redisplay marker)
    marker))

(cl-defun org-glance-mew:delete-marker (mew hash)
  (org-glance-mew:with-mew-buffer mew
    (when-let (marker (gethash hash (org-glance-> mew :markers)))
      (with-current-buffer (org-glance-> marker :buffer)
        (remove-text-properties (org-glance-> marker :beg)
                                (org-glance-> marker :end)
                                (list :marker marker)))
      (remhash hash (org-glance-> mew :markers)))))

(cl-defun org-glance-mew:update-headline (mew old-hash new-hash)
  (org-glance-mew:with-mew-buffer mew
    (pcase (gethash old-hash (org-glance-> mew :markers))
      ((and marker (guard (not (null marker)))) ;; marker exists, let's go and update headline
       ;; assume that all marker positions are consistent by material-mode hooks
       (save-excursion
         (goto-char (org-glance-> marker :beg))
         (org-glance-headline:with-headline-at-point
           (delete-region (point-min) (point-max))
           (org-glance-headline-insert (org-glance-store:get (org-glance-> mew :view :store) new-hash))
           (org-glance-mew:delete-marker mew old-hash)
           (org-glance-mew:create-marker mew new-hash))))
      (_ (error "Marker %s not found for UPDATE in buffer %s" old-hash (current-buffer))))))

(defun org-glance-fetch ()
  (interactive)
  (org-glance-mew:fetch (org-glance-mew:get-buffer-mew)))

(cl-defmacro org-glance-mew:with-mew-buffer (mew &rest forms)
  (declare (indent 1))
  `(save-match-data
     (let ((buffer (get-file-buffer (org-glance-> ,mew :location))))
       (when (and buffer (buffer-live-p buffer))
         (with-current-buffer buffer
           (save-excursion
             (save-restriction
               ,@forms)))))))

(cl-defun org-glance-mew:fetch (&optional (mew (org-glance-mew:get-buffer-mew)))
  (thunk-let* ((store (org-glance-> mew :view :store))
               (offset (org-glance-mew:get-offset mew))
               (events (--take-while
                        (time-less-p offset (org-glance-> it :offset))
                        (org-glance-store:events store))))
    (when (time-less-p offset (org-glance-store:offset store))
      (dolist (event events)
        (cl-typecase event
          (org-glance-event:UPDATE
           (org-glance-mew:update-headline mew (org-glance-> event :hash) (org-glance-> event :headline :hash))
           (org-glance-mew:set-offset mew (org-glance-> event :offset)))
          (otherwise (user-error "events PUT and DEL not implemented yet")))))))

(cl-defun org-glance-mew:get-offset (mew)
  (declare (indent 1))
  (let ((buffer-offset (org-glance-mew:with-mew-buffer mew
                         (time-convert (read (org-glance-mew:get-property "OFFSET")) 'list))))
    (unless (time-equal-p buffer-offset (org-glance-> mew :offset))
      ;; (warn "Buffer offset not matches mew offset")
      )
    buffer-offset))

(cl-defun org-glance-mew:set-offset (mew offset)
  (declare (indent 1))
  (org-glance-mew:with-mew-buffer mew
    (org-glance-mew:set-property "OFFSET" offset)
    (setf (org-glance-> mew :offset) offset)))

(cl-defun org-glance-mew:commit (&optional (mew (org-glance-buffer:mew)))
  (org-glance-mew:with-mew-buffer mew
    (let ((store (org-glance-> mew :view :store)))
      (org-glance-mew:fetch mew)  ;; mew should be up to date
      (org-glance-mew:pop-changes (marker mew)
        (let* ((headline (org-glance-marker:headline marker))
               (old-hash (org-glance-> marker :hash))
               (new-hash (org-glance-> headline :hash)))
          (org-glance-store:update store old-hash headline)
          (setf (org-glance-> marker :state :committed) t
                (org-glance-> marker :state :changed) nil
                (org-glance-> marker :hash) new-hash)
          (org-glance-marker:redisplay marker)))

      (org-glance-mew:set-offset mew (org-glance-store:flush store))

      ;; (dolist (another-mew (--filter (not (eq mew it)) (hash-table-values org-glance-mews)))
      ;;   (org-glance-mew:fetch another-mew))
      )))

(cl-defun org-glance-mew:normalize-marker (mew marker)
  (org-glance-headline:with-headline-at-point
    (let ((diff (- (point-max) (org-glance-> marker :end))))
      (org-glance-mew:move-markers mew (org-glance-> marker :end) diff))
    (setf (org-glance-> marker :beg) (point-min)
          (org-glance-> marker :end) (point-max))))

(cl-defun org-glance-mew:move-markers (mew beg diff)
  (when (/= 0 diff)
    (dolist (marker (--filter (>= (org-glance-> it :beg) beg)
                              (hash-table-values (org-glance-> mew :markers))))
      (cl-incf (org-glance-> marker :beg) diff)
      (cl-incf (org-glance-> marker :end) diff)
      (org-glance-marker:redisplay marker))))

(cl-defun org-glance-mew:mark-current-buffer (&optional (mew (org-glance-mew:get-buffer-mew)))
  (org-glance-headline:map (headline)
    (org-glance-mew:create-marker mew (org-glance-> headline :hash))
    ;; FIXME https://ftp.gnu.org/old-gnu/Manuals/elisp-manual-21-2.8/html_node/elisp_530.html
    (save-buffer)))

(provide 'org-glance-mew)
