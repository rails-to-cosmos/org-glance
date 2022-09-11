;; -*- lexical-binding: t; -*-

(require 'eieio)
(require 'org-macs)
(require 'org-glance-helpers)
(require 'org-glance-headline)

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
     (offset
      :type time
      :initarg :offset)
     (hash->midx
      :type hash-table
      :initarg :hash->midx
      :initform (make-hash-table :test #'equal)
      :documentation "Hash to idx.")
     (changed-markers
      :type bool-vector
      :initarg :changed-markers)
     (committed-markers
      :type bool-vector
      :initarg :committed-markers)
     (corrupted-markers
      :type bool-vector
      :initarg :corrupted-markers)
     (marker-positions
      :type vector
      :initarg :marker-positions)
     (marker-overlays
      :type vector
      :initarg :marker-overlays)
     (marker-hashes
      :type vector
      :initarg :marker-hashes))
  "Materialized viEW.")

;; TODO implement material offsets
;; On commit check if our offset is latest
;;  Latest -- proceed, update offset and header
;;  Not latest -- rebase, last write wins or user diff
;; On material mode check if our offset is latest, update offset and header

(cl-defmacro org-glance-mew:if-safe-marker (mew midx then &rest else)
  (declare (indent 3))
  `(if (< -1 ,midx (length (org-glance-> ,mew :marker-positions)))
       ,then
     ,@else))

(cl-defun org-glance-mew:get-marker-position (mew midx)
  (org-glance-mew:if-safe-marker mew midx
      (aref (org-glance-> mew :marker-positions) midx)
    (point-max)))

(cl-defun org-glance-mew:set-marker-position (mew midx val)
  (org-glance-mew:if-safe-marker mew midx
      (aset (org-glance-> mew :marker-positions) midx val)))

(cl-defun org-glance-mew:set-marker-hash (mew midx val)
  (org-glance-mew:if-safe-marker mew midx
      (progn
        (remhash (org-glance-mew:get-marker-hash mew midx) (org-glance-> mew :hash->midx))
        (puthash val midx  (org-glance-> mew :hash->midx))
        (aset (org-glance-> mew :marker-hashes) midx val))))

(cl-defun org-glance-mew:get-marker-hash (mew midx)
  (org-glance-mew:if-safe-marker mew midx
      (aref (org-glance-> mew :marker-hashes) midx)))

(cl-defun org-glance-mew:get-marker-overlay (mew midx)
  (org-glance-mew:if-safe-marker mew midx
      (aref (org-glance-> mew :marker-overlays) midx)))

(cl-defun org-glance-mew:set-marker-overlay (mew midx val)
  (org-glance-mew:if-safe-marker mew midx
      (aset (org-glance-> mew :marker-overlays) midx val)))

(cl-defun org-glance-mew:highlight-marker (mew midx color)
  (org-glance-mew:if-safe-marker mew midx
      (progn
        (when (org-glance-mew:get-marker-overlay mew midx)
          (org-glance-mew:delete-marker-overlay mew midx))
        (let ((overlay (make-overlay (org-glance-mew:get-marker-position mew midx)
                                     (1+ (org-glance-mew:get-marker-position mew midx)))))
          (overlay-put overlay 'face `(:foreground ,color))
          (org-glance-mew:set-marker-overlay mew midx overlay)))))

(cl-defun org-glance-mew:delete-marker-overlay (mew midx)
  (org-glance-mew:if-safe-marker mew midx
      (progn
        (delete-overlay (org-glance-mew:get-marker-overlay mew midx))
        (org-glance-mew:set-marker-overlay mew midx nil))))

(cl-defun org-glance-mew:marker-changed-p (mew midx)
  (org-glance-mew:if-safe-marker mew midx
      (aref (org-glance-> mew :changed-markers) midx)))

(cl-defun org-glance-mew:set-marker-changed (mew midx val)
  (org-glance-mew:if-safe-marker mew midx
      (aset (org-glance-> mew :changed-markers) midx val)))

(cl-defun org-glance-mew:marker-corrupted-p (mew midx)
  (org-glance-mew:if-safe-marker mew midx
      (aref (org-glance-> mew :corrupted-markers) midx)))

(cl-defun org-glance-mew:set-marker-corrupted (mew midx val)
  (org-glance-mew:if-safe-marker mew midx
      (aset (org-glance-> mew :corrupted-markers) midx val)))

(cl-defun org-glance-mew:marker-committed-p (mew midx)
  (org-glance-mew:if-safe-marker mew midx
      (aref (org-glance-> mew :committed-markers) midx)))

(cl-defun org-glance-mew:set-marker-committed (mew midx val)
  (org-glance-mew:if-safe-marker mew midx
      (aset (org-glance-> mew :committed-markers) midx val)))

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
          (re-search-forward (format "^\\#\\+%s: " property))
          (buffer-substring-no-properties (point) (line-end-position)))
      (search-failed nil))))

(cl-defun org-glance-mew:set-property (property value)
  (org-glance-message "* Change property %s from %s to %s" property (org-glance-mew:get-property property) value)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (condition-case nil
          (progn
            (re-search-forward (format "^\\#\\+%s: " property))
            (org-glance-message "  Delete region: \"%s\"" (buffer-substring-no-properties (point) (line-end-position)))
            (org-glance-message "  Delete region from %d to %d" (point) (line-end-position))
            (org-glance-message "  Insert \"%s\"" (prin1-to-string value))
            (org-glance-message "")
            (delete-region (point) (line-end-position))
            (insert (prin1-to-string value)))
        (search-failed nil)))))

(cl-defun org-glance-mew:get-buffer-store ()
  "Get `org-glance-store' associated with current buffer."
  (-some->> (org-glance-mew:get-property "TYPE")
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

(cl-defun org-glance-mew:current ()
  "Get `org-glance-mew' instance associated with current buffer."
  (when (buffer-file-name)
    (let ((filename (file-truename (buffer-file-name))))
      (or (gethash filename org-glance-mews)
          (let ((view (org-glance-mew:get-buffer-view)))
            (puthash filename (org-glance-view:materialize view filename) org-glance-mews))))))

(cl-defmacro org-glance-mew:consume-changes (spec &rest forms)
  "Pop changed markers one by one from current buffer binding each marker to VAR and executing FORMS.

\(fn (VAR MEW) FORMS...)"
  ;; TODO lock, possible data loss
  (declare (indent 1) (debug ((symbolp form &optional form) forms)))
  (unless (consp spec) (signal 'wrong-type-argument (list 'consp spec)))
  (unless (= 2 (length spec)) (signal 'wrong-number-of-arguments (list '(2 . 2) (length spec))))
  (let ((mew (car spec))
        (midx-var-name (cadr spec)))
    `(org-glance-mew:with-current-buffer ,mew
       (cl-loop
          for change across-ref (org-glance-> ,mew :changed-markers)
          for midx from 0
          when change
          collect (unwind-protect
                       (save-excursion
                         (goto-char (org-glance-mew:get-marker-position ,mew midx))
                         (let ((,midx-var-name midx))
                           ,@forms))
                    (org-glance-mew:set-marker-changed ,mew midx nil))))))

(cl-defun org-glance-mew:replace-headline (mew old-hash new-hash)
  (declare (indent 0))
  (org-glance-mew:with-current-buffer mew
    (thunk-let* ((midx (gethash old-hash (org-glance-> mew :hash->midx)))
                 (marker-position (org-glance-mew:get-marker-position mew midx))
                 (new-headline (org-glance-store:get (org-glance-> mew :view :store) new-hash)))
      (when midx
        (goto-char marker-position)
        (org-glance-headline:with-headline-at-point
          (org-edit-headline (org-glance-> new-headline :title))
          (org-todo (org-glance-> new-headline :state))
          (org-set-tags (org-glance-> new-headline :class))
          (delete-region (save-excursion
                           (goto-char (point-min))
                           (forward-line)
                           (point))
                         (point-max))
          (insert (with-temp-buffer
                    (insert (org-glance-> new-headline :contents))
                    (goto-char (point-min))
                    (forward-line)
                    (buffer-substring-no-properties (point) (point-max))))
          (goto-char (point-min))
          (org-glance-mew:set-marker-hash mew midx new-hash))))))

(cl-defmacro org-glance-mew:with-current-buffer (mew &rest forms)
  (declare (indent 1))
  `(save-match-data
     (let ((buffer (get-file-buffer (org-glance-> ,mew :location))))
       (when (and buffer (buffer-live-p buffer))
         (with-current-buffer buffer
           (save-excursion
             (save-restriction
               ,@forms)))))))

(cl-defun org-glance-mew:mark (&optional (mew (org-glance-mew:current)))
  (org-glance-mew:with-current-buffer mew
    (cl-loop
       with store = (org-glance-> mew :view :store)
       with headlines = (org-glance-headline:map (headline) (list (point-min) (org-glance-> headline :hash)))
       with marker-positions = (make-vector (length headlines) 0)
       with marker-hashes = (make-vector (length headlines) nil)
       with corrupted-markers = (make-bool-vector (length headlines) nil)
       with markers = (make-hash-table :test #'equal)
       for (pos hash) in headlines
       for midx from 0
       do
         (puthash hash midx markers)
         (aset marker-hashes midx hash)
         (aset marker-positions midx pos)
         (aset corrupted-markers midx (null (org-glance-store:in store hash)))
       finally do
         (setf (org-glance-> mew :marker-hashes) marker-hashes
               (org-glance-> mew :marker-positions) marker-positions
               (org-glance-> mew :marker-overlays) (make-vector (length headlines) nil)
               (org-glance-> mew :changed-markers) (make-bool-vector (length headlines) nil)
               (org-glance-> mew :committed-markers) (make-bool-vector (length headlines) nil)
               (org-glance-> mew :corrupted-markers) corrupted-markers
               (org-glance-> mew :hash->midx) markers))))

(cl-defun org-glance-mew:fetch (&optional (mew (org-glance-mew:current)))
  (org-glance-mew:with-current-buffer mew
    (thunk-let* ((store (org-glance-> mew :view :store))
                 (offset (org-glance-mew:get-offset mew))
                 (events (--take-while
                          (time-less-p offset (org-glance-> it :offset))
                          (org-glance-store:events store))))
      (when (time-less-p offset (org-glance-store:offset store))
        (dolist (event events)
          (cl-typecase event
            (org-glance-event:UPDATE
             (org-glance-mew:replace-headline
               mew
               (org-glance-> event :hash)
               (org-glance-> event :headline :hash)))
            (otherwise (user-error "events PUT and DEL not implemented yet"))))
        (when events
          (org-glance-mew:set-offset mew (org-glance-> (car (last events)) :offset)))))))

(cl-defun org-glance-mew:commit (&optional (mew (org-glance-mew:current)))
  (org-glance-mew:with-current-buffer mew
    (let ((store (org-glance-> mew :view :store)))
      (org-glance-mew:fetch mew)

      (org-glance-mew:consume-changes (mew midx)
        (thunk-let* ((headline (save-excursion
                                 (goto-char (org-glance-mew:get-marker-position mew midx))
                                 (org-glance-headline-at-point)))
                     (old-hash (org-glance-mew:get-marker-hash mew midx))
                     (new-hash (org-glance-> headline :hash)))
          (org-glance-store:update store old-hash headline)
          (org-glance-mew:set-marker-committed mew midx t)
          (org-glance-mew:set-marker-changed mew midx nil)
          (org-glance-mew:set-marker-hash mew midx new-hash)))

      (let ((offset (org-glance-store:flush store)))
        (org-glance-mew:set-offset mew offset))

      (dolist (mew (--filter (not (eq mew it)) (hash-table-values org-glance-mews)))
        (org-glance-mew:fetch mew)))))

(cl-defun org-glance-mew:get-offset (mew)
  (declare (indent 1))
  (let ((buffer-offset (org-glance-mew:with-current-buffer mew
                         (time-convert (read (org-glance-mew:get-property "OFFSET")) 'list))))
    (unless (time-equal-p buffer-offset (org-glance-> mew :offset))
      ;; (warn "Buffer offset not matches mew offset")
      )
    buffer-offset))

(cl-defun org-glance-mew:set-offset (mew offset)
  (declare (indent 1))
  (org-glance-mew:with-current-buffer mew
    (org-glance-mew:set-property "OFFSET" offset)
    (setf (org-glance-> mew :offset) offset)))

(cl-defun org-glance-mew:marker-at-point
    (&optional
       (mew (org-glance-mew:current))
       (point (point)))
  (org-glance:binary-search (org-glance-> mew :marker-positions) point))

;; (cl-defun org-glance-mew:update-overlay (mew midx)
;;   "Refresh MARKER overlay."
;;   (org-glance-message "Marker index to change: %d" midx)
;;   (thunk-let ((marked (not (null (org-glance-mew:get-marker-overlay mew midx))))
;;               (changed (org-glance-mew:marker-changed-p mew midx))
;;               (committed (org-glance-mew:marker-committed-p mew midx))
;;               (corrupted (org-glance-mew:marker-corrupted-p mew midx)))
;;     (cond ((and changed (not marked))
;;            (org-glance-mew:highlight-marker mew midx "#ffcc00"))
;;           ((and changed committed marked)
;;            (org-glance-mew:set-marker-committed mew midx nil)
;;            (org-glance-mew:highlight-marker mew midx "#ffcc00"))
;;           ((and (not changed) (not committed) (not corrupted) marked)
;;            (org-glance-mew:delete-marker-overlay mew midx))
;;           ((and (not changed) marked committed)
;;            (org-glance-mew:highlight-marker mew midx "#27ae60"))
;;           ((and corrupted (not marked))
;;            (org-glance-mew:highlight-marker mew midx "#e74c3c"))
;;           ;; (t
;;           ;;  (org-glance-mew:highlight-marker mew midx "#749AF7"))
;;           )))

(cl-defun org-glance-mew:shift-markers (mew midx diff)
  (cl-loop for i from (1+ midx) below (length (org-glance-> mew :marker-positions))
     do (org-glance-mew:set-marker-position mew i (+ (org-glance-mew:get-marker-position mew i) diff))))

(cl-defun org-glance-mew:consistent-p ()
  (save-match-data
    (--all-p (eq it t)
             (org-glance-headline:map (headline)
               (thunk-let* ((mew (org-glance-mew:current))
                            (midx (org-glance-mew:marker-at-point mew (point-min))))
                 (and (> midx -1)
                      (= (org-glance-mew:get-marker-position mew midx) (point-min))))))))


(provide 'org-glance-mew)
