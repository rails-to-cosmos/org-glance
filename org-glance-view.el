;; -*- lexical-binding: t; -*-

(require 'eieio)
(require 'f)
(require 'org-macs)

(require 'org-glance-debug)
(require 'org-glance-types)
(require 'org-glance-headline)
(require 'org-glance-helpers)
(require 'org-glance-offset)

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
      :documentation "String declaration that transforms into predicate of
      one argument: `org-glance-headline'. View is guaranteed to
      contain only headlines for which predicate returns non-nil
      value.")
     (location
      :type org-glance-file
      :initarg :location
      :documentation "Location where view persists.")
     (template
      :type string
      :initarg :template
      :initform "* %?"
      :documentation "Capture template for current view.")
     (offset
      :type org-glance-offset
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
      :initarg :marker-hashes)))

(org-glance:declare-type org-glance-view:create : org-glance-store string string org-glance-view)
(cl-defun org-glance-view:create (store type location)
  (thunk-let* ((views (org-glance-> store :views))
               (location (file-truename location))
               (key (list type location))
               (view-exists? (and (f-exists? location) (f-file? location) views))
               (location-exists? (f-exists? (file-name-directory location)))
               (view-cache (gethash key views))
               (headlines (org-glance-store:headlines store))
               (view (org-glance-view :store store
                                      :type type
                                      :location location
                                      :offset (org-glance-store:offset store)))
               (header (org-glance-view:header view)))
    (cond (view-exists? view-cache)
          (t (unless location-exists?
               (f-mkdir-full-path (file-name-directory location)))
             (org-glance--with-temp-file location
               (insert header)
               (cl-dolist (headline headlines)
                 (when (org-glance-view:filter view headline)
                   (org-glance-headline:insert
                    (org-glance-store:get store (org-glance-> headline :hash))))))
             (puthash key view (org-glance-> store :views))))))

(org-glance:declare-type org-glance-store:filter : org-glance-view org-glance-headline boolean)
(cl-defun org-glance-view:filter (view headline)
  "Decide if HEADLINE should be a part of VIEW."
  (member (downcase (org-glance-> view :type)) (org-glance-> headline :class)))

(cl-defmacro org-glance-view:if-safe-marker (view midx then &rest else)
  (declare (indent 3))
  `(if (< -1 ,midx (length (org-glance-> ,view :marker-positions)))
       ,then
     ,@else))

(cl-defun org-glance-view:get-marker-position (view midx)
  (org-glance-view:if-safe-marker view midx
      (aref (org-glance-> view :marker-positions) midx)
    (point-max)))

(cl-defun org-glance-view:set-marker-position (view midx val)
  (org-glance-view:if-safe-marker view midx
      (aset (org-glance-> view :marker-positions) midx val)))

(cl-defun org-glance-view:set-marker-hash (view midx val)
  (org-glance-view:if-safe-marker view midx
      (progn
        (remhash (org-glance-view:get-marker-hash view midx) (org-glance-> view :hash->midx))
        (puthash val midx  (org-glance-> view :hash->midx))
        (aset (org-glance-> view :marker-hashes) midx val))))

(cl-defun org-glance-view:get-marker-hash (view midx)
  (org-glance-view:if-safe-marker view midx
      (aref (org-glance-> view :marker-hashes) midx)))

(cl-defun org-glance-view:get-marker-overlay (view midx)
  (org-glance-view:if-safe-marker view midx
      (aref (org-glance-> view :marker-overlays) midx)))

(cl-defun org-glance-view:set-marker-overlay (view midx val)
  (org-glance-view:if-safe-marker view midx
      (aset (org-glance-> view :marker-overlays) midx val)))

(cl-defun org-glance-view:highlight-marker (view midx color)
  (org-glance-view:if-safe-marker view midx
      (progn
        (when (org-glance-view:get-marker-overlay view midx)
          (org-glance-view:delete-marker-overlay view midx))
        (let ((overlay (make-overlay (org-glance-view:get-marker-position view midx)
                                     (1+ (org-glance-view:get-marker-position view midx)))))
          (overlay-put overlay 'face `(:foreground ,color))
          (org-glance-view:set-marker-overlay view midx overlay)))))

(cl-defun org-glance-view:delete-marker-overlay (view midx)
  (org-glance-view:if-safe-marker view midx
      (progn
        (delete-overlay (org-glance-view:get-marker-overlay view midx))
        (org-glance-view:set-marker-overlay view midx nil))))

(cl-defun org-glance-view:marker-changed-p (view midx)
  (org-glance-view:if-safe-marker view midx
      (aref (org-glance-> view :changed-markers) midx)))

(cl-defun org-glance-view:set-marker-changed (view midx val)
  (org-glance-view:if-safe-marker view midx
      (aset (org-glance-> view :changed-markers) midx val)))

(cl-defun org-glance-view:marker-corrupted-p (view midx)
  (org-glance-view:if-safe-marker view midx
      (aref (org-glance-> view :corrupted-markers) midx)))

(cl-defun org-glance-view:set-marker-corrupted (view midx val)
  (org-glance-view:if-safe-marker view midx
      (aset (org-glance-> view :corrupted-markers) midx val)))

(cl-defun org-glance-view:marker-committed-p (view midx)
  (org-glance-view:if-safe-marker view midx
      (aref (org-glance-> view :committed-markers) midx)))

(cl-defun org-glance-view:set-marker-committed (view midx val)
  (org-glance-view:if-safe-marker view midx
      (aset (org-glance-> view :committed-markers) midx val)))

(cl-defun org-glance-view:header (view)
  "Generate header for VIEW."
  (s-join "\n"
          (list "#  -*- mode: org; mode: org-glance-material -*-"
                ""
                "#+STARTUP: overview"
                (format "#+TYPE: %s :: %s"
                        (org-glance-> view :store :location)
                        (org-glance-> view :type))
                (format "#+OFFSET: %s"
                        (org-glance-> view :offset))
                ""
                "")))

(cl-defun org-glance-view:get-property (property)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (condition-case nil
          (save-match-data
            (re-search-forward (format "^\\#\\+%s: " property))
            (buffer-substring-no-properties (point) (line-end-position)))
        (search-failed nil)))))

(cl-defun org-glance-view:set-property (property value)
  (org-glance-debug "* Change property %s from %s to %s" property (org-glance-view:get-property property) value)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (condition-case nil
          (progn
            (re-search-forward (format "^\\#\\+%s: " property))
            (org-glance-debug "  Delete region: \"%s\"" (buffer-substring-no-properties (point) (line-end-position)))
            (org-glance-debug "  Delete region from %d to %d" (point) (line-end-position))
            (org-glance-debug "  Insert \"%s\"" (prin1-to-string value))
            (org-glance-debug "")
            (delete-region (point) (line-end-position))
            (insert (prin1-to-string value)))
        (search-failed nil)))))

(cl-defun org-glance-view:get-buffer-store ()
  "Get `org-glance-store' associated with current buffer."
  (-some->> (org-glance-view:get-property "TYPE")
    (s-split " :: ")
    cl-first
    org-glance-store:read))

(cl-defun org-glance-view:get-buffer-type ()
  "Get `org-glance-store' associated with current buffer."
  (or (-some->> (org-glance-view:get-property "TYPE")
        (s-split " :: ")
        cl-second)
      "_"))

(cl-defun org-glance-view:get-buffer-view ()
  "Get `org-glance-view' associated with current buffer."
  (let ((store (org-glance-view:get-buffer-store))
        (type (org-glance-view:get-buffer-type)))
    (unless store
      (user-error "Unable to get store from buffer %s" (current-buffer)))
    (org-glance-view:create store type (buffer-file-name))))

(cl-defmacro org-glance-view:consume-changes (spec &rest forms)
  "Pop changed markers one by one from current buffer binding each marker to VAR and executing FORMS.

\(fn (VAR VIEW) FORMS...)"
  ;; TODO lock, possible data loss
  (declare (indent 1) (debug ((symbolp form &optional form) forms)))
  (unless (consp spec) (signal 'wrong-type-argument (list 'consp spec)))
  (unless (= 2 (length spec)) (signal 'wrong-number-of-arguments (list '(2 . 2) (length spec))))
  (let ((view (car spec))
        (midx-var-name (cadr spec)))
    `(org-glance-view:with-current-buffer ,view
       (cl-loop
          for change across-ref (org-glance-> ,view :changed-markers)
          for midx from 0
          when change
          collect (unwind-protect
                       (save-excursion
                         (goto-char (org-glance-view:get-marker-position ,view midx))
                         (let ((,midx-var-name midx))
                           ,@forms))
                    (org-glance-view:set-marker-changed ,view midx nil))))))

(cl-defun org-glance-view:replace-headline (view old-hash new-hash)
  (declare (indent 0))
  (unless (string= old-hash new-hash)
    (org-glance-view:with-current-buffer view
      (when-let (midx (gethash old-hash (org-glance-> view :hash->midx)))
        (let ((marker-position (org-glance-view:get-marker-position view midx))
              (new-headline (org-glance-store:get (org-glance-> view :store) new-hash)))
          (goto-char marker-position)
          (org-glance-headline:with-headline-at-point
            (let ((inhibit-message t))
              (org-edit-headline (org-glance-> new-headline :title))
              (org-todo (org-glance-> new-headline :state))
              (when (org-glance-> new-headline :commented-p)
                (org-toggle-comment))
              (org-set-tags (org-glance-> new-headline :class)))

            (goto-char (point-min))
            (when (= 0 (forward-line))
              (delete-region (point) (point-max)))

            (goto-char (point-max))

            (insert (with-temp-buffer
                      (insert (org-glance-> new-headline :contents))
                      (goto-char (point-min))
                      (forward-line)
                      (buffer-substring-no-properties (point) (point-max))))

            (unless (string= (buffer-substring-no-properties (1- (point-max)) (point-max)) "\n")
              (insert "\n"))

            (org-glance-view:set-marker-hash view midx new-hash)))))))

(cl-defmacro org-glance-view:with-current-buffer (view &rest forms)
  (declare (indent 1))
  `(save-match-data
     (let ((buffer (get-file-buffer (org-glance-> ,view :location))))
       (when (and buffer (buffer-live-p buffer))
         (with-current-buffer buffer
           (save-excursion
             (save-restriction
               ,@forms)))))))

(cl-defun org-glance-view:mark (&optional (view (org-glance-view:get-buffer-view)))
  (org-glance-view:with-current-buffer view
    (cl-loop
       with store = (org-glance-> view :store)
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
         (setf (org-glance-> view :marker-hashes) marker-hashes
               (org-glance-> view :marker-positions) marker-positions
               (org-glance-> view :marker-overlays) (make-vector (length headlines) nil)
               (org-glance-> view :changed-markers) (make-bool-vector (length headlines) nil)
               (org-glance-> view :committed-markers) (make-bool-vector (length headlines) nil)
               (org-glance-> view :corrupted-markers) corrupted-markers
               (org-glance-> view :hash->midx) markers))))

(cl-defun org-glance-view:commit (&optional (view (org-glance-view:get-buffer-view)))
  (org-glance-view:with-current-buffer view
    (let ((store (org-glance-> view :store)))
      (org-glance-view:fetch view)

      (org-glance-view:consume-changes (view midx)
        (let* ((headline (save-excursion
                           (goto-char (org-glance-view:get-marker-position view midx))
                           (org-glance-headline-at-point)))
               (old-hash (org-glance-view:get-marker-hash view midx))
               (new-hash (org-glance-> headline :hash)))
          (org-glance-store:update store old-hash headline)
          (org-glance-view:set-marker-committed view midx t)
          (org-glance-view:set-marker-changed view midx nil)
          (org-glance-view:set-marker-hash view midx new-hash)))

      (let ((offset (org-glance-store:flush store)))
        (org-glance-view:set-offset view offset))

      (dolist (it (hash-table-values (org-glance-> store :views)))
        (when (not (eq view it))
          (org-glance-view:fetch it))))))

(cl-defun org-glance-view:fetch (&optional (view (org-glance-view:get-buffer-view)))
  (org-glance-view:with-current-buffer view
    (thunk-let* ((store (org-glance-> view :store))
                 (view-offset (org-glance-view:offset view))
                 (store-offset (org-glance-store:offset store))
                 (events (--take-while
                          (org-glance-offset:less-p view-offset (org-glance-> it :offset))
                          (org-glance-store:events store))))
      (when (org-glance-offset:less-p view-offset store-offset)
        (dolist (event events)
          (cl-typecase event
            (org-glance-event:UPDATE
             (org-glance-view:replace-headline view
                                               (org-glance-> event :hash)
                                               (org-glance-> event :headline :hash)))
            (otherwise (user-error "events PUT and DEL not implemented yet"))))
        (org-glance-view:set-offset view store-offset)))))

(cl-defun org-glance-view:offset (view)
  (declare (indent 1))
  (let ((buffer-offset (org-glance-view:with-current-buffer view
                         (org-glance-offset:read (org-glance-view:get-property "OFFSET"))))
        (memory-offset (org-glance-> view :offset)))
    (-max-by #'org-glance-offset:less-p (list buffer-offset memory-offset))))

(cl-defun org-glance-view:set-offset (view offset)
  (declare (indent 1))
  (org-glance-view:with-current-buffer view
    (org-glance-view:set-property "OFFSET" offset)
    (setf (org-glance-> view :offset) offset)))

(cl-defun org-glance-view:marker-at-point
    (&optional
       (view (org-glance-view:get-buffer-view))
       (point (point)))
  (org-glance:binary-search (org-glance-> view :marker-positions) point))

;; (cl-defun org-glance-view:update-overlay (view midx)
;;   "Refresh MARKER overlay."
;;   (org-glance-debug "Marker index to change: %d" midx)
;;   (thunk-let ((marked (not (null (org-glance-view:get-marker-overlay view midx))))
;;               (changed (org-glance-view:marker-changed-p view midx))
;;               (committed (org-glance-view:marker-committed-p view midx))
;;               (corrupted (org-glance-view:marker-corrupted-p view midx)))
;;     (cond ((and changed (not marked))
;;            (org-glance-view:highlight-marker view midx "#ffcc00"))
;;           ((and changed committed marked)
;;            (org-glance-view:set-marker-committed view midx nil)
;;            (org-glance-view:highlight-marker view midx "#ffcc00"))
;;           ((and (not changed) (not committed) (not corrupted) marked)
;;            (org-glance-view:delete-marker-overlay view midx))
;;           ((and (not changed) marked committed)
;;            (org-glance-view:highlight-marker view midx "#27ae60"))
;;           ((and corrupted (not marked))
;;            (org-glance-view:highlight-marker view midx "#e74c3c"))
;;           ;; (t
;;           ;;  (org-glance-view:highlight-marker view midx "#749AF7"))
;;           )))

(cl-defun org-glance-view:shift-markers (view midx diff)
  (cl-loop for i from (1+ midx) below (length (org-glance-> view :marker-positions))
     do (org-glance-view:set-marker-position view i (+ (org-glance-view:get-marker-position view i) diff))))

(provide 'org-glance-view)
