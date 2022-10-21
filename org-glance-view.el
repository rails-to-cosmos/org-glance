;;; org-glance-view.el --- View of the world  -*- lexical-binding: t; -*-
;;; Commentary:
;;

(require 'eieio)
(require 'f)
(require 'org-macs)

(require 'org-glance-log)
(require 'org-glance-types)
(require 'org-glance-offset)
(require 'org-glance-helpers)

(require 'org-glance-headline)

;;; Code:

(declare-function f-mkdir-full-path 'f)

(org-glance-class org-glance-marker nil
    ((hash
      :type string
      :initarg :hash)
     (position
      :type number
      :initarg :position)
     (changed?
      :type boolean
      :initarg :changed?
      :initform nil)))

(org-glance-class org-glance-view nil
    ((world
      :type org-glance-world
      :initarg :world
      :documentation "Original `org-glance-world' instance.")
     ;; available TODO states
     ;; capture template
     (type
      :type (or symbol list)
      :initarg :type
      :documentation "Type declaration that transforms into predicate of
      one argument: `org-glance-headline'. View is guaranteed to
      contain only headlines for which predicate returns non-nil
      value.")
     (location
      :type org-glance-file
      :initarg :location
      :documentation "Location where view persists.")
     (offset
      :type org-glance-offset
      :initarg :offset)
     (markers
      :type vector
      :initarg :markers)
     (hash->midx
      :type hash-table
      :initarg :hash->midx
      :initform (make-hash-table :test #'equal)
      :documentation "Hash to idx.")))

(cl-defun org-glance-view:create (world type location &optional
                                                        (backfill? t)
                                                        (initial-offset (org-glance-world:offset world)))
  "Create symbol `org-glance-view' instance from WORLD by TYPE and store it in LOCATION."
  (thunk-let* ((views (org-glance- world :views))
               (view-location (file-truename (f-join (org-glance- world :location) location)))
               (key (list type view-location))
               (cached-view (gethash key views))
               (view-exists? (and (f-exists? view-location)
                                  (f-file? view-location)
                                  cached-view))
               (headlines (org-glance-world:headlines world))
               (view (org-glance-view :world world
                                      :type type
                                      :location view-location
                                      :offset initial-offset))
               (header (org-glance-view:header view)))
    (cond (view-exists?
           (org-glance-log:cache "org-glance-view: cache hit %s" type)
           cached-view)
          (t
           (org-glance-log:cache "org-glance-view: create from scratch %s" type)
           (unless (f-exists? view-location)
             (f-mkdir-full-path (f-parent view-location)))

           (org-glance--with-temp-file view-location
             (insert header)
             (when backfill?
               (cl-dolist (headline headlines)
                 (when (org-glance-view:member? view headline)
                   (org-glance-world:insert-headline world headline)))))

           (puthash key view (org-glance- world :views))))))

(cl-defun org-glance-view:member? (view headline)
  "Decide if HEADLINE should be a part of VIEW."
  (eval (org-glance- view :type) (org-glance-headline:partitions headline)))

(cl-defmacro org-glance-view:if-safe-marker (view midx then &rest else)
  (declare (indent 3))
  `(if (< -1 ,midx (length (org-glance- ,view :markers)))
       ,then
     ,@else))

(cl-defun org-glance-view:get-marker-position (view midx)
  (org-glance-view:if-safe-marker view midx
      (org-glance- view :markers [midx] :position)
    (point-max)))

(cl-defun org-glance-view:set-marker-position (view midx val)
  (org-glance-view:if-safe-marker view midx
      (setf (org-glance- view :markers [midx] :position) val)))

(cl-defun org-glance-view:set-marker-hash (view midx val)
  (org-glance-view:if-safe-marker view midx
      (progn
        (remhash (org-glance-view:get-marker-hash view midx) (org-glance- view :hash->midx))
        (puthash val midx (org-glance- view :hash->midx))
        (setf (org-glance- view :markers [midx] :hash) val))))

(cl-defun org-glance-view:get-marker-hash (view midx)
  (org-glance-view:if-safe-marker view midx
      (org-glance- view :markers [midx] :hash)))

(cl-defun org-glance-view:marker-changed? (view midx)
  (org-glance-view:if-safe-marker view midx
      (org-glance- view :markers [midx] :changed?)))

(cl-defun org-glance-view:set-marker-changed (view midx val)
  (org-glance-view:if-safe-marker view midx
      (setf (org-glance- view :markers [midx] :changed?) val)))

(cl-defun org-glance-view:header (view)
  "Generate header for VIEW."
  (s-join "\n"
          (list "#  -*- mode: org; mode: org-glance-material -*-"
                ""
                "#+STARTUP: overview"
                (format "#+TYPE: %s :: %s"
                        (org-glance- view :world :location)
                        (org-glance- view :type))
                (format "#+OFFSET: %s"
                        (org-glance- view :offset))
                (format "#+PROPERTY: ATTACH_DIR ./../../resources/%s/%s/"
                        (thread-first (org-glance- view :location)
                          f-parent
                          file-name-nondirectory
                          downcase)
                        (thread-first (org-glance- view :location)
                          file-name-nondirectory
                          file-name-sans-extension
                          downcase))
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
  (org-glance-log:scenario "SET VIEW PROPERTY %s = \"%s\" WHERE %s = \"%s\""
    property value
    property (org-glance-view:get-property property))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (condition-case nil
          (progn
            (re-search-forward (format "^\\#\\+%s: " property))
            (org-glance-log:debug "Delete region: \"%s\"" (buffer-substring-no-properties (point) (line-end-position)))
            (org-glance-log:debug "Delete region from %d to %d" (point) (line-end-position))
            (org-glance-log:debug "Insert \"%s\"" (prin1-to-string value))
            (delete-region (point) (line-end-position))
            (insert (prin1-to-string value)))
        (search-failed nil)))))

(cl-defun org-glance-view:get-buffer-world ()
  "Get `org-glance-world' associated with current buffer."
  (-some->> (org-glance-view:get-property "TYPE")
    (s-split " :: ")
    cl-first
    org-glance-world:read))

(cl-defun org-glance-view:get-buffer-type ()
  "Get `org-glance-world' associated with current buffer."
  (-some->> (org-glance-view:get-property "TYPE")
    (s-split " :: ")
    cl-second
    read))

(cl-defun org-glance-view:get-buffer-view ()
  "Get `org-glance-view' associated with current buffer."
  (let ((world (org-glance-view:get-buffer-world))
        (type (org-glance-view:get-buffer-type)))
    (unless world
      (user-error "Unable to get world from buffer %s" (current-buffer)))
    (org-glance-view:create world type (buffer-file-name))))

(cl-defmacro org-glance-view:with-current-buffer (view &rest forms)
  (declare (indent 1))
  `(save-match-data
     (let ((buffer (get-file-buffer (org-glance- ,view :location))))
       (when (and buffer (buffer-live-p buffer))
         (with-current-buffer buffer
           (save-excursion
             (save-restriction
               ,@forms)))))))

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
          for marker across-ref (org-glance- ,view :markers)
          for midx from 0
          when (org-glance- marker :changed?)
          collect (unwind-protect
                       (save-excursion
                         (goto-char (org-glance-view:get-marker-position ,view midx))
                         (let ((,midx-var-name midx))
                           ,@forms))
                    (org-glance-view:set-marker-changed ,view midx nil))))))

(cl-defun org-glance-view:add-headline (view headline)
  (declare (indent 0))
  (org-glance-view:with-current-buffer view
    (goto-char (point-max))
    (org-glance-world:insert-headline (org-glance- view :world) headline)
    (org-glance-view:mark view)))

(cl-defun org-glance-view:remove-headline (view hash)
  (org-glance-view:with-current-buffer view
    (let* ((midx (gethash hash (org-glance- view :hash->midx)))
           (marker-position (org-glance-view:get-marker-position view midx))
           (size (1- (hash-table-count (org-glance- view :hash->midx))))
           (markers* (make-vector size nil)))
      (cl-loop for idx from 0 to size
         when (< idx midx)
         do (aset markers* idx (aref (org-glance- view :markers) idx))
         when (> idx midx)
         do (aset markers* (1- idx) (aref (org-glance- view :markers) (1- idx)))
         finally
           (setf (org-glance- view :markers) markers*)
           (remhash hash (org-glance- view :hash->midx))
           (goto-char marker-position)
           (org-glance-headline:with-headline-at-point
             (delete-region (point-min) (point-max)))))))

(cl-defun org-glance-view:replace-headline (view old-hash headline)
  (declare (indent 0))
  (thunk-let* ((new-hash (org-glance- headline :hash))
               (midx (gethash old-hash (org-glance- view :hash->midx)))
               (marker-position (org-glance-view:get-marker-position view midx)))
    (org-glance-log:scenario "Update \"%s\" set hash = \"%s\"" (org-glance- headline :title) new-hash)
    (org-glance-view:with-current-buffer view
      (goto-char marker-position)
      (org-glance-headline:with-headline-at-point
        (let ((inhibit-message t)
              (org-log-state-notes-into-drawer nil)
              (org-log-into-drawer nil)
              (org-log-note-state nil)
              (org-todo-log-states nil)
              (org-log-done nil))
          (org-edit-headline (org-glance- headline :title))
          (org-todo (org-glance- headline :state))
          (when (org-glance- headline :commented?)
            (org-toggle-comment))
          (org-set-tags (org-glance- headline :tags)))

        (goto-char (point-min))
        (when (= 0 (forward-line))
          (delete-region (point) (point-max)))

        (goto-char (point-max))

        (insert (with-temp-buffer
                  (insert (org-glance- headline :contents))
                  (goto-char (point-min))
                  (forward-line)
                  (buffer-substring-no-properties (point) (point-max))))

        (unless (string= (buffer-substring-no-properties (1- (point-max)) (point-max)) "\n")
          (insert "\n"))

        (org-glance-view:set-marker-hash view midx new-hash)))))

(cl-defun org-glance-view:mark (&optional (view (org-glance-view:get-buffer-view)))
  "Create effective in-memory representation of VIEW org-mode buffer."
  ;; TODO make dynamic arrays to optimize add operation
  (org-glance-view:with-current-buffer view
    (let* ((headlines (org-glance-headline:map (headline) (list (point-min) (org-glance- headline :hash))))
           (markers (make-vector (length headlines) nil))
           (hash->midx (make-hash-table :test #'equal)))
      (cl-loop
         for (position hash) in headlines
         for midx from 0
         do
           (puthash hash midx hash->midx)
           (aset markers midx (org-glance-marker :hash hash :position position)))
      (setf (org-glance- view :markers) markers
            (org-glance- view :hash->midx) hash->midx))))

(cl-defun org-glance-view:commit (&optional (view (org-glance-view:get-buffer-view)))
  (org-glance-view:with-current-buffer view
    (let ((world (org-glance- view :world))
          (to-remove '()))
      (org-glance-view:fetch view)

      (org-glance-view:consume-changes (view midx)
        (let* ((headline (save-excursion
                           (goto-char (org-glance-view:get-marker-position view midx))
                           (org-glance-headline-at-point)))
               (old-hash (org-glance-view:get-marker-hash view midx))
               (new-hash (org-glance- headline :hash)))
          (org-glance-world:update world old-hash headline)
          (org-glance-view:set-marker-changed view midx nil)
          (org-glance-view:set-marker-hash view midx new-hash)
          (unless (org-glance-view:member? view headline)
            (push new-hash to-remove))))

      (dolist (hash to-remove)
        (org-glance-view:remove-headline view hash))

      (let ((offset (org-glance-world:persist world)))
        (org-glance-view:set-offset view offset))

      (dolist (it (hash-table-values (org-glance- world :views)))
        (when (not (eq view it))
          (org-glance-view:fetch it))))))

(cl-defun org-glance-view:fetch (&optional (view (org-glance-view:get-buffer-view)))
  (cl-labels ((derive (h ;; hash
                       rs ;; relations
                       i ;; relation index
                       )
                (cond ((gethash h (org-glance- view :hash->midx)) h)
                      ((> i 0) (cl-loop for j from i downto 0
                                  for r = (aref rs j) ;; relation
                                  for s = (car r)     ;; source
                                  for d = (cdr r)     ;; derivation
                                  when (string= h d)
                                  return (derive s rs j)))
                      (t nil) ;; not found
                      )))
    (thunk-let* ((world (org-glance- view :world))
                 (view-offset (org-glance-view:offset view))
                 (world-offset (org-glance-world:offset world))
                 (events (--take-while (org-glance-offset:less? view-offset (org-glance- it :offset))
                                       (org-glance-world:events world))))
      (org-glance-view:with-current-buffer view
        (when (org-glance-offset:less? view-offset world-offset)
          (cl-loop
             with relations = (make-vector (length events) nil)
             for event in (reverse events)  ;; TODO optimize me
             for idx from 0
             for headline = (org-glance-world:get-headline world (org-glance- event :headline :hash))
             do (org-glance-log:context "Event: %s" event)
             do (cl-typecase event
                  (org-glance-event:UPDATE (org-glance-log:scenario "Replace headline \"%s\" with \"%s\"" (org-glance- event :hash) (org-glance- headline :hash))
                                           (cond ((string= (org-glance- headline :hash) (org-glance- event :hash))
                                                  (org-glance-log:debug "Skip UPDATE event for \"%s\"" (org-glance- headline :title))
                                                  (org-glance-log:reason "Hashes are equal"))
                                                 ((not (org-glance-view:member? view headline))
                                                  (aset relations idx (cons (org-glance- event :hash) (org-glance- headline :hash)))

                                                  (when (gethash (org-glance- event :hash) (org-glance- view :hash->midx))
                                                    (org-glance-view:remove-headline view (org-glance- event :hash)))

                                                  (org-glance-log:debug "Skip UPDATE event for \"%s\"" (org-glance- headline :title))
                                                  (org-glance-log:reason "Validation failed")
                                                  (org-glance-log:context "State: %s" (org-glance- headline :state))
                                                  (org-glance-log:context "View: %s" (org-glance- view :type))
                                                  (org-glance-log:context "Relations: %s" relations))
                                                 ((gethash (org-glance- event :hash) (org-glance- view :hash->midx))
                                                  (org-glance-view:replace-headline view (org-glance- event :hash) headline))
                                                 (t
                                                  (org-glance-log:debug "Track UPDATE event as ADD event for \"%s\"" (org-glance- headline :title))
                                                  (org-glance-log:reason "Event hash not found")
                                                  (org-glance-log:context "Event hash: %s" (org-glance- event :hash))
                                                  (org-glance-log:context "Relations: %s" relations)
                                                  (org-glance-log:context "Available hashes: %s" (hash-table-keys (org-glance- view :hash->midx)))
                                                  (org-glance-log:debug "Derive hash from relations")
                                                  (if-let (hash (derive (org-glance- event :hash) relations idx))
                                                      (unless (string= hash (org-glance- headline :hash))
                                                        (org-glance-view:replace-headline view hash headline))
                                                    (org-glance-log:debug "Derived hash not found. Add headline")
                                                    (org-glance-view:add-headline view headline)))))
                  (org-glance-event:PUT (cond ((not (org-glance-view:member? view headline))
                                               (org-glance-log:debug "Skip ADD event for \"%s\"" (org-glance- headline :title))
                                               (org-glance-log:reason "Validation failed")
                                               (org-glance-log:context "State: %s" (org-glance- headline :state))
                                               (org-glance-log:context "View: %s" (org-glance- view :type)))
                                              (t
                                               (org-glance-view:add-headline view headline))))
                  (org-glance-event:RM (org-glance-view:remove-headline view (org-glance- event :hash)))
                  (otherwise (user-error "Don't know how to handle event of type %s" (type-of event)))))
          (org-glance-view:set-offset view world-offset)
          (save-buffer))))))

(cl-defun org-glance-view:offset (view)
  (declare (indent 1))
  (let ((buffer-offset (org-glance-view:with-current-buffer view
                         (org-glance-offset:read (org-glance-view:get-property "OFFSET"))))
        (memory-offset (org-glance- view :offset)))
    (-max-by #'org-glance-offset:less? (list buffer-offset memory-offset))))

(cl-defun org-glance-view:set-offset (view offset)
  (declare (indent 1))
  (org-glance-view:with-current-buffer view
    (org-glance-view:set-property "OFFSET" offset)
    (setf (org-glance- view :offset) offset)))

(cl-defun org-glance-view:marker-at-point
    (&optional
       (view (org-glance-view:get-buffer-view))
       (point (point)))
  (org-glance:binary-search (org-glance- view :markers) point
    :key #'(lambda (vec idx) (org-glance- (aref vec idx) :position))))

(cl-defun org-glance-view:shift-markers (view midx diff)
  (cl-loop for idx from (1+ midx) below (length (org-glance- view :markers))
     do (org-glance-view:set-marker-position view idx (+ (org-glance-view:get-marker-position view idx) diff))))

(provide 'org-glance-view)

;;; org-glance-view.el ends here
