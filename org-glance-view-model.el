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
(require 'org-glance-dimension)

;;; Code:

(defconst org-glance-view--header-extension ".h.el")
(defconst org-glance-view--marker-extension ".m.txt")

(declare-function f-mkdir-full-path 'f)

(org-glance-class org-glance-view--marker nil
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
      :type org-glance-type:optional-org-file
      :initarg :location
      :documentation "Location where view persists.")
     (offset
      :type org-glance-type:offset
      :initarg :offset)
     (markers
      :type vector
      :initarg :markers)
     (hash->midx
      :type hash-table
      :initarg :hash->midx
      :initform (make-hash-table :test #'equal)
      :documentation "Hash to idx.")))

(cl-defun org-glance-view:create (world type location offset)
  "Create `org-glance-view' instance from WORLD by TYPE and store it in LOCATION with initial OFFSET."
  (let ((view (org-glance-view :world world
                               :type type
                               :location location
                               :offset offset)))
    (unless (f-exists? (org-glance- view :location))
      (f-mkdir-full-path (f-parent (org-glance- view :location)))
      (org-glance-view:write-header view)
      (org-glance:with-temp-file location
        (insert (s-join "\n"
                        (list "#  -*- mode: org; mode: org-glance-material -*-"
                              ""
                              "#+STARTUP: overview"
                              ;; (format "#+TYPE: %s :: %s"
                              ;;         (org-glance- view :world :location)
                              ;;         (cl-prin1-to-string (org-glance- view :type)))
                              ;; (format "#+OFFSET: %s"
                              ;;         (org-glance- view :offset))
                              ;; (format "#+PROPERTY: ATTACH_DIR ./../../resources/%s/%s/"
                              ;;         (thread-first (org-glance- view :location)
                              ;;           f-parent
                              ;;           file-name-nondirectory
                              ;;           downcase)
                              ;;         (thread-first (org-glance- view :location)
                              ;;           file-name-nondirectory
                              ;;           file-name-sans-extension
                              ;;           downcase))
                              ""
                              "")))))
    view))

(cl-defmacro org-glance-view:if-safe-marker (view midx then &rest else)
  (declare (indent 3))
  `(if (< -1 ,midx (length (org-glance- ,view :markers)))
       ,then
     ,@else))

(cl-defun org-glance-view:get-marker-headline (view midx)
  (org-glance-view:if-safe-marker view midx
      (save-excursion
        (goto-char (org-glance-view:get-marker-position view midx))
        (org-glance-headline-at-point))))

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
  (goto-char (point-max))
  (-> (org-glance- view :world)
      (org-glance-world:get-headline (org-glance- headline :hash))
      (org-glance-headline:insert))
  ;; TODO optimize this, no need to remark all headlines
  (org-glance-view:mark-buffer view))

(cl-defun org-glance-view:remove-headline (view hash)
  (let* ((midx (gethash hash (org-glance- view :hash->midx)))
         (marker-position (org-glance-view:get-marker-position view midx))
         (size (1- (hash-table-count (org-glance- view :hash->midx))))
         (markers* (make-vector size nil)))
    (cl-loop for idx from 0 to size
       when (< idx midx)
       do (aset markers* idx (aref (org-glance- view :markers) idx))
       when (> idx midx)
       do (aset markers* (1- idx) (aref (org-glance- view :markers) (1- idx)))
       finally do
         (setf (org-glance- view :markers) markers*)
         (remhash hash (org-glance- view :hash->midx))
         (goto-char marker-position)
         (org-glance-headline:with-headline-at-point
           (delete-region (point-min) (point-max))))))

(cl-defun org-glance-view:replace-headline (view old-hash headline)
  (declare (indent 0))
  (thunk-let* ((new-hash (org-glance- headline :hash))
               (midx (gethash old-hash (org-glance- view :hash->midx)))
               (marker-position (org-glance-view:get-marker-position view midx)))
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

      (org-glance-view:set-marker-hash view midx new-hash))))

(cl-defun org-glance-view:mark-buffer (&optional (view (org-glance-view:get-buffer-view)))
  "Create effective in-memory representation of VIEW org-mode buffer."
  ;; TODO make dynamic arrays to optimize add operation
  ;; TODO save markers for further optimizations
  (cl-loop
     with mark-cache-file = (org-glance-view:locate-markers view)
     with markers = (or (and (f-exists? mark-cache-file)
                             (condition-case nil
                                 (prog1 (org-glance-view:load-markers view mark-cache-file)
                                   (org-glance-log :cache "cache hit: read markers"))
                               (user-error (org-glance-log :cache "cache miss: recalculate markers"))))
                        (prog1 (org-glance-headline:map (headline)
                                 (org-glance-view--marker :hash (org-glance- headline :hash)
                                                          :position (point-min)))
                          (org-glance-log :cache "cache miss: recalculate markers")))
     with result = (make-vector (length markers) nil)
     with hash->midx = (make-hash-table :test #'equal)
     for marker in markers
     for midx from 0
     do
       (puthash (org-glance- marker :hash) midx hash->midx)
       (aset result midx marker)
     finally do
       (setf (org-glance- view :markers) result
             (org-glance- view :hash->midx) hash->midx)
       (org-glance-view:save-markers view mark-cache-file)))

(cl-defun org-glance-view:commit (&optional (view (org-glance-view:get-buffer-view)))
  (let ((world (org-glance- view :world))
        (to-remove '()))

    (org-glance-log :buffers "[%s] Buffer before commit:\n%s" (org-glance- view :type) (buffer-string))

    (org-glance-view:fetch view)

    (org-glance-view:consume-changes (view midx)
      (let* ((headline (org-glance-view:get-marker-headline view midx))
             (old-hash (org-glance-view:get-marker-hash view midx))
             (new-hash (org-glance- headline :hash)))
        (org-glance-world:update-headline world old-hash headline)
        (org-glance-view:set-marker-changed view midx nil)
        (org-glance-view:set-marker-hash view midx new-hash)

        (unless (org-glance-dimension:validate (org-glance- view :type) headline (org-glance- world :dimensions))
          (push new-hash to-remove))))

    (dolist (hash to-remove)
      (org-glance-view:remove-headline view hash))

    (let ((offset (org-glance-world:persist world)))
      (org-glance-log :world "Set view offset: %s" offset)
      (org-glance-view:set-offset view offset)
      (org-glance-log :world "View offset: %s" (org-glance- view :offset)))

    ;; (cl-loop
    ;;    with progress-reporter = (make-progress-reporter "Fetching related views" 0 (hash-table-count (org-glance- world :views)))
    ;;    for related-view being the hash-values of (org-glance- world :views)
    ;;    for idx from 0
    ;;    do (progress-reporter-update progress-reporter idx (prin1-to-string (org-glance- related-view :type)))
    ;;    when (not (eq view related-view))
    ;;    do (org-glance-view:with-current-buffer related-view
    ;;         (org-glance-view:fetch related-view))
    ;;    finally do (progress-reporter-done progress-reporter))

    (org-glance-view:save-markers view (org-glance-view:locate-markers view))

    (org-glance-log :buffers "[%s] Buffer after commit:\n%s" (org-glance- view :type) (buffer-string))))

(cl-defun org-glance-view:save-markers (view location)
  (org-glance-view:with-current-buffer view
    (let ((hash (buffer-hash)))
      (with-temp-file location
        (insert (format "%s\n" hash))
        (cl-loop for marker across-ref (org-glance- view :markers)
           do (insert (format "%s %d\n" (org-glance- marker :hash) (org-glance- marker :position))))))))

(cl-defun org-glance-view:load-markers (view location)
  (org-glance-view:with-current-buffer view
    (let ((view-hash (buffer-hash)))
      (with-temp-buffer
        (insert-file-contents location)
        (goto-char (point-min))
        (unless (string= (buffer-substring-no-properties (point) (line-end-position)) view-hash)
          (user-error "Mark cache validation failed: \"%s\" vs \"%s\""
                      (buffer-substring-no-properties (point-min) (line-end-position))
                      view-hash))
        (cl-loop while (and (= 0 (forward-line 1)) (not (eobp)))
           collect (cl-destructuring-bind (hash position)
                       (s-split " " (buffer-substring-no-properties (point) (line-end-position)))

                     (org-glance-view--marker :hash hash :position (string-to-number position))))))))

(cl-defun org-glance-view:fetch (&optional (view (org-glance-view:get-buffer-view)))
  (cl-labels ((derive (h  ;; hash
                       rs ;; relations
                       i  ;; relation index
                       )
                (cond ((gethash h (org-glance- view :hash->midx)) h)
                      ((> i 0) (cl-loop for j from i downto 0
                                  for r = (aref rs j) ;; relation
                                  for s = (car r)     ;; source
                                  for d = (cdr r)     ;; derivation
                                  when (string= h d)
                                  return (derive s rs j)))
                      ;; not found
                      (t nil))))
    (let ((world (org-glance- view :world))
          (view-offset (org-glance-view:get-offset view))
          (events (reverse (org-glance-world:events world)))
          (relations (make-vector (length events) nil))
          (progress-reporter (make-progress-reporter "Fetching events" 0 (length events)))
          (committed-offset view-offset))
      (cl-loop
         for event in events ;; TODO optimize
         for idx from 0
         for headline = (org-glance-world:get-headline world (org-glance- event :headline :hash))
         for event-offset = (org-glance- event :offset)

         when (cl-typep event 'org-glance-event:UPDATE)
         do (aset relations idx (cons (org-glance- event :hash) (org-glance- headline :hash)))

         when (org-glance-offset:less? view-offset event-offset)
         do (condition-case nil
                (thunk-let* ((event-hash (org-glance- event :hash))
                             (headline-hash (org-glance- headline :hash))
                             (derived-hash (derive event-hash relations idx))
                             (dimensions (org-glance- world :dimensions))
                             (view-type (org-glance- view :type))

                             (hashes-equal? (string= headline-hash event-hash))
                             (headline-derived? (string= derived-hash headline-hash))
                             (dimension-valid? (org-glance-dimension:validate view-type headline dimensions))
                             (dimension-invalid? (not dimension-valid?))
                             (headline-exists? (gethash event-hash (org-glance- view :hash->midx)))

                             (remove-headline! (org-glance-view:remove-headline view event-hash))
                             (replace-headline! (org-glance-view:replace-headline view event-hash headline))
                             (derive-headline! (org-glance-view:replace-headline view derived-hash headline))
                             (add-headline! (org-glance-view:add-headline view headline)))
                  (cl-typecase event
                    (org-glance-event:UPDATE (cond (hashes-equal? nil)
                                                   ((and dimension-invalid? (not headline-exists?)) nil)
                                                   ((and dimension-invalid? headline-exists?) remove-headline!)
                                                   ((and dimension-valid? headline-exists?) replace-headline!)
                                                   ((and derived-hash (not headline-derived?)) derive-headline!)
                                                   ((not derived-hash) add-headline!)))
                    (org-glance-event:PUT (when dimension-valid? add-headline!))
                    (org-glance-event:RM remove-headline!)
                    (otherwise (user-error "Don't know how to handle event of type %s" (type-of event)))))
              (quit (cl-return (org-glance-view:set-offset view committed-offset))))
           (setq committed-offset event-offset)
           (progress-reporter-update progress-reporter idx (format " (processed %d events of %d)" idx (length events)))
         finally do
           (org-glance-view:set-offset view committed-offset)
           (progress-reporter-done progress-reporter)))))

(cl-defun org-glance-view:get-offset (view)
  (let ((buffer-offset (condition-case nil
                           (thread-first (org-glance- view :location)
                             (org-glance-view:locate-header)
                             (org-glance-view:read-header)
                             (a-get :offset))
                         (file-missing (org-glance-offset:zero))))
        (memory-offset (org-glance- view :offset)))
    (-min-by #'org-glance-offset:less? (list buffer-offset memory-offset))))

(cl-defun org-glance-view:set-offset (view offset)
  (setf (org-glance- view :offset) offset)
  (org-glance-view:write-header view))

(cl-defun org-glance-view:marker-at-point
    (&optional
       (view (org-glance-view:get-buffer-view))
       (point (point)))
  (org-glance:binary-search (org-glance- view :markers) point
    :key #'(lambda (vec idx) (org-glance- (aref vec idx) :position))))

(cl-defun org-glance-view:shift-markers (view midx diff)
  (cl-loop for idx from (1+ midx) below (length (org-glance- view :markers))
     do (org-glance-view:set-marker-position view idx (+ (org-glance-view:get-marker-position view idx) diff))))

(cl-defun org-glance-view:write-header (view)
  (with-temp-file (org-glance-view:locate-header (org-glance- view :location))
    (insert (pp-to-string (a-list
                           :type (org-glance- view :type)
                           :offset (org-glance- view :offset))))))

(cl-defun org-glance-view:read-header (filename)
  (cl-assert (and (f-exists? filename)
                  (s-ends-with? org-glance-view--header-extension filename)))

  (with-temp-buffer
    (insert-file-contents filename)
    (read (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defun org-glance-view:locate-header (view-location)
  (thread-first view-location
    (file-name-sans-extension)
    (concat org-glance-view--header-extension)))

(cl-defun org-glance-view:locate-markers (view)
  (cl-check-type view org-glance-view)
  (thread-first view
    (org-glance- :location)
    (file-name-sans-extension)
    (concat org-glance-view--marker-extension)))

(provide 'org-glance-view-model)
;;; org-glance-view-model.el ends here
