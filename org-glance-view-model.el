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

(org-glance-class org-glance-vector nil
    ((array    :type vector :initarg :array)
     (size     :type number :initarg :size)
     (capacity :type number :initarg :capacity)))

(defconst org-glance-vector:DEFAULT-CAPACITY 64)

(cl-defun org-glance-vector:create ()
  (let ((capacity org-glance-vector:DEFAULT-CAPACITY))
    (org-glance-vector :array (make-vector capacity nil)
                       :size 0
                       :capacity capacity)))

(cl-defun org-glance-vector:double-capacity! (vec)
  (cl-check-type vec org-glance-vector)

  (let* ((old-arr (org-glance- vec :array))
         (old-capacity (org-glance- vec :capacity))
         (new-capacity (* 2 old-capacity))
         (new-arr (make-vector new-capacity nil)))
    (cl-loop for idx below old-capacity
       do (aset new-arr idx (aref old-arr idx)))
    (org-glance! vec :array    := new-arr)
    (org-glance! vec :capacity := new-capacity)))

(cl-defun org-glance-vector:half-capacity! (vec)
  (cl-check-type vec org-glance-vector)

  (let* ((old-arr (org-glance- vec :array))
         (new-capacity (/ (org-glance- vec :capacity) 2))
         (new-arr (make-vector new-capacity nil)))
    (cl-loop for idx below new-capacity
       do (aset new-arr idx (aref old-arr idx)))
    (org-glance! vec :array    := new-arr)
    (org-glance! vec :size     := (min (org-glance- vec :size) new-capacity))
    (org-glance! vec :capacity := new-capacity)))

(cl-defun org-glance-vector:enlarge-maybe! (vec)
  (cl-check-type vec org-glance-vector)

  (when (>= (org-glance- vec :size) (org-glance- vec :capacity))
    (org-glance-vector:double-capacity! vec)))

(cl-defun org-glance-vector:shrink-maybe! (vec)
  (cl-check-type vec org-glance-vector)

  (when (< (* 4 (org-glance- vec :size)) (org-glance- vec :capacity))
    (org-glance-vector:half-capacity! vec)))

(cl-defun org-glance-vector:push-back! (vec elem)
  (cl-check-type vec org-glance-vector)

  (org-glance-vector:enlarge-maybe! vec)
  (let ((idx (org-glance- vec :size)))
    (org-glance! vec :array [idx] := elem))
  (org-glance++ vec :size))

(cl-defun org-glance-vector:push-at! (vec idx elem)
  (cl-check-type vec org-glance-vector)
  (cl-check-type idx (org-glance-type:bounded-by (org-glance- vec :size)))

  (org-glance-vector:enlarge-maybe! vec)
  (cl-loop for j from (org-glance- vec :size) downto (1+ idx)
     do (org-glance! vec :array [j] := vec :array [(- j 1)]))
  (org-glance! vec :array [idx] := elem)
  (org-glance++ vec :size))

(cl-defun org-glance-vector:remove-at! (vec idx)
  (cl-check-type vec org-glance-vector)
  (cl-check-type idx (org-glance-type:bounded-by (org-glance- vec :size)))

  (cl-loop for j from (1+ idx) below (org-glance- vec :size)
     do (org-glance! vec :array [(- j 1)] := vec :array [j]))
  (org-glance-- vec :size)
  (org-glance-vector:shrink-maybe! vec))

(cl-defun org-glance-vector:get (vec idx)
  (cl-check-type vec org-glance-vector)
  (cl-check-type idx (org-glance-type:bounded-by (org-glance- vec :size)))

  (org-glance- vec :array [idx]))

(cl-defun org-glance-vector:size (vec)
  (cl-check-type vec org-glance-vector)

  (org-glance- vec :size))

(cl-defun org-glance-vector:empty? (vec)
  (cl-check-type vec org-glance-vector)

  (= 0 (org-glance-vector:size vec)))

(cl-defun org-glance-vector:clear! (vec)
  (cl-check-type vec org-glance-vector)

  (org-glance! vec :size := 0)
  (while (> (org-glance- vec :capacity) org-glance-vector:DEFAULT-CAPACITY)
    (org-glance-vector:shrink-maybe! vec))
  vec)

(cl-defun org-glance-vector:non-binary-search (vec v &key
                                                   (len #'(lambda (vec) (org-glance-vector:size vec)))
                                                   (key #'(lambda (vec idx) (org-glance- (org-glance-vector:get vec idx) :position)))
                                                   (l 0)
                                                       (r (1- (funcall len vec))))
  "Binary search for non-binary persons."
  (declare (indent 2))
  (cl-check-type vec org-glance-vector)
  (cl-check-type v number)

  (thunk-let* ((m (/ (+ l r 1) 2))
               (mv (funcall key vec m))
               (lv (funcall key vec l))
               (rv (funcall key vec r)))
    (cond ((= 0 (funcall len vec)) -1)
          ((< v lv) -1)
          ((= v lv) l)
          ((>= v rv) r)
          ((>= v mv) (org-glance-vector:non-binary-search vec v :l m :r r :key key :len len))
          (t (org-glance-vector:non-binary-search vec v :l l :r (1- m) :key key :len len)))))

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
      :type org-glance-vector
      :initarg :markers
      :initform (org-glance-vector:create)
      :documentation "Dynamic array with headlines.")
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
  `(if (< -1 ,midx (org-glance-vector:size (org-glance- ,view :markers)))
       ,then
     ,@else))

(cl-defun org-glance-view:get-marker-headline (view midx)
  (org-glance-view:if-safe-marker view midx
      (save-excursion
        (goto-char (org-glance-view:get-marker-position view midx))
        (org-glance-headline-at-point))))

(cl-defun org-glance-view:get-marker-index (view hash)
  (gethash hash (org-glance- view :hash->midx)))

(cl-defun org-glance-view:get-marker-position (view midx)
  (org-glance-view:if-safe-marker view midx
      (let ((marker (org-glance-vector:get (org-glance- view :markers) midx)))
        (org-glance- marker :position))
    (point-max)))

(cl-defun org-glance-view:set-marker-position (view midx val)
  (org-glance-view:if-safe-marker view midx
      (let ((marker (org-glance-vector:get (org-glance- view :markers) midx)))
        (org-glance! marker :position := val))))

(cl-defun org-glance-view:set-marker-hash (view midx val)
  (org-glance-view:if-safe-marker view midx
      (progn
        (remhash (org-glance-view:get-marker-hash view midx) (org-glance- view :hash->midx))
        (puthash val midx (org-glance- view :hash->midx))
        (let ((marker (org-glance-vector:get (org-glance- view :markers) midx)))
          (org-glance! marker :hash := val)))))

(cl-defun org-glance-view:get-marker-hash (view midx)
  (org-glance-view:if-safe-marker view midx
      (let ((marker (org-glance-vector:get (org-glance- view :markers) midx)))
        (org-glance- marker :hash))))

(cl-defun org-glance-view:marker-changed? (view midx)
  (org-glance-view:if-safe-marker view midx
      (let ((marker (org-glance-vector:get (org-glance- view :markers) midx)))
        (org-glance- marker :changed?))))

(cl-defun org-glance-view:set-marker-changed (view midx val)
  (org-glance-view:if-safe-marker view midx
      (let ((marker (org-glance-vector:get (org-glance- view :markers) midx)))
        (org-glance! marker :changed? := val))))

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
          with markers = (org-glance- ,view :markers)
          for midx from 0 below (org-glance-vector:size markers)
          for marker = (org-glance-vector:get markers midx)
          when (org-glance- marker :changed?)
          collect (unwind-protect
                       (save-excursion
                         (goto-char (org-glance- marker :position))
                         (let ((,midx-var-name midx))
                           ,@forms))
                    (org-glance-view:set-marker-changed ,view midx nil))))))

(cl-defun org-glance-view:add-headline (view headline)
  (cl-check-type view org-glance-view)
  (cl-check-type headline org-glance-headline)

  (let* ((markers (org-glance- view :markers))
         (hash (org-glance- headline :hash))
         (marker (org-glance-view--marker :hash hash :position (point-max))))
    (org-glance-vector:push-back! markers marker)
    (goto-char (point-max))
    (org-glance-headline:insert headline)
    (puthash hash (- (org-glance-vector:size markers) 1) (org-glance- view :hash->midx))))

(cl-defun org-glance-view:remove-headline (view hash)
  (cl-check-type view org-glance-view)
  (cl-check-type hash org-glance-type:hash)

  (org-glance-log :markers "Remove hash: %s" hash)
  (let* ((midx (org-glance-view:get-marker-index view hash))
         (mpos (org-glance-view:get-marker-position view midx))
         (markers (org-glance- view :markers)))
    (goto-char mpos)
    (org-glance-headline:with-headline-at-point
      (delete-region (point-min) (point-max)))
    (org-glance-vector:remove-at! markers midx)
    (remhash hash (org-glance- view :hash->midx))))

(cl-defun org-glance-view:replace-headline (view old-hash headline)
  (cl-check-type view org-glance-view)
  (cl-check-type old-hash org-glance-type:hash)
  (cl-check-type headline org-glance-headline)

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

;; (cl-defun org-glance-view:mark (view)
;;   "Create effective in-memory representation of VIEW org-mode buffer."
;;   (cl-check-type view org-glance-view)

;;   (let* ((markers-cache-file (org-glance-view:locate-markers view))
;;          (markers (or (and (f-exists? markers-cache-file)
;;                            (condition-case nil
;;                                (prog1 (org-glance-view:load-markers view markers-cache-file)
;;                                  (org-glance-log :cache "cache hit: read markers"))
;;                              (user-error (org-glance-log :cache "cache miss: recalculate markers"))))
;;                       (let ((markers (org-glance-vector:create)))
;;                         (org-glance-log :cache "[org-glance-view:mark] cache miss: recalculate markers")
;;                         (org-glance-headline:map (headline)
;;                           (let ((marker (org-glance-view--marker :hash (org-glance- headline :hash)
;;                                                                  :position (point-min))))
;;                             (org-glance-vector:push-back! markers marker)))
;;                         markers))))
;;     (cl-loop with hash->midx = (make-hash-table :test #'equal)
;;        for midx below (org-glance-vector:size markers)
;;        for marker = (org-glance-vector:get markers midx)
;;        do (puthash (org-glance- marker :hash) midx hash->midx)
;;        finally do
;;          (setf (org-glance- view :markers) markers
;;                (org-glance- view :hash->midx) hash->midx)
;;          (org-glance-view:save-markers view markers-cache-file))))

(cl-defun org-glance-view:commit (&optional (view (org-glance-view:get-buffer-view)))
  (let ((world (org-glance- view :world))
        (to-remove '()))

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
      (org-glance-view:set-offset view offset))

    (org-glance-view:save-markers view (org-glance-view:locate-markers view))))

(cl-defun org-glance-view:save-markers (view location)
  (cl-check-type view org-glance-view)

  (org-glance-view:with-current-buffer view
    (let ((markers (org-glance- view :markers))
          (buffer-hash (buffer-hash)))
      (with-temp-file location
        (insert (format "%s\n" buffer-hash))
        (cl-loop for midx below (org-glance-vector:size markers)
           for marker = (org-glance-vector:get markers midx)
           do (insert (format "%s %d\n" (org-glance- marker :hash) (org-glance- marker :position))))))))

(cl-defun org-glance-view:load-markers (view location)
  (cl-check-type view org-glance-view)

  (org-glance-view:with-current-buffer view
    (let ((view-hash (buffer-hash)))
      (with-temp-buffer
        (insert-file-contents location)
        (goto-char (point-min))
        (unless (string= (buffer-substring-no-properties (point) (line-end-position)) view-hash)
          (user-error "Mark cache validation failed: \"%s\" vs \"%s\""
                      (buffer-substring-no-properties (point-min) (line-end-position))
                      view-hash))
        (cl-loop with markers = (org-glance-vector:create)
           while (and (= 0 (forward-line 1)) (not (eobp)))
           for marker = (cl-destructuring-bind (hash position)
                            (s-split " " (buffer-substring-no-properties (point) (line-end-position)))
                          (org-glance-view--marker :hash hash :position (string-to-number position)))
           do (org-glance-vector:push-back! markers marker)
           finally return markers)))))

(cl-defun org-glance-view:fetch (view)
  (cl-check-type view org-glance-view)

  (cl-labels ((derive (h  ;; hash
                       rs ;; relations
                       i  ;; relation index
                       hs ;; hash store
                       )
                (cond ((gethash h hs) h)
                      ((> i 0) (cl-loop for j from i downto 0
                                  for r = (aref rs j) ;; relation
                                  for s = (car r)     ;; source
                                  for d = (cdr r)     ;; derivation
                                  when (string= h d)
                                  return (derive s rs j hs)))
                      ;; not found
                      (t nil))))
    (let* ((world (org-glance- view :world))
           (view-offset (org-glance-view:get-offset view))
           (events (reverse (org-glance-world:events world)))
           (relations (make-vector (length events) nil))
           (progress-reporter (make-progress-reporter "Fetching events" 0 (length events)))
           (committed-offset view-offset)
           (to-add (make-hash-table :test #'equal)))

      ;; initial state
      (cl-loop for hash being the hash-keys of (org-glance- view :hash->midx) using (hash-values midx)
         do (puthash hash (org-glance-view:get-marker-headline view midx) to-add))

      (cl-loop
         for event in events ;; TODO optimize
         for idx from 0
         for event-offset = (org-glance- event :offset)

         when (cl-typep event 'org-glance-event:UPDATE)
         do (aset relations idx (cons (org-glance- event :hash) (org-glance- event :headline :hash)))

         when (org-glance-offset:less? view-offset event-offset)
         do (thunk-let* ((headline* (org-glance- event :headline))

                         (event-hash (org-glance- event :hash))
                         (headline-hash (org-glance- headline* :hash))
                         (derived-hash (derive event-hash relations idx to-add))
                         (dimensions (org-glance- world :dimensions))
                         (view-type (org-glance- view :type))

                         (headline (org-glance-world:get-headline world headline-hash))

                         (hashes-equal? (string= headline-hash event-hash))
                         (headline-derived? (string= derived-hash headline-hash))
                         (dimension-valid? (org-glance-dimension:validate view-type headline* dimensions))
                         (dimension-invalid? (not dimension-valid?))
                         (headline-exists? (not (null (gethash event-hash to-add))))

                         (add-headline! (puthash headline-hash headline to-add))
                         (derive-headline!  (progn (puthash headline-hash headline to-add)
                                                   (remhash derived-hash to-add)))
                         (remove-headline!  (progn (remhash event-hash to-add)))
                         (replace-headline! (progn (puthash headline-hash headline to-add)
                                                   (remhash event-hash to-add))))
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
           (setq committed-offset event-offset)
           (progress-reporter-update progress-reporter idx (format " (processed %d events of %d)" idx (length events)))
         finally do
           (progress-reporter-done progress-reporter)
           (goto-char (point-min))
           (outline-next-heading)
           (delete-region (point) (point-max))
           (org-glance-vector:clear! (org-glance- view :markers))
           (dolist-with-progress-reporter (headline (hash-table-values to-add))
               "Insert headlines"
             (org-glance-view:add-headline view headline))
           (org-glance-view:set-offset view committed-offset)))))

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
  (org-glance-vector:non-binary-search (org-glance- view :markers) point))

(cl-defun org-glance-view:shift-markers (view midx diff)
  (cl-loop for idx from (1+ midx) below (org-glance-vector:size (org-glance- view :markers))
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
