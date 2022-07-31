(require 'org-glance-headline)
(require 'org-glance-store)

(defvar org-glance-overlay-manager nil
  "Painter thread.")

(defvar org-glance-material-stores (make-hash-table)
  "Buffer to origin alist.")

(defvar org-glance-material-offsets (make-hash-table)
  "Buffer to last committed offset alist.")

(defvar org-glance-material-points* (make-hash-table)
  "Changed (buffer . point) cons cells.")

(defvar org-glance-material-markers* (make-hash-table)
  "Set of changed markers.")

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with synchronization abilities.")

(defvar org-glance-material-overlay-manager--mutex (make-mutex)
  "Mutex for material overlay manager.")

(defvar org-glance-material-garbage-collector--mutex (make-mutex)
  "Mutex for material garbage collector.")

(defconst org-glance-material-header
  "#  -*- mode: org; mode: org-glance-material -*-

#+ORIGIN: %s

"
  "Header template of material files.")

(cl-defstruct (org-glance-material-marker (:constructor org-glance-material-marker)
                                          (:copier nil))
  "Sync metadata about headlines to be processed and visualized by `org-glance-overlay-manager'."
  (beg nil         :type number :read-only nil :documentation "Beginning of headline.")
  (end nil         :type number :read-only nil :documentation "End of headline.")
  (buffer nil      :type buffer :read-only t   :documentation "Materialized buffer.")
  (hash nil        :type string :read-only nil :documentation "Hash of headline origin.")
  (overlay nil     :type object :read-only nil :documentation "Current overlay for status reporting.")
  (changed-p nil   :type bool   :read-only nil :documentation "Has headline changed?")
  (persisted-p nil :type bool   :read-only t   :documentation "Whether headline has origin or not.")
  (committed-p nil :type bool   :read-only nil :documentation "Whether changes have been committed."))

(cl-defun org-glance-material-store ()
  "Get `org-glance-store' instance associated with current material buffer."
  (when-let (origin (condition-case nil
                        (save-excursion
                          (goto-char (point-min))
                          (search-forward "#+ORIGIN: ")
                          (buffer-substring-no-properties (point) (point-at-eol)))
                      (search-failed nil)))
    (org-glance-store origin)))

(define-minor-mode org-glance-material-mode
    "A minor mode to be activated only in materialized view editor.
This is the only point to consistently mutate state of underlying store.
In other places `org-glance-store' should act like functional thread-safe append-only storage."
  nil nil org-glance-material-mode-map
  (cond (org-glance-material-mode
         (let ((store (org-glance-material-store)))
           (when (null store)
             (user-error "Unable to start material mode: associated store has not been found."))
           (puthash (current-buffer) store org-glance-material-stores)
           (org-glance-map (headline)
             (let* ((hash (org-glance-headline-hash headline))
                    (marker (org-glance-material-marker
                             :hash hash
                             :beg (point-min)
                             :end (point-max)
                             :buffer (current-buffer)
                             :overlay nil
                             :changed-p nil
                             :committed-p nil
                             :persisted-p (when (org-glance-store-get-headline-by-hash store hash) t))))
               (add-text-properties (point-min) (point-max) (list :marker marker))
               (with-mutex org-glance-material-overlay-manager--mutex
                 (puthash marker (point-min) org-glance-material-points*))))
           (org-glance-material-overlay-manager-redisplay*))
         (add-hook 'post-command-hook #'org-glance-material-debug nil t)
         (add-hook 'after-change-functions #'org-glance-material-edit nil t)
         (add-hook 'before-save-hook #'org-glance-material-commit nil t))
        (t
         (remove-hook 'before-save-hook #'org-glance-material-commit t)
         (remove-hook 'after-change-functions #'org-glance-material-edit t)
         (remove-hook 'post-command-hook #'org-glance-material-debug t))))

(cl-defgeneric org-glance-materialize (source target)
  "Materialize SOURCE to TARGET.
After materialiation calling to `org-glance-material-commit' from TARGET should be applied to SOURCE.")

(cl-defmethod org-glance-materialize ((store org-glance-store) (file string))
  "Insert STORE headlines into the FILE and provide ability to push changes
to its origins by calling `org-glance-material-commit'."
  (org-glance--with-temp-file file
    (insert (format org-glance-material-header (org-glance-store-location store)))
    (cl-loop for headline in (org-glance-store-headlines store)
       do (let ((headline (org-glance-store-headline store headline)))
            (org-glance-headline-insert headline)))
    store))

(cl-defun org-glance-material-edit (&rest _)
  "Mark current headline as changed in current buffer."
  (puthash (get-text-property (point) :marker) (point) org-glance-material-points*)
  (org-glance-material-overlay-manager-redisplay*))

(cl-defun org-glance-material-overlay-manager-redisplay ()
  "Actualize all overlays in changed material buffers."
  (interactive)
  (with-mutex org-glance-material-overlay-manager--mutex
    (cl-loop for marker being the hash-keys of org-glance-material-points* using (hash-values point)
       when (and marker
                 (org-glance-material-marker-buffer marker)
                 (buffer-live-p (org-glance-material-marker-buffer marker)))
       do (with-current-buffer (org-glance-material-marker-buffer marker)
            (save-excursion
              (goto-char point)
              (when (string= (org-glance-material-marker-hash (get-text-property point :marker))
                             (org-glance-material-marker-hash marker))
                (org-glance--with-heading-at-point
                  (let* ((headline (org-glance-headline-at-point))
                         (hash-old (org-glance-material-marker-hash marker))
                         (changed-p (org-glance-material-marker-changed-p marker))
                         (persisted-p (org-glance-material-marker-persisted-p marker))
                         (overlay (org-glance-material-marker-overlay marker))
                         (hash-new (org-glance-headline-hash headline))
                         (returned-to-unchanged-state-p (and (string= hash-old hash-new) changed-p))
                         (first-change-p (and (not (string= hash-old hash-new)) (not changed-p)))
                         (further-change-p (and (not (string= hash-old hash-new)) changed-p)))
                    (cond
                      ((not persisted-p)
                       ;; (when (yes-or-no-p "New headline detected. Do you want to add it to store?")
                       ;;   (puthash marker org-glance-material-markers*))
                       (org-glance-material-marker-redisplay marker))
                      (returned-to-unchanged-state-p
                       (progn
                         (setf (org-glance-material-marker-changed-p marker) nil)
                         (org-glance-material-marker-redisplay marker)
                         (remhash marker org-glance-material-markers*)))
                      (first-change-p
                       (progn
                         (setf (org-glance-material-marker-changed-p marker) t
                               (org-glance-material-marker-beg marker) (point-min)
                               (org-glance-material-marker-end marker) (point-max))
                         (org-glance-material-marker-redisplay marker)
                         (puthash marker t org-glance-material-markers*)))
                      (further-change-p
                       (progn
                         (setf (org-glance-material-marker-changed-p marker) t
                               (org-glance-material-marker-beg marker) (point-min)
                               (org-glance-material-marker-end marker) (point-max))
                         (org-glance-material-marker-redisplay marker)))))))))
       finally do (clrhash org-glance-material-points*))))

(cl-defun org-glance-material-overlay-manager-redisplay* ()
  "Run `org-glance-material-marker-redisplay' in separate thread."
  (unless (and (threadp org-glance-overlay-manager)
               (thread-alive-p org-glance-overlay-manager))
    (setq org-glance-overlay-manager (make-thread #'org-glance-material-overlay-manager-redisplay "org-glance-overlay-manager"))))

(cl-defun org-glance-material-marker-redisplay (marker)
  "Refresh MARKER overlay."
  (let ((hash (org-glance-material-marker-hash marker))
        (buffer (org-glance-material-marker-buffer marker))
        (overlay (org-glance-material-marker-overlay marker))
        (beg (org-glance-material-marker-beg marker))
        (changed-p (org-glance-material-marker-changed-p marker))
        (committed-p (org-glance-material-marker-committed-p marker))
        (persisted-p (org-glance-material-marker-persisted-p marker)))
    (cond ((and changed-p (not overlay))
           (progn
             (let ((overlay (make-overlay beg (1+ beg))))
               (setf (org-glance-material-marker-overlay marker) overlay)
               (overlay-put overlay 'face '(:foreground "#ffcc00")))))
          ((and changed-p overlay committed-p)
           (progn
             (delete-overlay overlay)
             (let ((overlay (make-overlay beg (1+ beg))))
               (setf (org-glance-material-marker-overlay marker) overlay
                     (org-glance-material-marker-committed-p marker) nil)
               (overlay-put overlay 'face '(:foreground "#ffcc00")))))
          ((and (not changed-p) overlay (not committed-p))
           (progn
             (delete-overlay overlay)
             (setf (org-glance-material-marker-overlay marker) nil)))
          ((and (not changed-p) overlay committed-p)
           (progn
             (delete-overlay overlay)
             (let ((overlay (make-overlay beg (1+ beg))))
               (setf (org-glance-material-marker-overlay marker) overlay)
               (overlay-put overlay 'face '(:foreground "#27ae60")))))
          ((and (not persisted-p) (not overlay))
           (progn
             (let ((overlay (make-overlay beg (1+ beg))))
               (setf (org-glance-material-marker-overlay marker) overlay)
               (overlay-put overlay 'face '(:foreground "#e74c3c"))))))))

(cl-defun org-glance-material-commit ()
  "Apply all changes of buffer headlines to its origins.
Returns new store with changes reflected in WAL.

TODO:
- It should be generalized to other materialization types."
  (interactive)
  (cl-loop for marker in (-non-nil (hash-table-keys org-glance-material-markers*))
     with store = (gethash (current-buffer) org-glance-material-stores)
     when (eq (current-buffer) (org-glance-material-marker-buffer marker))
     collect (let ((headline
                    (org-glance-headline-from-region
                     (org-glance-material-marker-beg marker)
                     (org-glance-material-marker-end marker))))
               (cons headline marker))
     into h&ms
     finally return
       (prog1 (puthash (current-buffer)
                       (org-glance-store-commit
                        (-reduce-from
                         (lambda (store h&m) ;; pure transform
                           (cl-destructuring-bind (headline . marker) h&m
                             (-> store
                                 (org-glance-store-remove-headline-by-hash (org-glance-material-marker-hash marker))
                                 (org-glance-store-put-headlines headline))))
                         store
                         h&ms))
                       org-glance-material-stores)
         (dolist (h&m h&ms) ;; overlay manager side-effects
           (cl-destructuring-bind (headline . marker) h&m
             (with-mutex org-glance-material-overlay-manager--mutex
               (setf (org-glance-material-marker-changed-p marker) nil
                     (org-glance-material-marker-committed-p marker) t
                     (org-glance-material-marker-hash marker) (org-glance-headline-hash headline))
               (org-glance-material-marker-redisplay marker)
               (remhash marker org-glance-material-markers*)))
           (org-glance-material-overlay-manager-redisplay*))))
  ;; TODO work with deleted buffers in `org-glance-material-stores'
  )

(cl-defun org-glance-material-debug (&rest _)
  (prin1 (get-text-property (point) :marker)))

(provide 'org-glance-material)
