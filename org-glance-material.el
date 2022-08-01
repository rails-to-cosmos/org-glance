(require 'org-glance-headline)
(require 'org-glance-store)

(defvar org-glance-overlay-manager nil
  "Painter thread.")

(defvar org-glance-material--buffer-to-store (make-hash-table)
  "Buffer to origin alist.")

(defvar org-glance-material--buffer-to-offset (make-hash-table)
  "Buffer to last committed offset map.")

(defvar org-glance-material--marker-to-point (make-hash-table)
  "Marker to point map.")

(defvar org-glance-material--changed-markers-set (make-hash-table)
  "Set of changed markers.")

(defvar org-glance-material--hash-to-marker (make-hash-table)
  "Hash to marker map.")

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with synchronization abilities.")

(defvar org-glance-material-overlay-manager--mutex (make-mutex)
  "Mutex for material overlay manager.")

(defvar org-glance-material-garbage-collector--mutex (make-mutex)
  "Mutex for material garbage collector.")

(defconst org-glance-material-header
  "#  -*- mode: org; mode: org-glance-material -*-

#+ORIGIN: %s:%s

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
  (committed-p nil :type bool   :read-only nil :documentation "Whether changes have been committed.")
  (outdated-p nil  :type bool   :read-only t   :documentation "If there are changes that not reflected in current materialization."))

(cl-defun org-glance-material-store ()
  "Get `org-glance-store' instance associated with current material buffer."
  (or (gethash (current-buffer) org-glance-material--buffer-to-store)
      (puthash (current-buffer) (when-let (origin (condition-case nil
                                                      (save-excursion
                                                        (goto-char (point-min))
                                                        (search-forward "#+ORIGIN: ")
                                                        (buffer-substring-no-properties (point)
                                                                                        (save-excursion
                                                                                          (search-forward ":")
                                                                                          (1- (point)))))
                                                    (search-failed nil)))
                                  (org-glance-store origin))
               org-glance-material--buffer-to-store)))

(cl-defun org-glance-material-offset ()
  "Get actual wal offset associated with current material buffer."
  (condition-case nil
      (save-excursion
        (goto-char (point-min))
        (search-forward "#+ORIGIN: ")
        (search-forward ":")
        (read (buffer-substring-no-properties (point) (point-at-eol))))
    (search-failed nil)))

(define-minor-mode org-glance-material-mode
    "A minor mode to be activated only in materialized view editor.
This is the only point to consistently mutate state of underlying store.
In other places `org-glance-store' should act like functional thread-safe append-only storage."
  nil nil org-glance-material-mode-map
  (cond (org-glance-material-mode
         (let ((store (org-glance-material-store))
               (offset (org-glance-material-offset))
               (buffer (current-buffer)))
           (when (null store)
             (user-error "Unable to start material mode: associated store has not been found."))
           (puthash buffer store org-glance-material--buffer-to-store)
           (puthash buffer offset org-glance-material--buffer-to-offset)
           (org-glance-map (headline)
             (let ((hash (org-glance-headline-hash headline)))
               (cl-destructuring-bind (headline-offset . headline)
                   (org-glance-store-get-latest-offset-of-headline-by-hash store hash)
                 (let ((marker (org-glance-material-marker
                                :hash hash
                                :beg (point-min)
                                :end (point-max)
                                :buffer buffer
                                :overlay nil
                                :changed-p nil
                                :committed-p nil
                                :persisted-p (not (null headline))
                                :outdated-p (if (> headline-offset offset) t nil))))
                   (add-text-properties (point-min) (point-max) (list :marker marker))
                   (save-buffer) ;; FIXME https://ftp.gnu.org/old-gnu/Manuals/elisp-manual-21-2.8/html_node/elisp_530.html
                   (with-mutex org-glance-material-overlay-manager--mutex
                     (puthash marker (point-min) org-glance-material--marker-to-point)
                     (cl-pushnew marker (gethash hash org-glance-material--hash-to-marker)))))))
           (org-glance-material-overlay-manager-redisplay*))
         (add-hook 'post-command-hook #'org-glance-material-debug nil t)
         (add-hook 'after-change-functions #'org-glance-material-edit nil t)
         (add-hook 'before-save-hook #'org-glance-material-commit nil t))
        (t
         (remove-hook 'before-save-hook #'org-glance-material-commit t)
         (remove-hook 'after-change-functions #'org-glance-material-edit t)
         (remove-hook 'post-command-hook #'org-glance-material-debug t))))

(cl-defun org-glance-materialize (store dest)
  "Insert STORE headlines into the DEST and provide ability to push changes
to its origins by calling `org-glance-material-commit'."
  (org-glance--with-temp-file dest
    (insert (org-glance-store-material-header store))
    (cl-loop
       with location = (org-glance-store-location store)
       with wal = (org-glance-store-wal store)
       with watermark = (cl-destructuring-bind (ts _ _) (car (last wal)) ts)
       for hash in (org-glance-store-hashes store)
       for headline = (org-glance-store-headline store hash)
       do (org-glance-headline-insert headline)
       finally return (org-glance-store--create
                       :location location
                       :watermark watermark
                       :wal wal))))

(cl-defun org-glance-material-edit (&rest _)
  "Mark current headline as changed in current buffer."
  (with-mutex org-glance-material-overlay-manager--mutex
    (puthash (get-text-property (point) :marker) (point) org-glance-material--marker-to-point))
  (org-glance-material-overlay-manager-redisplay*))

(cl-defun org-glance-material-overlay-manager-redisplay ()
  "Actualize all overlays in changed material buffers."
  (interactive)
  (with-mutex org-glance-material-overlay-manager--mutex
    (cl-loop for marker being the hash-keys of org-glance-material--marker-to-point using (hash-values point)
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
                       ;;   (puthash marker org-glance-material--changed-markers-set))
                       (org-glance-material-marker-redisplay marker))
                      (returned-to-unchanged-state-p
                       (progn
                         (setf (org-glance-material-marker-changed-p marker) nil)
                         (org-glance-material-marker-redisplay marker)
                         (remhash marker org-glance-material--changed-markers-set)))
                      (first-change-p
                       (progn
                         (setf (org-glance-material-marker-changed-p marker) t
                               (org-glance-material-marker-beg marker) (point-min)
                               (org-glance-material-marker-end marker) (point-max))
                         (org-glance-material-marker-redisplay marker)
                         (puthash marker t org-glance-material--changed-markers-set)))
                      (further-change-p
                       (progn
                         (setf (org-glance-material-marker-changed-p marker) t
                               (org-glance-material-marker-beg marker) (point-min)
                               (org-glance-material-marker-end marker) (point-max))
                         (org-glance-material-marker-redisplay marker)))))))))
       finally do (clrhash org-glance-material--marker-to-point))))

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
        (persisted-p (org-glance-material-marker-persisted-p marker))
        (outdated-p (org-glance-material-marker-outdated-p marker)))
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
          (outdated-p
           (progn
             (let ((overlay (make-overlay beg (1+ beg))))
               (setf (org-glance-material-marker-overlay marker) overlay)
               (overlay-put overlay 'face '(:foreground "#749AF7")))))
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
  (cl-loop with markers = (-non-nil (hash-table-keys org-glance-material--changed-markers-set))
     with store = (org-glance-material-store)
     for marker in markers

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
                       org-glance-material--buffer-to-store)
         (dolist (h&m h&ms) ;; overlay manager side-effects
           (cl-destructuring-bind (headline . marker) h&m
             (with-mutex org-glance-material-overlay-manager--mutex
               (setf (org-glance-material-marker-changed-p marker) nil
                     (org-glance-material-marker-committed-p marker) t
                     (org-glance-material-marker-hash marker) (org-glance-headline-hash headline))
               (org-glance-material-marker-redisplay marker)
               (remhash marker org-glance-material--changed-markers-set)))
           (org-glance-material-overlay-manager-redisplay*))))
  ;; TODO work with deleted buffers in `org-glance-material--buffer-to-store'
  )

(cl-defun org-glance-material-debug (&rest _)
  (when-let (marker (get-text-property (point) :marker))
    (message "%s" marker)))

(cl-defun org-glance-store-material-header (store)
  "Generate materialization header for STORE."
  (format org-glance-material-header
          (org-glance-store-location store)
          (org-glance-store-watermark store)))

(provide 'org-glance-material)
