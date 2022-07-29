(require 'org-glance-generic)
(require 'org-glance-headline)
(require 'org-glance-store)

(defvar org-glance-material-mode-timer nil
  "Timer instance.")

(defvar org-glance-material-mode-origins (make-hash-table)
  "Buffer to origin alist.")

(defvar org-glance-material-mode-changes (make-hash-table)
  "Contains changed markers.")

(defvar org-glance-material-mode-marker-queue (list)
  "Marker queue to be processed by `org-glance-material-mode-timer'.")

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with synchronization abilities.")

(defvar org-glance-material-mode-mutex (make-mutex "org-glance-material-mode-mutex")
  "Thread synchronization.")

(cl-defstruct (org-glance-material-mode-marker (:constructor org-glance-material-mode-marker)
                                               (:copier nil))
  "Sync metadata about headlines to be processed and visualized by `org-glance-material-mode-timer'."
  (beg nil         :type number :read-only nil :documentation "Beginning of headline.")
  (end nil         :type number :read-only nil :documentation "End of headline.")
  (buffer nil      :type buffer :read-only t   :documentation "Materialized buffer.")
  (hash nil        :type string :read-only nil :documentation "Hash of headline origin.")
  (overlay nil     :type object :read-only nil :documentation "Current overlay for status reporting.")
  (changed-p nil   :type bool   :read-only nil :documentation "Has headline changed?")
  (persisted-p nil :type bool   :read-only t   :documentation "Whether headline has origin or not.")
  (committed-p nil :type bool   :read-only nil :documentation "Whether changes have been committed."))

(define-minor-mode org-glance-material-mode
    "A minor mode to be activated only in materialized view editor."
  nil nil org-glance-material-mode-map
  (cond (org-glance-material-mode
         (let* ((store (org-glance-store (save-excursion
                                           (goto-char (point-min))
                                           (search-forward "#+ORIGIN: ")
                                           (buffer-substring-no-properties (point) (point-at-eol)))))
                (buffer (current-buffer))
                (markers (org-glance-map (headline)
                           (let ((hash (org-glance-hash headline)))
                             (org-glance-material-mode-marker
                              :hash hash
                              :beg (point-min)
                              :end (point-max)
                              :buffer buffer
                              :overlay nil
                              :changed-p nil
                              :committed-p nil
                              :persisted-p (when (org-glance-store-get store hash) t))))))
           (with-mutex org-glance-material-mode-mutex ;; mutation. Possible conflicts with timers/other threads
             (puthash buffer store org-glance-material-mode-origins)
             (cl-loop for marker in (reverse markers) ;; reverse to be user-friendly and mark visible headlines first
                do (push marker org-glance-material-mode-marker-queue)
                  (add-text-properties (org-glance-material-mode-marker-beg marker)
                                       (org-glance-material-mode-marker-end marker)
                                       (list :marker marker)))))
         (unless org-glance-material-mode-timer
           (setq org-glance-material-mode-timer (run-with-timer 0.1 0.1 #'org-glance-material-mode-timer-tick)))
         (add-hook 'post-command-hook #'org-glance-material-mode-debug nil t)
         (add-hook 'after-change-functions #'org-glance-material-mode-edit nil t)
         (add-hook 'before-save-hook #'org-glance-material-mode-commit nil t))
        (t
         (remove-hook 'before-save-hook #'org-glance-material-mode-commit t)
         (remove-hook 'after-change-functions #'org-glance-material-mode-edit t)
         (remove-hook 'post-command-hook #'org-glance-material-mode-debug t))))

(cl-defun org-glance-material-mode-edit (&rest _)
  "Mark current headline as changed in current buffer."
  (org-glance--with-heading-at-point
    (let* ((marker (get-text-property (point) :marker))
           (hash-old (org-glance-material-mode-marker-hash marker))
           (changed-p (org-glance-material-mode-marker-changed-p marker))
           (persisted-p (org-glance-material-mode-marker-persisted-p marker))
           (overlay (org-glance-material-mode-marker-overlay marker))
           (headline (org-glance-headline-at-point)) ;; FIXME do not construct headline in future, optimize me
           (hash-new (org-glance-hash headline))
           (returned-to-unchanged-state-p (and (string= hash-old hash-new) changed-p))
           (first-change-p (and (not (string= hash-old hash-new)) (not changed-p)))
           (further-change-p (and (not (string= hash-old hash-new)) changed-p)))
      (cond
        ((not persisted-p)
         ;; skip it or prompt user to add it to store
         ;; (when (yes-or-no-p "Attempt to change unsynced headline. Do you want to add it to store?")
         ;;   ;; TODO sync it
         ;;   )
         )
        (returned-to-unchanged-state-p
         (with-mutex org-glance-material-mode-mutex
           (setf (org-glance-material-mode-marker-changed-p marker) nil)
           (push marker org-glance-material-mode-marker-queue)
           (remhash marker org-glance-material-mode-changes)))
        (first-change-p
         (with-mutex org-glance-material-mode-mutex
           (setf (org-glance-material-mode-marker-changed-p marker) t
                 (org-glance-material-mode-marker-beg marker) (point-min)
                 (org-glance-material-mode-marker-end marker) (point-max))
           (push marker org-glance-material-mode-marker-queue)
           (puthash marker t org-glance-material-mode-changes)))
        (further-change-p
         (with-mutex org-glance-material-mode-mutex
           (setf (org-glance-material-mode-marker-changed-p marker) t
                 (org-glance-material-mode-marker-beg marker) (point-min)
                 (org-glance-material-mode-marker-end marker) (point-max))
           (push marker org-glance-material-mode-marker-queue)))))))

(cl-defun org-glance-material-mode-timer-tick ()
  (when org-glance-material-mode-marker-queue
    (when-let (marker (with-mutex org-glance-material-mode-mutex
                        (pop org-glance-material-mode-marker-queue)))
      (let ((hash (org-glance-material-mode-marker-hash marker))
            (buffer (org-glance-material-mode-marker-buffer marker))
            (overlay (org-glance-material-mode-marker-overlay marker))
            (beg (org-glance-material-mode-marker-beg marker))
            (changed-p (org-glance-material-mode-marker-changed-p marker))
            (committed-p (org-glance-material-mode-marker-committed-p marker))
            (persisted-p (org-glance-material-mode-marker-persisted-p marker)))
        (when (and (bufferp buffer) (buffer-live-p buffer))
          (with-current-buffer buffer
            (cond ((and changed-p (not overlay)) (with-mutex org-glance-material-mode-mutex
                                                   (let ((overlay (make-overlay beg (1+ beg))))
                                                     (setf (org-glance-material-mode-marker-overlay marker) overlay)
                                                     (overlay-put overlay 'face '(:foreground "#ffcc00")))))
                  ((and changed-p overlay committed-p) (with-mutex org-glance-material-mode-mutex
                                                         (delete-overlay overlay)
                                                         (let ((overlay (make-overlay beg (1+ beg))))
                                                           (setf (org-glance-material-mode-marker-overlay marker) overlay
                                                                 (org-glance-material-mode-marker-committed-p marker) nil)
                                                           (overlay-put overlay 'face '(:foreground "#ffcc00")))))
                  ((and (not changed-p) overlay (not committed-p)) (with-mutex org-glance-material-mode-mutex
                                                                     (delete-overlay overlay)
                                                                     (setf (org-glance-material-mode-marker-overlay marker) nil)))
                  ((and (not changed-p) overlay committed-p) (with-mutex org-glance-material-mode-mutex
                                                               (delete-overlay overlay)
                                                               (let ((overlay (make-overlay beg (1+ beg))))
                                                                 (setf (org-glance-material-mode-marker-overlay marker) overlay)
                                                                 (overlay-put overlay 'face '(:foreground "#27ae60")))))
                  ((and (not persisted-p) (not overlay)) (with-mutex org-glance-material-mode-mutex
                                                           (let ((overlay (make-overlay beg (1+ beg))))
                                                             (setf (org-glance-material-mode-marker-overlay marker) overlay)
                                                             (overlay-put overlay 'face '(:foreground "#e74c3c"))))))))))))

(cl-defun org-glance-material-mode-commit ()
  "Apply all changes of buffer headlines to its origins in STORE.

TODO:
- It should be generalized to other materialization types.
- [x] Rebuild store indexes.
- Return store."
  (interactive)
  (with-mutex org-glance-material-mode-mutex
    (cl-loop for marker being the hash-keys of org-glance-material-mode-changes
       with store = (gethash (current-buffer) org-glance-material-mode-origins)
       when (eq (current-buffer) (org-glance-material-mode-marker-buffer marker))
       collect (let ((headline (org-glance-headline-from-string
                                (buffer-substring-no-properties
                                 (org-glance-material-mode-marker-beg marker)
                                 (org-glance-material-mode-marker-end marker)))))
                 (setq store (org-glance-store-put store headline)) ;; FIXME optimize
                 (setf (org-glance-material-mode-marker-changed-p marker) nil
                       (org-glance-material-mode-marker-committed-p marker) t
                       (org-glance-material-mode-marker-hash marker) (org-glance-headline-hash headline))
                 (push marker org-glance-material-mode-marker-queue)
                 marker)
       into markers
       finally return (progn (dolist (marker markers)
                               (remhash marker org-glance-material-mode-changes))
                             store)))

  ;; TODO remove old headline from store or mark for deletion
  ;; TODO work with stale links (broken)
  )

(cl-defun org-glance-material-mode-debug (&rest _)
  (prin1 (get-text-property (point) :marker))
  ;; (message "%d %s %s %s %s"
  ;;          (point)
  ;;          org-glance-material-mode-markers
  ;;          (get-text-property (point) :changed-p)
  ;;          (get-text-property (point) :synced-p)
  ;;          (get-text-property (point) :hash))
  )

(provide 'org-glance-material-mode)
