(require 'org-glance-generic)
(require 'org-glance-headline)
(require 'org-glance-store)

(defvar org-glance-material-painter nil
  "Timer instance.")

(defvar org-glance-material-origins (make-hash-table)
  "Buffer to origin alist.")

(defvar org-glance-material-points* (make-hash-table)
  "Changed points.")

(defvar org-glance-material-markers* (make-hash-table)
  "Changed markers.")

(defvar org-glance-material-paint-q (list)
  "Marker queue to be processed by `org-glance-material-painter'.")

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with synchronization abilities.")

(defvar org-glance-material-mutex (make-mutex "org-glance-material-mutex")
  "Thread synchronization.")

(cl-defstruct (org-glance-material-marker (:constructor org-glance-material-marker)
                                          (:copier nil))
  "Sync metadata about headlines to be processed and visualized by `org-glance-material-painter'."
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
                             (org-glance-material-marker
                              :hash hash
                              :beg (point-min)
                              :end (point-max)
                              :buffer buffer
                              :overlay nil
                              :changed-p nil
                              :committed-p nil
                              :persisted-p (when (org-glance-store-get store hash) t))))))
           (with-mutex org-glance-material-mutex ;; mutation. Possible conflicts with painters/other threads
             (puthash buffer store org-glance-material-origins)
             (cl-loop for marker in (reverse markers) ;; reverse to be user-friendly and mark visible headlines first
                do (push marker org-glance-material-paint-q)
                  (add-text-properties (org-glance-material-marker-beg marker)
                                       (org-glance-material-marker-end marker)
                                       (list :marker marker))))
           (org-glance-material-redisplay*))
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
    (insert (format "#  -*- mode: org; mode: org-glance-material -*-

#+ORIGIN: %s

"
                    (org-glance-store-location store)))
    (cl-loop for headline in (org-glance-headlines store)
       do
         (let* ((headline (org-glance-store-headline-full store headline)))
           (-> headline
               ;; (org-glance-headline-set-org-properties "Hash" (org-glance-hash headline))
               (org-glance-headline-insert))))))

(cl-defun org-glance-material-edit (&rest _)
  "Mark current headline as changed in current buffer."
  (puthash (cons (current-buffer) (point)) t org-glance-material-points*)
  (org-glance-material-redisplay*))

(cl-defun org-glance-material-redisplay* ()
  (unless (and org-glance-material-painter (thread-alive-p org-glance-material-painter))
    (setq org-glance-material-painter (make-thread #'org-glance-material-redisplay "org-glance-material-painter"))))

(cl-defun org-glance-material-redisplay ()
  (with-mutex org-glance-material-mutex
    (when org-glance-material-points*
      (cl-loop for bufpoint being the hash-keys of org-glance-material-points*
         for buffer = (car bufpoint)
         for point = (cdr bufpoint)
         do (with-current-buffer buffer
              (save-excursion
                (goto-char point)
                (org-glance--with-heading-at-point
                  (let* ((marker (get-text-property point :marker))
                         (hash-old (org-glance-material-marker-hash marker))
                         (changed-p (org-glance-material-marker-changed-p marker))
                         (persisted-p (org-glance-material-marker-persisted-p marker))
                         (overlay (org-glance-material-marker-overlay marker))
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
                       (progn
                         (setf (org-glance-material-marker-changed-p marker) nil)
                         (push marker org-glance-material-paint-q)
                         (remhash marker org-glance-material-markers*)))
                      (first-change-p
                       (progn
                         (setf (org-glance-material-marker-changed-p marker) t
                               (org-glance-material-marker-beg marker) (point-min)
                               (org-glance-material-marker-end marker) (point-max))
                         (push marker org-glance-material-paint-q)
                         (puthash marker t org-glance-material-markers*)))
                      (further-change-p
                       (progn
                         (setf (org-glance-material-marker-changed-p marker) t
                               (org-glance-material-marker-beg marker) (point-min)
                               (org-glance-material-marker-end marker) (point-max))
                         (push marker org-glance-material-paint-q))))))))
         finally do (clrhash org-glance-material-points*)))

    (when org-glance-material-paint-q
      (cl-loop for marker in org-glance-material-paint-q
         do (let ((hash (org-glance-material-marker-hash marker))
                  (buffer (org-glance-material-marker-buffer marker))
                  (overlay (org-glance-material-marker-overlay marker))
                  (beg (org-glance-material-marker-beg marker))
                  (changed-p (org-glance-material-marker-changed-p marker))
                  (committed-p (org-glance-material-marker-committed-p marker))
                  (persisted-p (org-glance-material-marker-persisted-p marker)))
              (when (and (bufferp buffer) (buffer-live-p buffer))
                (with-current-buffer buffer
                  (cond ((and changed-p (not overlay)) (with-mutex org-glance-material-mutex
                                                         (let ((overlay (make-overlay beg (1+ beg))))
                                                           (setf (org-glance-material-marker-overlay marker) overlay)
                                                           (overlay-put overlay 'face '(:foreground "#ffcc00")))))
                        ((and changed-p overlay committed-p) (with-mutex org-glance-material-mutex
                                                               (delete-overlay overlay)
                                                               (let ((overlay (make-overlay beg (1+ beg))))
                                                                 (setf (org-glance-material-marker-overlay marker) overlay
                                                                       (org-glance-material-marker-committed-p marker) nil)
                                                                 (overlay-put overlay 'face '(:foreground "#ffcc00")))))
                        ((and (not changed-p) overlay (not committed-p)) (with-mutex org-glance-material-mutex
                                                                           (delete-overlay overlay)
                                                                           (setf (org-glance-material-marker-overlay marker) nil)))
                        ((and (not changed-p) overlay committed-p) (with-mutex org-glance-material-mutex
                                                                     (delete-overlay overlay)
                                                                     (let ((overlay (make-overlay beg (1+ beg))))
                                                                       (setf (org-glance-material-marker-overlay marker) overlay)
                                                                       (overlay-put overlay 'face '(:foreground "#27ae60")))))
                        ((and (not persisted-p) (not overlay)) (with-mutex org-glance-material-mutex
                                                                 (let ((overlay (make-overlay beg (1+ beg))))
                                                                   (setf (org-glance-material-marker-overlay marker) overlay)
                                                                   (overlay-put overlay 'face '(:foreground "#e74c3c")))))))))
         finally do (setq org-glance-material-paint-q nil)))))

(cl-defun org-glance-material-commit ()
  "Apply all changes of buffer headlines to its origins in STORE.

TODO:
- It should be generalized to other materialization types.
- [x] Rebuild store indexes.
- Return store."
  (interactive)
  (with-mutex org-glance-material-mutex
    (let ((store (gethash (current-buffer) org-glance-material-origins)))
      (cl-loop for marker being the hash-keys of org-glance-material-markers*
         when (eq (current-buffer) (org-glance-material-marker-buffer marker))
         do (let ((headline (org-glance-headline-from-string
                             (buffer-substring-no-properties
                              (org-glance-material-marker-beg marker)
                              (org-glance-material-marker-end marker)))))
              (setq store (-> store
                              ;; (org-glance-store-rem (org-glance-material-marker-hash marker))
                              (org-glance-store-put headline)))
              (setf (org-glance-material-marker-changed-p marker) nil
                    (org-glance-material-marker-committed-p marker) t
                    (org-glance-material-marker-hash marker) (org-glance-headline-hash headline))
              (push marker org-glance-material-paint-q)
              (remhash marker org-glance-material-markers*)))
      (org-glance-material-redisplay*)
      store))

  ;; TODO remove old headline from store or mark for deletion
  ;; TODO work with stale links (broken)
  )

(cl-defun org-glance-material-debug (&rest _)
  (prin1 (get-text-property (point) :marker)))

(provide 'org-glance-material-mode)
