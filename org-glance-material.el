;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-macs)

(require 'org-glance-headline)
(require 'org-glance-store)
(require 'org-glance-materialization)

(defvar org-glance-overlay-manager nil
  "Painter thread that provides visual feedback.")

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with synchronization abilities.")

(defvar org-glance-material-overlay-manager--mutex (make-mutex)
  "Mutex for material overlay manager.")

(define-minor-mode org-glance-material-mode
    "A minor mode to be activated only in materialized view
editor."
  nil nil org-glance-material-mode-map
  (cond (org-glance-material-mode
         (org-glance:with-m13n
           (org-glance-materialization:prepare-markers m13n))
         (add-hook 'post-command-hook #'org-glance-material-debug nil t)
         (add-hook 'after-change-functions #'org-glance-material-edit nil t)
         (add-hook 'before-save-hook #'org-glance-commit nil t)
         (add-hook 'org-insert-heading-hook #'org-glance-material-new-heading nil t))
        (t
         (remove-hook 'before-save-hook #'org-glance-commit t)
         (remove-hook 'after-change-functions #'org-glance-material-edit t)
         (remove-hook 'post-command-hook #'org-glance-material-debug t)
         (remove-hook 'org-insert-heading-hook #'org-glance-material-new-heading t))))

(cl-defun org-glance-material-new-heading ()
  (message "New heading captured!"))

(cl-defun org-glance-material-edit (&rest _)
  "Mark current headline as changed in current buffer."
  (org-glance:with-m13n
    (org-glance-materialization:edit m13n))
  (org-glance-material-overlay-manager-redisplay*))

(cl-defun org-glance-material-overlay-manager-redisplay ()
  "Actualize all overlays in changed material buffers."
  (interactive)
  (org-glance:with-m13n
    (org-glance-materialization:do-markers m13n
      (let* ((marker (org-glance-marker:at-point))
             (headline (org-glance-headline-at-point))
             (hash-old (org-glance-marker:hash marker))
             (hash-new (org-glance-headline:hash headline))
             (changed-p (org-glance-marker:changed-p marker))
             (persisted-p (org-glance-marker:persisted-p marker))
             (returned-to-unchanged-state-p (and (string= hash-old hash-new) changed-p))
             (first-change-p (and (not (string= hash-old hash-new)) (not changed-p)))
             (further-change-p (and (not (string= hash-old hash-new)) changed-p)))
        (cond
          ((not persisted-p)
           ;; (when (yes-or-no-p "New headline detected. Do you want to add it to store?")
           ;;   (puthash marker org-glance-material--changed-markers-set))
           (org-glance-material-mode:update-marker-overlay marker))
          (returned-to-unchanged-state-p
           (progn
             (setf (org-glance-> marker :changed-p) nil)
             (org-glance-material-mode:update-marker-overlay marker)
             (remhash marker (org-glance-> m13n :changes))))
          (first-change-p
           (progn
             (setf (org-glance-> marker :changed-p) t
                   (org-glance-> marker :beg) (point-min)
                   (org-glance-> marker :end) (point-max))
             (org-glance-material-mode:update-marker-overlay marker)
             (puthash marker t (org-glance-> m13n :changes))))
          (further-change-p
           (progn
             (setf (org-glance-> marker :changed-p) t
                   (org-glance-> marker :beg) (point-min)
                   (org-glance-> marker :end) (point-max))
             (org-glance-material-mode:update-marker-overlay marker))))))))

(cl-defun org-glance-material-overlay-manager-redisplay* ()
  "Run `org-glance-material-marker-redisplay' in separate thread."
  (unless (and (threadp org-glance-overlay-manager)
               (thread-alive-p org-glance-overlay-manager))
    (setq org-glance-overlay-manager (make-thread #'org-glance-material-overlay-manager-redisplay "org-glance-overlay-manager"))))

(cl-defun org-glance-material-mode:update-marker-overlay (marker)
  "Refresh MARKER overlay."
  (with-mutex org-glance-material-overlay-manager--mutex
    (let ((overlay (org-glance-marker:overlay marker))
          (beg (org-glance-marker:beg marker))
          (changed-p (org-glance-marker:changed-p marker))
          (committed-p (org-glance-marker:committed-p marker))
          (persisted-p (org-glance-marker:persisted-p marker))
          (outdated-p (org-glance-marker:outdated-p marker)))
      (cond ((and changed-p (not overlay))
             (progn
               (let ((overlay (make-overlay beg (1+ beg))))
                 (setf (org-glance-> marker :overlay) overlay)
                 (overlay-put overlay 'face '(:foreground "#ffcc00")))))
            ((and changed-p overlay committed-p)
             (progn
               (delete-overlay overlay)
               (let ((overlay (make-overlay beg (1+ beg))))
                 (setf (org-glance-> marker :overlay) overlay
                       (org-glance-> marker :committed-p) nil)
                 (overlay-put overlay 'face '(:foreground "#ffcc00")))))
            ((and (not changed-p) overlay (not committed-p))
             (progn
               (delete-overlay overlay)
               (setf (org-glance-> marker :overlay) nil)))
            ((and (not changed-p) overlay committed-p)
             (progn
               (delete-overlay overlay)
               (let ((overlay (make-overlay beg (1+ beg))))
                 (setf (org-glance-> marker :overlay) overlay)
                 (overlay-put overlay 'face '(:foreground "#27ae60")))))
            (outdated-p
             (progn
               (let ((overlay (make-overlay beg (1+ beg))))
                 (setf (org-glance-> marker :overlay) overlay)
                 (overlay-put overlay 'face '(:foreground "#749AF7")))))
            ((and (not persisted-p) (not overlay))
             (progn
               (let ((overlay (make-overlay beg (1+ beg))))
                 (setf (org-glance-> marker :overlay) overlay)
                 (overlay-put overlay 'face '(:foreground "#e74c3c")))))))))

(cl-defun org-glance-commit ()
  "Apply all changes of buffer headlines to its origins.
Returns new store with changes reflected in WAL.

TODO:
- It should be generalized to other materialization types."
  (interactive)
  (org-glance:with-store
    (org-glance-materialization:do-changes (org-glance-materialization:get-buffer-materialization)
      (let* ((marker (org-glance-marker:at-point))
             (headline (org-glance-headline-from-region
                        (org-glance-marker:beg marker)
                        (org-glance-marker:end marker))))
        (org-glance-store:put store headline)))
    (org-glance-store:flush store)))

(cl-defun org-glance-material-debug (&rest _)
  (when-let (marker (org-glance-marker:at-point))
    (org-glance-marker:print marker)))

(provide 'org-glance-material)
