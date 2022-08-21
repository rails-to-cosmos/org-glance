;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-macs)

(require 'org-glance-headline)
(require 'org-glance-materialization)
(require 'org-glance-store)

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
         (let ((materialization (org-glance-buffer-materialization)))
           (org-glance-marker:map-buffer (marker)
             (add-text-properties (point-min) (point-max) (list :marker marker))
             ;; FIXME https://ftp.gnu.org/old-gnu/Manuals/elisp-manual-21-2.8/html_node/elisp_530.html
             ;; (save-buffer)
             (puthash marker (point-min) (org-glance-> materialization :marker->point))))
         (add-hook 'post-command-hook #'org-glance-material-debug nil t)
         ;; (add-hook 'after-change-functions #'org-glance-material-edit nil t)
         (add-hook 'before-save-hook #'org-glance-commit nil t)
         (add-hook 'org-insert-heading-hook #'org-glance-material-new-heading nil t))
        (t
         (remove-hook 'before-save-hook #'org-glance-commit t)
         ;; (remove-hook 'after-change-functions #'org-glance-material-edit t)
         (remove-hook 'post-command-hook #'org-glance-material-debug t)
         (remove-hook 'org-insert-heading-hook #'org-glance-material-new-heading t))))

(cl-defun org-glance-material-new-heading ()
  (message "New heading captured!"))

(cl-defun org-glance-material-edit (&rest _)
  "Mark current headline as changed in current buffer."
  (let ((materialization (org-glance-buffer-materialization)))
    (org-glance-materialization:update materialization))
  ;; (org-glance-material-overlay-manager-redisplay*)
  (org-glance-material-overlay-manager-redisplay)
  )

(cl-defun org-glance-material-overlay-manager-redisplay ()
  "Actualize all overlays in changed material buffers."
  (interactive)
  (let ((materialization (org-glance-buffer-materialization)))
    (org-glance-materialization:do-markers materialization
      (let* ((marker (org-glance-marker:at-point))
             (headline (org-glance-headline-at-point))
             (old-state (org-glance-> marker :state))
             (new-state (org-glance-marker:get-actual-state marker headline)))
        (cond
          ((not (org-glance-> old-state :persisted))
           ;; (when (yes-or-no-p "New headline detected. Do you want to add it to store?")
           ;;   (puthash marker org-glance-material--changed-markers-set))
           (org-glance-material-mode:update-marker-overlay marker))
          ((and (not (org-glance-> new-state :changed))
                (org-glance-> marker :state :changed))
           (setf (org-glance-> marker :state :changed) nil)
           (org-glance-material-mode:update-marker-overlay marker)
           (cl-remf (org-glance-> materialization :changes) marker))
          ((org-glance-> new-state :changed)
           (setf (org-glance-> marker :state :changed) t
                 (org-glance-> marker :beg) (point-min)
                 (org-glance-> marker :end) (point-max))
           (org-glance-material-mode:update-marker-overlay marker)
           (cl-pushnew marker (org-glance-> materialization :changes))))))))

(cl-defun org-glance-material-overlay-manager-redisplay* ()
  "Run `org-glance-material-marker-redisplay' in separate thread."
  (unless (and (threadp org-glance-overlay-manager)
               (thread-alive-p org-glance-overlay-manager))
    (setq org-glance-overlay-manager
          (make-thread #'org-glance-material-overlay-manager-redisplay "org-glance-overlay-manager"))))

(cl-defun org-glance-material-mode:update-marker-overlay (marker)
  "Refresh MARKER overlay."
  (let ((beg (org-glance-> marker :beg))
        (marked (slot-boundp marker :overlay))
        (changed (org-glance-> marker :state :changed))
        (committed (org-glance-> marker :state :committed))
        (persisted (org-glance-> marker :state :persisted))
        (outdated (org-glance-> marker :state :outdated)))
    (cond ((and changed (not marked))
           (let ((overlay (make-overlay beg (1+ beg))))
             (setf (org-glance-> marker :overlay) overlay)
             (overlay-put overlay 'face '(:foreground "#ffcc00"))))
          ((and changed committed marked)
           (let ((overlay (make-overlay beg (1+ beg))))
             (delete-overlay (org-glance-> marker :overlay))
             (slot-makeunbound marker :overlay)
             (setf (org-glance-> marker :overlay) overlay
                   (org-glance-> marker :state :committed) nil)
             (overlay-put overlay 'face '(:foreground "#ffcc00"))))
          ((and (not changed) (not committed) marked)
           (progn
             (delete-overlay (org-glance-> marker :overlay))
             (slot-makeunbound marker :overlay)))
          ((and (not changed) marked committed)
           (progn
             (delete-overlay (org-glance-> marker :overlay))
             (let ((overlay (make-overlay beg (1+ beg))))
               (setf (org-glance-> marker :overlay) overlay)
               (overlay-put overlay 'face '(:foreground "#27ae60")))))
          (outdated
           (progn
             (let ((overlay (make-overlay beg (1+ beg))))
               (setf (org-glance-> marker :overlay) overlay)
               (overlay-put overlay 'face '(:foreground "#749AF7")))))
          ((and (not persisted) (not marked))
           (let ((overlay (make-overlay beg (1+ beg))))
             (setf (org-glance-> marker :overlay) overlay)
             (overlay-put overlay 'face '(:foreground "#e74c3c")))))))

(cl-defun org-glance-commit ()
  "Apply all changes of buffer headlines to its origins.

TODO:
- It should be generalized to other materialization types."
  (interactive)
  (org-glance-materialization:commit
   (org-glance-buffer-materialization)))

(cl-defun org-glance-material-debug (&rest _)
  (when-let (marker (org-glance-marker:at-point))
    (hlt-unhighlight-region)

    (when (member this-command '(org-self-insert-command org-delete-backward-char))
      (org-glance-material-edit))

    (org-glance-debug
     this-command
     "* Marker:"
     (org-glance-marker:prin1-to-string marker)
     "* Changes:"
     (s-join "\n" (mapcar #'org-glance-marker:prin1-to-string
                          (org-glance-> (org-glance-buffer-materialization)
                            :changes))))

    (hlt-highlight-region (org-glance-> marker :beg)
                          (org-glance-> marker :end)
                          'region)))

(provide 'org-glance-material)
