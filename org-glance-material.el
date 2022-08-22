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
         (let ((store (org-glance-materialization:get-buffer-store)))
           (org-glance-headline:map-buffer (headline)
             (let* ((hash (org-glance-headline:hash headline))
                    (marker (org-glance-marker
                             :hash hash
                             :beg (point-min) ;; beginning of headline in narrowed buffer
                             :end (point-max) ;; end of headline in narrowed buffer
                             :buffer (current-buffer)
                             :state (org-glance-marker-state
                                     ;; FIXME Getting full headline is unneccessary
                                     :corrupted (null (org-glance-store:in store hash))
                                     ))))
               (add-text-properties (point-min) (point-max) (list :marker marker))
               (org-glance-marker:redisplay marker)
               ;; FIXME https://ftp.gnu.org/old-gnu/Manuals/elisp-manual-21-2.8/html_node/elisp_530.html
               ;; (save-buffer)
               )))

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
  ;; (org-glance-material-overlay-manager-redisplay*)
  (org-glance-material-overlay-manager-redisplay))

(cl-defun org-glance-material-overlay-manager-redisplay ()
  "Actualize marker overlay."
  (interactive)
  (let* ((materialization (org-glance-buffer-materialization))
         (marker (org-glance-marker:at-point))
         (headline (org-glance-headline-at-point))
         (old-state (org-glance-> marker :state))
         (new-state (org-glance-marker:get-actual-state marker headline)))
    (cond
      ((org-glance-> old-state :corrupted)
       ;; (when (yes-or-no-p "New headline detected. Do you want to add it to store?")
       ;;   (puthash marker org-glance-material--changed-markers-set))
       (org-glance-marker:redisplay marker))
      ((and (org-glance-> old-state :changed)
            (not (org-glance-> new-state :changed)))
       (setf (org-glance-> marker :state :changed) nil)
       (cl-remf (org-glance-> materialization :changes) marker)
       (org-glance-marker:redisplay marker))
      ((org-glance-> new-state :changed)
       (org-glance--with-headline-at-point
         (setf (org-glance-> marker :state :changed) t
               (org-glance-> marker :beg) (point-min)
               (org-glance-> marker :end) (point-max)))
       (cl-pushnew marker (org-glance-> materialization :changes))
       (org-glance-marker:redisplay marker)))))

(cl-defun org-glance-material-overlay-manager-redisplay* ()
  "Run `org-glance-material-marker-redisplay' in separate thread."
  (unless (and (threadp org-glance-overlay-manager)
               (thread-alive-p org-glance-overlay-manager))
    (setq org-glance-overlay-manager
          (make-thread #'org-glance-material-overlay-manager-redisplay "org-glance-overlay-manager"))))

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
