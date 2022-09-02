;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-macs)
(require 'highlight)

(require 'org-glance-headline)
(require 'org-glance-mew)
(require 'org-glance-store)

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with synchronization abilities.")

(define-minor-mode org-glance-material-mode
    "A minor mode to be activated only in materialized view
editor."
  nil nil org-glance-material-mode-map
  (cond (org-glance-material-mode

         (let ((store (org-glance-mew:get-buffer-store))
               (mew (org-glance-mew:get-buffer-mew)))

           (org-glance-headline:map-buffer (headline)
             (org-glance-mew:create-marker mew (org-glance-> headline :hash))
             ;; FIXME https://ftp.gnu.org/old-gnu/Manuals/elisp-manual-21-2.8/html_node/elisp_530.html
             ;; (save-buffer)
             )

           ;; do we really need this hack?
           (add-text-properties (1- (point-max)) (point-max)
                                (list :marker (save-excursion
                                                (goto-char (point-max))
                                                (org-back-to-heading-or-point-min)
                                                (get-text-property (point) :marker)))))

         (add-hook 'post-command-hook #'org-glance-material-mode:debug nil t)
         ;; (add-hook 'post-command-hook #'org-glance-material-mode:after-change nil t)
         ;; (add-hook 'org-insert-heading-hook #'org-glance-material-mode:update nil t)
         ;; (add-hook 'org-after-todo-state-change-hook #'org-glance-material-mode:update nil t)
         ;; (add-hook 'org-after-tags-change-hook #'org-glance-material-mode:update nil t)
         (add-hook 'after-change-functions #'org-glance-material-mode:after-change nil t)
         (add-hook 'before-save-hook #'org-glance-material-mode:commit nil t)
         (org-glance-mew:fetch (org-glance-mew:get-buffer-mew)))
        (t
         (remove-hook 'before-save-hook #'org-glance-material-mode:commit t)
         ;; (remove-hook 'org-after-tags-change-hook #'org-glance-material-mode:update t)
         ;; (remove-hook 'post-command-hook #'org-glance-material-mode:after-change t)
         ;; (remove-hook 'org-insert-heading-hook #'org-glance-material-mode:update t)
         ;; (remove-hook 'org-after-todo-state-change-hook #'org-glance-material-mode:update t)
         (remove-hook 'after-change-functions #'org-glance-material-mode:after-change t)
         (remove-hook 'post-command-hook #'org-glance-material-mode:debug t))))

(cl-defun org-glance-material-mode:after-change (&rest args)
  "Mark current headline as changed in current buffer."
  (message "This command: %s" this-command)
  (message "Last command: %s" last-command)
  (message "Command args: %s" args)
  (message "Was: %s" (org-glance-> (org-glance-marker:at-point) :hash))
  (message "Then: %s" (org-glance-> (org-glance-headline-at-point) :hash))
  (when (member this-command '(org-self-insert-command org-delete-backward-char))
    (org-glance-material-mode:update)))

(cl-defun org-glance-material-mode:update (&rest _)
  "Actualize marker overlay."
  (interactive)
  (let* ((mew (org-glance-buffer:mew))
         (marker (org-glance-marker:at-point))
         (headline (org-glance-headline-at-point))
         (old-state (org-glance-> marker :state))
         (new-state (org-glance-marker:get-actual-state marker headline)))
    (cond
      ((org-glance-> old-state :corrupted)
       ;; (when (yes-or-no-p "New headline detected. Do you want to add it to store or remove from mew?")
       ;;   )
       (org-glance-marker:redisplay marker))
      ((and (org-glance-> old-state :changed)
            (not (org-glance-> new-state :changed)))
       (setf (org-glance-> marker :state :changed) nil)
       (cl-remf (org-glance-> mew :changes) marker)
       (org-glance-marker:redisplay marker))
      ((org-glance-> new-state :changed)
       (org-glance--with-headline-at-point
         (let ((beg-diff (- (point-min) (org-glance-> marker :beg)))
               (end-diff (- (point-max) (org-glance-> marker :end))))
           ;; TODO beg diff
           (when (/= 0 end-diff)
             (dolist (marker (--filter (> (org-glance-> it :end) (org-glance-> marker :end))
                                       (hash-table-values (org-glance-> mew :markers))))
               (cl-incf (org-glance-> marker :beg) end-diff))))
         (setf (org-glance-> marker :state :changed) t
               (org-glance-> marker :beg) (point-min)
               (org-glance-> marker :end) (point-max)))
       (cl-pushnew marker (org-glance-> mew :changes))
       (org-glance-marker:redisplay marker)))))

;; (cl-defun org-glance-material-mode:update* ()
;;   "Run `org-glance-material-marker-redisplay' in separate thread."
;;   (unless (and (threadp org-glance-overlay-manager)
;;                (thread-alive-p org-glance-overlay-manager))
;;     (setq org-glance-overlay-manager
;;           (make-thread #'org-glance-material-mode:update "org-glance-overlay-manager"))))

(cl-defun org-glance-material-mode:commit ()
  "Apply all changes of buffer headlines to its origins.

TODO:
- It should be generalized to other mew types."
  (interactive)
  (org-glance-mew:commit (org-glance-buffer:mew)))

(cl-defun org-glance-material-mode:debug (&rest _)
  (when-let (marker (org-glance-marker:at-point))
    (hlt-unhighlight-region)
    (org-glance-debug
     this-command
     "* Marker:"
     (org-glance-marker:prin1-to-string marker)
     "* Changes:"
     (s-join "\n" (mapcar #'org-glance-marker:prin1-to-string
                          (org-glance-> (org-glance-buffer:mew)
                            :changes))))

    (hlt-highlight-region (org-glance-> marker :beg)
                          (org-glance-> marker :end)
                          'region)))

(provide 'org-glance-material-mode)