;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-macs)
(require 'thunk)
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
         (org-glance-mew:mark-current-buffer)
         (org-glance-mew:fetch)

         (add-hook 'post-command-hook #'org-glance-material-mode:debug nil t)
         (add-hook 'after-change-functions #'org-glance-material-mode:update nil t)
         (add-hook 'before-save-hook #'org-glance-mew:commit nil t))
        (t
         (remove-hook 'before-save-hook #'org-glance-mew:commit t)
         (remove-hook 'after-change-functions #'org-glance-material-mode:update t)
         (remove-hook 'post-command-hook #'org-glance-material-mode:debug t))))

(cl-defun org-glance-material-mode:update (change-beg change-end pre-change-length)
  "Actualize marker overlay."
  (interactive)
  (thunk-let ((mew (org-glance-buffer:mew))
              (headline (org-glance-headline-at-point))
              (marker (org-glance-marker:at-point change-end))
              (before-first-headline-p (org-glance:before-first-headline-p)))
    (cond (before-first-headline-p
           (let ((diff (cond ((< pre-change-length (- change-end change-beg)) (- change-end change-beg))
                             ((> pre-change-length (- change-end change-beg)) (- pre-change-length))
                             (t 0))))
             (org-glance-mew:move-markers mew change-beg diff)))
          (t
           (let ((old-state (org-glance-> marker :state))
                 (new-state (org-glance-marker:actualize-state marker headline)))
             (cond ((org-glance-> old-state :corrupted) nil)
                   ((and (org-glance-> old-state :changed) (not (org-glance-> new-state :changed)))
                    (org-glance-mew:normalize-marker mew marker)
                    (setf (org-glance-> marker :state :changed) nil)
                    (cl-remf (org-glance-> mew :changes) marker))
                   ((org-glance-> new-state :changed)
                    (org-glance-mew:normalize-marker mew marker)
                    (setf (org-glance-> marker :state :changed) t)
                    (cl-pushnew marker (org-glance-> mew :changes))))
             (org-glance-marker:redisplay marker))))))

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
