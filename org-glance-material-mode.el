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

         (let ((store (org-glance-mew:get-buffer-store))
               (mew (org-glance-mew:get-buffer-mew)))

           (org-glance-headline:map (headline)
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
         (add-hook 'after-change-functions #'org-glance-material-mode:update nil t)
         (add-hook 'before-save-hook #'org-glance-material-mode:commit nil t)
         (org-glance-mew:fetch (org-glance-mew:get-buffer-mew)))
        (t
         (remove-hook 'before-save-hook #'org-glance-material-mode:commit t)
         (remove-hook 'after-change-functions #'org-glance-material-mode:update t)
         (remove-hook 'post-command-hook #'org-glance-material-mode:debug t))))

(cl-defun org-glance-material-mode:move-markers (mew beg diff)
  (when (/= 0 diff)
    (dolist (marker (--filter (>= (org-glance-> it :beg) beg)
                              (hash-table-values (org-glance-> mew :markers))))
      (cl-incf (org-glance-> marker :beg) diff)
      (cl-incf (org-glance-> marker :end) diff))))

(cl-defun org-glance-material-mode:normalize-marker (mew marker)
  (org-glance-headline:with-headline-at-point
    (let ((diff (- (point-max) (org-glance-> marker :end))))
      (org-glance-material-mode:move-markers mew (org-glance-> marker :end) diff))
    (setf (org-glance-> marker :beg) (point-min)
          (org-glance-> marker :end) (point-max))))

(cl-defun org-glance-material-mode:update (change-beg change-end pre-change-length)
  "Actualize marker overlay."
  (interactive)
  (thunk-let ((mew (org-glance-buffer:mew))
              (headline (org-glance-headline-at-point))
              (marker (org-glance-marker:at-point change-end))
              (before-first-headline-p (org-glance:before-first-headline-p))
              (method (cond ((< pre-change-length (- change-end change-beg)) 'INSERT)
                            ((> pre-change-length (- change-end change-beg)) 'DELETE)
                            (t 'SKIP))))
    (cond (before-first-headline-p
           (setf (org-glance-> mew :first-headline-pos) (org-glance:first-headline-pos))
           (let ((diff (cl-case method
                         ('INSERT (- change-end change-beg))
                         ('DELETE (- pre-change-length))
                         ('SKIP 0))))
             (org-glance-material-mode:move-markers mew change-beg diff)))
          (t
           (let ((old-state (org-glance-> marker :state))
                 (new-state (org-glance-marker:actualize-state marker headline)))
             (cond ((org-glance-> old-state :corrupted) nil)
                   ((and (org-glance-> old-state :changed) (not (org-glance-> new-state :changed)))
                    (org-glance-material-mode:normalize-marker mew marker)
                    (setf (org-glance-> marker :state :changed) nil)
                    (cl-remf (org-glance-> mew :changes) marker))
                   ((org-glance-> new-state :changed)
                    (org-glance-material-mode:normalize-marker mew marker)
                    (setf (org-glance-> marker :state :changed) t)
                    (cl-pushnew marker (org-glance-> mew :changes))))
             (org-glance-marker:redisplay marker))))))

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
