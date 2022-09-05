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
         (org-glance-mew:mark-buffer)
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
  (thunk-let* ((mew (org-glance-mew:current))
               (midx (org-glance-mew:marker-at-point mew change-beg))
               (diff (cond ((< pre-change-length (- change-end change-beg)) (- change-end change-beg))
                           ((> pre-change-length (- change-end change-beg)) (- pre-change-length))
                           (t 0))))
    (org-glance-mew:set-marker-changed mew midx t)
    (cl-loop for i from (1+ midx) below (length (org-glance-> mew :marker-positions))
       do (aset (org-glance-> mew :marker-positions) i (+ (aref (org-glance-> mew :marker-positions) i) diff)))))

(cl-defun org-glance-material-mode:debug (&rest _)
  (let ((mew (org-glance-mew:current))
        (midx (org-glance-mew:marker-at-point)))
    (hlt-unhighlight-region)

    (org-glance-debug
     (point)
     midx
     (format "Changed: %s" (org-glance-mew:marker-changed-p mew midx))
     (format "Corrupted: %s" (org-glance-mew:marker-corrupted-p mew midx))
     (format "Committed: %s" (org-glance-mew:marker-committed-p mew midx))
     (org-glance-mew:get-marker-position mew midx)
     (org-glance-mew:get-marker-position mew (1+ midx))
     (org-glance-> mew :marker-positions))

    (when (> midx -1)
      (hlt-highlight-region (org-glance-mew:get-marker-position mew midx)
                            (org-glance-mew:get-marker-position mew (1+ midx))
                            'region))
    ))

(provide 'org-glance-material-mode)
