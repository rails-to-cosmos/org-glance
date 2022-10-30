;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-macs)
(require 'highlight)

(require 'org-glance-log)
(require 'org-glance-headline)
(require 'org-glance-view)

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with synchronization abilities.")

(define-minor-mode org-glance-material-mode
    "A minor mode to be activated only in materialized view
editor."
  nil nil org-glance-material-mode-map
  (cond (org-glance-material-mode
         (org-glance-log :performance
             (org-glance-view:mark-buffer))
         (add-hook 'after-change-functions #'org-glance-material-mode:update nil t)
         (add-hook 'before-save-hook #'org-glance-view:commit nil t)
         (org-overview))
        (t
         (remove-hook 'before-save-hook #'org-glance-view:commit t)
         (remove-hook 'after-change-functions #'org-glance-material-mode:update t))))

(cl-defun org-glance-material-mode:update (change-beg change-end pre-change-length)
  "Actualize marker overlay."
  (interactive)
  (let* ((view (org-glance-view:get-buffer-view))
         (diff (- (- change-end change-beg) pre-change-length))
         (midx (org-glance-view:marker-at-point view (- change-beg 1))))
    (org-glance-log :performance
        (org-glance-view:set-marker-changed view midx t))
    (org-glance-log :performance
        (org-glance-view:shift-markers view midx diff))))

(provide 'org-glance-material-mode)
