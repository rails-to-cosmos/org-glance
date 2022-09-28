;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-macs)
(require 'thunk)
(require 'highlight)
(require 'benchmark)

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
         (org-glance-mew:mark)
         (org-glance-mew:fetch)
         (add-hook 'after-change-functions #'org-glance-material-mode:update-benchmark nil t)
         (add-hook 'before-save-hook #'org-glance-mew:commit nil t))
        (t
         (remove-hook 'before-save-hook #'org-glance-mew:commit t)
         (remove-hook 'after-change-functions #'org-glance-material-mode:update-benchmark t))))

(cl-defun org-glance-material-mode:update-benchmark (change-beg change-end pre-change-length)
  (benchmark-progn
    (org-glance-material-mode:update change-beg change-end pre-change-length)))

(cl-defun org-glance-material-mode:update (change-beg change-end pre-change-length)
  "Actualize marker overlay."
  (interactive)
  (let* ((mew (org-glance-mew:current))
         (diff (- (- change-end change-beg) pre-change-length))
         (midx (org-glance-mew:marker-at-point mew (- change-beg 1))))
    (org-glance-mew:set-marker-changed mew midx t)
    (org-glance-mew:shift-markers mew midx diff)))

(provide 'org-glance-material-mode)
