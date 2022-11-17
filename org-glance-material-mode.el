;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'cl-macs)

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
         (add-hook 'before-change-functions #'org-glance-material-mode:before-update nil t)
         (add-hook 'after-change-functions #'org-glance-material-mode:after-update nil t)
         (add-hook 'before-save-hook #'org-glance-view:commit nil t))
        (t
         (remove-hook 'before-save-hook #'org-glance-view:commit t)
         (remove-hook 'before-change-functions #'org-glance-material-mode:before-update t)
         (remove-hook 'after-change-functions #'org-glance-material-mode:after-update t))))

(cl-defun org-glance-material-mode:before-update (change-beg change-end)
  "Actualize marker overlay."
  (interactive)
  (org-glance-log :markers "Before update change beg: %d" change-beg)
  (org-glance-log :markers "Before update change end: %d" change-end)
  (org-glance-log :markers "Before update marker substring: \"%s\"" (buffer-substring-no-properties change-beg change-end))
  (org-glance-log :contents "Before update contents: \"%s\"" (buffer-string)))

(cl-defun org-glance-material-mode:after-update (change-beg change-end change-len)
  "Actualize marker overlay."
  (interactive)
  (let* ((view (org-glance-view:get-buffer-view))
         (diff (- (- change-end change-beg) change-len))
         (midx (org-glance-view:marker-at-point view (- change-beg 1)))
         (buffer (current-buffer)))
    (org-glance-view:set-marker-changed view midx t)
    (org-glance-view:shift-markers! view midx diff)

    (org-glance-log :markers "After update change beg: %d" change-beg)
    (org-glance-log :markers "After update change end: %d" change-end)
    (org-glance-log :markers "After update change len: %d" change-len)
    (condition-case nil
        (org-glance-log :markers "After update marker substring: \"%s\"" (buffer-substring-no-properties change-beg change-end))
      (error nil))
    (org-glance-log :contents "After update contents: \"%s\"" (buffer-string))
    (org-glance-log :markers "After update markers: %s" (pp-to-string (org-glance? view :markers)))

    (save-match-data
      (with-current-buffer (get-buffer-create "*glance-markers*")
        (delete-region (point-min) (point-max))
        (insert (pp-to-string (org-glance? view :markers)))
        (insert "\n" (pp-to-string (org-glance? view :world :changelog*)))))))

(provide 'org-glance-material-mode)
