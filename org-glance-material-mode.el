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
  (let ((changed-contents (buffer-substring-no-properties change-beg change-end)))
    (with-current-buffer (get-buffer-create "*glance-markers*")
      (delete-region (point-min) (point-max))
      (insert "Before update:\n")
      (insert (format "Change beg: %d\n" change-beg))
      (insert (format "Change end: %d\n" change-end))
      (insert (format "Change reg: \n%s\n" changed-contents)))))

(cl-defun org-glance-material-mode:after-update (change-beg change-end pre-change-length)
  "Actualize marker overlay."
  (interactive)
  (let* ((view (org-glance-view:get-buffer-view))
         (diff (- (- change-end change-beg) pre-change-length))
         (midx (org-glance-view:marker-at-point view (- change-beg 1)))
         (buffer (current-buffer)))
    (org-glance-view:set-marker-changed view midx t)
    (org-glance-view:shift-markers view midx diff)
    (with-current-buffer (get-buffer-create "*glance-markers*")
      (insert "After update:\n")
      (insert (pp-to-string (org-glance? view :markers)))
      (insert (format "\nChange beg: %d" change-beg))
      (insert (format "\nChange end: %d" change-end))
      (insert (format "\nChange len: %d" pre-change-length))
      (cl-loop with markers = (org-glance? view :markers)
         for midx below (org-glance-vector:size markers)
         for marker = (org-glance-vector:get markers midx)
         for asterisk = (with-current-buffer buffer
                          (buffer-substring-no-properties (1- (org-glance? marker :position)) (1+ (org-glance? marker :position))))
         do (insert (format "\nMarker %d: \"%s\"" midx (if (string= "\n*" asterisk) "T" "F")))))))

(provide 'org-glance-material-mode)
