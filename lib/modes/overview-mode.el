(require 'org-glance-module)

(defvar org-glance-overview-mode-map (make-sparse-keymap)
  "Show read-only outlines for `org-glance' views.")

(define-key org-glance-overview-mode-map (kbd "RET") 'org-glance-overview:visit)
(define-key org-glance-overview-mode-map (kbd "a") 'org-glance-overview:agenda)
(define-key org-glance-overview-mode-map (kbd "d") 'org-glance-overview:doctor)
(define-key org-glance-overview-mode-map (kbd "g") 'org-glance-overview:pull)
(define-key org-glance-overview-mode-map (kbd "n") 'next-line)
(define-key org-glance-overview-mode-map (kbd "o") 'org-open-at-point)
(define-key org-glance-overview-mode-map (kbd "p") 'previous-line)
(define-key org-glance-overview-mode-map (kbd "q") 'bury-buffer)
(define-key org-glance-overview-mode-map (kbd "r") 'org-glance:add-relation)
(define-key org-glance-overview-mode-map (kbd "v") 'org-glance-overview:visit)
(define-key org-glance-overview-mode-map (kbd ";") 'org-glance-overview:comment)

(define-minor-mode org-glance-overview-mode
    "A minor read-only mode to use in .org_* files."
  nil nil org-glance-overview-mode-map
  (read-only-mode 'toggle))

(cl-defun org-glance-overview:agenda ()
  (interactive)
  (let ((org-agenda-files (list (buffer-file-name))))
    (org-agenda-list)
    (org-agenda-day-view)))

(cl-defun org-glance-overview:visit ()
  (interactive)
  (if (org-before-first-heading-p)
      (message "not implemented yet")
    (->> (org-glance-headline:at-point)
      org-glance-headline:id
      org-glance-metastore:get-headline-by-id
      org-glance-headline:visit)))

(cl-defun org-glance-overview:doctor ()
  (interactive)
  (if (org-before-first-heading-p)
      (let ((category (org-get-category)))
        (org-glance-view:doctor (intern category)))
    (message "not implemented yet")))

(cl-defun org-glance-overview:comment ()
  (interactive)
  (save-window-excursion
    (->> (org-glance-headline:at-point)
      (org-glance-headline:id)
      (org-glance-metastore:get-headline-by-id)
      (org-glance-headline:visit))
    (org-toggle-comment)
    (save-buffer))
  (org-glance-overview:pull))

(cl-defun org-glance-overview:pull ()
  (interactive)
  (if (org-before-first-heading-p)
      (let ((category (org-get-category)))
        (when (y-or-n-p (org-glance:f** "Update view ${category}?"))
          (org-glance-view:summary (intern category))))
    (let* ((inhibit-read-only t)
           (initial-point (point))
           (current-headline (org-glance-headline:at-point))
           (current-headline-id (org-glance-headline:id current-headline))
           (current-headline-indent (org-glance-headline:indent current-headline))
           (original-headline (org-glance-metastore:get-headline-by-id current-headline-id))
           (original-headline-contents (org-glance-headline:contents original-headline)))
      (if original-headline-contents
          (save-excursion
            (save-restriction
              (org-glance-headline:search-backward)
              (org-narrow-to-subtree)
              (delete-region (point-min) (point-max))
              (insert original-headline-contents)
              (goto-char (point-min))
              (cl-loop for i from 1 to (1- current-headline-indent)
                 do (org-demote-subtree)))
            (org-cycle-hide-drawers 'all))
        (when (y-or-n-p "Original heading not found. Remove it?")
          (kill-region (org-entry-beginning-position) (org-entry-end-position))))
      (goto-char initial-point)
      (save-buffer))))

(org-glance-module-provide)
