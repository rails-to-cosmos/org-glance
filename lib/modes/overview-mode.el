(require 'org-glance-module)

(defvar org-glance-overview-mode-map (make-sparse-keymap)
  "Show read-only outlines for `org-glance' views.")

(define-key org-glance-overview-mode-map (kbd "a") 'org-glance-overview:agenda)
(define-key org-glance-overview-mode-map (kbd "r") 'org-glance:add-relation)
(define-key org-glance-overview-mode-map (kbd "d") 'org-glance-overview:doctor)
(define-key org-glance-overview-mode-map (kbd "q") 'bury-buffer)
(define-key org-glance-overview-mode-map (kbd "v") 'org-glance-overview:visit)
(define-key org-glance-overview-mode-map (kbd "RET") 'org-glance-overview:visit)
(define-key org-glance-overview-mode-map (kbd "g") 'org-glance-headline:sync)
(define-key org-glance-overview-mode-map (kbd "n") 'next-line)
(define-key org-glance-overview-mode-map (kbd "p") 'previous-line)
(define-key org-glance-overview-mode-map (kbd "o") 'org-open-at-point)

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
    (org-glance-headline:visit (org-glance-headline:at-point))))

(cl-defun org-glance-overview:doctor ()
  (interactive)
  (if (org-before-first-heading-p)
      (let ((category (org-get-category)))
        (org-glance-view:doctor (intern category)))
    (message "not implemented yet")))

(org-glance-module-provide)
