(require 'org-glance-module)

(defvar org-glance-overview-mode-map (make-sparse-keymap)
  "Show read-only outlines for `org-glance' views.")

(define-key org-glance-overview-mode-map (kbd "a") 'org-attach)
(define-key org-glance-overview-mode-map (kbd "q") 'bury-buffer)
(define-key org-glance-overview-mode-map (kbd "v") 'org-glance-headline:visit-headline-at-point)
(define-key org-glance-overview-mode-map (kbd "RET") 'org-glance-headline:visit-headline-at-point)
(define-key org-glance-overview-mode-map (kbd "g") 'org-glance-headline:sync)
(define-key org-glance-overview-mode-map (kbd "m") 'org-glance-view-materialize-original-heading)
(define-key org-glance-overview-mode-map (kbd "n") 'next-line)
(define-key org-glance-overview-mode-map (kbd "p") 'previous-line)
(define-key org-glance-overview-mode-map (kbd "o") 'org-open-at-point)

(define-minor-mode org-glance-overview-mode
    "A minor read-only mode to use in .org_* files."
  nil nil org-glance-overview-mode-map
  (read-only-mode 'toggle))

(org-glance-module-provide)
