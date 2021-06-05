(require 'org-glance-module)

(defvar org-glance-summary-mode-map (make-sparse-keymap)
  "Show read-only outlines for `org-glance' views.")

;; (define-key org-glance-summary-mode-map (kbd "C-x C-s") #'org-glance-view-sync-subtree)

(define-key org-glance-summary-mode-map (kbd "q") #'bury-buffer)
(define-key org-glance-summary-mode-map (kbd "v") #'org-glance-headline:visit-headline-at-point)
(define-key org-glance-summary-mode-map (kbd "m") #'org-glance-view-materialize-original-heading)
(define-key org-glance-summary-mode-map (kbd "n") #'next-line)
(define-key org-glance-summary-mode-map (kbd "p") #'previous-line)
(define-key org-glance-summary-mode-map (kbd "o") #'org-open-at-point)

(define-minor-mode org-glance-summary-mode
    "A minor mode to be activated only in .org_summary files."
  nil nil org-glance-summary-mode-map
  (read-only-mode 'toggle))

(org-glance-module-provide)
