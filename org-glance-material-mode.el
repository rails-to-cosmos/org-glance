"Materialize headline to apply changes in transaction manner."

(require 'org)

(require 'org-glance-headline)
(require 'org-glance-registry)

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with synchronization abilities.")

(define-minor-mode org-glance-material-mode
    "A minor mode to be activated only in materialized view editor."
  nil nil org-glance-material-mode-map)

(defvar-local org-glance-material-mode--original-headline nil)
(defvar org-glance-material-mode-buffers (make-hash-table))

(defcustom org-glance-after-materialize-sync-hook nil
  "Hook that is run after a materialized buffer is synchronized to its origin."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'org-glance)

(defcustom org-glance-before-materialize-sync-hook nil
  "Normal hook that is run before a materialized buffer is synchronized to its origin."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'org-glance)

(define-key org-glance-material-mode-map (kbd "C-x C-s") #'org-glance-material-mode-apply)

(define-error 'org-glance-material-mode:HEADLINE-NOT-MODIFIED "No changes made in materialized buffer" 'user-error)
(cl-defun org-glance-material-mode:HEADLINE-NOT-MODIFIED (format &rest args)
  (signal 'org-glance-material-mode:HEADLINE-NOT-MODIFIED (list (apply #'format-message format args))))

(cl-defmethod org-glance-material-mode--generate-new-buffer ((headline org-glance-headline))
  (generate-new-buffer (concat "org-glance:<" (org-glance-headline:title headline) ">")))

(cl-defmethod org-glance-material-mode-capture ((registry org-glance-registry) &rest headlines)
  "Materialize HEADLINE. Return materialized buffer."
  (with-current-buffer (org-glance-material-mode--generate-new-buffer headlines)
    (dolist (headline headlines)
      (setq-local default-directory (org-glance-headline:directory headline))
      (org-mode)
      (org-glance-material-mode +1)
      (insert (org-glance-headline:contents headline))
      (goto-char (point-min))
      (setq-local org-glance-material-mode--original-headline headline)
      (org-display-inline-images)
      (org-cycle-hide-drawers 'all)
      (current-buffer))))

(defalias #'org-glance-material-mode-capture 'org-glance-headline-materialize)

(cl-defmethod org-glance-material-mode-apply ((registry org-glance-registry))
  "Apply all changes of the current headline to REGISTRY."

  ;; (unless org-glance-material-mode--original-headline
  ;;   (org-glance-material-mode:ORIGINAL-HEADLINE-NOT-FOUND))

  ;; (with-demoted-errors "Hook error: %s"
  ;;   (run-hooks 'org-glance-before-materialize-sync-hook))

  ;; (goto-char (point-min))

  ;; Ensure current buffer is materialized

  ;; Narrow to headline
  ;; Determine output location
  ;; Check hashes
  ;; Write
  )

(provide 'org-glance-material-mode)
