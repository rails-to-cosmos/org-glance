(require 'org-glance-module)

(org-glance:require
  lib.core.actions
  lib.core.metastore
  lib.core.view

  lib.modes.materialized-headline-mode)

(org-glance-action-define materialize (headline) :for all
  "Materialize HEADLINE in separate buffer."
  (let ((buffer (generate-new-buffer (org-glance-headline:materialized-buffer-name headline))))
    (save-window-excursion
      (org-glance-headline:visit headline)
      (let* ((id (org-glance-headline:id headline))
             (file (org-glance-headline:file headline))
             (beg (org-glance-headline:begin headline))
             (end (save-excursion (org-end-of-subtree t)))
             (contents (org-glance-headline:contents headline)))
        (with-current-buffer (get-buffer-create buffer)
          (delete-region (point-min) (point-max))
          (org-mode)
          (org-glance-materialized-headline-mode +1)
          (insert contents)
          (goto-char (point-min))
          (org-content 1)
          (org-cycle-hide-drawers 'all)
          (set (make-local-variable '--org-glance-materialized-headline:id) id)
          (set (make-local-variable '--org-glance-materialized-headline:file) file)
          (set (make-local-variable '--org-glance-materialized-headline:begin) beg)
          (set (make-local-variable '--org-glance-materialized-headline:end) end)
          (set (make-local-variable '--org-glance-materialized-headline:hash) (org-glance-headline:hash))
          ;; run hooks on original subtree
          (with-demoted-errors "Hook error: %s" (run-hooks 'org-glance-after-materialize-hook))
          ;; then promote it saving original level
          (set (make-local-variable '--org-glance-materialized-headline:indent) (1- (org-glance-headline:level)))
          (org-glance-headline:promote-to-the-first-level)
          (org-cycle 'contents))))
    (switch-to-buffer buffer)))

(org-glance:provide)
