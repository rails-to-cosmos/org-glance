(require 'org-glance-module)

(org-glance:require
  lib.core.actions
  lib.core.metastore
  lib.core.view

  lib.modes.materialized-headline-mode)

(defun --org-glance-headline:promote.deprecated ()
  "Deprecated because of side-effects."
  (cl-loop while (condition-case nil
                     (org-with-limited-levels (org-map-tree 'org-promote) t)
                   (error nil))
     with promote-level = 0
     do (cl-incf promote-level)
     finally (return promote-level)))

(org-glance-action-define materialize (headline) :for all
  "Materialize HEADLINE in separate buffer."
  (let ((buffer org-glance-materialized-headline-buffer))
    (save-window-excursion
      (org-glance-headline:visit headline)
      (let* ((file (org-glance-headline:file headline))
             (beg (org-glance-headline:begin headline))
             (end (save-excursion (org-end-of-subtree t)))
             (contents (->> (buffer-substring-no-properties beg end)
                         (s-trim))))
        (when (get-buffer buffer)
          (switch-to-buffer buffer)
          (condition-case nil
              (org-glance-materialized-headline:sync)
            (org-glance-exception:headline-not-modified nil))
          (kill-buffer buffer))
        (with-current-buffer (get-buffer-create buffer)
          (delete-region (point-min) (point-max))
          (org-mode)
          (org-glance-materialized-headline-mode)
          (insert contents)
          (goto-char (point-min))
          (org-content 1)
          (org-cycle-hide-drawers 'all)
          (setq-local --org-glance-view-src file)
          (setq-local --org-glance-view-beg beg)
          (setq-local --org-glance-view-end end)
          ;; extract hash from promoted subtree
          (setq-local --org-glance-view-hash (org-glance-materialized-headline:hash))
          ;; run hooks on original subtree
          (with-demoted-errors (run-hooks 'org-glance-after-materialize-hook))
          ;; then promote it saving original level
          (setq-local --org-glance-view-indent (--org-glance-headline:promote.deprecated))
          (org-cycle 'contents))))
    (switch-to-buffer buffer)))

(org-glance:provide)
