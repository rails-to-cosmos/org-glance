(require 'org-glance-module)

(org-glance-module-import lib.core.metastore)
(org-glance-module-import lib.core.actions)
(org-glance-module-import lib.core.view)

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
  (let ((buffer org-glance-materialized-view-buffer))
    (save-window-excursion
      (org-glance-headline:visit* headline)
      (let* ((file (org-element-property :file headline))
             (beg (org-glance-headline:begin))
             (end (save-excursion (org-end-of-subtree t)))
             (contents (org-glance-headline:buffer-contents beg end)))
        (when (get-buffer buffer)
          (switch-to-buffer buffer)
          (condition-case nil
              (org-glance-view-sync-subtree)
            (org-glance-view-not-modified nil))
          (kill-buffer buffer))
        (with-current-buffer (get-buffer-create buffer)
          (delete-region (point-min) (point-max))
          (org-mode)
          (org-glance-view-mode)
          (insert contents)
          (goto-char (point-min))
          (org-content 1)
          (org-cycle-hide-drawers 'all)
          (setq-local --org-glance-view-src file)
          (setq-local --org-glance-view-beg beg)
          (setq-local --org-glance-view-end end)
          ;; extract hash from promoted subtree
          (setq-local --org-glance-view-hash (org-glance-view-subtree-hash))
          ;; run hooks on original subtree
          (with-demoted-errors (run-hooks 'org-glance-after-materialize-hook))
          ;; then promote it saving original level
          (setq-local --org-glance-view-indent (--org-glance-headline:promote.deprecated))
          (org-cycle 'contents))))
    (switch-to-buffer buffer)))

(org-glance-module-provide)
