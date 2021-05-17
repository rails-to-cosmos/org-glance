(require 'org-glance-module)

(org-glance-module-import lib.core.metastore)
(org-glance-module-import lib.core.actions)
(org-glance-module-import lib.core.view)

(org-glance-action-define materialize (headline) :for all
  "Materialize HEADLINE in separate buffer."
  (cl-labels ((first-level-heading () (save-excursion
                                        (unless (org-at-heading-p) (org-back-to-heading))
                                        (beginning-of-line)
                                        (point)))
              (end-of-subtree () (save-excursion (org-end-of-subtree t)))
              (buffer-contents (beg end) (->> (buffer-substring-no-properties beg end)
                                           (s-trim))))
    (let ((buffer org-glance-materialized-view-buffer))
      (save-window-excursion
        (org-glance-action-call 'visit :on headline)
        (let* ((file (org-element-property :file headline))
               (beg (first-level-heading))
               (end (end-of-subtree))
               (contents (buffer-contents beg end)))
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
            (setq-local --org-glance-view-indent (org-glance-headline:promote)))
          (org-cycle 'contents)))
      (switch-to-buffer buffer))))

(cl-defmacro org-glance-with-headline-materialized (headline &rest forms)
  "Materialize HEADLINE, execute FORMS in materialized buffer."
  (declare (indent 1) (debug t))
  `(let* ((file (org-element-property :file ,headline))
          (file-buffer (get-file-buffer file)))
     (org-glance-action-call 'materialize :on ,headline)
     (unwind-protect
          (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
            ,@forms)
       (kill-buffer org-glance-materialized-view-buffer))
     (cond (file-buffer (bury-buffer file-buffer))
           (t (kill-buffer (get-file-buffer file))))))

(org-glance-module-provide)
