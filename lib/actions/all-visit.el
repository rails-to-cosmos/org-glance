(require 'org-glance-module)

(org-glance-module-import lib.core.metastore)
(org-glance-module-import lib.core.actions)
(org-glance-module-import lib.core.view)

(org-glance-action-define visit (headline) :for all
  "Visit HEADLINE."
  (let* ((file (org-element-property :file headline))
         (buffer (get-file-buffer file)))

    (cond ((file-exists-p file) (find-file file))
          (t (org-glance-db-outdated "File not found: %s" file)))

    (widen)

    (let ((points (org-element-map (org-element-parse-buffer 'headline) 'headline
                    (lambda (hl) (when (org-glance-headline:eq hl headline)
                              (org-element-property :begin hl))))))

      (unless points
        (org-glance-db-outdated "Headline not found in file %s: %s" file headline))

      (when (> (length points) 1)
        (warn "Headline ID %s not unique" (org-glance-headline:id headline)))

      (goto-char (car points))
      (save-excursion
        (org-glance-headline:expand-parents))
      (org-show-children))))

(defun org-glance-view-visit-original-heading ()
  (interactive)
  (save-excursion
    (org-glance-headline:expand-parents)
    (let* ((id (org-element-property :ORG_GLANCE_ID (org-element-at-point)))
           (hl (org-glance-headline:by-id id)))
      (org-glance-action-call 'visit :on hl))))

(define-key org-glance-view-mode-map (kbd "C-c C-v") #'org-glance-view-visit-original-heading)

(defmacro org-glance-with-headline-narrowed (headline &rest forms)
  "Visit HEADLINE, narrow to its subtree and execute FORMS on it."
  (declare (indent defun))
  `(let* ((file (org-element-property :file ,headline))
          (file-buffer (get-file-buffer file))
          (visited-buffer (current-buffer)))
     (org-glance-action-call 'visit :on ,headline)
     (widen)
     (org-narrow-to-subtree)
     (unwind-protect
          (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
            ,@forms)
       (widen))
     (cond ((and file-buffer (not (eq file-buffer (current-buffer)))) (bury-buffer file-buffer))
           ((and file-buffer (eq file-buffer (current-buffer))) (progn (switch-to-buffer visited-buffer)
                                                                       (bury-buffer file-buffer)))
           (t (kill-buffer (get-file-buffer file))))))

(org-glance-module-provide)
