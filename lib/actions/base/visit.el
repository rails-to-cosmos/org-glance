(require 'pythonic-import)
(pythonic-import lib.core.serde)
(require 'org-glance-view)
(require 'org-glance-action)

(org-glance-action-define visit (headline) :for all
  "Visit HEADLINE."
  (let* ((file (org-element-property :file headline))
         (point (org-element-property :begin headline))
         (buffer (get-file-buffer file)))
    (message "Visit file %s" file)
    (cond ((file-exists-p file) (find-file file))
          (t (org-glance-db-outdated "File not found: %s" file)))
    (widen)
    (goto-char point)
    (cond ((org-glance--element-equals-headline headline)
           (cl-loop while (org-up-heading-safe)) ;; expand parents
           (org-narrow-to-subtree)
           (widen)
           (goto-char point)
           (org-show-children))
          (t (unless buffer (kill-buffer))
             (message "Unable to visit headline %s" headline)
             (org-glance-db-outdated "Visited headline cache corrupted, please reread")))))

(defun org-glance-view-visit-original-heading ()
  (interactive)
  (save-excursion
    (cl-loop while (org-up-heading-safe))
    (let* ((heading (list :file --org-glance-view-src
                          :begin --org-glance-view-beg
                          :raw-value (org-element-property :raw-value (org-element-at-point))))
           (virtual-element (org-element-create 'headline heading)))
      (org-glance-action-call 'visit :on virtual-element))))

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
