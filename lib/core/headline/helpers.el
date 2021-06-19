(require 'org-glance-module)

(org-glance-module-import lib.core.headline.def)
(org-glance-module-import lib.core.headline.visit)
(org-glance-module-import lib.core.metastore)

(cl-defun org-glance-headline:at-point ()
  "Get org-glance-headline from subtree at point.
Subtree must satisfy the requirements of `org-glance-headline-p'"
  (save-excursion
    (org-glance-headline:goto-beginning-of-nearest-headline)
    (org-glance-metastore:headline (org-glance-headline:id))))

(defmacro org-glance-with-headline-narrowed (headline &rest forms)
  "Visit HEADLINE, narrow to its subtree and execute FORMS on it."
  (declare (indent 1) (debug t))
  `(let* ((file (org-element-property :file ,headline))
          (file-buffer (get-file-buffer file))
          (visited-buffer (current-buffer))
          res)
     (org-glance-headline:visit ,headline)
     (org-narrow-to-subtree)
     (unwind-protect
          (setq res (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
                      ,@forms))
       (widen))
     (cond ((and file-buffer (not (eq file-buffer (current-buffer)))) (bury-buffer file-buffer))
           ((and file-buffer (eq file-buffer (current-buffer))) (progn (switch-to-buffer visited-buffer)
                                                                       (bury-buffer file-buffer)))
           (t (kill-buffer (get-file-buffer file))))
     res))

(cl-defun org-glance-headline:contents (headline)
  (save-window-excursion
    (save-excursion
      (org-glance-headline:visit headline)
      (save-restriction
        (org-narrow-to-subtree)
        (org-glance-headline:normalize-indentation)))))

(cl-defun org-glance-headline:links (headline)
  (org-glance-with-headline-narrowed headline
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (cons
         (substring-no-properties
          (or (nth 2 link)                            ;; link alias
              (org-element-property :raw-link link))) ;; full link if alias is none
         (org-element-property :begin link))))))

(cl-defun org-glance-headline:modtime (headline)
  (org-glance-with-headline-narrowed headline
    (visited-file-modtime)))

(cl-defun org-glance-headline:modtime* (headline)
  (format-time-string "%Y-%m-%d %H:%M:%S" (org-glance-headline:modtime headline)))

(cl-defun org-glance-headline:state (&optional headline)
  (save-window-excursion
    (save-excursion
      (when headline
        (org-glance-headline:visit headline))
      (condition-case nil
          (substring-no-properties (org-get-todo-state))
        (error "")))))

(org-glance-module-provide)
