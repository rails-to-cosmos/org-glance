(require 'org-glance-module)

(org-glance-module-import lib.core.headline.def)
(org-glance-module-import lib.core.headline.visit)
(org-glance-module-import lib.core.headline.factory)
(org-glance-module-import lib.core.metastore)

(defmacro org-glance-headline:narrow* (headline &rest forms)
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

(cl-defun org-glance-headline:promote-to-first-level ()
  (org-glance-headline:ensure-at-heading)
  (while (and (org-glance-headline:at?) (looking-at "^\\*\\*"))
    (org-promote-subtree)))

(cl-defun org-glance-headline:contents (headline)
  (let* ((headline (org-glance-headline headline)))
    (with-temp-buffer
      (org-mode)
      (insert-file-contents (org-glance-headline:file headline))
      (org-glance-headline:search-buffer headline)
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (org-glance-headline:promote-to-first-level)
      (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defun org-glance-headline:contents* (headline)
  (org-glance:f*
   "* $title
      |:PROPERTIES:
      |:ORG_GLANCE_ID: $id
      |:END:"
   :title (org-glance-headline:title headline)
   :id (org-glance-headline:id headline)))

(cl-defun org-glance-headline:links (headline)
  (org-glance-headline:narrow* headline
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (cons
         (substring-no-properties
          (or (nth 2 link)                            ;; link alias
              (org-element-property :raw-link link))) ;; full link if alias is none
         (org-element-property :begin link))))))

(cl-defun org-glance-headline:modtime (headline)
  (org-glance-headline:narrow* headline
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

(cl-defun org-glance-headline:sync ()
  (interactive)
  (if (org-before-first-heading-p)
      (let ((category (org-get-category)))
        (when (y-or-n-p (org-glance:f** "Update view ${category}?"))
          (org-glance-view:summary (intern category))))
    (let ((initial-point (point))
          (inhibit-read-only t))
      (org-glance-headline:goto-beginning-of-nearest-headline)
      (if-let (original-contents (condition-case nil
                                     (org-glance-headline:contents (org-glance-headline nil))
                                   (error nil)))
          (when-let (current-headline (org-glance-headline:at-point))
            (save-restriction
              (org-narrow-to-subtree)
              (goto-char (point-min))
              (insert original-contents)
              (delete-region (point) (point-max))
              (goto-char (point-min))
              (cl-loop
                 for i from 1 to (1- (org-glance-headline:indent current-headline))
                 do (org-demote-subtree)))
            (goto-char initial-point)
            (if (org-glance-headline:at?)
                (org-overview)
              (org-cycle-hide-drawers 'all)))
        (when (y-or-n-p "Original heading not found. Remove it?")
          (kill-region (org-entry-beginning-position) (org-entry-end-position))))
      (save-buffer))))

(org-glance-module-provide)
