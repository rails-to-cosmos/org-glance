(require 'org-glance-module)

(org-glance-module-import lib.core.headline)
(org-glance-module-import lib.core.metastore)

(cl-defgeneric org-glance-headline:visit (headline)
  "Visit HEADLINE.")

(cl-defmethod org-glance-headline:visit ((id symbol))
  "Visit HEADLINE by headline id symbol name. Grab source file from metastore."
  (org-glance-headline:visit (symbol-name id)))

(cl-defmethod org-glance-headline:visit ((id list))
  "Visit HEADLINE by headline id symbol name. Grab source file from metastore."
  (org-glance-headline:visit (org-glance-headline:id id)))

(cl-defmethod org-glance-headline:visit ((id string))
  "Visit HEADLINE by id. Grab source file from metastore."
  (let* ((headline (org-glance-metastore:get-headline-by-id id))
         (file (org-glance-headline:file headline))
         (buffer (org-glance-headline:buffer headline)))

    (cond ((file-exists-p file) (find-file file))
          (t (org-glance-db-outdated "File not found: %s" file)))

    ;; we are now visiting headline file, let's remove restrictions
    (widen)

    ;; search for headline in buffer
    (org-glance-headline:search-buffer headline)
    (org-glance-headline:expand-parents)
    (org-overview)
    (org-cycle 'contents)))

(defmacro org-glance-headline:narrow (headline &rest forms)
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
  (org-glance:ensure-at-heading)
  (while (and (org-glance-headline-p) (looking-at "^\\*\\*"))
    (org-promote-subtree)))

(cl-defun org-glance-headline:contents (headline)
  (with-temp-buffer
    (org-mode)
    (insert-file-contents (org-glance-headline:file headline))
    (org-glance-headline:search-buffer headline)
    (org-narrow-to-subtree)
    (goto-char (point-min))
    (org-glance-headline:promote-to-first-level)
    (buffer-substring-no-properties (point-min) (point-max))))

(cl-defun org-glance-headline:links (headline)
  (org-glance-headline:narrow headline
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (cons
         (substring-no-properties
          (or (nth 2 link)                            ;; link alias
              (org-element-property :raw-link link))) ;; full link if alias is none
         (org-element-property :begin link))))))

(cl-defun org-glance-headline:sync ()
  (interactive)
  (if (org-before-first-heading-p)
      (let ((category (org-get-category)))
        (when (y-or-n-p (org-glance:f** "Update view ${category}?"))
          (org-glance-view:summary (intern category))))
    (let* ((initial-point (point))
           (inhibit-read-only t)
           (current-headline (org-glance-headline:at-point))
           (original-headline (org-glance-metastore:get-headline-by-id (org-glance-headline:id current-headline)))
           (original-contents (condition-case nil
                                  (org-glance-headline:contents original-headline)
                                (error nil))))
      (if original-contents
          (save-restriction
            (org-glance-headline:goto-beginning-of-nearest-headline)
            (org-narrow-to-subtree)
            (goto-char (point-min))
            (insert original-contents)
            (delete-region (point) (point-max))
            (goto-char (point-min))
            (cl-loop
               for i from 1 to (1- (org-glance-headline:indent current-headline))
               do (org-demote-subtree)))
        (goto-char initial-point)
        (if (org-glance-headline-p)
            (org-overview)
          (org-cycle-hide-drawers 'all))
        (when (y-or-n-p "Original heading not found. Remove it?")
          (kill-region (org-entry-beginning-position) (org-entry-end-position))))
      (save-buffer))))

(org-glance-module-provide)
