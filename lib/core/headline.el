(require 'org-glance-module)

(org-glance-module-import lib.utils.helpers)
(org-glance-module-import lib.core.metastore)

(cl-defun org-glance-headline:by-id (id)
  "Get org-element headline by ID."
  (or
   (cl-loop for vid in (org-glance-view:ids)
      for metastore = (->> vid
                        org-glance-view
                        org-glance-view-metadata-location
                        org-glance-metastore:read)
      for headline = (gethash id metastore)
      when headline
      do (return (org-glance-metastore:deserialize headline)))
   ;; headline not found
   (org-glance-headline-not-found "%s. Try to update view or make sure the headline was not deleted" id)))

(cl-defun org-glance-headline:at-point ()
  "Get org-glance-headline from subtree at point.
Subtree must satisfy the requirements of `org-glance-headline-p'"
  (save-excursion
    (org-glance-headline:goto-beginning-of-nearest-headline)
    (org-glance-headline:by-id (org-glance-headline:id))))

(cl-defun org-glance-headline:search-buffer (headline)
  "Search buffer for HEADLINE and return its point.
Raise `org-glance-headline-not-found` error on fail.''"
  (let ((points (org-element-map (org-element-parse-buffer 'headline) 'headline
                  (lambda (hl) (when (org-glance-headline:eq hl headline)
                            (org-element-property :begin hl))))))
    (unless points
      (org-glance-headline-not-found "Headline not found in file %s: %s" file headline))

    (when (> (length points) 1)
      (warn "Headline ID %s not unique" (org-glance-headline:id headline)))

    (car points)))

(cl-defgeneric org-glance-headline:visit (headline)
  "Visit HEADLINE.")

(cl-defmethod org-glance-headline:visit ((headline symbol))
  "Visit HEADLINE by headline id symbol name."
  (org-glance-headline:visit (symbol-name headline)))

(cl-defmethod org-glance-headline:visit ((headline list))
  "Visit HEADLINE by headline id symbol name."
  (org-glance-headline:visit (org-glance-headline:id headline)))

(cl-defmethod org-glance-headline:visit ((headline string))
  "Visit HEADLINE by id."
  (let* ((headline (org-glance-headline:by-id headline))
         ;; extract headline filename
         (file (org-element-property :file headline))
         ;; cache file buffer
         (buffer (get-file-buffer file)))

    (cond ((file-exists-p file) (find-file file))
          (t (org-glance-db-outdated "File not found: %s" file)))

    ;; we are now at headline file, let's remove restrictions
    (widen)

    ;; search for headline in buffer
    (goto-char (org-glance-headline:search-buffer headline))
    (org-glance-headline:expand-parents)
    (org-overview)
    (org-cycle 'contents)))

(cl-defun org-glance-headline:visit-headline-at-point ()
  (interactive)
  (save-excursion
    (org-glance-headline:goto-first-level-headline)
    (org-glance-headline:visit (org-glance-headline:id))))

(defmacro org-glance-with-headline-narrowed (headline &rest forms)
  "Visit HEADLINE, narrow to its subtree and execute FORMS on it."
  (declare (indent defun))
  `(let* ((file (org-element-property :file ,headline))
          (file-buffer (get-file-buffer file))
          (visited-buffer (current-buffer)))
     (org-glance-headline:visit ,headline)
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
