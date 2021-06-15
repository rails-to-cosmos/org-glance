(require 'org-glance-module)

(org-glance-module-import lib.core.headline.def)
(org-glance-module-import lib.core.metastore)

(org-glance-module-import lib.utils.helpers)

(cl-defun org-glance-headline:search-buffer (headline)
  "Search buffer for HEADLINE and return its point.
Raise `org-glance-headline-not-found` error on fail.''"
  (let ((points (org-element-map (org-element-parse-buffer 'headline) 'headline
                  (lambda (hl) (when (org-glance-headline:eq hl headline)
                            (org-element-property :begin hl))))))
    (unless points
      (org-glance-headline-not-found "Headline not found in file %s: %s" file headline))

    (when (> (length points) 1)
      (warn "Headline ID %s is not unique in file %s"
            (org-glance-headline:id headline)
            (org-glance-headline:file headline)))

    (car points)))

(cl-defgeneric org-glance-headline:visit (headline)
  "Visit HEADLINE.")

(cl-defmethod org-glance-headline:visit ((id symbol))
  "Visit HEADLINE by headline id symbol name."
  (org-glance-headline:visit (symbol-name id)))

(cl-defmethod org-glance-headline:visit ((id list))
  "Visit HEADLINE by headline id symbol name."
  (org-glance-headline:visit (org-glance-headline:id id)))

(cl-defmethod org-glance-headline:visit ((id string))
  "Visit HEADLINE by id."
  (let* ((headline (org-glance-metastore:headline id))
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

(org-glance-module-provide)