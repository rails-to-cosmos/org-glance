(require 'org-glance-module)

(org-glance-module-import lib.core.headline.def)
(org-glance-module-import lib.core.metastore)
(org-glance-module-import lib.utils.helpers)

(cl-defgeneric org-glance-headline:visit (headline)
  "Visit HEADLINE.")

(cl-defmethod org-glance-headline:visit ((headline null))
  "Visit HEADLINE, grab headline id from headline at point. Grab source file from metastore."
  (let ((original-point (point))
        offset)
    (save-excursion
      (org-glance-headline:goto-first-level-headline)
      (setq offset (- original-point (point)))
      (org-glance-headline:visit (org-glance-headline:id))
      (forward-char offset))))

(cl-defmethod org-glance-headline:visit ((id symbol))
  "Visit HEADLINE by headline id symbol name. Grab source file from metastore."
  (org-glance-headline:visit (symbol-name id)))

(cl-defmethod org-glance-headline:visit ((id list))
  "Visit HEADLINE by headline id symbol name. Grab source file from metastore."
  (org-glance-headline:visit (org-glance-headline:id id)))

(cl-defmethod org-glance-headline:visit ((id string))
  "Visit HEADLINE by id. Grab source file from metastore."
  (let* ((headline (org-glance-metastore:headline id))
         (file (org-glance-headline:file headline))
         (buffer (org-glance-headline:buffer headline)))

    (cond ((file-exists-p file) (find-file file))
          (t (org-glance-db-outdated "File not found: %s" file)))

    ;; we are now at headline file, let's remove restrictions
    (widen)

    ;; search for headline in buffer
    (org-glance-headline:search-buffer headline)
    (org-glance-headline:expand-parents)
    (org-overview)
    (org-cycle 'contents)))

(org-glance-module-provide)
