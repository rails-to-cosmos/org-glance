(require 'org)
(require 'org-element)
(require 'org-glance-module)

(cl-defun org-glance-headline:id (&optional (headline (org-element-at-point)))
  (org-element-property :ORG_GLANCE_ID headline))

(cl-defun org-glance-headline:title (headline)
  (or (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)))

(cl-defun org-glance-headline:file (headline)
  (org-element-property :file headline))

(cl-defun org-glance-headline:indent (headline)
  (org-element-property :indent headline))

(cl-defun org-glance-headline:buffer (headline)
  (get-file-buffer (org-glance-headline:file headline)))

(cl-defun org-glance-headline:begin (headline)
  (org-element-property :begin headline))

(cl-defun org-glance-headline:view-id (headline)
  (org-element-property :ORG_GLANCE_VIEW_ID headline))

(cl-defun org-glance-headline:serialize (headline)
  (list (org-glance-headline:title headline)
        (org-glance-headline:begin headline)
        (org-glance-headline:file headline)))

(cl-defun org-glance-headline-p (headline)
  (not (null (org-glance-headline:id headline))))

(cl-defun org-glance-headline:at? ()
  (and (org-at-heading-p) (org-glance-headline-p (org-element-at-point))))

(cl-defun org-glance-headline:in? ()
  (save-excursion
    (unless (org-at-heading-p) (org-back-to-heading-or-point-min))
    (while (and (not (org-glance-headline:at?))
                (> (point) (point-min)))
      (org-up-heading-or-point-min))
    (org-glance-headline:at?)))

(cl-defun org-glance-headline:search-buffer (headline)
  (let ((points (org-element-map (org-element-parse-buffer 'headline) 'headline
                  (lambda (hl) (when (org-glance-headline:eq hl headline)
                            (org-element-property :begin hl))))))
    (unless points
      (org-glance-exception:headline-not-found "Headline not found in file %s: %s" file headline))

    (when (> (length points) 1)
      (warn "Headline ID %s is not unique in file %s"
            (org-glance-headline:id headline)
            (org-glance-headline:file headline)))

    (goto-char (car points))))

(cl-defun org-glance-headline:at-point ()
  (interactive)
  (save-excursion
    (unless (org-at-heading-p) (org-back-to-heading-or-point-min))

    (while (and (not (org-glance-headline:at?))
                (> (point) (point-min)))
      (org-up-heading-or-point-min))

    (when (> (point) (point-min))
      (-> (org-element-at-point)
        (org-element-put-property :file (buffer-file-name))
        (org-element-put-property :indent (save-excursion
                                            (beginning-of-line)
                                            (cl-loop while (looking-at "\\*")
                                               for i from 0 do (forward-char)
                                               finally (return i))))))))

(org-glance-module-provide)
