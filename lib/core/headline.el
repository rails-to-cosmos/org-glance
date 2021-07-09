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

(cl-defun org-glance-headline-p (headline)
  (not (null (org-glance-headline:id headline))))

(cl-defun org-glance-headline:at? ()
  (and (org-at-heading-p) (org-glance-headline-p (org-element-at-point))))

(cl-defun org-glance-headline:search-buffer (headline)
  (let ((points (org-element-map (org-element-parse-buffer 'headline) 'headline
                  (lambda (elem) (when (org-glance-headline:eq elem headline)
                              (org-element-property :begin elem))))))
    (unless points
      (org-glance-exception:headline-not-found "Headline not found in file %s: %s" file headline))

    (when (> (length points) 1)
      (warn "Headline ID %s is not unique in file %s"
            (org-glance-headline:id headline)
            (org-glance-headline:file headline)))

    (goto-char (car points))))

(cl-defun org-glance-headline:ensure-at-heading ()
  (unless (org-at-heading-p)
    (org-back-to-heading-or-point-min)))

(cl-defun org-glance-headline:back-to-heading ()
  (while (and (not (org-glance-headline:at?))
              (> (point) (point-min)))
    (org-up-heading-or-point-min)))

(cl-defun org-glance-headline:at-point ()
  "Build `org-glance-headline' from `org-element' at point.
If point is inside subtree, search backward for the first occurence of `org-glance-headline'."
  (save-excursion
    (org-glance-headline:ensure-at-heading)
    (org-glance-headline:back-to-heading)
    (when (org-glance-headline:at?)
      (-some-> (org-element-at-point)
        (org-element-put-property :file (buffer-file-name))
        (org-element-put-property :indent (save-excursion
                                            (beginning-of-line)
                                            (cl-loop while (looking-at "\\*")
                                               for i from 0 do (forward-char)
                                               finally (return i))))))))

(org-glance-module-provide)
