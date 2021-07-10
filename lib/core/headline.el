(require 'org)
(require 'org-element)
(require 'org-glance-module)

(org-glance-module-import lib.utils.org)

(cl-defun org-glance-headline-p (&optional (headline (org-element-at-point)))
  "Assume HEADLINE is an `org-element' with :ORG_GLANCE_ID property.
Return HEADLINE or nil if it is not a proper `org-glance-headline'."
  (when (not (null (org-element-property :ORG_GLANCE_ID headline)))
    headline))

(cl-defun org-glance-headline:search-backward ()
  (org-glance:ensure-at-heading)
  (while (and (not (org-glance-headline-p))
              (> (point) (point-min)))
    (org-up-heading-or-point-min))
  (org-glance-headline-p))

(cl-defun org-glance-headline:at-point ()
  "Build `org-glance-headline' from `org-element' at point.
If point is inside subtree, search backward for the first occurence of `org-glance-headline'."
  (save-excursion
    (-some-> (org-glance-headline:search-backward)
      (org-element-put-property :file (buffer-file-name))
      (org-element-put-property :indent (org-glance:indent-level)))))

(cl-defun org-glance-headline:id (&optional (headline (org-glance-headline:at-point)))
  "Return unique identifer of HEADLINE."
  (org-element-property :ORG_GLANCE_ID headline))

(cl-defun org-glance-headline:state (&optional (headline (org-glance-headline:at-point)))
  (substring-no-properties (org-element-property :todo-keyword headline)))

(cl-defun org-glance-headline:title (&optional (headline (org-glance-headline:at-point)))
  (or (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)))

(cl-defun org-glance-headline:modtime (&optional (headline (org-glance-headline:at-point)))
  (--> headline
    org-glance-headline:file
    file-attributes
    file-attribute-modification-time
    (format-time-string "%Y-%m-%d %H:%M:%S")))

(cl-defun org-glance-headline:file (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :file headline))

(cl-defun org-glance-headline:indent (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :indent headline))

(cl-defun org-glance-headline:buffer (&optional (headline (org-glance-headline:at-point)))
  (get-file-buffer (org-glance-headline:file headline)))

(cl-defun org-glance-headline:begin (&optional (headline (org-glance-headline:at-point)))
  (org-element-property :begin headline))

(cl-defun org-glance-headline:view-ids (&optional (headline (org-glance-headline:at-point)))
  (mapcar #'s-titleized-words (org-element-property :tags headline)))

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

(org-glance-module-provide)
