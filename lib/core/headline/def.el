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

(cl-defun org-glance-headline:begin (headline)
  (org-element-property :begin headline))

(cl-defun org-glance-headline:view-id (headline)
  (org-element-property :ORG_GLANCE_VIEW_ID headline))

(cl-defun org-glance-headline:serialize (headline)
  (list (org-glance-headline:title headline)
        (org-glance-headline:begin headline)
        (org-glance-headline:file headline)))

(org-glance-module-provide)
