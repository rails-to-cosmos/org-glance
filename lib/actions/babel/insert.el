(require 'org-glance-module)

(org-glance:require lib.core.metastore)
(org-glance:require lib.core.actions)

(org-glance-action-define insert (headline) :for babel
  "Visit HEADLINE, get contents and insert it."
  (insert (org-glance-headline:narrow headline
            (org-babel-next-src-block)
            (buffer-substring-no-properties
             (org-element-property :begin (org-element-at-point))
             (org-element-property :end (org-element-at-point))))))

(org-glance:provide)
