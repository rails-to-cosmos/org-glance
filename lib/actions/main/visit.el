(require 'org-glance-module)

(org-glance:require lib.core.headline)

(org-glance-action-define visit (headline) :for all
  "Visit HEADLINE."
  (org-glance-headline:visit headline))

(org-glance:provide)
