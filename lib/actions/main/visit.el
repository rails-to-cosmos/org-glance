(require 'org-glance-module)

(org-glance-module-import lib.core.headline)

(org-glance-action-define visit (headline) :for all
  "Visit HEADLINE."
  (org-glance-headline:visit headline))

(org-glance-module-provide)
