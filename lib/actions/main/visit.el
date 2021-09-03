(require 'org-glance-module)

(org-glance:import lib.core.headline)

(org-glance-action-define visit (headline) :for all
  "Visit HEADLINE."
  (org-glance-headline:visit headline))

(org-glance-module-provide)
