(require 'org-glance-module)

(org-glance:require
  lib.core.actions
  lib.core.view)

(org-glance-action-define extract (headline) :for (kvs crypt)
  "Materialize HEADLINE, decrypt it, then run completing read on all properties to kill ring."
  (save-window-excursion
    (org-glance-action-call 'materialize :on headline :for 'crypt)
    (org-cycle-hide-drawers 'all)
    (unwind-protect
         (org-glance-buffer-properties-to-kill-ring)
      (kill-buffer (org-glance-headline:materialized-buffer-name headline)))))

(org-glance:provide)
