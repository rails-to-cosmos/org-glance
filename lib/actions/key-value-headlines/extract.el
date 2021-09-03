(require 'org-glance-module)

(org-glance:import lib.core.actions)
(org-glance:import lib.utils.helpers)

(org-glance-action-define extract (headline) :for kvs
  "Completing read all properties from HEADLINE and its successors to kill ring."
  (save-window-excursion
    (org-glance-action-call 'materialize :on headline)
    (org-glance-buffer-properties-to-kill-ring)))

(org-glance-module-provide)
