(require-relative "lib/utils/helpers.el")

(require 'org-glance-action)

(org-glance-action-define extract-property (headline) :for kvs
  "Completing read all properties from HEADLINE and its successors to kill ring."
  (save-window-excursion
    (org-glance-action-call 'materialize :on headline)
    (org-glance-buffer-properties-to-kill-ring)))
