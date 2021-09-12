(require 'org-glance-module)

(org-glance:require
  transient
  lib.core.view
  lib.transient.base)

(transient-define-prefix org-glance-form-action ()
  "Perform action on selected view/headlines"
  ["Overview"
   [("a" "Agenda" org-glance-overview:agenda*)
    ("o" "Overview" org-glance-overview)]]
  ["Headline actions"
   [("+" "Capture" org-glance-overview:capture)
    ("e" "Extract" org-glance-action-extract)
    ("j" "Jump" org-glance-action-open)
    ("m" "Materialize" org-glance-action-materialize)]]
  (interactive)
  (org-glance:system-init)
  (transient-setup 'org-glance-form-action))

(org-glance:provide)
