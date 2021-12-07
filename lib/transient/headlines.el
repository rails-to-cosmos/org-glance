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
   [("+" "Capture" org-glance:capture)
    ("e" "Extract" org-glance:extract)
    ("j" "Jump" org-glance:open)
    ("m" "Materialize" org-glance:materialize)]]
  (interactive)
  (org-glance:init)
  (transient-setup 'org-glance-form-action))

(org-glance:provide)
