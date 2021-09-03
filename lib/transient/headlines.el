(require 'org-glance-module)

(require 'transient)

(org-glance:import lib.core.view)
(org-glance:import lib.transient.base)

(transient-define-prefix org-glance-form-action ()
  "Perform action on selected view/headlines"
  ["Overview"
   [("a" "Agenda" org-glance-overview:agenda*)
    ("o" "Overview" org-glance-overview:visit)]]
  ["Headline actions"
   [("e" "Extract" org-glance-action-extract)
    ("j" "Jump" org-glance-action-open)
    ("m" "Materialize" org-glance-action-materialize)]])

(org-glance-module-provide)
