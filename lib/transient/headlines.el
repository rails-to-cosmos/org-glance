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
  ["Actions"
   [("+" "Capture headline" org-glance-capture)
    ("e" "Extract property" org-glance:extract)
    ("k" "Kill headline" org-glance:revoke)
    ("l" "Open link" org-glance:open)
    ("m" "Materialize headline" org-glance:materialize)]]
  (interactive)
  (org-glance-init)
  (transient-setup 'org-glance-form-action))

(transient-define-prefix org-glance-form-action--cyrillic ()
  "Perform action on selected view/headlines"
  ["Overview"
   [("ф" "Agenda" org-glance-overview:agenda*)
    ("щ" "Overview" org-glance-overview)]]
  ["Actions"
   [("+" "Capture headline" org-glance-capture)
    ("у" "Extract property" org-glance:extract)
    ("л" "Kill headline" org-glance:revoke)
    ("д" "Open link" org-glance:open)
    ("ь" "Materialize headline" org-glance:materialize)]]
  (interactive)
  (org-glance-init)
  (transient-setup 'org-glance-form-action--cyrillic))

(org-glance:provide)
