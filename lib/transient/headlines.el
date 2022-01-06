(require 'org-glance-module)

(org-glance:require
  transient
  lib.core.view
  lib.transient.base)

(transient-define-prefix org-glance-form-action ()
  "Perform action on selected view/headlines"
  ["Overview"
   [("a" "Agenda" org-glance-agenda)
    ("o" "Overview" org-glance-overview)]]
  ["Headline actions"
   [("+" "Capture" org-glance-capture)
    ("e" "Extract" org-glance:extract)
    ("k" "Kill" org-glance:revoke)
    ("j" "Jump" org-glance:open)
    ("m" "Materialize" org-glance:materialize)]]
  (interactive)
  (org-glance-init)
  (transient-setup 'org-glance-form-action))

(transient-define-prefix org-glance-form-action--cyrillic ()
  "Perform action on selected view/headlines"
  ["Overview"
   [("ф" "Agenda" org-glance-overview:agenda*)
    ("щ" "Overview" org-glance-overview)]]
  ["Headline actions"
   [("+" "Capture" org-glance-capture)
    ("у" "Extract" org-glance:extract)
    ("л" "Kill" org-glance:revoke)
    ("о" "Jump" org-glance:open)
    ("ь" "Materialize" org-glance:materialize)]]
  (interactive)
  (org-glance-init)
  (transient-setup 'org-glance-form-action--cyrillic))

(org-glance:provide)
