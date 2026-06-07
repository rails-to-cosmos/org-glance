;; -*- lexical-binding: t -*-

(require 'transient)
(require 'org-glance-utils)

(transient-define-prefix org-glance-form-action ()
  "Perform action on selected view/headlines"
  ["Overview"
   [("a" "Agenda" org-glance-agenda-v2)
    ("o" "Overview" org-glance-overview-v2)]]
  ["Actions"
   [("+" "Capture headline" org-glance-capture-v2)
    ("e" "Extract property" org-glance-extract-v2)
    ("j" "Open link" org-glance-open-v2)
    ("m" "Materialize headline" org-glance-materialize-v2)]]
  (interactive)
  (transient-setup 'org-glance-form-action))

(provide 'org-glance-ui)
