;; -*- lexical-binding: t -*-

(require 'transient)
(require 'org-glance-utils)

(transient-define-prefix org-glance-form-action ()
  "Perform action on selected view/headlines"
  ["Overview"
   [("a" "Agenda" org-glance-agenda)
    ("o" "Overview" org-glance-overview)]]
  ["Actions"
   [("+" "Capture headline" org-glance-capture)
    ("e" "Extract property" org-glance-extract)
    ("j" "Open link" org-glance-open)
    ("m" "Materialize headline" org-glance-materialize)]]
  (interactive)
  (transient-setup 'org-glance-form-action))

(provide 'org-glance-ui)
