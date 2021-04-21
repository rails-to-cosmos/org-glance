(require 'transient)
(require 'load-relative)
(require 'org-glance-transient-variables)
(require 'org-glance-view)
(require 'org-glance-action-custom)

(transient-define-prefix org-glance-act ()
  "In Glance-View buffer, perform action on selected view"
  ["Analytics"

   [("A" "Agenda" org-glance-view-agenda)
    ("D" "Dashboard" org-glance-show-report)]

   [("-v" "View" org-glance-act.view)]]

  ["Actions"
   [("U" "Update" org-glance-view-update)
    ("V" "Visit" org-glance-view-visit)]]

  ["Headlines"
   ;; [("c" "Capture" org-glance-action-extract-property)]
   [("e" "Extract" org-glance-action-extract-property)]
   [("j" "Jump" org-glance-action-open)]
   [("i" "Insert" org-glance-action-insert)]
   [("m" "Materialize" org-glance-action-materialize)]
   [("v" "Visit" org-glance-action-visit)]])

(provide-me)
