(require 'org-glance-module)

(require 'transient)
(require 'load-relative)
(require 'org-glance-view)

(org-glance-module-import lib.forms.transient)

(defvar org-glance-transient--view "all")

(defclass org-glance-transient-variable:view (org-glance-transient-variable) ())

(cl-defmethod transient-infix-read ((obj org-glance-transient-variable:view))
  (oset obj value (symbol-name (org-glance-read-view-id))))

(cl-defmethod transient-format-value ((obj org-glance-transient-variable:view))
  (let* ((val (or (oref obj value) (oref obj default)))
         (val-pretty (propertize val 'face 'transient-argument)))
    (format "(%s)" val-pretty)))

(transient-define-infix org-glance-form-action-view ()
  :class 'org-glance-transient-variable:view
  :variable 'org-glance-transient--view
  :reader 'org-glance-read-view-id
  :default "all")

(transient-define-prefix org-glance-form-action ()
  "Perform action on selected view/headlines"

  ["Analytics"

   [("A" "Agenda" org-glance-view-agenda)
    ("D" "Dashboard" org-glance-show-report)]

   [("-v" "View" org-glance-form-action-view)]]

  ["Views"
   [("U" "Update" org-glance-view-update)]
   [("V" "Visit" org-glance-view-visit)]]

  ["Headlines"
   ;; [("c" "Capture" org-glance-action-extract)]
   [("e" "Extract" org-glance-action-extract)]
   [("j" "Open" org-glance-action-open)]
   ;; [("i" "Insert" org-glance-action-insert)]
   [("m" "Materialize" org-glance-action-materialize)]
   [("v" "Visit" org-glance-action-visit)]])

(org-glance-module-provide)
