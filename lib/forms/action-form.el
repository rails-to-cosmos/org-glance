(require 'org-glance-module)

(require 'transient)
(require 'load-relative)

(org-glance-module-import lib.core.view)
(org-glance-module-import lib.forms.base)

(defvar org-glance-transient--view "all")

(defclass org-glance-transient-variable:view (org-glance-transient-variable) ())

(cl-defmethod transient-infix-read ((obj org-glance-transient-variable:view))
  (oset obj value (symbol-name (org-glance-view:completing-read))))

(cl-defmethod transient-format-value ((obj org-glance-transient-variable:view))
  (let* ((val (or (oref obj value) (oref obj default)))
         (val-pretty (propertize val 'face 'transient-argument)))
    (format "(%s)" val-pretty)))

(transient-define-infix org-glance-form-action-view ()
  :class 'org-glance-transient-variable:view
  :variable 'org-glance-transient--view
  :reader 'org-glance-view:completing-read
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
   [("e" "Extract" org-glance-action-extract)]
   [("j" "Open" org-glance-action-open)]
   [("m" "Materialize" org-glance-action-materialize)]
   [("v" "Visit" org-glance-action-visit)]]

  ["Subtree"
   [("c" "Capture" org-glance-capture-subtree-at-point)]
   [("r" "Relation" org-glance:add-relation)]
   ])

(org-glance-module-provide)
