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
   [("a" "Agenda" org-glance-view-agenda)
    ("d" "Dashboard" org-glance-show-report)]

   [("-v" "View" org-glance-form-action-view)]]

  ["View"
   [("u" "Update" org-glance-view-update)
    ("s" "Summary" org-glance-view-visit)]]

  ["Headline"
   [("e" "Extract" org-glance-action-extract)
    ("j" "Open" org-glance-action-open)
    ("m" "Materialize" org-glance-action-materialize)
    ("v" "Visit" org-glance-action-visit)]
   [("c" "Capture" org-glance-capture-subtree-at-point)
    ("r" "Refer" org-glance:add-relation)]])

(org-glance-module-provide)
