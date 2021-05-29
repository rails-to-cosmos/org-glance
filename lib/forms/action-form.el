(require 'org-glance-module)

(require 'transient)
(require 'load-relative)

(org-glance-module-import lib.core.view)
(org-glance-module-import lib.forms.base)

(defconst org-glance-view:all "All")
(defvar org-glance-form:view org-glance-view:all)

(defclass org-glance-transient-variable:view (org-glance-transient-variable) ())

(cl-defmethod transient-infix-read ((obj org-glance-transient-variable:view))
  (if (string= org-glance-form:view org-glance-view:all)
      (oset obj value (symbol-name (org-glance-view:completing-read)))
    (oset obj value org-glance-view:all)))

(cl-defmethod transient-format-value ((obj org-glance-transient-variable:view))
  (let* ((val (or (oref obj value) (oref obj default)))
         (val-pretty (propertize val 'face 'transient-argument)))
    (format "(%s)" val-pretty)))

(transient-define-infix org-glance-form-action-view ()
  :class 'org-glance-transient-variable:view
  :variable 'org-glance-form:view
  :reader 'org-glance-view:completing-read
  :default org-glance-form:view)

(transient-define-prefix org-glance-form-action ()
  "Perform action on selected view/headlines"

  ["View"
   [("v a" "Agenda" org-glance-view-agenda)
    ("v u" "Update" org-glance-view:update)
    ("v s" "Summary" org-glance-view-visit)
    ("v d" "Dashboard" org-glance-show-report)]
   [("V" "View Filter" org-glance-form-action-view)]]

  ["Headline"
   [("e" "Extract property" org-glance-action-extract)
    ("j" "Open links" org-glance-action-open)
    ("m" "Materialize" org-glance-action-materialize)
    ;; ("v" "Visit" org-glance-action-visit)
    ]

   [("c" "Capture" org-glance-capture-subtree-at-point)
    ("r" "Refer" org-glance:add-relation)]])

(org-glance-module-provide)
