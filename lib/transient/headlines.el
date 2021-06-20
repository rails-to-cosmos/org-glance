(require 'org-glance-module)

(require 'transient)
(require 'load-relative)

(org-glance-module-import lib.core.view)
(org-glance-module-import lib.transient.base)

(defclass org-glance-transient-variable:view (org-glance-transient-variable) ())

(cl-defmethod transient-infix-read ((obj org-glance-transient-variable:view))
  (if (string= org-glance-form:view org-glance-view:all)
      (condition-case nil
          (oset obj value (symbol-name (org-glance-view:completing-read)))
        (error nil))
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

  ;; ["Filters"
  ;;  [("-v" "Filter results by VIEW" org-glance-form-action-view)]]

  ["Actions"
   [("e" "Extract" org-glance-action-extract)
    ("j" "Jump" org-glance-action-open)]]

  ["Manage views"
   [("v a" "Agenda" org-glance-view:agenda)
    ;; ("v d" "Dashboard" org-glance-show-report)
    ("v d" "Doctor" org-glance-view:doctor)
    ]
   [("v u" "Update" org-glance-view:summary)
    ("v v" "Visit" org-glance-view-visit)]]

  ["Manage headlines"
   [("h m" "Materialize" org-glance-action-materialize)
    ("h v" "Visit" org-glance-action-visit)]
   [("h c" "Capture" org-glance-capture-subtree-at-point)
    ("h r" "Refer" org-glance:add-relation)]])

(org-glance-module-provide)
