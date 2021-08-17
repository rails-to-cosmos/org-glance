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
  ["Actions"
   [
    ("e" "Extract" org-glance-action-extract)
    ("j" "Jump" org-glance-action-open)
    ("m" "Materialize" org-glance-action-materialize)
    ("v" "Visit" org-glance-overview:visit)]
   ]

  ["Capture"
   ("c" "Capture" org-glance-view:capture-headline-at-point)])

(org-glance-module-provide)
