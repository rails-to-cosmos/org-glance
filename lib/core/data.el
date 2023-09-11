(require 'org-glance-module)

(cl-defstruct (org-glance-view (:constructor org-glance-view:create))
  "This structure contains metadata about categorized `org-mode' headlines."
  (id nil :type 'symbol :read-only t :documentation "Unique identifier for `org-glance-view'.")
  (type nil :type 'list :read-only nil :documentation "Determines list of actions allowed to use on headlines of this view.")
  (scope nil :type 'list :read-only nil :documentation "List of files where org-glance should search for headlines for this view."))

(org-glance:provide)
