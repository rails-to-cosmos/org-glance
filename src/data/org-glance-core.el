;;; org-glance-core.el --- org-glance session state -*- lexical-binding: t; -*-

;;; Commentary:
;; The session-level primitives every command depends on: the customization
;; group, the content directory, the current global graph, and the init check.
;; They live in this low-level module -- required by the view/command modules --
;; so an autoloaded entry command finds them even when the umbrella `org-glance'
;; file (which requires all the parts, so cannot be required back) is not loaded.
;; `org-glance-init' stays in `org-glance.el' (it drives migration); it SETS the
;; `org-glance-graph' defined here.

;;; Code:

(require 'cl-lib)
(require 'org)                          ; `org-directory'

(defgroup org-glance nil "Org-mode mindmap explorer."
  :tag "Org Glance"
  :group 'org)

(defcustom org-glance-directory org-directory
  "Main location for all Org mode content managed by `org-glance'."
  :group 'org-glance
  :type 'directory)

(defvar org-glance-graph nil
  "Current global graph instance.
Constructed by `org-glance-init'; nil until the system is initialized.")

(cl-defun org-glance-initialized? ()
  "Return the global graph if the system is initialized, else nil."
  org-glance-graph)

(declare-function org-glance-init "org-glance")

(cl-defun org-glance-ensure-init ()
  "Return the global graph, initializing org-glance on first use.
The guard commands run before touching the graph: when unbuilt, run
`org-glance-init' (autoloaded) so a fresh install needs no manual init."
  (or org-glance-graph
      (progn (org-glance-init) org-glance-graph)))

(defcustom org-glance-overview-default-view 'org-glance-table
  "Which view `org-glance-overview' opens by default.
Value `org-glance-table' opens the sortable table dashboard (default); value
`org-glance-overview' opens the classic org-text overview (backward-compatible).
`T' toggles either view to the other.  Legacy values `table'/`org' still work."
  :group 'org-glance
  :type '(choice (const :tag "Table dashboard (org-glance-table)" org-glance-table)
                 (const :tag "Org-text overview (org-glance-overview)" org-glance-overview)))

(cl-defun org-glance-overview--default-table? ()
  "Non-nil when `org-glance-overview-default-view' selects the table dashboard."
  (memq org-glance-overview-default-view '(org-glance-table table)))

(provide 'org-glance-core)
;;; org-glance-core.el ends here
