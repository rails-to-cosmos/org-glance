;;; org-glance-core.el --- org-glance session state -*- lexical-binding: t; -*-

;; Low-level: `org-glance' requires this file, so it can never be required back.

;;; Code:

(require 'cl-lib)
(require 'org)                          ; `org-directory'

(defgroup org-glance nil "Projections over Org-mode headlines."
  :tag "Org Glance"
  :group 'org)

(defcustom org-glance-directory org-directory
  "Main location for all Org mode content managed by `org-glance'."
  :group 'org-glance
  :type 'directory)

(defvar org-glance-graph nil
  "Current global graph instance; nil until `org-glance-init' builds it.")

(cl-defun org-glance-initialized? ()
  "Return the global graph if the system is initialized, else nil."
  org-glance-graph)

(declare-function org-glance-init "org-glance")

(cl-defun org-glance-ensure-init ()
  "Return the global graph, running `org-glance-init' on first use."
  (or org-glance-graph
      (progn (org-glance-init) org-glance-graph)))

(defcustom org-glance-overview-default-view 'org-glance-table
  "Which view `org-glance-overview' opens by default.
`org-glance-table' (table dashboard) or `org-glance-overview' (org text); `T'
toggles it.  The legacy values `table' and `org' still work."
  :group 'org-glance
  :type '(choice (const :tag "Table dashboard (org-glance-table)" org-glance-table)
                 (const :tag "Org-text overview (org-glance-overview)" org-glance-overview)))

(cl-defun org-glance-overview--default-table? ()
  "Non-nil when `org-glance-overview-default-view' is the table dashboard."
  (memq org-glance-overview-default-view '(org-glance-table table)))

(provide 'org-glance-core)
