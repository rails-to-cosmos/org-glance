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
The guard every interactive command runs before touching the graph.  When the
graph is not built yet, run `org-glance-init' (autoloaded from the umbrella
`org-glance' file) so a fresh install just works without a manual init step."
  (or org-glance-graph
      (progn (org-glance-init) org-glance-graph)))

(provide 'org-glance-core)
;;; org-glance-core.el ends here
