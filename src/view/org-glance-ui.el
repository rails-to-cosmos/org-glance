;; -*- lexical-binding: t -*-

(require 'transient)
(require 'org-glance-utils)
(require 'org-glance-graph)
(require 'org-glance-filter)

(require 'org-glance-core)
;; The transient's suffix commands live in other files; declared for the compiler.
(declare-function org-glance-table "org-glance-table")
(declare-function org-glance-tags "org-glance-tags")
(declare-function org-glance-tag-config-edit "org-glance-tag-config")
(defvar org-glance-plugins)   ; org-glance.el: the enabled plugin list, read by
                              ; the System heading (that file requires this one)
(declare-function org-glance-plugin-feature "org-glance")

;;; Ambient filter controls
;;
;; These mirror the in-overview `/' menu (`org-glance-overview-filter') but
;; target the global `org-glance-filter-spec' -- the filter every action here
;; respects: the pickers gate their candidates by it, and the overview/agenda
;; overlay it.  Both menus share the clause builders in `org-glance-filter', so
;; the two stay consistent.

(transient-define-suffix org-glance-transient:filter-by-state ()
  "Set the ambient filter's todo-state dimension (active / done / all / a state)."
  :transient t
  (interactive)
  (setq org-glance-filter-spec
        (org-glance-filter:set-state
         org-glance-filter-spec
         (org-glance-filter:read-state (and (org-glance-initialized?) org-glance-graph)))))

(transient-define-suffix org-glance-transient:filter-by-substring ()
  "Set the ambient filter's title-substring dimension (empty input clears it)."
  :transient t
  (interactive)
  (setq org-glance-filter-spec
        (org-glance-filter:set-substring org-glance-filter-spec
                                         (read-string "Title contains: "))))

(transient-define-suffix org-glance-transient:filter-clear ()
  "Clear the ambient filter: act on all headlines."
  :transient t
  (interactive)
  (setq org-glance-filter-spec nil))

(defun org-glance-transient--view-mode ()
  "Current overview display mode as a short string: \"table\" or \"org\".
Display only -- the toggle and its label branch on
`org-glance-overview--default-table?' directly."
  (if (org-glance-overview--default-table?) "table" "org"))

(defun org-glance-transient--overview-description (&rest _)
  "Overview label tagged with the current `org-glance-overview-default-view'."
  (format "Overview [%s]"
          (propertize (org-glance-transient--view-mode) 'face 'transient-value)))

(defun org-glance-transient--toggle-view-description (&rest _)
  "Toggle label naming the display mode it would switch TO."
  (format "Switch layout -> %s"
          (propertize (if (org-glance-overview--default-table?) "org" "table")
                      'face 'transient-value)))

(transient-define-suffix org-glance-transient:toggle-view ()
  "Toggle `org-glance-overview-default-view' between the table and org-text view.
Symmetric to the `T' key inside a view; stays in the menu so the `o' label and
this one update in place."
  :transient t
  (interactive)
  (setq org-glance-overview-default-view
        (if (org-glance-overview--default-table?)
            'org-glance-overview
          'org-glance-table))
  (message "Overview default view: %s" (org-glance-transient--view-mode)))

(defun org-glance-transient--plugins-description (&rest _)
  "The System group heading, naming the enabled plugins.
A plugin whose library failed to load is marked: the init loader is
error-demoted (invariant 9), so a broken or missing one stays enabled yet
absent, and this heading is where that shows."
  (concat "System   plugins: "
          (if org-glance-plugins
              (mapconcat (lambda (plugin)
                           (if (featurep (org-glance-plugin-feature plugin))
                               (symbol-name plugin)
                             (format "%s (not loaded)" plugin)))
                         org-glance-plugins ", ")
            "none")))

;;;###autoload
(transient-define-prefix org-glance-transient ()
  "Perform action on selected view/headlines"
  [:description (lambda (&rest _) (format "Filter: %s" (org-glance-filter:describe org-glance-filter-spec)))
   ("s" "Todo state" org-glance-transient:filter-by-state)
   ("/" "Title substring" org-glance-transient:filter-by-substring)
   ("c" "Clear (all)" org-glance-transient:filter-clear)]
  ;; Three semantic layers: TAG (collections), HEADLINE (one node; the
  ;; material buffer mirrors these under C-c), SYSTEM (the installation).
  ["Tag"
   [("o" org-glance-overview :description org-glance-transient--overview-description)
    ;; ("a" "Agenda" org-glance-agenda)
    ("T" "All tags" org-glance-tags)
    ("C" "Configure" org-glance-tag-config-edit)]
   [("-l" org-glance-transient:toggle-view :description org-glance-transient--toggle-view-description)]]
  ["Headline"
   [("+" "Capture" org-glance-capture)
    ("m" "Materialize" org-glance-materialize)
    ("j" "Open link" org-glance-open)
    ("e" "Extract property" org-glance-extract)]
   [("-d" "Decrypt" "--decrypt")]]
  [:description org-glance-transient--plugins-description
   [("I" "Enable plugin" org-glance-plugin-enable)
    ("U" "Disable plugin" org-glance-plugin-disable)
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (transient-setup 'org-glance-transient))

(provide 'org-glance-ui)
