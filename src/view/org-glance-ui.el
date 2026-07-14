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

;;;###autoload
(transient-define-prefix org-glance-transient ()
  "Perform action on selected view/headlines"
  [:description (lambda (&rest _) (format "Filter: %s" (org-glance-filter:describe org-glance-filter-spec)))
   ("s" "Todo state" org-glance-transient:filter-by-state)
   ("/" "Title substring" org-glance-transient:filter-by-substring)
   ("c" "Clear (all)" org-glance-transient:filter-clear)]
  ["Overview"
   [("a" "Agenda" org-glance-agenda)
    ("o" "Overview" org-glance-overview)
    ("t" "All tags" org-glance-tags)]]
  ["Actions"
   [("+" "Capture headline" org-glance-capture)
    ("e" "Extract property" org-glance-extract)
    ("j" "Open link" org-glance-open)
    ("m" "Materialize headline" org-glance-materialize)
    ("l" "LLM session" org-glance-llm)
    ("C" "Configure tags" org-glance-tag-config-edit)]]
  (interactive)
  (transient-setup 'org-glance-transient))

(provide 'org-glance-ui)
