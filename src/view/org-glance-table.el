;; -*- lexical-binding: t -*-

;;; org-glance-table.el --- table-view-backed headline dashboard

;;; Commentary:
;; A sortable, badge-coloured TABLE view over the graph, sister to the org-text
;; `org-glance-overview'.  Where the overview is a real org file (folding,
;; planning, agenda, an on-disk cache + coherence machinery), this is a flat,
;; in-memory `table-view' buffer: one row per matching headline, columns for
;; state / title / tags / scheduled / deadline / priority, client-side sort, and
;; the same id-keyed actions (materialize / open / extract).
;;
;; It reuses the headless half of org-glance verbatim: rows come from
;; `org-glance-graph:headlines' filtered by `org-glance-filter:predicate' (the
;; exact pair the overview renders from), and the spec is built in pure elisp --
;; the generic `table-view' core needs no backend process.
;;
;; COHERENCE.  Two mechanisms keep a live table from showing stale rows, mirroring
;; the overview but lighter (no disk cache to keep byte-fresh):
;;   1. EAGER push on the edit path: every metadata refresh
;;      (`org-glance-material:sync-functions') upserts JUST the affected row when
;;      it still matches the buffer's filter, or removes it when it no longer
;;      does.
;;   2. LAZY pull at the display boundary: when the buffer is (re)displayed or its
;;      window selected, it re-fills from the graph iff `headlines.jsonl' advanced
;;      since the last fill -- catching capture / delete / reindex / compaction /
;;      another Emacs, which the sync hook never fires for.
;;
;; Browse with `org-glance-table'; act on the row at point.

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'f)
(require 's)

(require 'table-view)
(require 'org-glance-graph)
(require 'org-glance-filter)
(require 'org-glance-material)

(defvar org-glance-graph)
(declare-function org-glance-initialized? "org-glance")
(declare-function org-glance-overview:visit "org-glance-overview")
(declare-function org-glance-capture "org-glance-capture")
(declare-function org-glance-capture:completing-read-tag "org-glance-capture")

;;; State colour palette
;;
;; The badge palette also fixes the sort priority for the state column (the core
;; sorts a badge column by declared order), so the spec lists the graph's actual
;; states active-before-done -- making "sort by state" default to active first.

(defcustom org-glance-table-state-colors
  '(("TODO"      . "#e0af68")
    ("NEXT"      . "#e67e22")
    ("STARTED"   . "#749AF7")
    ("WAITING"   . "#9b59b6")
    ("HOLD"      . "#9b59b6")
    ("DONE"      . "#9ece6a")
    ("CANCELLED" . "#565f89")
    ("CANCELED"  . "#565f89"))
  "Foreground colour for each todo state in the table's State badge column.
States not listed here render in `org-glance-table-default-state-color'."
  :group 'org-glance
  :type '(alist :key-type string :value-type color))

(defcustom org-glance-table-default-state-color "#7aa2f7"
  "Badge colour for a todo state not found in `org-glance-table-state-colors'."
  :group 'org-glance
  :type 'color)

(cl-defun org-glance-table:--state-color (state)
  "Badge colour for todo STATE."
  (or (cdr (assoc state org-glance-table-state-colors))
      org-glance-table-default-state-color))

(cl-defun org-glance-table:--state-badges (graph)
  "Badge palette (a list of `((value . S) (color . C))') for GRAPH's states.
Active states first then done states, each group in the sorted order
`org-glance-graph:states' returns, so the palette doubles as an active-first sort
priority.  `org-done-keywords' is bound for the active/done split, since it is
unset outside an Org buffer."
  (let* ((states (org-glance-graph:states graph))
         (org-done-keywords (org-glance--done-keywords))
         (done (cl-remove-if-not (lambda (s) (member s org-done-keywords)) states))
         (active (cl-remove-if (lambda (s) (member s org-done-keywords)) states)))
    (cl-loop for state in (append active done)
             collect `((value . ,state) (color . ,(org-glance-table:--state-color state))))))

;;; Spec + rows (built in pure elisp -- no backend process)

(cl-defun org-glance-table:--spec (graph filter)
  "Build the `table-view' spec (a plain alist) for GRAPH under FILTER.
FILTER is only used for the human-readable title; rows are produced by the
fill-fn.  Columns: state (badge), title, tags, scheduled, deadline, priority --
all sortable.  Default sort is the state column ascending (active first)."
  `((title . ,(format "org-glance table: %s" (org-glance-filter:describe filter)))
    (columns . (((key . "state")    (header . "State")     (type . "badge") (sortable . t) (align . "left")
                 (badges . ,(org-glance-table:--state-badges graph)))
                ((key . "title")    (header . "Title")     (type . "text")  (sortable . t) (align . "left"))
                ((key . "tags")     (header . "Tags")      (type . "text")  (sortable . t) (align . "left"))
                ((key . "schedule") (header . "Scheduled") (type . "text")  (sortable . t) (align . "left"))
                ((key . "deadline") (header . "Deadline")  (type . "text")  (sortable . t) (align . "left"))
                ((key . "priority") (header . "Pri")       (type . "text")  (sortable . t) (align . "left"))))
    (actions . (((key . "RET") (command . "materialize") (label . "Materialize"))
                ((key . "m")   (command . "materialize") (label . "Materialize"))
                ((key . "o")   (command . "open")        (label . "Open link"))
                ((key . "e")   (command . "extract")     (label . "Extract"))
                ((key . "g")   (command . "refresh")     (label . "Refresh"))
                ((key . "T")   (command . "overview")    (label . "Overview"))
                ((key . "+")   (command . "capture")     (label . "Capture"))))
    (sort . ((column . "state") (ascending . t)))))

(cl-defun org-glance-table:--row (metadata)
  "Build a `table-view' row alist for headline METADATA.
The id is the ORG_GLANCE_ID (passed to the action handlers); cells are display
strings: tags are joined with `:' (they are interned symbols, never a raw list),
priority is its letter, absent values are the empty string."
  (cl-check-type metadata org-glance-headline-metadata)
  (let ((tags (append (org-glance-headline-metadata:tags metadata) nil))
        (priority (org-glance-headline-metadata:priority metadata)))
    `((id . ,(org-glance-headline-metadata:id metadata))
      (cells . ((state    . ,(or (org-glance-headline-metadata:state metadata) ""))
                (title    . ,(or (org-glance-headline-metadata:title metadata) ""))
                (tags     . ,(if tags (s-join ":" (mapcar (lambda (x) (format "%s" x)) tags)) ""))
                (schedule . ,(or (org-glance-headline-metadata:schedule metadata) ""))
                (deadline . ,(or (org-glance-headline-metadata:deadline metadata) ""))
                (priority . ,(if (integerp priority) (char-to-string priority) "")))))))

;;; Per-buffer state

(defvar-local org-glance-table--graph nil
  "Graph backing the current table buffer (also the org-glance-table marker).")
(defvar-local org-glance-table--spec nil
  "Normalised filter spec the current table buffer was generated with.")
(defvar-local org-glance-table--mtime nil
  "Mtime of `headlines.jsonl' at the current table buffer's last fill.")

(cl-defun org-glance-table:--mtime (path)
  "Modification time of PATH, or nil when it does not exist."
  (and (f-exists? path)
       (file-attribute-modification-time (file-attributes path))))

(cl-defun org-glance-table:--rows (graph keep?)
  "Rows for GRAPH's live headlines satisfying predicate KEEP?, in graph order."
  (cl-loop for meta in (org-glance-graph:headlines graph)
           when (funcall keep? meta)
           collect (org-glance-table:--row meta)))

(cl-defun org-glance-table:--reload (buffer)
  "Re-fill BUFFER from the live graph, then re-apply its current/default sort.
Used by `g' (refresh) and the lazy display-boundary check.  `table-view-refresh'
re-runs the fill-fn (which resets the sort), so re-apply it to keep the view
ordered the way the user left it (or the spec default on first sort)."
  (table-view-refresh buffer)
  (when-let ((buf (get-buffer buffer)))
    (with-current-buffer buf (table-view-sort))))

;;; Live coherence

(cl-defun org-glance-table:--patch (metadata)
  "Reflect METADATA in the CURRENT table buffer: upsert it when it still matches
this buffer's filter, drop it when it no longer does.
Unlike the overview's org-text patch, a newly-matching headline is simply
appended by `table-view-upsert-row' (the user re-sorts), so no full rebuild is
needed."
  (let ((id (org-glance-headline-metadata:id metadata))
        (keep? (org-glance-filter:predicate org-glance-table--spec)))
    (if (funcall keep? metadata)
        (table-view-upsert-row (current-buffer) (org-glance-table:--row metadata))
      (table-view-remove-row (current-buffer) id))))

(cl-defun org-glance-table:on-headline-update (graph metadata)
  "Patch METADATA into every open table buffer of GRAPH.
Registered on `org-glance-material:sync-functions'; a view update must never
break the save that triggered it, so per-buffer errors are demoted."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'table-view-mode)
                 org-glance-table--graph
                 (eq org-glance-table--graph graph))
        (with-demoted-errors "org-glance: table update failed: %S"
          (org-glance-table:--patch metadata))))))

(add-hook 'org-glance-material:sync-functions #'org-glance-table:on-headline-update)

(cl-defun org-glance-table:--stale? ()
  "Non-nil when the CURRENT table buffer's fill predates the store's last change."
  (let ((src (org-glance-graph:headline-meta-path org-glance-table--graph)))
    (and (f-exists? src)
         (or (null org-glance-table--mtime)
             (time-less-p org-glance-table--mtime (org-glance-table:--mtime src))))))

(cl-defun org-glance-table:--refresh-when-stale (&optional window)
  "Reload WINDOW's (or the current) table iff the store changed since its fill.
The lazy half of coherence: runs when the buffer is (re)displayed or selected,
catching mutations the sync hook never fires for (capture, delete, reindex,
compaction, another Emacs)."
  (with-current-buffer (if (windowp window) (window-buffer window) (current-buffer))
    (when (and (derived-mode-p 'table-view-mode)
               org-glance-table--graph
               (org-glance-table:--stale?))
      (with-demoted-errors "org-glance: table refresh failed: %S"
        (org-glance-table:--reload (current-buffer))))))

;;; Actions (id-keyed; the table-view core hands each handler the row's id)

(cl-defun org-glance-table:--act-materialize (graph id _row)
  (when id (switch-to-buffer (org-glance-material:open graph id))))

(cl-defun org-glance-table:--headline (graph id)
  "The live headline for ID, or a `user-error' (the table can outlive the graph)."
  (or (org-glance-graph:headline graph id)
      (user-error "Headline no longer in graph (table is stale; press `g' to refresh)")))

(cl-defun org-glance-table:--act-open (graph id _row)
  (when id (org-glance-material:open-link (org-glance-table:--headline graph id))))

(cl-defun org-glance-table:--act-extract (graph id _row)
  (when id (org-glance-material:extract (org-glance-table:--headline graph id))))

;;; Browser

(cl-defun org-glance-table:visit (graph &optional filter)
  "Open GRAPH's table for FILTER, one buffer per filter description.
Honours the same filter language as the overview (see
`org-glance-filter:predicate')."
  (let* ((spec (org-glance-filter:normalize-spec filter))
         (keep? (org-glance-filter:predicate spec))
         (buffer-name (format "*org-glance-table: %s*" (org-glance-filter:describe spec)))
         (src (org-glance-graph:headline-meta-path graph))
         (fill-fn (lambda (buf)
                    (with-current-buffer buf
                      (table-view-set-rows buf (org-glance-table:--rows graph keep?))
                      (setq org-glance-table--mtime (org-glance-table:--mtime src)))))
         (handlers (list (cons "materialize" (lambda (id row) (org-glance-table:--act-materialize graph id row)))
                         (cons "open"        (lambda (id row) (org-glance-table:--act-open graph id row)))
                         (cons "extract"     (lambda (id row) (org-glance-table:--act-extract graph id row)))
                         (cons "refresh"     (lambda (_id _row) (org-glance-table:--reload (current-buffer))))
                         (cons "overview"    (lambda (_id _row) (org-glance-overview:visit graph spec)))
                         (cons "capture"     (lambda (_id _row)
                                               (org-glance-capture (or (org-glance-filter:tags spec)
                                                                       (org-glance-capture:completing-read-tag))
                                                                   "")))))
         (buf (table-view-display buffer-name (org-glance-table:--spec graph spec) handlers fill-fn)))
    (with-current-buffer buf
      (setq org-glance-table--graph graph
            org-glance-table--spec spec)
      (table-view-sort)                 ; honour the spec's default sort on open
      (add-hook 'window-buffer-change-functions #'org-glance-table:--refresh-when-stale nil t)
      (add-hook 'window-selection-change-functions #'org-glance-table:--refresh-when-stale nil t))
    buf))

(cl-defun org-glance-table:completing-read-tag ()
  "Prompt for a tag from the graph's headlines; empty input means \"all\"."
  (cl-assert (org-glance-initialized?))
  (let ((choice (completing-read "Table tag (empty for all): "
                                 (org-glance-graph:tags org-glance-graph))))
    (unless (string-empty-p choice) choice)))

;;;###autoload
(cl-defun org-glance-table (&optional tag)
  "Browse the graph as a sortable, badge-coloured table, optionally filtered.
Interactively, prompt for a tag (empty input = no tag constraint) and overlay it
on the ambient `org-glance-filter-spec' (default: active headlines) -- exactly
like `org-glance-overview', but rendered as a flat table.  Sort with `^' (cycle
column) and `~' (toggle direction); act on the row at point with RET/m, o, e."
  (interactive (list (org-glance-table:completing-read-tag)))
  (cl-assert (org-glance-initialized?))
  (org-glance-table:visit org-glance-graph
                          (org-glance-filter:merge org-glance-filter-spec tag)))

(provide 'org-glance-table)
;;; org-glance-table.el ends here
