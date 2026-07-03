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
;; COHERENCE.  PULL at the display boundary: a materialized save only appends to
;; the WAL and FLAGS open views stale (`org-glance-view:mark-graph-stale' -- a
;; boolean + the `glance:stale' lighter), never patching a row here.  When the
;; buffer is (re)displayed or its window selected, it re-fills from the graph iff
;; `headlines.jsonl' advanced since its last fill -- catching its own saves AND
;; capture / delete / reindex / compaction / another Emacs by the same check.
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
(require 'org-glance-tag-config)
(require 'org-glance-material)
(require 'org-glance-view)

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

(cl-defun org-glance-table--state-color (state)
  "Badge colour for todo STATE."
  (or (cdr (assoc state org-glance-table-state-colors))
      org-glance-table-default-state-color))

(cl-defun org-glance-table--state-badges (graph)
  "Badge palette (a list of `((value . S) (color . C))') for GRAPH's states.
Active states first then done states, each group in the sorted order
`org-glance-graph:states' returns, so the palette is also an active-first sort
priority.  The active/done split uses the ambient `org-done-keywords' (bound by
`org-glance-table:visit' to the tag's cycle), falling back to the global set."
  (let* ((states (org-glance-graph:states graph))
         (done-kw (or org-done-keywords (org-glance--done-keywords)))
         (done (cl-remove-if-not (lambda (s) (member s done-kw)) states))
         (active (cl-remove-if (lambda (s) (member s done-kw)) states)))
    (cl-loop for state in (append active done)
             collect `((value . ,state) (color . ,(org-glance-table--state-color state))))))

;;; Spec + rows (built in pure elisp -- no backend process)

(cl-defun org-glance-table--spec (graph filter)
  "Build the `table-view' spec (a plain alist) for GRAPH under FILTER.
FILTER is only used for the human-readable title; rows are produced by the
fill-fn.  Columns: state (badge), title, tags, scheduled, deadline, priority --
all sortable.  Default sort is the state column ascending (active first)."
  `((title . ,(format "org-glance table: %s" (org-glance-filter:describe filter)))
    (columns . (((key . "state")    (header . "State")     (type . "badge") (sortable . t) (align . "left")
                 (badges . ,(org-glance-table--state-badges graph)))
                ((key . "title")    (header . "Title")     (type . "text")  (sortable . t) (align . "left"))
                ((key . "tags")     (header . "Tags")      (type . "text")  (sortable . t) (align . "left"))
                ((key . "schedule") (header . "Scheduled") (type . "text")  (sortable . t) (align . "left"))
                ((key . "deadline") (header . "Deadline")  (type . "text")  (sortable . t) (align . "left"))
                ((key . "priority") (header . "Pri")       (type . "text")  (sortable . t) (align . "left"))))
    (actions . (((key . "RET") (command . "materialize") (label . "Materialize"))
                ((key . "o")   (command . "open")        (label . "Open link"))
                ((key . "e")     (command . "extract")   (label . "Extract"))
                ((key . "g")     (command . "refresh")   (label . "Refresh"))
                ((key . "T")     (command . "overview")  (label . "Overview"))
                ((key . "+")     (command . "capture")   (label . "Capture"))
                ((key . "C-c C-t") (command . "todo")    (label . "Todo"))))
    (sort . ((column . "state") (ascending . t)))))

(cl-defun org-glance-table--row (metadata)
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

(defvar-local org-glance-table--spec nil
  "Normalised filter spec the current table buffer was generated with.")
(defvar-local org-glance-table--mtime nil
  "Mtime of `headlines.jsonl' at the current table buffer's last fill.")

(cl-defun org-glance-table--mtime (path)
  "Modification time of PATH, or nil when it does not exist."
  (and (f-exists? path)
       (file-attribute-modification-time (file-attributes path))))

(cl-defun org-glance-table--rows (graph keep?)
  "Rows for GRAPH's live headlines satisfying predicate KEEP?, in graph order."
  (cl-loop for meta in (org-glance-graph:headlines graph)
           when (funcall keep? meta)
           collect (org-glance-table--row meta)))

(cl-defun org-glance-table--apply-sort ()
  "Apply the sort currently in `table-view--sort-keys' -- the spec's declared
default (seeded by `table-view-display'), a restored per-view config, or the
sort the user left before a reload.
NB reaches into `table-view' internals (`--sort-keys'/`--sorted'): the core
seeds the keys but applies them only when rows arrive with the spec, whereas
org-glance fills rows via `fill-fn' AFTER display.  A public \"apply seeded
sort\" entry point in `table-view' would remove this coupling (matters when
de-vendoring to MELPA)."
  (when table-view--sort-keys
    (setq table-view--sorted t)
    (table-view-sort)))

(cl-defun org-glance-table--reload (buffer)
  "Re-fill BUFFER from the live graph, then re-apply its current sort.
Used by `g' (refresh) and the lazy display-boundary check.  `table-view-refresh'
re-runs the fill-fn (which resets rows to load order but keeps `--sort-keys'),
so re-apply the sort -- preserving whatever ordering the user left."
  (table-view-refresh buffer)
  (when-let ((buf (get-buffer buffer)))
    (with-current-buffer buf
      (org-glance-table--apply-sort)
      (org-glance-view:mark-fresh))))

;;; Live coherence (pull at the display boundary, driven by `org-glance-view')
;;
;; This file supplies only the two view-specific pieces `org-glance-view:register'
;; takes -- the STALE-FN (`--stale?') and the RELOAD-FN (`--reload') -- in
;; `org-glance-table:visit'.  A table is a NON-FILE projection rebuilt from the
;; graph, so the shared driver always reloads it at the display boundary (it
;; discards no user data, unlike the overview's file-backed buffer).

(cl-defun org-glance-table--stale? (graph)
  "Non-nil when the CURRENT table buffer's fill predates GRAPH's last change.
The view's STALE-FN (see `org-glance-view:register'), closed over GRAPH at visit
so the buffer needs no graph of its own."
  (let ((src (org-glance-graph:headline-meta-path graph)))
    (and (f-exists? src)
         (or (null org-glance-table--mtime)
             (time-less-p org-glance-table--mtime (org-glance-table--mtime src))))))

;;; Actions (id-keyed; the table-view core hands each handler the row's id)

(cl-defun org-glance-table--act-materialize (graph id _row)
  (when id (switch-to-buffer (org-glance-material:open graph id))))

(cl-defun org-glance-table--headline (graph id)
  "The live headline for ID, or a `user-error' (the table can outlive the graph)."
  (or (org-glance-graph:headline graph id)
      (user-error "Headline no longer in graph (table is stale; press `g' to refresh)")))

(cl-defun org-glance-table--act-open (graph id _row)
  (when id (org-glance-material:open-link (org-glance-table--headline graph id))))

(cl-defun org-glance-table--act-extract (graph id _row)
  (when id (org-glance-material:extract (org-glance-table--headline graph id))))

(cl-defun org-glance-table--act-todo (graph id _row)
  "Advance ID's TODO state exactly like `C-c C-t' (via
`org-glance-material:change-todo-live'), then reload the table and return to the
row once the change (and any note) is committed."
  (when id
    (let ((arg current-prefix-arg)          ; the dispatch lambda is a bare `interactive'
          (line (line-number-at-pos)))       ; the reload re-renders from the top
      (org-glance-material:change-todo-live
       graph id arg
       (lambda (state)
         (org-glance-table--reload (current-buffer))
         ;; Preserve point: follow the row if it is still visible, else keep the
         ;; screen line (the row may have left the filter -- e.g. now DONE under an
         ;; active filter -- and the next row shifts into it, org-agenda-style)
         ;; rather than jumping to the beginning of the buffer.
         (unless (table-view--goto-id id)
           (goto-char (point-min))
           (forward-line (1- line)))
         (message "State: %s" (if (s-present? state) state "(none)")))))))

;;; Per-view persistence: column order + sort, keyed by filter identity
;;
;; The user's column reordering (`table-view' column-move) and sort choice live in
;; the table buffer (`table-view--spec' columns + `table-view--sort-keys').  Persist
;; them PER FILTER so reopening the same view restores the layout across sessions.
;; Stored as one `read'-able alist at `<store>/config/table-views.eld', keyed by the
;; canonical filter identity (the same key basis the overview cache uses).  A change
;; is saved from a buffer-local `post-command-hook' that only writes when the layout
;; actually differs from the last snapshot (column moves / sorts are rare).

(cl-defun org-glance-table--config-file (graph)
  "Path of GRAPH's table-view config store (may not exist)."
  (f-join (org-glance-graph:store-path graph) "config" "table-views.eld"))

(cl-defun org-glance-table--config-all (graph)
  "Saved per-filter table configs: alist of (identity-string . config-plist)."
  (let ((path (org-glance-table--config-file graph)))
    (when (f-exists? path)
      (ignore-errors (car (read-from-string (f-read-text path 'utf-8)))))))

(cl-defun org-glance-table--config-get (graph spec)
  "Saved view-config plist for SPEC (`:columns' KEYS `:sort' SORT-KEYS), or nil."
  (alist-get (org-glance-filter:identity spec)
             (org-glance-table--config-all graph) nil nil #'equal))

(cl-defun org-glance-table--config-put (graph spec config)
  "Persist CONFIG (a plist) for SPEC, merged into GRAPH's config store."
  (let ((path (org-glance-table--config-file graph))
        (all (org-glance-table--config-all graph))
        (id (org-glance-filter:identity spec)))
    (setf (alist-get id all nil nil #'equal) config)
    (f-mkdir-full-path (f-dirname path))
    (f-write-text (prin1-to-string all) 'utf-8 path)))

(cl-defun org-glance-table--reorder-columns (columns order)
  "COLUMNS reordered so their `key's follow ORDER (a list of keys).
Columns whose key is absent from ORDER keep their relative position at the end,
so a schema change (a new column) degrades gracefully."
  (append
   (delq nil (mapcar (lambda (k)
                       (cl-find k columns :test #'equal
                                :key (lambda (c) (alist-get 'key c))))
                     order))
   (cl-remove-if (lambda (c) (member (alist-get 'key c) order)) columns)))

(defvar-local org-glance-table--config-snapshot nil
  "Last persisted view config for this buffer (the change-detection baseline).")

(cl-defun org-glance-table--current-config ()
  "This buffer's current view config: (:columns KEYS :sort SORT-KEYS)."
  (list :columns (mapcar (lambda (c) (alist-get 'key c))
                         (alist-get 'columns table-view--spec))
        :sort (copy-tree table-view--sort-keys)))

(cl-defun org-glance-table--persist-config ()
  "Buffer-local `post-command-hook': persist the view config iff it changed.
Cheap on the common path -- compares the (column-order, sort) tuple to the last
snapshot and writes only on an actual layout change.  (`org-glance-table--spec'
may be nil -- the \"all\" filter -- so guard only on being a registered view.)"
  (when org-glance-view--graph
    (let ((cur (org-glance-table--current-config)))
      (unless (equal cur org-glance-table--config-snapshot)
        (setq org-glance-table--config-snapshot cur)
        (with-demoted-errors "org-glance: table config save failed: %S"
          (org-glance-table--config-put org-glance-view--graph org-glance-table--spec cur))))))

;;; Browser

(cl-defun org-glance-table:visit (graph &optional filter)
  "Open GRAPH's table for FILTER, one buffer per filter description.
Honours the same filter language as the overview (see
`org-glance-filter:predicate')."
  (let* ((spec (org-glance-filter:normalize-spec filter))
         (saved (org-glance-table--config-get graph spec))   ; restored column order + sort
         ;; Resolve the active/done split ONCE -- the single configured tag's todo
         ;; cycle, else the global keywords -- and bind it while the `:done'
         ;; predicate AND the badge split are built, so the table agrees with the
         ;; overview (W2).  The predicate is captured in `fill-fn', so a reload
         ;; reuses this exact split with no further bookkeeping.
         (cycle (org-glance-tag-config:cycle-for-filter graph spec))
         (done-keywords (if cycle (org-glance-tag-config:done-keywords cycle)
                          (org-glance--done-keywords)))
         (org-done-keywords done-keywords)
         (keep? (org-glance-filter:predicate spec))
         (buffer-name (format "*org-glance-table: %s*" (org-glance-filter:describe spec)))
         (src (org-glance-graph:headline-meta-path graph))
         (fill-fn (lambda (buf)
                    (with-current-buffer buf
                      (table-view-set-rows buf (org-glance-table--rows graph keep?))
                      (setq org-glance-table--mtime (org-glance-table--mtime src)))))
         (handlers (list (cons "materialize" (lambda (id row) (org-glance-table--act-materialize graph id row)))
                         (cons "open"        (lambda (id row) (org-glance-table--act-open graph id row)))
                         (cons "extract"     (lambda (id row) (org-glance-table--act-extract graph id row)))
                         (cons "todo"        (lambda (id row) (org-glance-table--act-todo graph id row)))
                         (cons "refresh"     (lambda (_id _row) (org-glance-table--reload (current-buffer))))
                         (cons "overview"    (lambda (_id _row) (org-glance-overview:visit graph spec)))
                         (cons "capture"     (lambda (_id _row)
                                               (org-glance-capture (or (org-glance-filter:tags spec)
                                                                       (org-glance-capture:completing-read-tag))
                                                                   "")))))
         ;; Build the spec, restoring the saved column order (if any) before display.
         (tspec (let ((s (org-glance-table--spec graph spec)))
                  (when-let ((order (plist-get saved :columns)))
                    (setf (alist-get 'columns s)
                          (org-glance-table--reorder-columns (alist-get 'columns s) order)))
                  s))
         (buf (table-view-display buffer-name tspec handlers fill-fn)))
    (with-current-buffer buf
      (setq org-glance-table--spec spec
            ;; Match the corresponding overview (`org-glance-overview:visit'): run
            ;; directory-relative actions (dired, shell, relative links) from the
            ;; graph's ROOT, not wherever this non-file buffer happened to spawn.
            default-directory (file-name-as-directory (org-glance-graph:directory graph)))
      (org-glance-view:register graph
                                :stale-fn  (lambda () (org-glance-table--stale? graph))
                                :reload-fn (lambda () (org-glance-table--reload (current-buffer))))
      ;; Restore the saved sort (else the spec default seeded by display), apply it,
      ;; then persist any subsequent layout change (column move / sort) for this filter.
      (when-let ((sort (plist-get saved :sort)))
        (setq-local table-view--sort-keys sort))
      (org-glance-table--apply-sort)
      (setq org-glance-table--config-snapshot (org-glance-table--current-config))
      (add-hook 'post-command-hook #'org-glance-table--persist-config nil t))
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
like `org-glance-overview', but rendered as a flat table.  Sort with `^' (sorts
by the column at point; repeat toggles direction, `C-u ^' adds a tie-breaker);
act on the row at point with RET/m, o, e."
  (interactive (list (org-glance-table:completing-read-tag)))
  (cl-assert (org-glance-initialized?))
  (org-glance-table:visit org-glance-graph
                          (org-glance-filter:merge org-glance-filter-spec tag)))

(provide 'org-glance-table)
;;; org-glance-table.el ends here
