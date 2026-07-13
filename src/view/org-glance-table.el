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

(require 'org-glance-core)
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
FILTER titles the view and keys the per-tag custom-column schema; rows are
produced by the fill-fn.  Built-in columns: state (badge), title, tags,
scheduled, deadline, priority -- all sortable -- followed by any custom
property columns saved for its tags (see `org-glance-table--apply-schema').
Default sort is the state column ascending (active first)."
  `((title . ,(format "org-glance table: %s" (org-glance-filter:describe filter)))
    (columns . ,(org-glance-table--apply-schema
                 graph filter
                 `(((key . "state")    (header . "State")     (type . "badge") (sortable . t) (align . "left")
                    (badges . ,(org-glance-table--state-badges graph)))
                   ((key . "title")    (header . "Title")     (type . "text")  (sortable . t) (align . "left"))
                   ((key . "tags")     (header . "Tags")      (type . "text")  (sortable . t) (align . "left"))
                   ((key . "schedule") (header . "Scheduled") (type . "text")  (sortable . t) (align . "left"))
                   ((key . "deadline") (header . "Deadline")  (type . "text")  (sortable . t) (align . "left"))
                   ((key . "priority") (header . "Pri")       (type . "text")  (sortable . t) (align . "left")))))
    (actions . (((key . "RET") (command . "materialize") (label . "Materialize"))
                ((key . "o")   (command . "open")        (label . "Open link"))
                ((key . "e")     (command . "extract")   (label . "Extract"))
                ((key . "g")     (command . "refresh")   (label . "Refresh"))
                ((key . "T")     (command . "overview")  (label . "Overview"))
                ((key . "+")     (command . "capture")   (label . "Capture"))
                ((key . ":")     (command . "tag")       (label . "Tag"))
                ((key . "-")     (command . "delcolumn") (label . "Del col"))
                ((key . "C-c C-t") (command . "todo") (bulk . t) (label . "Todo"))))
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

(cl-defun org-glance-table--file-mtime (path)
  "Modification time of PATH, or nil when it does not exist."
  (and (f-exists? path)
       (file-attribute-modification-time (file-attributes path))))

(cl-defun org-glance-table--rows (graph keep?)
  "Rows for GRAPH's live headlines satisfying predicate KEEP?, in graph order."
  (cl-loop for meta in (org-glance-graph:headlines graph)
           when (funcall keep? meta)
           collect (org-glance-table--row meta)))

(cl-defun org-glance-table--apply-sort ()
  "Apply the sort in `table-view--sort-keys' to rows filled after display.
The keys are the spec's declared default (seeded by `table-view-display'), a
restored per-view config, or the sort the user left before a reload.  Uses
`table-view-apply-sort', the public entry point for `fill-fn' buffers whose
rows arrive after the spec (a no-op with no keys; leaves the filter intact)."
  (table-view-apply-sort))

(cl-defun org-glance-table--restore-point (id line)
  "Return point to the row with ID; if that row is gone, to screen LINE.
The row may have left the current filter (e.g. now DONE under an active filter);
fall back to the same screen line, org-agenda-style, instead of the buffer top."
  (unless (and id (table-view--goto-id id))
    (goto-char (point-min))
    (forward-line (1- line))))

(cl-defun org-glance-table--reload (buffer)
  "Re-fill BUFFER from the live graph, re-apply its sort, and keep point in place.
Used by `g' (refresh) and the lazy display-boundary check.  `table-view-refresh'
re-runs the fill-fn (rows back to load order, `--sort-keys' kept) and
`--apply-sort' restores the ordering; capture the row under point up front and
return to it afterwards, since the intermediate render + sort restore point by
LINE, which drifts to another row once the sort reorders them."
  (when-let ((buf (get-buffer buffer)))
    (with-current-buffer buf
      (let ((id (get-text-property (point) 'table-view-id))
            (line (line-number-at-pos)))
        (table-view-refresh buf)
        (org-glance-table--apply-sort)
        (org-glance-view:mark-fresh)
        (org-glance-table--restore-point id line)))))

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
             (time-less-p org-glance-table--mtime (org-glance-table--file-mtime src))))))

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
         (org-glance-table--restore-point id line)
         (message "State: %s" (if (s-present? state) state "(none)")))))))

(cl-defun org-glance-table--todo-candidates (graph filter)
  "Candidate target states for a bulk change under FILTER.
FILTER's single todo cycle (active + done keywords) when it has one, else the
states currently in use across GRAPH -- so the prompt always offers something."
  (let ((cycle (org-glance-tag-config:cycle-for-filter graph filter)))
    (delete-dups
     (append (when cycle (remove "|" (split-string cycle)))
             (org-glance-graph:states graph)))))

(cl-defun org-glance-table--act-todo-bulk (graph rows)
  "Set the marked ROWS to one chosen TODO state, prompted once (org-agenda `B t').
Delegates to `org-glance-material:set-todo-bulk': materialize + set + sync each
row (timestamps, no note), then reload the table and clear the marks.
Bound to `C-c C-t' with rows marked; with none, a bare `C-c C-t' stays the
single-row `org-glance-table--act-todo' (cycle + note)."
  (let ((ids (delq nil (mapcar (lambda (r) (alist-get 'id r)) rows))))
    (when ids
      (let ((at-id (get-text-property (point) 'table-view-id))   ; row under point now
            (line (line-number-at-pos))                          ; fallback anchor
            (state (completing-read
                    (format "Set %d headline(s) to state: " (length ids))
                    (org-glance-table--todo-candidates graph org-glance-table--spec)
                    nil t)))
        (unless (string-empty-p state)
          (org-glance-material:set-todo-bulk
           graph ids state
           (lambda (changed skipped)
             (org-glance-table--reload (current-buffer))
             (table-view-unmark-all)
             (org-glance-table--restore-point at-id line)
             (message "Set %d headline(s) to %s%s"
                      (length changed) state
                      (if skipped (format " (%d skipped)" (length skipped)) "")))))))))

(cl-defun org-glance-table--metadata (graph id)
  "Return live headline metadata for ID in GRAPH, or a `user-error' when gone."
  (let ((meta (org-glance-graph:get-headline graph id)))
    (if (org-glance-headline-metadata? meta)
        meta
      (user-error "Headline no longer in graph (table is stale; press `g' to refresh)"))))

(cl-defun org-glance-table--act-tag (graph id)
  "Add a tag to headline ID at point, or remove one of its tags with a prefix.
Bare `:' completing-reads a tag from GRAPH's tag universe and accepts a new tag
\(no match required); `C-u :' completing-reads one of the headline's own tags
\(match required) and removes it.  On a change, reload the table and keep point
on the row."
  (unless id (user-error "Point is not on a row"))
  (let* ((line (line-number-at-pos))
         (remove current-prefix-arg)
         (tag (if remove
                  (let ((tags (mapcar (lambda (x) (format "%s" x))
                                      (org-glance-headline-metadata:tags
                                       (org-glance-table--metadata graph id)))))
                    (if tags
                        (completing-read "Remove tag: " tags nil t)
                      (user-error "Headline has no tags to remove")))
                (s-trim (completing-read "Add tag: " (org-glance-graph:tags graph))))))
    (when (and tag (not (string-empty-p tag))
               (org-glance-material:retag graph id tag :remove remove))
      (org-glance-table--reload (current-buffer))
      (org-glance-table--restore-point id line)
      (message "%s tag `%s'" (if remove "Removed" "Added") tag))))

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

(cl-defun org-glance-table--read-eld (path)
  "Read the single form in the .eld PATH, or nil when absent/unreadable."
  (when (f-exists? path)
    (ignore-errors (car (read-from-string (f-read-text path 'utf-8))))))

(cl-defun org-glance-table--write-eld (path form)
  "Serialize FORM to the .eld PATH, creating parent dirs."
  (f-mkdir-full-path (f-dirname path))
  (f-write-text (prin1-to-string form) 'utf-8 path))

(cl-defun org-glance-table--config-all (graph)
  "Saved per-filter table configs: alist of (identity-string . config-plist)."
  (org-glance-table--read-eld (org-glance-table--config-file graph)))

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
    (org-glance-table--write-eld path all)))

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

;;; Custom property columns (`C-u +' adds, `-' removes, persisted per tag)
;;
;; `C-u +' prompts for an org drawer property (e.g. AUTHOR) and appends a column
;; showing that property for each row.  The value is read lazily via a
;; `table-view' `value-fn'; because the metadata projection the table renders from
;; does not carry arbitrary drawer properties, the cell is pulled from the full
;; headline -- cached per id and keyed by the content hash, so a reload only
;; re-parses the rows that actually changed.  The set of custom columns (a schema)
;; is persisted PER TAG (the filter's tags) at `<store>/config/table-columns.eld',
;; so every view of a tag inherits its columns.  Only these added columns are
;; removable (`-' on one); the built-in columns are fixed.

(defvar-local org-glance-table--prop-cache nil
  "Per-buffer cache for custom-column values: id -> (HASH . NODE-PROPERTIES).
The cell for a property column is the headline's drawer property, which lives in
the content blob, not the cheap metadata.  Caching the parsed drawer per id and
invalidating on the content HASH keeps a reload from re-parsing unchanged rows.")

(cl-defun org-glance-table--headline-property (graph id property)
  "Value of ID's headline drawer PROPERTY in GRAPH, via the per-buffer cache.
Reuses the cached parse while the headline's content hash is unchanged; only a
new or modified row pays the blob read + parse.  Returns nil when ID is gone."
  (let* ((cache (or org-glance-table--prop-cache
                    (setq org-glance-table--prop-cache (make-hash-table :test #'equal))))
         (meta (org-glance-graph:get-headline graph id))
         (hash (and (org-glance-headline-metadata? meta)
                    (org-glance-headline-metadata:hash meta)))
         (entry (gethash id cache)))
    (unless (and entry (equal (car entry) hash))
      (setq entry (cons hash (ignore-errors
                               (org-glance-headline:node-properties
                                (org-glance-graph:headline graph id)))))
      (puthash id entry cache))
    (alist-get (upcase property) (cdr entry) nil nil #'string=)))

(cl-defun org-glance-table--property-column (graph property &optional header)
  "A `table-view' column displaying drawer PROPERTY for each row's headline.
PROPERTY is upcased for the drawer lookup; HEADER defaults to its capitalised
form.  Carries a `prop' marker so the per-tag schema can round-trip the column
without persisting its (unreadable) `value-fn' closure."
  (let ((prop (upcase (string-trim property))))
    `((key . ,prop)
      (header . ,(or header (capitalize prop)))
      (type . "text")
      (sortable . t)
      (align . "left")
      (prop . ,prop)
      (value-fn . ,(lambda (id _row)
                     (or (org-glance-table--headline-property graph id prop) ""))))))

(cl-defun org-glance-table--add-column-prompt ()
  "Prompt for a drawer property and return a `table-view' column showing it.
Bound buffer-locally as `table-view-add-column-function' so `C-u +' adds a
column for that property; empty input cancels the add."
  (let ((property (string-trim (read-string "Property column (drawer key): "))))
    (unless (string-empty-p property)
      (org-glance-table--property-column org-glance-view--graph property))))

;;; Per-tag column schema store (`<store>/config/table-columns.eld')

(cl-defun org-glance-table--schema-file (graph)
  "Path of GRAPH's per-tag custom-column schema store (may not exist)."
  (f-join (org-glance-graph:store-path graph) "config" "table-columns.eld"))

(cl-defun org-glance-table--schema-all (graph)
  "Saved per-tag schemas: alist of (tag-key . (:columns ((PROP . HEADER) ...)))."
  (org-glance-table--read-eld (org-glance-table--schema-file graph)))

(cl-defun org-glance-table--schema-key (filter)
  "Canonical per-tag key for FILTER: its tags sorted and `+'-joined, or
\":none:\" when the filter carries no tag constraint.  Keying on the tags (not
the full filter identity) is what shares a tag's columns across its views."
  (let ((tags (sort (mapcar #'symbol-name (org-glance-filter:tags filter)) #'string<)))
    (if tags (s-join "+" tags) ":none:")))

(cl-defun org-glance-table--schema-get (graph filter)
  "Ordered custom columns saved for FILTER's tags.
A list of (PROP . HEADER), or nil."
  (plist-get (alist-get (org-glance-table--schema-key filter)
                        (org-glance-table--schema-all graph) nil nil #'equal)
             :columns))

(cl-defun org-glance-table--schema-put (graph filter columns)
  "Persist COLUMNS (a list of (PROP . HEADER)) for FILTER's tags in GRAPH's store.
An empty COLUMNS drops the entry so the store does not accrete empties."
  (let* ((path (org-glance-table--schema-file graph))
         (key (org-glance-table--schema-key filter))
         (all (cl-remove key (org-glance-table--schema-all graph)
                         :key #'car :test #'equal)))
    (when columns
      (setq all (cons (cons key (list :columns columns)) all)))
    (org-glance-table--write-eld path all)))

(cl-defun org-glance-table--apply-schema (graph filter columns)
  "COLUMNS with GRAPH's saved custom property columns for FILTER appended.
Each saved (PROP . HEADER) becomes a `value-fn' column reading that drawer
property.  Absent a saved schema, COLUMNS is returned unchanged."
  (append columns
          (mapcar (lambda (pair)
                    (org-glance-table--property-column graph (car pair) (cdr pair)))
                  (org-glance-table--schema-get graph filter))))

(cl-defun org-glance-table--persist-schema ()
  "Buffer-local `table-view-schema-changed-hook': save this filter's custom
columns (the `prop'-marked columns of the live spec, in display order) per tag."
  (when org-glance-view--graph
    (let ((columns (cl-loop for c in (table-view--columns table-view--spec)
                            when (alist-get 'prop c)
                            collect (cons (alist-get 'prop c) (alist-get 'header c)))))
      (with-demoted-errors "org-glance: table schema save failed: %S"
        (org-glance-table--schema-put org-glance-view--graph
                                      org-glance-table--spec columns)))))

(cl-defun org-glance-table--act-delcolumn ()
  "`-' handler: remove the custom property column at point (built-ins are fixed)."
  (let* ((key (get-text-property (point) 'table-view-col))
         (col (and key (table-view--column table-view--spec key))))
    (cond
     ((null col) (message "Point is not on a column"))
     ((not (alist-get 'prop col))
      (message "Only added property columns are removable (%s is built-in)" key))
     (t (table-view-remove-column key)))))

;;; Browser

(defun org-glance-table:filter-or-reset ()
  "Filter or narrow the table; with a prefix arg, clear the active filter.
With no prefix, defer to `table-view-filter-or-narrow' -- narrow to the marked
rows, or prompt for a substring filter when none are marked.  With a prefix arg
\(`C-u /'), clear the current substring filter without prompting."
  (interactive)
  (if current-prefix-arg
      (table-view-filter "")
    (call-interactively #'table-view-filter-or-narrow)))

(cl-defun org-glance-table:visit (graph &optional filter)
  "Open GRAPH's table for FILTER, one buffer per filter description.
Honours the same filter language as the overview (see
`org-glance-filter:predicate')."
  (let* ((from-view (and org-glance-view--graph t))   ; re-navigation from within a view?
         (spec (org-glance-filter:normalize-spec filter))
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
                      (setq org-glance-table--mtime (org-glance-table--file-mtime src)))))
         (handlers (list (cons "materialize" (lambda (id row) (org-glance-table--act-materialize graph id row)))
                         (cons "open"        (lambda (id row) (org-glance-table--act-open graph id row)))
                         (cons "extract"     (lambda (id row) (org-glance-table--act-extract graph id row)))
                         (cons "todo"        (lambda (rows)
                                               ;; `bulk' handler: a row LIST.  With marks -> bulk
                                               ;; (one prompt, all marked); else the row at point
                                               ;; gets the full single-row `C-c C-t' (cycle + note).
                                               (if (table-view-marked-rows)
                                                   (org-glance-table--act-todo-bulk graph rows)
                                                 (let ((row (car rows)))
                                                   (org-glance-table--act-todo graph (alist-get 'id row) row)))))
                         (cons "refresh"     (lambda (_id _row) (org-glance-table--reload (current-buffer))))
                         (cons "overview"    (lambda (_id _row) (org-glance-overview:visit graph spec)))
                         (cons "delcolumn"   (lambda (_id _row) (org-glance-table--act-delcolumn)))
                         (cons "capture"     (lambda (_id _row)
                                               ;; `C-u +' adds a custom property column; a bare `+' captures.
                                               (if current-prefix-arg
                                                   (call-interactively #'table-view-add-column)
                                                 (org-glance-capture (or (org-glance-filter:tags spec)
                                                                         (org-glance-capture:completing-read-tag))
                                                                     ""))))
                         (cons "tag"      (lambda (id _row) (org-glance-table--act-tag graph id)))))
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
      ;; Custom property columns: `C-u +' builds one via the prompt below, and any
      ;; add/remove is persisted per tag through the schema-changed hook.
      (setq-local table-view-add-column-function #'org-glance-table--add-column-prompt)
      ;; `C-u /' clears the active filter; a bare `/' stays filter-or-narrow.
      (local-set-key "/" #'org-glance-table:filter-or-reset)
      (add-hook 'table-view-schema-changed-hook #'org-glance-table--persist-schema nil t)
      ;; Restore the saved sort (else the spec default seeded by display), apply it,
      ;; then persist any subsequent layout change (column move / sort) for this filter.
      (when-let ((sort (plist-get saved :sort)))
        (setq-local table-view--sort-keys sort))
      (org-glance-table--apply-sort)
      (setq org-glance-table--config-snapshot (org-glance-table--current-config))
      (add-hook 'post-command-hook #'org-glance-table--persist-config nil t)
      (org-glance-view:fill-frame from-view))
    buf))

(cl-defun org-glance-table:completing-read-tag ()
  "Prompt for a tag from the graph's headlines; empty input means \"all\"."
  (org-glance-ensure-init)
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
  (org-glance-ensure-init)
  (org-glance-table:visit org-glance-graph
                          (org-glance-filter:merge org-glance-filter-spec tag)))

(provide 'org-glance-table)
;;; org-glance-table.el ends here
