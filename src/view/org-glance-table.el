;; -*- lexical-binding: t -*-

;;; org-glance-table.el --- table-view-backed headline dashboard

;;; Commentary:
;; A sortable, badge-coloured TABLE view over the graph, sister to the org-text
;; `org-glance-overview'.  Where the overview is a real org file (folding,
;; planning, agenda, an on-disk cache + coherence machinery), this is a flat,
;; in-memory `table-view' buffer: one row per matching headline, columns for
;; state / title / tags / scheduled / deadline / priority, client-side sort, and
;; the same id-keyed actions (materialize / open / extract / tag / encrypt).
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
(require 'org-glance-utils)
(require 'org-glance-graph)
(require 'org-glance-property-index)
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

(cl-defun org-glance-table--face-color (face)
  "Foreground colour of FACE -- a face symbol, colour string, or attribute
plist (the three `org-todo-keyword-faces' value forms) -- or nil."
  (cond ((stringp face) face)
        ((facep face) (face-foreground face nil t))
        ((listp face) (plist-get face :foreground))))

(cl-defun org-glance-table--state-color (state)
  "Badge colour for todo STATE.
`org-glance-table-state-colors' first, else the foreground of the user's
`org-todo-keyword-faces' entry (so a state coloured in org buffers keeps
its colour here), else `org-glance-table-default-state-color'."
  (or (cdr (assoc state org-glance-table-state-colors))
      (org-glance-table--face-color (cdr (assoc state org-todo-keyword-faces)))
      org-glance-table-default-state-color))

(cl-defun org-glance-table--split-states (graph)
  "GRAPH's states split as (ACTIVE . DONE), each in `org-glance-graph:states'
sorted order.  The split reads the ambient `org-done-keywords' (bound by
`org-glance-table:visit' to the tag's cycle, else the global done set) through
`org-glance--done-keywords'."
  (let ((states (org-glance-graph:states graph))
        (done-kw (org-glance--done-keywords)))
    (cons (cl-remove-if     (lambda (s) (member s done-kw)) states)
          (cl-remove-if-not (lambda (s) (member s done-kw)) states))))

(cl-defun org-glance-table--state-badges (graph)
  "Badge palette (a list of `((value . S) (color . C))') for GRAPH's states.
Active states first then done, each group in `org-glance-graph:states' sorted
order, so the palette doubles as an active-first sort priority."
  (pcase-let ((`(,active . ,done) (org-glance-table--split-states graph)))
    (cl-loop for state in (append active done)
             collect `((value . ,state) (color . ,(org-glance-table--state-color state))))))

(cl-defun org-glance-table--colorize-state (state)
  "STATE (a string) propertized with its todo-state colour and bold weight."
  (propertize state 'face
              (list :foreground (org-glance-table--state-color state) :weight 'bold)))

(cl-defun org-glance-table--todo-line (graph)
  "A `#+TODO:'-style line of GRAPH's states -- active, then `|', then done --
each coloured by the state palette; nil when the graph has no states.  Shown
always in the table header via the `table-view' `subtitle' (never hidden by the
`?' action-legend toggle)."
  (pcase-let ((`(,active . ,done) (org-glance-table--split-states graph)))
    (when (or active done)
      (concat "#+TODO: "
              (mapconcat #'org-glance-table--colorize-state active " ")
              (when done
                (concat (if active " " "") "| "
                        (mapconcat #'org-glance-table--colorize-state done " ")))))))

;;; Spec + rows (built in pure elisp -- no backend process)

(cl-defun org-glance-table--base-columns (graph)
  "The fixed built-in table columns for GRAPH, in default order.
Title is mandatory (`C-u -' refuses it); the rest are removable, and which are
hidden persists per tag (see `org-glance-table--apply-schema').  The single
source of the built-in key set -- `org-glance-table--persist-schema' diffs the
live spec against these keys to record the hidden ones."
  `(((key . "state")    (header . "State")     (type . "badge") (sortable . t) (align . "left")
     (badges . ,(org-glance-table--state-badges graph)))
    ((key . "title")    (header . "Title")     (type . "text")  (sortable . t) (align . "left"))
    ((key . "schedule") (header . "Scheduled") (type . "text")  (sortable . t) (align . "left"))
    ((key . "deadline") (header . "Deadline")  (type . "text")  (sortable . t) (align . "left"))
    ((key . "interval") (header . "Interval")  (type . "text")  (sortable . t) (align . "left"))
    ((key . "priority") (header . "Pri")       (type . "text")  (sortable . t) (align . "left"))
    ((key . "encrypted") (header . "Enc")      (type . "text")  (sortable . t) (align . "center"))
    ((key . "repeated") (header . "Rep")       (type . "text")  (sortable . t) (align . "center"))
    ((key . "tags")     (header . "Tags")      (type . "text")  (sortable . t) (align . "left"))))

(cl-defun org-glance-table--spec (graph filter)
  "Build the `table-view' spec (a plain alist) for GRAPH under FILTER.
FILTER titles the view and keys the per-tag column schema; rows are produced by
the fill-fn.  Columns are the built-ins (`org-glance-table--base-columns') minus
the ones hidden for its tags, followed by any custom property columns saved for
them (see `org-glance-table--apply-schema').  Default sort is the state column
ascending (active first)."
  `((title . ,(format "org-glance table: %s" (org-glance-filter:describe filter)))
    (subtitle . ,(org-glance-table--todo-line graph))
    (columns . ,(org-glance-table--apply-schema
                 graph filter (org-glance-table--base-columns graph)))
    (actions . (((key . "RET") (command . "materialize") (label . "Materialize"))
                ((key . "j")   (command . "open")        (label . "Open link"))
                ((key . "e")     (command . "extract")   (label . "Extract"))
                ((key . "g")     (command . "refresh")   (label . "Refresh"))
                ((key . "O")     (command . "overview")  (label . "Overview"))
                ((key . "+")     (command . "capture")   (label . "Capture"))
                ((key . ":")     (command . "tag")       (label . "Tag"))
                ((key . "#")     (command . "crypt")     (label . "Crypt"))
                ((key . "l")     (command . "history")   (label . "Log"))
                ((key . "i")     (command . "edit")      (label . "Edit cell"))
                ((key . "C-c p") (command . "duplicate") (label . "Copy"))
                ((key . "-")     (command . "remove")    (label . "Untag"))
                ((key . "C-c C-t") (command . "todo") (bulk . t) (label . "Todo"))
                ((key . "D")     (command . "delete")    (label . "Delete"))
                ((key . "C-c C-s") (command . "schedule") (label . "Schedule"))
                ((key . "C-c C-d") (command . "deadline") (label . "Deadline"))))
    (sort . ((column . "state") (ascending . t)))))

(cl-defun org-glance-table--interval-cell (range)
  "RANGE (FROM TO) as the compact sortable cell `2021-12-18..2021-12-19'.
Date parts only -- ISO, so the string sort orders by start; \"\" when nil."
  (pcase range
    (`(,from ,to)
     (let ((day (lambda (ts) (if (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" ts)
                                 (match-string 0 ts) ts))))
       (concat (funcall day from) ".." (funcall day to))))
    (_ "")))

(cl-defun org-glance-table--row (metadata)
  "Build a `table-view' row alist for headline METADATA.
The id is the ORG_GLANCE_ID (passed to the action handlers); cells are display
strings: tags are joined with `:' (they are interned symbols, never a raw list),
priority is its letter, absent values are the empty string."
  (cl-check-type metadata org-glance-headline-metadata)
  (let ((tags (org-glance-headline-metadata:tag-strings metadata))
        (priority (org-glance-headline-metadata:priority metadata)))
    `((id . ,(org-glance-headline-metadata:id metadata))
      (cells . ((state    . ,(or (org-glance-headline-metadata:state metadata) ""))
                (title    . ,(or (org-glance-headline-metadata:title metadata) ""))
                (tags     . ,(if tags (s-join ":" tags) ""))
                (schedule . ,(or (org-glance-headline-metadata:schedule metadata) ""))
                (deadline . ,(or (org-glance-headline-metadata:deadline metadata) ""))
                (interval . ,(org-glance-table--interval-cell
                              (org-glance-headline-metadata:range metadata)))
                (priority . ,(if (integerp priority) (char-to-string priority) ""))
                (encrypted . ,(if (org-glance-headline-metadata:encrypted? metadata) "🔒" ""))
                (repeated . ,(if (org-glance-headline-metadata:repeated? metadata) "↻" "")))))))

;;; Per-buffer state

(defvar-local org-glance-table--spec nil
  "Normalised filter spec the current table buffer was generated with.")
(cl-defun org-glance-table--rows (graph keep?)
  "Rows for GRAPH's live headlines satisfying predicate KEEP?, in graph order."
  (cl-loop for meta in (org-glance-graph:headlines graph)
           when (funcall keep? meta)
           collect (org-glance-table--row meta)))

(cl-defun org-glance-table--restore-point (id line)
  "Return point to the row with ID; if that row is gone, to screen LINE.
The row may have left the current filter (e.g. now DONE under an active filter);
fall back to the same screen line, org-agenda-style, instead of the buffer top."
  (unless (and id (table-view--goto-id id))
    (goto-char (point-min))
    (forward-line (1- line))))

(cl-defun org-glance-table--finish (id line fmt &rest args)
  "Reload the table, return point to row ID (else screen LINE), message FMT ARGS."
  (org-glance-table--reload (current-buffer))
  (org-glance-table--restore-point id line)
  (message "%s" (apply #'format fmt args)))

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
        (table-view-apply-sort)
        (org-glance-view:mark-fresh)
        (org-glance-table--restore-point id line)))))

;;; Live coherence (pull at the display boundary, driven by `org-glance-view')
;;
;; This file supplies only the two view-specific pieces `org-glance-view:register'
;; takes -- the STALE-FN (`--stale?') and the RELOAD-FN (`--reload') -- in
;; `org-glance-table:visit'.  A table is a NON-FILE projection rebuilt from the
;; graph, so the shared driver always reloads it at the display boundary (it
;; discards no user data, unlike the overview's file-backed buffer).

;;; Actions (id-keyed; the table-view core hands each handler the row's id)

(cl-defun org-glance-table--act-materialize (graph id)
  (when id (switch-to-buffer (org-glance-material:open graph id))))

(cl-defun org-glance-table--headline (graph id)
  "The live headline for ID, or a `user-error' (the table can outlive the graph)."
  (or (org-glance-graph:headline graph id)
      (user-error "Headline no longer in graph (table is stale; press `g' to refresh)")))

(cl-defun org-glance-table--act-open (graph id)
  (when id (org-glance-material:open-link (org-glance-table--headline graph id))))

(cl-defun org-glance-table--act-extract (graph id)
  (when id (org-glance-material:extract-pairs (org-glance-property-index:body graph id))))

(cl-defun org-glance-table--act-todo (graph id)
  "Advance ID's TODO state exactly like `C-c C-t' (via
`org-glance-material:change-todo-live'), then reload the table and return to the
row once the change (and any note) is committed."
  (when id
    (let ((arg current-prefix-arg)          ; the dispatch lambda is a bare `interactive'
          (line (line-number-at-pos)))       ; the reload re-renders from the top
      (org-glance-material:change-todo-live
       graph id arg
       (lambda (state)
         (org-glance-table--finish id line "State: %s"
                                   (if (s-present? state) state "(none)")))))))

(cl-defun org-glance-table--read-state-native (graph filter)
  "Org's own fast TODO selection (the `C-c C-t' buffer) for FILTER's cycle.
Runs `org-fast-todo-selection' in a temp org buffer initialized with the
tag's `#+TODO:' cycle (else the global keywords), so keys, faces and the
active/done split match the material buffer exactly.  Returns a keyword
string, or the symbol `none' when the user picks \"clear\"."
  (let ((cycle (org-glance-tag-config:cycle-for-filter graph filter)))
    (with-temp-buffer
      (let ((org-todo-keywords
             (org-glance-tag-config:cycle->keywords-or cycle org-todo-keywords)))
        (org-glance--org-mode))
      (org-fast-todo-selection))))

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
            (state (org-glance-table--read-state-native graph org-glance-table--spec)))
        (when state                       ; `none' clears; C-g aborts before here
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
Bare `:' completing-reads a tag the headline does NOT already carry (GRAPH's tag
universe minus its own) and accepts a new tag (no match required); `C-u :'
completing-reads one of the headline's own tags (match required) and removes it.
On a change, reload the table and keep point on the row."
  (unless id (user-error "Point is not on a row"))
  (let* ((line (line-number-at-pos))
         (remove current-prefix-arg)
         (own (org-glance-headline-metadata:tag-strings
               (org-glance-table--metadata graph id)))
         (tag (if remove
                  (if own
                      (completing-read "Remove tag: " own nil t)
                    (user-error "Headline has no tags to remove"))
                (s-trim (completing-read
                         "Add tag: "
                         (cl-remove-if (lambda (x) (member x own))
                                       (org-glance-graph:tags graph)))))))
    (when (and tag (not (string-empty-p tag))
               (org-glance-material:retag graph id tag :remove remove))
      (org-glance-table--finish id line "%s tag `%s'"
                                (if remove "Removed" "Added") tag))))

(cl-defun org-glance-table--act-crypt (graph id)
  "Toggle encryption of headline ID at point in GRAPH; `C-u' changes the password.
Bare: encrypt a plaintext headline, or decrypt an encrypted one.  With a prefix
arg on an encrypted headline: re-key it (old password, then new).  Prompts for
passwords (confirmed when setting a new one) and reloads the row."
  (unless id (user-error "Point is not on a row"))
  (let* ((line (line-number-at-pos))
         (encrypted (org-glance-headline-metadata:encrypted?
                     (org-glance-table--metadata graph id)))
         (done (cond
                (current-prefix-arg
                 (unless encrypted
                   (user-error "Headline is not encrypted -- nothing to re-key"))
                 (and (org-glance-material:crypt-rekey
                       graph id (read-passwd "Old password: ")
                       (read-passwd "New password (confirm): " t))
                      "Password changed"))
                (t
                 (and (org-glance-material:crypt-set
                       graph id (not encrypted)
                       (if encrypted (read-passwd "Password to decrypt: ")
                         (read-passwd "Password to encrypt (confirm): " t)))
                      (if encrypted "Headline decrypted" "Headline encrypted"))))))
    (when done
      (org-glance-table--finish id line "%s" done))))

;;; Per-view persistence: column order + sort, keyed by filter identity
;;
;; The user's column reordering (`table-view' column-move) and sort choice live in
;; the table buffer (read via `table-view-layout').  Persist
;; them PER FILTER so reopening the same view restores the layout across sessions.
;; Stored as one `read'-able alist at `<store>/config/table-views.eld', keyed by the
;; canonical filter identity (the same key basis the overview cache uses).  A change
;; is saved from a buffer-local `post-command-hook' that only writes when the layout
;; actually differs from the last snapshot (column moves / sorts are rare).

(cl-defun org-glance-table--config-file (graph)
  "Path of GRAPH's table-view config store (may not exist)."
  (org-glance-graph:config-file graph "table-views.eld"))

(cl-defun org-glance-table--config-get (graph spec)
  "Saved view-config plist for SPEC (`:columns' KEYS `:sort' SORT-KEYS), or nil."
  (org-glance--eld-alist-ref (org-glance-table--config-file graph)
                             (org-glance-filter:identity spec)))

(cl-defun org-glance-table--config-put (graph spec config)
  "Persist CONFIG (a plist) for SPEC in GRAPH's config store."
  (org-glance--eld-alist-set (org-glance-table--config-file graph)
                             (org-glance-filter:identity spec) config))

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
  "Last persisted view config for this buffer (the change-detection baseline).
In a reference view nothing auto-persists; there it is the last layout the
modified-nudge reported (see `org-glance-table--persist-config').")

(defvar-local org-glance-table--context nil
  "Reference-view context plist (`:anchor' ID `:dir' `refs'|`backlinks'), or nil.")

(cl-defun org-glance-table--current-config ()
  "This buffer's current view config: (:columns KEYS :sort SORT-KEYS).
The cheap change-detection projection of `org-glance-table--layout-snapshot'
\(no hidden-column diff), safe on the post-command hot path."
  (let ((layout (table-view-layout)))
    (list :columns (mapcar (lambda (c) (alist-get 'key c))
                           (plist-get layout :columns))
          :sort (plist-get layout :sort))))

(cl-defun org-glance-table--persist-config ()
  "Buffer-local `post-command-hook': react to a layout change.
Cheap on the common path -- one (column-order, sort) tuple comparison against
the last snapshot.  (`org-glance-table--spec' may be nil -- the \"all\"
filter -- so guard only on being a registered view.)  Persistent views save
the changed tuple on the spot.  Transient filters (relation views, `:where')
persist nothing automatically -- their identity embeds another headline's
id/link set and would accrete one entry per visit; a reference view instead
nudges, once per change, that `C-c C-c' applies the layout to a scope."
  (when org-glance-view--graph
    (let ((cur (org-glance-table--current-config)))
      (unless (equal cur org-glance-table--config-snapshot)
        (cond
         ((not (org-glance-filter:transient? org-glance-table--spec))
          (setq org-glance-table--config-snapshot cur)
          (with-demoted-errors "org-glance: table config save failed: %S"
            (org-glance-table--config-put org-glance-view--graph org-glance-table--spec cur)))
         (org-glance-table--context
          (setq org-glance-table--config-snapshot cur)
          (message "Layout modified — C-c C-c to apply it to a scope")))))))

;;; Column schema (`C-u +' adds, `C-u -' removes, persisted per tag)
;;
;; `C-u +' completing-reads an org drawer property the visible headlines carry
;; (`org-glance-property-index:keys') and appends a column showing it per row.
;; The metadata projection the table renders from does not carry arbitrary drawer
;; properties, so the cell is pulled from `org-glance-property-index' -- a derived,
;; hash-invalidated, persisted cache of each headline's properties (fallback: an
;; O(N) blob parse).  `C-u -' removes the column at point -- any column except the
;; mandatory Title.
;;
;; Both persist PER TAG (the filter's tags) at `<store>/config/table-columns.eld':
;; `:columns' the added custom columns, `:hidden' the removed built-in keys.  So
;; every view of a tag inherits its columns; a hidden built-in stays hidden and a
;; new built-in shipped later still appears (hidden is a denylist, not an
;; allowlist).  Column ORDER + sort persist separately, per filter (see above).

(cl-defun org-glance-table--property-column (graph property &optional header)
  "A `table-view' column displaying drawer PROPERTY for each row's headline.
PROPERTY is upcased for the drawer lookup; HEADER defaults to its capitalised
form.  Carries a `prop' marker so the per-tag schema can round-trip the column
without persisting its (unreadable) `value-fn' closure."
  (let ((prop (org-glance--property-key property)))
    `((key . ,prop)
      (header . ,(or header (capitalize prop)))
      (type . "text")
      (sortable . t)
      (align . "left")
      (prop . ,prop)
      (value-fn . ,(lambda (id _row)
                     (or (org-glance-property-index:property graph id prop) ""))))))

(cl-defun org-glance-table--edge-column (graph kind &optional header)
  "A `table-view' column showing the TITLES of KIND-edge targets per row.
Many-to-many joins with \", \"; a gone target falls back to its id.  Pure
metadata reads -- no blob parses.  The `prop' marker round-trips it through
the per-tag schema (see `org-glance-table--custom-column')."
  `((key . ,(concat "kind:" kind))       ; own namespace: never collides with built-ins
    (header . ,(or header (s-capitalize (org-glance--kind-pretty kind))))
    (type . "text")
    (sortable . t)
    (align . "left")
    (prop . ,kind)
    (value-fn . ,(lambda (id _row)
                   (let ((meta (org-glance-graph:get-headline graph id)))
                     (if (org-glance-headline-metadata? meta)
                         (s-join ", "
                                 (cl-loop for (target . k) in (org-glance-headline-metadata:relations meta)
                                          when (equal k kind)
                                          collect (org-glance-graph:title-or-id graph target)))
                       ""))))))

(cl-defun org-glance-table--property-key? (key)
  "Non-nil when KEY names a drawer-property column: the all-UPCASE tag.
Edge columns carry a `kind:' prefix, built-ins are lowercase."
  (string= key (upcase key)))

(cl-defun org-glance-table--custom-column (graph name &optional header)
  "Build the custom column NAME; its CASE is the persisted type tag.
Drawer columns persist UPCASE keys, relation kinds pure-downcase slugs -- so
an all-upcase NAME is a property column, anything else an edge column.
Deterministic (no live-graph membership scan, whose answer would flip when a
kind's last edge disappears), and \"AUTHOR\" the property coexists with
\"author\" the kind."
  (if (org-glance-table--property-key? name)
      (org-glance-table--property-column graph name header)
    (org-glance-table--edge-column graph name header)))

(cl-defun org-glance-table--add-column-prompt ()
  "Return a `table-view' column chosen by completing-read: a drawer property
or a relation kind the filtered headlines actually carry.  Required match,
empty input cancels.  Bound buffer-locally as `table-view-add-column-function'
so `C-u +' uses it."
  (let* ((graph org-glance-view--graph)
         (ids (delq nil (mapcar (lambda (r) (alist-get 'id r)) table-view--rows)))
         ;; kinds display PRETTY ("roasted by") but canonicalize to their slug
         (candidates (append (mapcar (lambda (k) (cons (org-glance--kind-pretty k) k))
                                     (org-glance-graph:edge-kinds graph ids))
                             (mapcar (lambda (k) (cons k k))
                                     (org-glance-property-index:keys graph ids)))))
    (if (null candidates)
        (user-error "No drawer properties or relation kinds on the headlines in this view")
      (let ((choice (completing-read "Column (property or relation kind): "
                                     candidates nil t)))
        (unless (string-empty-p choice)
          (org-glance-table--custom-column
           graph (cdr (assoc choice candidates))))))))

;;; Per-tag column schema store (`<store>/config/table-columns.eld')

(cl-defun org-glance-table--schema-file (graph)
  "Path of GRAPH's per-tag custom-column schema store (may not exist)."
  (org-glance-graph:config-file graph "table-columns.eld"))

(cl-defun org-glance-table--schema-key (filter)
  "Canonical per-tag key for FILTER: its tags sorted and `+'-joined, or
\":none:\" when the filter carries no tag constraint.  Keying on the tags (not
the full filter identity) is what shares a tag's columns across its views."
  (let ((tags (sort (mapcar #'symbol-name (org-glance-filter:tags filter)) #'string<)))
    (if tags (s-join "+" tags) ":none:")))

(cl-defun org-glance-table--schema-entry (graph filter)
  "FILTER's saved schema plist (:columns PROPS :hidden KEYS) for its tags, or nil."
  (org-glance--eld-alist-ref (org-glance-table--schema-file graph)
                             (org-glance-table--schema-key filter)))

(cl-defun org-glance-table--schema-get (graph filter)
  "Ordered custom columns saved for FILTER's tags.
A list of (PROP . HEADER), or nil."
  (plist-get (org-glance-table--schema-entry graph filter) :columns))

(cl-defun org-glance-table--schema-hidden (graph filter)
  "Built-in column keys hidden for FILTER's tags (a list of key strings), or nil."
  (plist-get (org-glance-table--schema-entry graph filter) :hidden))

(cl-defun org-glance-table--schema-put (graph filter &key columns hidden)
  "Persist FILTER's per-tag schema: custom COLUMNS ((PROP . HEADER) list) and
HIDDEN built-in column keys.  An all-empty schema drops the entry so the store
does not accrete empties."
  (org-glance--eld-alist-set
   (org-glance-table--schema-file graph)
   (org-glance-table--schema-key filter)
   (and (or columns hidden) (list :columns columns :hidden hidden))))

(cl-defun org-glance-table--compose-columns (graph base hidden pairs)
  "BASE columns minus the HIDDEN keys (Title never dropped), plus custom
columns built from PAIRS ((NAME . HEADER) list) via
`org-glance-table--custom-column'.  The single column-assembly core shared by
the per-tag schema and the scoped reference entries."
  (let ((hidden (remove "title" hidden)))
    (append (cl-remove-if (lambda (c) (member (alist-get 'key c) hidden)) base)
            (mapcar (lambda (pair)
                      (org-glance-table--custom-column graph (car pair) (cdr pair)))
                    pairs))))

(cl-defun org-glance-table--apply-schema (graph filter columns)
  "GRAPH's saved per-tag schema for FILTER applied to built-in COLUMNS.
Absent a schema, COLUMNS is returned unchanged."
  (org-glance-table--compose-columns graph columns
                                     (org-glance-table--schema-hidden graph filter)
                                     (org-glance-table--schema-get graph filter)))

(cl-defun org-glance-table--persist-schema ()
  "Buffer-local `table-view-schema-changed-hook': save this filter's schema per
tag -- the live spec's custom (`prop') columns, and which built-in columns are
hidden (a built-in key absent from the live spec).  Transient views (relation
filters) persist nothing -- their tagless schema key would edit the shared
untagged (\":none:\") entry."
  (when (and org-glance-view--graph
             (not (org-glance-filter:transient? org-glance-table--spec)))
    (let ((snap (org-glance-table--layout-snapshot)))
      (with-demoted-errors "org-glance: table schema save failed: %S"
        (org-glance-table--schema-put org-glance-view--graph org-glance-table--spec
                                      :columns (plist-get snap :columns)
                                      :hidden (plist-get snap :hidden))))))

;;; Scoped layout for reference tables (`C-c C-c' applies, scope-keyed)
;;
;; Reference tables (`C-c @' / `C-u C-c @') are transient: neither persistence
;; path above may touch them (their filter identity embeds another headline's
;; id/link set and would accrete one entry per visit).  Instead the user applies
;; an edited layout EXPLICITLY: `C-c C-c' prompts for a scope -- the anchor
;; headline itself, or one (anchor-tag x row-tag) pair -- and saves the full
;; layout (custom columns, hidden built-ins, order, sort) under that scope in
;; `<store>/config/table-refs.eld'.  A later reference table restores the
;; anchor's own entry first, else the latest-applied matching pair entry.
;; Scopes are per direction: references and back-references list different row
;; populations, so they never share an entry.

(cl-defun org-glance-table--refs-file (graph)
  "Path of GRAPH's scoped reference-layout store (may not exist)."
  (org-glance-graph:config-file graph "table-refs.eld"))

(cl-defun org-glance-table--refs-key-id (context)
  "Headline-scope store key for CONTEXT."
  (format "ref:%s:%s" (plist-get context :dir) (plist-get context :anchor)))

(cl-defun org-glance-table--refs-key-pair (dir from to)
  "Tag-pair store key for DIR and the FROM -> TO edge-tag pair."
  (format "pair:%s:%s>%s" dir from to))

(cl-defun org-glance-table--refs-tags (graph context)
  "CONTEXT's (ANCHOR-TAGS . ROW-TAGS), each sorted distinct downcased strings.
Rows are the anchor's edge targets (`refs') or every headline with an edge to
the anchor (`backlinks')."
  (cl-flet ((tags-of (meta) (mapcar (lambda (tag) (downcase (format "%s" tag)))
                                    (org-glance-headline-metadata:tags meta))))
    (let* ((anchor (plist-get context :anchor))
           (meta (org-glance-graph:get-headline graph anchor))
           (meta (and (org-glance-headline-metadata? meta) meta))
           (row-metas
            (pcase (plist-get context :dir)
              ('refs (when-let ((targets (and meta (delete-dups
                                                   (mapcar #'car (org-glance-headline-metadata:relations meta))))))
                       (org-glance-graph--metas graph targets)))
              ('backlinks (cl-remove-if-not
                           (org-glance-filter:predicate `(:refers-to ,anchor))
                           (org-glance-graph--metas graph))))))
      (cons (org-glance--sorted-distinct (and meta (tags-of meta)))
            (org-glance--sorted-distinct (cl-loop for m in row-metas append (tags-of m)))))))

(cl-defun org-glance-table--refs-tag-pairs (context anchor-tags row-tags)
  "CONTEXT's candidate tag pairs from ANCHOR-TAGS x ROW-TAGS, as (FROM . TO).
The single source of the pair order: edge direction, referrer tag first."
  (let ((refs? (eq (plist-get context :dir) 'refs)))
    (cl-loop for a in anchor-tags append
             (cl-loop for r in row-tags collect
                      (if refs? (cons a r) (cons r a))))))

(cl-defun org-glance-table--refs-pair-keys (context anchor-tags row-tags)
  "Every pair store key CONTEXT can match."
  (let ((dir (plist-get context :dir)))
    (mapcar (lambda (p) (org-glance-table--refs-key-pair dir (car p) (cdr p)))
            (org-glance-table--refs-tag-pairs context anchor-tags row-tags))))

(cl-defun org-glance-table--refs-resolve (graph context)
  "Scoped layout entry for CONTEXT, or nil.
The anchor's own entry wins; else among matching tag-pair entries the latest
`:applied' wins.  The row-tag scan runs only when pair entries exist for the
direction."
  (let ((all (org-glance--read-eld (org-glance-table--refs-file graph))))
    (or (cdr (assoc (org-glance-table--refs-key-id context) all))
        (let ((prefix (format "pair:%s:" (plist-get context :dir))))
          (when (cl-some (lambda (e) (string-prefix-p prefix (car e))) all)
            (let* ((tags (org-glance-table--refs-tags graph context))
                   (keys (org-glance-table--refs-pair-keys context (car tags) (cdr tags)))
                   (hits (cl-remove-if-not (lambda (e) (member (car e) keys)) all)))
              (cdr (car (cl-sort hits #'>
                                 :key (lambda (e) (or (plist-get (cdr e) :applied) 0)))))))))))

(cl-defun org-glance-table--refs-columns (graph entry base)
  "BASE columns filtered and extended per scoped ENTRY, in its saved order."
  (org-glance-table--reorder-columns
   (org-glance-table--compose-columns graph base (plist-get entry :hidden)
                                      (plist-get entry :columns))
   (plist-get entry :order)))

(cl-defun org-glance-table--layout-snapshot ()
  "This buffer's full layout as one persistable plist (unstamped).
`:columns' the custom (PROP . HEADER) pairs (round-tripped via the `prop'
marker exactly like the per-tag schema), `:hidden' the built-in keys absent
from the live view, `:order' every live key, `:sort' the sort chain.  Read
through `table-view-layout' -- no spec internals.  The scoped reference
entries store this whole plist; the persistent-view stores each persist a
projection of it."
  (let* ((layout (table-view-layout))
         (live (plist-get layout :columns))
         (live-keys (mapcar (lambda (c) (alist-get 'key c)) live)))
    (list :columns (cl-loop for c in live
                            when (alist-get 'prop c)
                            collect (cons (alist-get 'prop c) (alist-get 'header c)))
          :hidden (cl-remove-if
                   (lambda (k) (member k live-keys))
                   (mapcar (lambda (c) (alist-get 'key c))
                           (org-glance-table--base-columns org-glance-view--graph)))
          :order live-keys
          :sort (plist-get layout :sort))))

(cl-defun org-glance-table:apply-layout ()
  "Save this table's layout (`C-c C-c').
A reference view prompts for a scope (required match): the anchor headline
itself, or one anchor-tag x row-tag pair -- later reference tables matching
the scope restore the layout (see `org-glance-table--refs-resolve' for
precedence).  A persistent view saves its per-filter config and per-tag
schema on the spot -- the same state the automatic hooks persist.  Other
transient views (`:where') have no scope to save under."
  (interactive)
  (unless org-glance-view--graph (user-error "Not in an org-glance table"))
  (cond
   (org-glance-table--context (org-glance-table--apply-ref-layout))
   ((org-glance-filter:transient? org-glance-table--spec)
    (user-error "This view's layout cannot be persisted"))
   (t
    (org-glance-table--persist-schema)
    (org-glance-table--persist-config)
    (message "Layout saved for %s"
             (org-glance-filter:describe org-glance-table--spec)))))

(cl-defun org-glance-table--apply-ref-layout ()
  "Reference-view arm of `org-glance-table:apply-layout': prompt and persist."
  (let ((graph org-glance-view--graph)
        (context org-glance-table--context))
    (let* ((dir (plist-get context :dir))
           (tags (org-glance-table--refs-tags graph context))
           (candidates
            (cons (cons (format "this headline: %s"
                                (org-glance-graph:title-or-id
                                 graph (plist-get context :anchor)))
                        (org-glance-table--refs-key-id context))
                  (mapcar (lambda (p)
                            (cons (format "tag pair: %s → %s" (car p) (cdr p))
                                  (org-glance-table--refs-key-pair dir (car p) (cdr p))))
                          (org-glance-table--refs-tag-pairs context (car tags) (cdr tags)))))
           (choice (completing-read "Apply this layout to: "
                                    (mapcar #'car candidates) nil t)))
      (org-glance--eld-alist-set
       (org-glance-table--refs-file graph)
       (cdr (assoc choice candidates))
       (plist-put (org-glance-table--layout-snapshot) :applied (float-time)))
      (message "Layout applied to %s" choice))))

(cl-defun org-glance-table--act-delete (graph id)
  "`D' handler: delete the headline at point (referrer-aware confirmation)."
  (unless id (user-error "Point is not on a row"))
  (let ((line (line-number-at-pos)))
    (when (org-glance-material:delete graph id)
      (org-glance-table--finish id line "Headline deleted"))))

(cl-defun org-glance-table--act-planning (graph id kind)
  "Set (or with `C-u' clear) KIND planning of the row at point, like org's keys."
  (unless id (user-error "Point is not on a row"))
  (let ((line (line-number-at-pos))
        (remove current-prefix-arg))
    (org-glance-material:set-planning graph id kind remove)
    (org-glance-table--finish id line "%s %s" (capitalize (symbol-name kind))
                              (if remove "cleared" "set"))))

(cl-defun org-glance-table--act-duplicate (graph id)
  "`C-c p' handler: add a copy of the row's headline under a fresh id."
  (unless id (user-error "Point is not on a row"))
  (let ((line (line-number-at-pos))
        (new (org-glance-material:duplicate graph id)))
    (org-glance-table--finish new line "Headline copied")))

(cl-defun org-glance-table--act-edit (graph id)
  "`i' handler: edit the cell at point in place.
State reuses the todo flow (`C-c C-t'), schedule/deadline the calendar
planner (`org-read-date'), tags the `:' flow (a `C-u' passes through as
remove); title, priority and drawer-property columns take a string prompt
pre-filled with the current value.  Derived columns (interval, enc, rep,
relation kinds) refuse."
  (unless id (user-error "Point is not on a row"))
  (let ((key (get-text-property (point) 'table-view-col))
        (line (line-number-at-pos)))
    (pcase key
      ('nil (user-error "Point is not on a column"))
      ("state" (org-glance-table--act-todo graph id))
      ("tags" (org-glance-table--act-tag graph id))
      ("schedule" (org-glance-table--act-planning graph id 'schedule))
      ("deadline" (org-glance-table--act-planning graph id 'deadline))
      ("title"
       (org-glance-material:set-title
        graph id (read-string "Title: "
                              (org-glance-headline-metadata:title
                               (org-glance-table--metadata graph id))))
       (org-glance-table--finish id line "Title set"))
      ("priority"
       (let* ((cur (org-glance-headline-metadata:priority
                    (org-glance-table--metadata graph id)))
              (s (s-trim (read-string "Priority (empty clears): "
                                      (and (integerp cur) (char-to-string cur))))))
         (org-glance-material:set-priority
          graph id (unless (string-empty-p s) (string-to-char (upcase s))))
         (org-glance-table--finish id line "Priority %s"
                                   (if (string-empty-p s) "cleared" "set"))))
      ((pred org-glance-table--property-key?)       ; drawer-property column
       (let ((val (read-string (format "%s: " key)
                               (org-glance-property-index:property graph id key))))
         (org-glance-material:set-property graph id key val)
         (org-glance-table--finish id line "%s %s" key
                                   (if (org-glance--present-string? val)
                                       "set" "removed"))))
      (_ (user-error "Column `%s' is not editable here" key)))))

(cl-defun org-glance-table--act-history (graph id)
  "`l' handler: open one of ID's occurrence snapshots, read-only."
  (unless id (user-error "Point is not on a row"))
  (org-glance-view:pick-occurrence graph id))

(cl-defun org-glance-table--act-delcolumn ()
  "`C-u -' handler: remove the column at point.  Title is mandatory and refused."
  (let ((key (get-text-property (point) 'table-view-col)))
    (cond
     ((null key) (message "Point is not on a column"))
     ((equal key "title") (user-error "The Title column cannot be removed"))
     (t (table-view-remove-column key)))))

(cl-defun org-glance-table--act-deltag (graph id spec)
  "Bare `-' handler: drop the view's tag from the headline ID at point.
Mirror of the bare `+' capture -- the headline leaves the view but is NOT
deleted.  With several filter tags, ask which; unsaved material edits abort the
drop (retag's `user-error')."
  (let* ((tags (org-glance-filter:tags spec))
         (tag (cond ((null tags) (user-error "This view has no tag to remove"))
                    ((null (cdr tags)) (format "%s" (car tags)))
                    (t (completing-read "Remove which tag: "
                                        (mapcar (lambda (x) (format "%s" x)) tags)
                                        nil t)))))
    (when (y-or-n-p (format "Remove tag `%s' from the headline at point? " tag))
      (let ((buf (current-buffer)))
        (condition-case nil
            (progn (org-glance-material:retag graph id tag :remove t)
                   (org-glance-table--reload buf)
                   (message "Removed tag `%s'" tag))
          (user-error (message "Headline `%s' has unsaved edits; save it first" id)))))))

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

(cl-defun org-glance-table--handlers (graph spec)
  "The action-command handler alist for GRAPH's table under SPEC."
  (list (cons "materialize" (lambda (id _row) (org-glance-table--act-materialize graph id)))
        (cons "open"        (lambda (id _row) (org-glance-table--act-open graph id)))
        (cons "extract"     (lambda (id _row) (org-glance-table--act-extract graph id)))
        (cons "todo"        (lambda (rows)
                              ;; `bulk' handler: a row LIST.  With marks -> bulk
                              ;; (one prompt, all marked); else the row at point
                              ;; gets the full single-row `C-c C-t' (cycle + note).
                              (if (table-view-marked-rows)
                                  (org-glance-table--act-todo-bulk graph rows)
                                (let ((row (car rows)))
                                  (org-glance-table--act-todo graph (alist-get 'id row))))))
        (cons "refresh"     (lambda (_id _row) (org-glance-table--reload (current-buffer))))
        (cons "overview"    (lambda (_id _row) (org-glance-overview:visit graph spec)))
        (cons "remove"      (lambda (id _row)
                              ;; `C-u -' removes the column at point; a bare
                              ;; `-' drops the view's tag off the headline
                              ;; (mirror of `+' / `C-u +').
                              (if current-prefix-arg
                                  (org-glance-table--act-delcolumn)
                                (org-glance-table--act-deltag graph id spec))))
        (cons "capture"     (lambda (_id _row)
                              ;; `C-u +' adds a custom property column; a bare `+' captures.
                              (if current-prefix-arg
                                  (call-interactively #'table-view-add-column)
                                (org-glance-capture (or (org-glance-filter:tags spec)
                                                        (org-glance-capture:completing-read-tag))
                                                    ""))))
        (cons "tag"      (lambda (id _row) (org-glance-table--act-tag graph id)))
        (cons "crypt"    (lambda (id _row) (org-glance-table--act-crypt graph id)))
        (cons "history"  (lambda (id _row) (org-glance-table--act-history graph id)))
        (cons "edit"      (lambda (id _row) (org-glance-table--act-edit graph id)))
        (cons "duplicate" (lambda (id _row) (org-glance-table--act-duplicate graph id)))
        (cons "delete"   (lambda (id _row) (org-glance-table--act-delete graph id)))
        (cons "schedule" (lambda (id _row) (org-glance-table--act-planning graph id 'schedule)))
        (cons "deadline" (lambda (id _row) (org-glance-table--act-planning graph id 'deadline)))))

(cl-defun org-glance-table--visit-spec (graph spec saved ref-entry context)
  "The display spec for SPEC, its column set resolved by view kind.
A scoped REF-ENTRY replaces the whole set (built-ins minus its hidden, plus
its custom columns, its order); a scope-less reference view (CONTEXT) gets
plain defaults, never the shared untagged (\":none:\") per-tag schema; else
SAVED's column order is restored."
  (let ((s (org-glance-table--spec graph spec)))
    (cond (ref-entry
           (setf (alist-get 'columns s)
                 (org-glance-table--refs-columns
                  graph ref-entry (org-glance-table--base-columns graph))))
          (context
           (setf (alist-get 'columns s)
                 (org-glance-table--base-columns graph)))
          ((plist-get saved :columns)
           (setf (alist-get 'columns s)
                 (org-glance-table--reorder-columns
                  (alist-get 'columns s) (plist-get saved :columns)))))
    s))

(cl-defun org-glance-table:visit (graph &optional filter &key context)
  "Open GRAPH's table for FILTER, one buffer per filter description.
Honours the same filter language as the overview (see
`org-glance-filter:predicate').  CONTEXT marks a reference view
\(`:anchor' ID `:dir' `refs'|`backlinks'): it enables the scoped layout --
restore on open, `C-c C-c' to apply."
  (let* ((from-view (and org-glance-view--graph t))   ; re-navigation from within a view?
         (spec (org-glance-filter:normalize-spec filter))
         (saved (org-glance-table--config-get graph spec))   ; restored column order + sort
         (ref-entry (and context (org-glance-table--refs-resolve graph context)))
         ;; Resolve the active/done split ONCE and bind it while the `:done'
         ;; predicate AND the badge split are built, so the table agrees with the
         ;; overview (W2).  The predicate is captured in `fill-fn', so a reload
         ;; reuses this exact split with no further bookkeeping.
         (org-done-keywords
          (org-glance-tag-config:done-keywords-for-filter graph spec))
         (keep? (org-glance-filter:predicate spec))
         (buffer-name (format "*org-glance-table: %s*" (org-glance-filter:describe spec)))
         (src (org-glance-graph:headline-meta-path graph))
         (fill-fn (lambda (buf)
                    (with-current-buffer buf
                      (let ((rows (org-glance-table--rows graph keep?)))
                        (table-view-set-rows buf rows)
                        ;; `set-rows' already warmed the memo via the custom
                        ;; columns' value-fns; persist only if a blob re-parsed.
                        (org-glance-property-index--flush-if-dirty graph)
                        (org-glance-view:snapshot-mtime src)))))
         (buf (table-view-display
               buffer-name
               (org-glance-table--visit-spec graph spec saved ref-entry context)
               (org-glance-table--handlers graph spec)
               fill-fn)))
    (with-current-buffer buf
      (setq org-glance-table--context context)
      (local-set-key (kbd "C-c C-c") #'org-glance-table:apply-layout)
      (setq org-glance-table--spec spec
            ;; Match the corresponding overview (`org-glance-overview:visit'): run
            ;; directory-relative actions (dired, shell, relative links) from the
            ;; graph's ROOT, not wherever this non-file buffer happened to spawn.
            default-directory (file-name-as-directory (org-glance-graph:directory graph)))
      (org-glance-view:register graph
                                :stale-fn  (lambda () (org-glance-view:stale-vs-file? src))
                                :reload-fn (lambda () (org-glance-table--reload (current-buffer))))
      ;; Custom property columns: `C-u +' builds one via the prompt below, and any
      ;; add/remove is persisted per tag through the schema-changed hook.
      (setq-local table-view-add-column-function #'org-glance-table--add-column-prompt)
      ;; `C-u /' clears the active filter; a bare `/' stays filter-or-narrow.
      (local-set-key "/" #'org-glance-table:filter-or-reset)
      ;; `!' quietly aliases `j' (open link) -- dired's execute rhyme.
      (local-set-key (kbd "!") (lookup-key (current-local-map) (kbd "j")))
      (add-hook 'table-view-schema-changed-hook #'org-glance-table--persist-schema nil t)
      ;; Restore the saved sort (else the spec default seeded by display), apply it,
      ;; then persist any subsequent layout change (column move / sort) for this filter.
      (if-let ((sort (or (plist-get ref-entry :sort) (plist-get saved :sort))))
          (table-view-set-sort sort)
        (table-view-apply-sort))
      (setq org-glance-table--config-snapshot (org-glance-table--current-config))
      (add-hook 'post-command-hook #'org-glance-table--persist-config nil t)
      (org-glance-view:fill-frame from-view))
    buf))

;;;###autoload
(cl-defun org-glance-table (&optional tag)
  "Browse the graph as a sortable, badge-coloured table, optionally filtered.
Interactively, prompt for a tag (empty input = no tag constraint) and overlay it
on the ambient `org-glance-filter-spec' (default: active headlines) -- exactly
like `org-glance-overview', but rendered as a flat table.  Sort with `^' (sorts
by the column at point; repeat toggles direction, `C-u ^' adds a tie-breaker);
act on the row at point with RET/m, j, e."
  (interactive (list (org-glance-view:completing-read-tag "Table tag (empty for all): ")))
  (org-glance-ensure-init)
  (org-glance-table:visit org-glance-graph
                          (org-glance-filter:merge org-glance-filter-spec tag)))

(provide 'org-glance-table)
;;; org-glance-table.el ends here
