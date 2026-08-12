;; -*- lexical-binding: t -*-

;;; org-glance-table.el --- table-view-backed headline dashboard

;;; Commentary:
;; An in-memory `table-view' over the graph; sister to `org-glance-overview'.

;;; Code:

(require 'cl-lib)
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

;;; Spec + rows

(cl-defun org-glance-table--base-columns (graph)
  "The fixed built-in table columns for GRAPH, in default order.
Title is mandatory (`C-c -' refuses it); the rest are removable, and which are
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

(cl-defun org-glance-table--mandatory-column? (key)
  "Non-nil when column KEY may never be removed or hidden (invariant 15).
The single spelling of the rule, read by `--compose-columns',
`org-glance-table:remove-column' and `--read-column'."
  (equal key "title"))

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
                ((key . "@")     (command . "relations") (label . "Relations"))
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
     (cl-flet ((day (ts) (if (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" ts)
                             (match-string 0 ts) ts)))
       (concat (day from) ".." (day to))))
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

(defvar-local org-glance-table--spec nil
  "Normalised filter spec the current table buffer was generated with.")
(cl-defun org-glance-table--rows (graph keep?)
  "Rows for GRAPH's live headlines satisfying predicate KEEP?, in graph order."
  (cl-loop for meta in (org-glance-graph:headlines graph)
           when (funcall keep? meta)
           collect (org-glance-table--row meta)))

(cl-defun org-glance-table--finish (id line fmt &rest args)
  "Update row ID, return point to it (else screen LINE), message FMT ARGS.
A single-row action changes one headline, so the row is upserted from its fresh
metadata, or dropped when the headline is gone or has left this view's filter;
a full reload would re-derive all N rows for that one change.  The CELL under
point is kept (invariant 24).  A view with no predicate to judge by falls back
to a full reload, so it can never show a stale row."
  (let* ((buf (current-buffer))
         (col (org-glance-view:column-at-point))
         (graph org-glance-view--graph)
         (meta (and graph (org-glance-graph:live-meta graph id))))
    (cond
     ((null org-glance-table--keep-fn)
      (org-glance-table--reload buf))
     ((and meta (funcall org-glance-table--keep-fn meta))
      (table-view-upsert-row buf (org-glance-table--row meta))
      (table-view-apply-sort))
     (t (table-view-delete-row buf id)))
    ;; Our own write advanced the store; re-anchor mtime (invariant 7).
    (when graph
      (org-glance-view:snapshot-mtime (org-glance-graph:headline-meta-path graph)))
    (org-glance-view:mark-fresh)
    (org-glance-view:restore-point id line col))
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
      (pcase-let ((`(,id ,line ,col) (org-glance-view:point-context)))
        (table-view-refresh buf)
        (table-view-apply-sort)
        (org-glance-view:mark-fresh)
        (org-glance-view:restore-point id line col)))))

;;; Actions (id-keyed; the table-view core hands each handler the row's id)

(cl-defun org-glance-table--act-materialize (graph id)
  (when id (switch-to-buffer (org-glance-material:open graph id))))

(cl-defun org-glance-table--act-open (graph id)
  (when id (org-glance-material:open-link (org-glance-view:live-headline graph id))))

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
string, or nil when the user clears the state (org's own `SPC\' answer) --
which the bulk caller treats as \"no change\"."
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
  (let ((ids (org-glance-table--row-ids rows)))
    (when ids
      (pcase-let ((`(,at-id ,line ,col) (org-glance-view:point-context))
                  (state (org-glance-table--read-state-native graph org-glance-table--spec)))
        (when state                       ; `none' clears; C-g aborts before here
          (org-glance-material:set-todo-bulk
           graph ids state
           (lambda (changed skipped)
             (org-glance-table--reload (current-buffer))
             (table-view-unmark-all)
             (org-glance-view:restore-point at-id line col)
             (message "Set %d headline(s) to %s%s"
                      (length changed) state
                      (if skipped (format " (%d skipped)" (length skipped)) "")))))))))

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
               (org-glance-view:live-metadata graph id)))
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
                     (org-glance-view:live-metadata graph id)))
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

(cl-defun org-glance-table--column-keys (columns)
  "COLUMNS' key strings, in order."
  (mapcar (lambda (c) (alist-get 'key c)) columns))

(cl-defun org-glance-table--row-ids (rows)
  "The ids of ROWS, id-less rows dropped."
  (delq nil (mapcar (lambda (r) (alist-get 'id r)) rows)))

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

(defvar-local org-glance-table--keep-fn nil
  "This view's row predicate, as the fill-fn built it.
`org-glance-filter:predicate' of its spec, with the tag's done-set bound.  Kept
so a single-row action can ask whether one changed headline still belongs here,
using no other row.")

(defvar-local org-glance-table--context nil
  "Relation-view context plist (`:anchor' ID `:dir' `relations'), or nil.")

(cl-defun org-glance-table--current-config ()
  "This buffer's current view config: (:columns KEYS :sort SORT-KEYS).
The cheap change-detection projection of `org-glance-table--layout-snapshot'
\(no hidden-column diff), safe on the post-command hot path."
  (let ((layout (table-view-layout)))
    (list :columns (org-glance-table--column-keys (plist-get layout :columns))
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

;;; Column schema (`C-c +' / `C-c -'), persisted PER TAG -- invariant 16

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
                   (if-let ((meta (org-glance-graph:live-meta graph id)))
                       (s-join ", "
                               (cl-loop for (target . k) in (org-glance-headline-metadata:relations meta)
                                        when (equal k kind)
                                        collect (org-glance-graph:title-or-id graph target)))
                     "")))))

(cl-defun org-glance-table--edges-between (graph from to)
  "Return the kinds of FROM's edges to TO in GRAPH, nil when there is no edge.
One element per edge, `nil' for a kindless one, so a single kindless edge
reads as the one-element list (nil)."
  (when-let ((meta (org-glance-graph:live-meta graph from)))
    (cl-loop for (target . kind) in (org-glance-headline-metadata:relations meta)
             when (equal target to) collect kind)))

(cl-defun org-glance-table--relation-cell (graph anchor id)
  "Return ID's relation to ANCHOR in GRAPH as a display string.
`> KIND' when ANCHOR refers to ID, `< KIND' when ID refers to ANCHOR; a
kindless edge shows the arrow alone, a mutual pair both, joined with `, '.
Kinds display in their spaced form (`org-glance--kind-pretty')."
  (cl-flet ((edges (arrow kinds)
              (mapcar (lambda (kind)
                        (if kind
                            (concat arrow " " (org-glance--kind-pretty kind))
                          arrow))
                      kinds)))
    (s-join ", " (append (edges ">" (org-glance-table--edges-between graph anchor id))
                         (edges "<" (org-glance-table--edges-between graph id anchor))))))

(cl-defun org-glance-table--relation-column (graph anchor)
  "Return a `table-view' column showing each row's relation to ANCHOR.
Carried only by relation tables (`org-glance-table:visit-relations'), whose
rows are ANCHOR's neighbours in both directions; the cell names the direction
and kind (`org-glance-table--relation-cell').  Metadata reads only."
  `((key . "relation")
    (header . "Relation")
    (type . "text")
    (sortable . t)
    (align . "left")
    (value-fn . ,(lambda (id _row)
                   (org-glance-table--relation-cell graph anchor id)))))

(cl-defun org-glance-table--context-columns (graph context)
  "Built-in columns for a relation view of CONTEXT: the base plus `Relation'."
  (append (org-glance-table--base-columns graph)
          (list (org-glance-table--relation-column
                 graph (plist-get context :anchor)))))

(cl-defun org-glance-table--related-ids (graph id)
  "Return the distinct ids related to ID in GRAPH, in both directions.
ID's edge targets first, then every headline carrying an edge to ID: the row
population of a relation table."
  (delete-dups
   (append (when-let ((meta (org-glance-graph:live-meta graph id)))
             (org-glance-headline-metadata:relation-targets meta))
           (mapcar #'org-glance-headline-metadata:id
                   (cl-remove-if-not (org-glance-filter:predicate `(:refers-to ,id))
                                     (org-glance-graph--metas graph))))))

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
so `C-c +' uses it."
  (let* ((graph org-glance-view--graph)
         (ids (org-glance-table--row-ids table-view--rows))
         ;; invariant 13: kinds display PRETTY, canonicalize to their slug
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

(cl-defun org-glance-table--schema-file (graph)
  "Path of GRAPH's per-tag custom-column schema store (may not exist)."
  (org-glance-graph:config-file graph "table-columns.eld"))

(cl-defun org-glance-table--schema-key (filter)
  "Canonical per-tag key for FILTER: its tags sorted and `+'-joined, or
\":none:\" when the filter carries no tag constraint.  Keying on the tags
alone is what shares a tag's columns across all of its views."
  (let ((tags (sort (mapcar #'symbol-name (org-glance-filter:tags filter)) #'string<)))
    (if tags (s-join "+" tags) ":none:")))

(cl-defun org-glance-table--schema-entry (graph filter)
  "FILTER's saved schema plist for its tags, or nil.
`:columns' is an ordered ((PROP . HEADER)) list of custom columns, `:hidden'
the built-in column keys removed for those tags."
  (org-glance--eld-alist-ref (org-glance-table--schema-file graph)
                             (org-glance-table--schema-key filter)))

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
  (let ((hidden (cl-remove-if #'org-glance-table--mandatory-column? hidden)))
    (append (cl-remove-if (lambda (c) (member (alist-get 'key c) hidden)) base)
            (mapcar (lambda (pair)
                      (org-glance-table--custom-column graph (car pair) (cdr pair)))
                    pairs))))

(cl-defun org-glance-table--apply-schema (graph filter columns)
  "GRAPH's saved per-tag schema for FILTER applied to built-in COLUMNS.
Absent a schema, COLUMNS is returned unchanged."
  (let ((entry (org-glance-table--schema-entry graph filter)))   ; one eld read
    (org-glance-table--compose-columns graph columns
                                       (plist-get entry :hidden)
                                       (plist-get entry :columns))))

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

;;; Scoped reference layouts: `C-c C-c' only, scope-keyed (invariants 17-18)

(cl-defun org-glance-table--refs-file (graph)
  "Path of GRAPH's scoped reference-layout store (may not exist)."
  (org-glance-graph:config-file graph "table-refs.eld"))

(cl-defun org-glance-table--refs-key-id (context)
  "Headline-scope store key for CONTEXT."
  (format "ref:relations:%s" (plist-get context :anchor)))

(cl-defun org-glance-table--refs-key-pair (from to)
  "Tag-pair store key for the FROM (anchor) -> TO (row) tag pair."
  (format "pair:relations:%s>%s" from to))

(cl-defun org-glance-table--refs-tags (graph context)
  "CONTEXT's (ANCHOR-TAGS . ROW-TAGS), each sorted distinct downcased strings.
Rows are the anchor's neighbours in both directions
\(`org-glance-table--related-ids')."
  (let* ((anchor (plist-get context :anchor))
         (meta (org-glance-graph:live-meta graph anchor))
         (row-metas (org-glance-graph--metas
                     graph (org-glance-table--related-ids graph anchor))))
    (cons (org-glance--sorted-distinct
           (and meta (org-glance-headline-metadata:tag-strings meta)))
          (org-glance--sorted-distinct
           (cl-loop for m in row-metas append (org-glance-headline-metadata:tag-strings m))))))

(cl-defun org-glance-table--refs-tag-pairs (anchor-tags row-tags)
  "Candidate tag pairs from ANCHOR-TAGS x ROW-TAGS, as (FROM . TO).
The single source of the pair order: the anchor's tag first, the row's second.
A relation table lists both directions, so the pair is scope, never direction."
  (cl-loop for a in anchor-tags append
           (cl-loop for r in row-tags collect (cons a r))))

(cl-defun org-glance-table--refs-resolve (graph context)
  "Scoped layout entry for CONTEXT, or nil.
The anchor's own entry wins; else among matching tag-pair entries the latest
`:applied' wins.  The row-tag scan runs only when pair entries exist."
  (let ((all (org-glance--read-eld (org-glance-table--refs-file graph))))
    (or (cdr (assoc (org-glance-table--refs-key-id context) all))
        (when (cl-some (lambda (e) (string-prefix-p "pair:relations:" (car e))) all)
          (let* ((tags (org-glance-table--refs-tags graph context))
                 (keys (mapcar (lambda (p) (org-glance-table--refs-key-pair (car p) (cdr p)))
                               (org-glance-table--refs-tag-pairs (car tags) (cdr tags))))
                 (hits (cl-remove-if-not (lambda (e) (member (car e) keys)) all)))
            (cdr (car (cl-sort hits #'>
                               :key (lambda (e) (or (plist-get (cdr e) :applied) 0))))))))))

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
through `table-view-layout' -- no spec internals.  The scoped relation
entries store this whole plist; the persistent-view stores each persist a
projection of it.  `:hidden' diffs against the view's OWN built-in set --
a relation view's includes `Relation', so removing that column is recorded
like any other built-in instead of silently reappearing on restore."
  (let* ((layout (table-view-layout))
         (live (plist-get layout :columns))
         (live-keys (org-glance-table--column-keys live))
         (built-in (if org-glance-table--context
                       (org-glance-table--context-columns org-glance-view--graph
                                                          org-glance-table--context)
                     (org-glance-table--base-columns org-glance-view--graph))))
    (list :columns (cl-loop for c in live
                            when (alist-get 'prop c)
                            collect (cons (alist-get 'prop c) (alist-get 'header c)))
          :hidden (cl-remove-if
                   (lambda (k) (member k live-keys))
                   (org-glance-table--column-keys built-in))
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
  (org-glance-table--ensure)
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
    (let* ((tags (org-glance-table--refs-tags graph context))
           (candidates
            (cons (cons (format "this headline: %s"
                                (org-glance-graph:title-or-id
                                 graph (plist-get context :anchor)))
                        (org-glance-table--refs-key-id context))
                  (mapcar (lambda (p)
                            (cons (format "tag pair: %s → %s" (car p) (cdr p))
                                  (org-glance-table--refs-key-pair (car p) (cdr p))))
                          (org-glance-table--refs-tag-pairs (car tags) (cdr tags)))))
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
  (let ((key (org-glance-view:column-at-point))
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
                               (org-glance-view:live-metadata graph id))))
       (org-glance-table--finish id line "Title set"))
      ("priority"
       (let* ((cur (org-glance-headline-metadata:priority
                    (org-glance-view:live-metadata graph id)))
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

(cl-defun org-glance-table--act-deltag (graph id spec)
  "Bare `-' handler: drop the view's tag from the headline ID at point.
Mirror of the bare `+' capture -- the headline leaves the view but is NOT
deleted.  With several filter tags, ask which; unsaved material edits abort the
drop (retag's `user-error')."
  (let* ((tags (org-glance-filter:tags spec))
         (tag (cond ((null tags) (user-error "This view has no tag to remove"))
                    ((null (cdr tags)) (format "%s" (car tags)))
                    (t (completing-read "Remove which tag: "
                                        (org-glance--strings tags)
                                        nil t)))))
    (when (y-or-n-p (format "Remove tag `%s' from the headline at point? " tag))
      (let ((buf (current-buffer)))
        (condition-case nil
            (progn (org-glance-material:retag graph id tag :remove t)
                   (org-glance-table--reload buf)
                   (message "Removed tag `%s'" tag))
          (user-error (message "Headline `%s' has unsaved edits; save it first" id)))))))

(defun org-glance-table:filter-or-reset ()
  "Filter or narrow the table; with a prefix arg, clear the active filter.
With no prefix, defer to `table-view-filter-or-narrow' -- narrow to the marked
rows, or prompt for a substring filter when none are marked.  With a prefix arg
\(`C-u /'), clear the current substring filter without prompting."
  (interactive)
  (if current-prefix-arg
      (table-view-filter "")
    (call-interactively #'table-view-filter-or-narrow)))

(cl-defun org-glance-table--act-refresh ()
  "Drop this buffer's display refinements, then re-fill it from the graph (`g').
The `/' substring filter and a narrow-to-marked view refine what the table
shows on top of the filter it was opened with; `g' clears both, returning the
view to that filter.  Marks survive, being a selection.  Clearing `table-view''s
state directly keeps this to one render.
`org-glance-table--reload' PRESERVES the refinements, so neither an edit nor
the display-boundary refresh widens the view under the user."
  (setq table-view--filter nil
        table-view--narrowed nil)
  (org-glance-table--reload (current-buffer)))

(cl-defun org-glance-table--handlers (graph spec)
  "The action-command handler alist for GRAPH's table under SPEC."
  (list (cons "materialize" (lambda (id _row) (org-glance-table--act-materialize graph id)))
        (cons "open"        (lambda (id _row) (org-glance-table--act-open graph id)))
        (cons "extract"     (lambda (id _row) (org-glance-table--act-extract graph id)))
        (cons "todo"        (lambda (rows)
                              ;; `(bulk . t)' -> the core hands a row LIST.
                              (if (table-view-marked-rows)
                                  (org-glance-table--act-todo-bulk graph rows)
                                (let ((row (car rows)))
                                  (org-glance-table--act-todo graph (alist-get 'id row))))))
        (cons "refresh"     (lambda (_id _row) (org-glance-table--act-refresh)))
        (cons "overview"    (lambda (_id _row) (org-glance-overview:visit graph spec)))
        (cons "remove"      (lambda (id _row)
                              (org-glance-table--act-deltag graph id spec)))
        (cons "capture"     (lambda (_id _row)
                              (org-glance-capture (or (org-glance-filter:tags spec)
                                                      (org-glance-capture:completing-read-tag))
                                                  "")))
        (cons "relations" (lambda (id _row)
                            (unless id (user-error "Point is not on a row"))
                            (org-glance-table:visit-relations graph id)))
        (cons "tag"      (lambda (id _row) (org-glance-table--act-tag graph id)))
        (cons "crypt"    (lambda (id _row) (org-glance-table--act-crypt graph id)))
        (cons "history"  (lambda (id _row) (org-glance-table--act-history graph id)))
        (cons "edit"      (lambda (id _row) (org-glance-table--act-edit graph id)))
        (cons "duplicate" (lambda (id _row) (org-glance-table--act-duplicate graph id)))
        (cons "delete"   (lambda (id _row) (org-glance-table--act-delete graph id)))
        (cons "schedule" (lambda (id _row) (org-glance-table--act-planning graph id 'schedule)))
        (cons "deadline" (lambda (id _row) (org-glance-table--act-planning graph id 'deadline)))))

(cl-defun org-glance-table--visit-spec (graph spec &key saved ref-entry context)
  "The display spec for SPEC, its column set resolved by view kind.
A scoped REF-ENTRY replaces the whole set (built-ins minus its hidden, plus
its custom columns, its order); a scope-less relation view (CONTEXT) gets
plain defaults, never the shared untagged (\":none:\") per-tag schema.  Both
relation paths carry the extra `Relation' column
\(`org-glance-table--context-columns'); else SAVED's column order is restored."
  (let ((s (org-glance-table--spec graph spec)))
    (cond (context
           (let ((cols (org-glance-table--context-columns graph context)))
             (setf (alist-get 'columns s)
                   (if ref-entry
                       (org-glance-table--refs-columns graph ref-entry cols)
                     cols))))
          ((plist-get saved :columns)
           (setf (alist-get 'columns s)
                 (org-glance-table--reorder-columns
                  (alist-get 'columns s) (plist-get saved :columns)))))
    s))

(cl-defun org-glance-table:visit (graph &optional filter &key context)
  "Open GRAPH's table for FILTER, one buffer per filter description.
Honours the same filter language as the overview (see
`org-glance-filter:predicate').  CONTEXT marks a relation view
\(`:anchor' ID `:dir' `relations'): it enables the scoped layout -- restore on
open, `C-c C-c' to apply -- and the `Relation' column."
  (let* ((from-view (and org-glance-view--graph t))   ; re-navigation from within a view?
         (spec (org-glance-filter:normalize-spec filter))
         (saved (org-glance-table--config-get graph spec))   ; restored column order + sort
         (ref-entry (and context (org-glance-table--refs-resolve graph context)))
         ;; Bound BEFORE `keep?' and the badge split, so both read one done-set.
         (org-done-keywords
          (org-glance-tag-config:done-keywords-for-filter graph spec))
         (keep? (org-glance-filter:predicate spec))
         (buffer-name (format "*org-glance-table: %s*" (org-glance-filter:describe spec)))
         (src (org-glance-graph:headline-meta-path graph))
         (fill-fn (lambda (buf)
                    (with-current-buffer buf
                      (let ((rows (org-glance-table--rows graph keep?)))
                        (table-view-set-rows buf rows)
                        (org-glance-property-index--flush-if-dirty graph)
                        (org-glance-view:snapshot-mtime src)))))
         (buf (table-view-display
               buffer-name
               (org-glance-table--visit-spec graph spec :saved saved
                                            :ref-entry ref-entry
                                            :context context)
               (org-glance-table--handlers graph spec)
               fill-fn)))
    (with-current-buffer buf
      (setq org-glance-table--context context)
      (local-set-key (kbd "C-c C-c") #'org-glance-table:apply-layout)
      (setq org-glance-table--keep-fn keep?
            org-glance-table--spec spec
            default-directory (file-name-as-directory (org-glance-graph:directory graph)))
      (org-glance-view:register graph
                                :stale-fn  (lambda () (org-glance-view:stale-vs-file? src))
                                :reload-fn (lambda () (org-glance-table--reload (current-buffer))))
      (setq-local table-view-add-column-function #'org-glance-table--add-column-prompt)
      (local-set-key "/" #'org-glance-table:filter-or-reset)
      (local-set-key (kbd "!") (lookup-key (current-local-map) (kbd "j")))
      (local-set-key (kbd "C") #'org-glance-table:configure-tag)
      (local-set-key (kbd "C-c +") #'org-glance-table:add-column)
      (local-set-key (kbd "C-c -") #'org-glance-table:remove-column)
      (add-hook 'table-view-schema-changed-hook #'org-glance-table--persist-schema nil t)
      (if-let ((sort (or (plist-get ref-entry :sort) (plist-get saved :sort))))
          (table-view-set-sort sort)
        (table-view-apply-sort))
      (setq org-glance-table--config-snapshot (org-glance-table--current-config))
      (add-hook 'post-command-hook #'org-glance-table--persist-config nil t)
      (org-glance-view:fill-frame from-view))
    buf))

(cl-defun org-glance-table:visit-relations (graph id)
  "Open GRAPH's table of every headline related to ID, in both directions.
The entry point behind `@' (table, overview) and `C-c @' (material): rows are
ID's edge targets plus its referrers, each row's direction and kind in the
`Relation' column.  Relations read LAST-SAVED metadata, so save a material
buffer to see edges added in this session.  The bare relation filter passes,
keeping DONE headlines visible."
  (let ((related (org-glance-table--related-ids graph id)))
    (unless related
      (user-error "Headline has no relations (save after adding some)"))
    (org-glance-table:visit graph `(:id-any ,related)
                            :context (list :anchor id :dir 'relations))))

(cl-defun org-glance-table--ensure ()
  "Signal a `user-error' unless the current buffer is an org-glance table."
  (unless org-glance-view--graph
    (user-error "Not in an org-glance table")))

(cl-defun org-glance-table:add-column ()
  "Add a column to the table (`C-c +'), the mirror of `C-c -'.
Completing-read a drawer property or a relation kind the visible headlines
carry (`org-glance-table--add-column-prompt') and append it; the per-tag
schema records it like any other column change."
  (interactive)
  (org-glance-table--ensure)
  (call-interactively #'table-view-add-column))

(cl-defun org-glance-table:remove-column (&optional arg)
  "Remove a table column (`C-c -'), the one removal entry point.
The column at POINT; with ARG (`C-u C-c -'), or off a column, completing-read
which one.  Title stays mandatory (invariant 15): refused at point, never
offered.  The removal persists per tag, like the column `C-c +' adds."
  (interactive "P")
  (org-glance-table--ensure)
  (let ((at-point (unless arg (org-glance-view:column-at-point))))
    (when (org-glance-table--mandatory-column? at-point)
      (user-error "The Title column cannot be removed"))
    (let ((key (or at-point (org-glance-table--read-column))))
      (when key
        (table-view-remove-column key)
        (message "Removed column %s" key)))))

(cl-defun org-glance-table--read-column ()
  "Completing-read one of this view's REMOVABLE column keys, or nil.
Title is mandatory, so it is never a candidate (invariant 15)."
  (let ((candidates (cl-loop for c in (plist-get (table-view-layout) :columns)
                             for key = (alist-get 'key c)
                             unless (org-glance-table--mandatory-column? key)
                             collect (cons (or (alist-get 'header c) key) key))))
    (unless candidates
      (user-error "No removable columns in this view"))
    (let ((choice (completing-read "Remove column: " (mapcar #'car candidates) nil t)))
      (unless (string-empty-p choice)
        (cdr (assoc choice candidates))))))

(cl-defun org-glance-table:configure-tag ()
  "Configure this table's tag directly, skipping the tag prompt.
Bound to `C' -- the prompt-free counterpart to the transient's `C'.  Edit the
config of the sole tag this table filters on; the unfiltered or multi-tag view
names no single tag, so fall back to the tag prompt."
  (interactive)
  (org-glance-tag-config-edit (org-glance-filter:sole-tag org-glance-table--spec)))

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
