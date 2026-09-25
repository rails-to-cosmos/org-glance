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

;;; State colours

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
  "Return the foreground colour of FACE, or nil.
FACE is any `org-todo-keyword-faces' value: a face, colour string or plist."
  (cond ((stringp face) face)
        ((facep face) (face-foreground face nil t))
        ((listp face) (plist-get face :foreground))))

(cl-defun org-glance-table--state-color (state)
  "Return the badge colour for todo STATE.
Try `org-glance-table-state-colors', then STATE's `org-todo-keyword-faces'
foreground, then `org-glance-table-default-state-color'."
  (or (cdr (assoc state org-glance-table-state-colors))
      (org-glance-table--face-color (cdr (assoc state org-todo-keyword-faces)))
      org-glance-table-default-state-color))

(cl-defun org-glance-table--split-states (graph)
  "Split GRAPH's states into sorted (ACTIVE . DONE) lists.
Read `org-done-keywords', bound per tag by `org-glance-table:visit'."
  (let ((states (org-glance-graph:states graph))
        (done-kw (org-glance--done-keywords)))
    (cons (cl-remove-if     (lambda (s) (member s done-kw)) states)
          (cl-remove-if-not (lambda (s) (member s done-kw)) states))))

(cl-defun org-glance-table--state-badges (graph)
  "Return GRAPH's state badge palette, a list of ((value . S) (color . C)).
Active states come first, so the order doubles as the sort priority."
  (pcase-let ((`(,active . ,done) (org-glance-table--split-states graph)))
    (cl-loop for state in (append active done)
             collect `((value . ,state) (color . ,(org-glance-table--state-color state))))))

(cl-defun org-glance-table--colorize-state (state)
  "Return STATE propertized with its todo-state colour in bold."
  (propertize state 'face
              (list :foreground (org-glance-table--state-color state) :weight 'bold)))

(cl-defun org-glance-table--todo-line (graph)
  "Return a coloured `#+TODO:' line of GRAPH's states, or nil when it has none.
Active, then `|', then done; it is the table `subtitle', never hidden by `?'."
  (pcase-let ((`(,active . ,done) (org-glance-table--split-states graph)))
    (when (or active done)
      (concat "#+TODO: "
              (mapconcat #'org-glance-table--colorize-state active " ")
              (when done
                (concat (if active " " "") "| "
                        (mapconcat #'org-glance-table--colorize-state done " ")))))))

;;; Spec and rows

(cl-defun org-glance-table--base-columns (graph)
  "Return the built-in table columns for GRAPH, in default order.
The single source of the built-in key set (invariants 15-16)."
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
  "Non-nil when column KEY may never be removed or hidden (invariant 15)."
  (equal key "title"))

(cl-defun org-glance-table--spec (graph filter)
  "Build the `table-view' spec alist, without rows, for GRAPH under FILTER.
FILTER titles it and keys its per-tag columns; it sorts by state by default."
  `((title . ,(format "org-glance table: %s" (org-glance-filter:describe filter)))
    (subtitle . ,(org-glance-table--todo-line graph))
    (columns . ,(org-glance-table--apply-schema
                 graph filter (org-glance-table--base-columns graph)))
    (actions . (((key . "RET")     (command . "materialize") (label . "Materialize"))
                ((key . "j")       (command . "open")        (label . "Open link"))
                ((key . "o")       (command . "open")        (label . "Open link"))
                ((key . "e")       (command . "extract")   (label . "Extract"))
                ((key . "g")       (command . "refresh")   (label . "Refresh"))
                ((key . "O")       (command . "overview")  (label . "Overview"))
                ((key . "+")       (command . "capture")   (label . "Capture"))
                ((key . "@")       (command . "relations") (label . "Relations"))
                ((key . ":")       (command . "tag")       (label . "Tag"))
                ((key . "#")       (command . "crypt")     (label . "Crypt"))
                ((key . "l")       (command . "history")   (label . "Log"))
                ((key . "i")       (command . "edit")      (label . "Edit cell"))
                ((key . "C-c p")   (command . "duplicate") (label . "Copy"))
                ((key . "-")       (command . "remove")    (label . "Untag"))
                ((key . "C-c C-t") (command . "todo") (bulk . t) (label . "Todo"))
                ((key . "D")       (command . "delete")    (label . "Delete"))
                ((key . "C-c C-s") (command . "schedule") (label . "Schedule"))
                ((key . "s") (command . "schedule") (label . "Schedule"))
                ((key . "d") (command . "deadline") (label . "Deadline"))
                ((key . "C-c C-d") (command . "deadline") (label . "Deadline"))))
    (sort . ((column . "state") (ascending . t)))))

(cl-defun org-glance-table--interval-cell (range)
  "Return RANGE (FROM TO) as the sortable date cell `2021-12-18..2021-12-19'.
Return \"\" when RANGE is nil."
  (pcase range
    (`(,from ,to)
     (cl-flet ((day (ts) (if (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" ts)
                             (match-string 0 ts) ts)))
       (concat (day from) ".." (day to))))
    (_ "")))

(cl-defun org-glance-table--row (metadata)
  "Build a `table-view' row alist for headline METADATA, keyed by its id.
Cells are display strings: tags `:'-joined, priority its letter, absent \"\"."
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
  "Return rows for GRAPH's live headlines satisfying KEEP?, in graph order."
  (cl-loop for meta in (org-glance-graph:headlines graph)
           when (funcall keep? meta)
           collect (org-glance-table--row meta)))

(cl-defun org-glance-table--finish (id line fmt &rest args)
  "Update row ID, return point to it (else screen LINE), message FMT ARGS.
Upsert the row, or drop it once gone or out of the filter; with no
`org-glance-table--keep-fn', reload fully.  Keep the cell (invariant 24)."
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
  "Re-fill BUFFER from the graph, re-sort it, and restore point (invariant 24).
The `/' filter and narrowing survive."
  (when-let* ((buf (get-buffer buffer)))
    (with-current-buffer buf
      (pcase-let ((`(,id ,line ,col) (org-glance-view:point-context)))
        (table-view-refresh buf)
        (table-view-apply-sort)
        (org-glance-view:mark-fresh)
        (org-glance-view:restore-point id line col)))))

;;; Actions

(cl-defun org-glance-table--act-materialize (graph id)
  (when id (switch-to-buffer (org-glance-material:open graph id))))

(cl-defun org-glance-table--act-open (graph id)
  (when id (org-glance-material:open-link (org-glance-view:live-headline graph id))))

(cl-defun org-glance-table--act-extract (graph id)
  (when id (org-glance-material:extract-pairs (org-glance-property-index:body graph id))))

(cl-defun org-glance-table--act-todo (graph id)
  "Advance ID's TODO state in GRAPH like `C-c C-t', then update its row.
Delegate to `org-glance-material:change-todo-live'; any note commits first."
  (when id
    (let ((arg current-prefix-arg)          ; the dispatch lambda is a bare `interactive'
          (line (line-number-at-pos)))       ; the reload re-renders from the top
      (org-glance-material:change-todo-live
       graph id arg
       (lambda (state)
         (org-glance-table--finish id line "State: %s"
                                   (if (s-present? state) state "(none)")))))))

(cl-defun org-glance-table--read-state-native (graph filter)
  "Read a TODO state by org's fast selection, under FILTER's cycle in GRAPH.
The cycle falls back to the global keywords; return nil if the user clears it."
  (let ((cycle (org-glance-tag-config:cycle-for-filter graph filter)))
    (with-temp-buffer
      (let ((org-todo-keywords
             (org-glance-tag-config:cycle->keywords-or cycle org-todo-keywords)))
        (org-glance--org-mode))
      (org-fast-todo-selection))))

(cl-defun org-glance-table--act-todo-bulk (graph rows)
  "Set the marked ROWS of GRAPH to one TODO state, prompted once (`C-c C-t').
Delegate to `org-glance-material:set-todo-bulk' (no note; invariant 19), then
reload and unmark; a cleared state is a no-op.  With no marks, `C-c C-t' runs
`org-glance-table--act-todo'."
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

(cl-defun org-glance-table--require-row-id (id)
  "Return ID, or signal a `user-error' when point is outside a row."
  (or id (user-error "Point is not on a row")))

(cl-defun org-glance-table--act-tag (graph id)
  "Add a tag to headline ID in GRAPH (`:'), or remove one of its own (`C-u :').
Adding offers the tags it lacks, or a new one; removal requires a match."
  (setq id (org-glance-table--require-row-id id))
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
  "Toggle encryption of headline ID in GRAPH (`#'); `C-u #' re-keys it.
Prompt for passwords, confirming new ones; plaintext cannot be re-keyed."
  (setq id (org-glance-table--require-row-id id))
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

;;; Per-view persistence

(cl-defun org-glance-table--config-file (graph)
  "Path of GRAPH's table-view config store (may not exist)."
  (org-glance-graph:config-file graph "table-views.eld"))

(cl-defun org-glance-table--config-get (graph spec)
  "Return GRAPH's saved config plist for SPEC (`:columns', `:sort'), or nil."
  (org-glance--eld-alist-ref (org-glance-table--config-file graph)
                             (org-glance-filter:identity spec)))

(cl-defun org-glance-table--config-put (graph spec config)
  "Persist CONFIG (a plist) for SPEC in GRAPH's config store."
  (org-glance--eld-alist-set (org-glance-table--config-file graph)
                             (org-glance-filter:identity spec) config))

(cl-defun org-glance-table--column-keys (columns)
  "Return COLUMNS' key strings, in order."
  (mapcar (lambda (c) (alist-get 'key c)) columns))

(cl-defun org-glance-table--row-ids (rows)
  "Return the ids of ROWS, dropping id-less rows."
  (delq nil (mapcar (lambda (r) (alist-get 'id r)) rows)))

(cl-defun org-glance-table--reorder-columns (columns order)
  "Return COLUMNS in key ORDER, unlisted columns trailing in their own order."
  (append
   (delq nil (mapcar (lambda (k)
                       (cl-find k columns :test #'equal
                                :key (lambda (c) (alist-get 'key c))))
                     order))
   (cl-remove-if (lambda (c) (member (alist-get 'key c) order)) columns)))

(defvar-local org-glance-table--config-snapshot nil
  "Last persisted view config of this buffer, the change-detection baseline.
In a reference view, the last layout the modified-nudge reported instead.")

(defvar-local org-glance-table--keep-fn nil
  "This view's row predicate, built under the tag's done-set by the visit.
A single-row action asks it whether a changed headline still belongs.")

(defvar-local org-glance-table--context nil
  "Relation-view context plist (`:anchor' ID `:dir' `relations'), or nil.")

(cl-defun org-glance-table--current-config ()
  "Return this buffer's view config (:columns KEYS :sort SORT-KEYS).
Cheap enough for `post-command-hook'; see `org-glance-table--layout-snapshot'."
  (let ((layout (table-view-layout)))
    (list :columns (org-glance-table--column-keys (plist-get layout :columns))
          :sort (plist-get layout :sort))))

(cl-defun org-glance-table--persist-config ()
  "Persist a changed column order or sort; buffer-local `post-command-hook'.
Guarded on a registered view: the \"all\" filter has a nil spec.  A persistent
view saves on the spot; a transient one saves nothing (invariant
17); a reference view nudges toward `C-c C-c' once per change."
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

;;; Column schema (invariant 16)

(cl-defun org-glance-table--property-column (graph property &optional header)
  "Return a `table-view' column showing drawer PROPERTY from GRAPH per row.
PROPERTY is upcased; HEADER defaults to it capitalised."
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
  "Return a `table-view' column of each row's KIND-edge target titles in GRAPH.
Titles join with \", \", a gone target shows its id; HEADER defaults to KIND."
  `((key . ,(concat "kind:" kind))       ; own namespace: never collides with built-ins
    (header . ,(or header (s-capitalize (org-glance--kind-pretty kind))))
    (type . "text")
    (sortable . t)
    (align . "left")
    (prop . ,kind)
    (value-fn . ,(lambda (id _row)
                   (if-let* ((meta (org-glance-graph:live-meta graph id)))
                       (s-join ", "
                               (cl-loop for (target . k) in (org-glance-headline-metadata:relations meta)
                                        when (equal k kind)
                                        collect (org-glance-graph:title-or-id graph target)))
                     "")))))

(cl-defun org-glance-table--edges-between (graph from to)
  "Return the kinds of FROM's edges to TO in GRAPH, nil when there is no edge.
One element per edge; a kindless edge contributes nil."
  (when-let* ((meta (org-glance-graph:live-meta graph from)))
    (cl-loop for (target . kind) in (org-glance-headline-metadata:relations meta)
             when (equal target to) collect kind)))

(cl-defun org-glance-table--relation-cell (graph anchor id)
  "Return ID's relation to ANCHOR in GRAPH as a display string.
`> KIND' or `< KIND' when ANCHOR refers to ID or ID to ANCHOR; joined by `, '."
  (cl-flet ((edges (arrow kinds)
              (mapcar (lambda (kind)
                        (if kind
                            (concat arrow " " (org-glance--kind-pretty kind))
                          arrow))
                      kinds)))
    (s-join ", " (append (edges ">" (org-glance-table--edges-between graph anchor id))
                         (edges "<" (org-glance-table--edges-between graph id anchor))))))

(cl-defun org-glance-table--relation-column (graph anchor)
  "Return a column showing each row's relation to ANCHOR in GRAPH.
Relation tables only; see `org-glance-table--relation-cell'."
  `((key . "relation")
    (header . "Relation")
    (type . "text")
    (sortable . t)
    (align . "left")
    (value-fn . ,(lambda (id _row)
                   (org-glance-table--relation-cell graph anchor id)))))

(cl-defun org-glance-table--context-columns (graph context)
  "Return GRAPH's base columns plus `Relation' for relation view CONTEXT."
  (append (org-glance-table--base-columns graph)
          (list (org-glance-table--relation-column
                 graph (plist-get context :anchor)))))

(cl-defun org-glance-table--related-ids (graph id)
  "Return the distinct ids related to ID in GRAPH: its targets, then referrers."
  (delete-dups
   (append (when-let* ((meta (org-glance-graph:live-meta graph id)))
             (org-glance-headline-metadata:relation-targets meta))
           (mapcar #'org-glance-headline-metadata:id
                   (cl-remove-if-not (org-glance-filter:predicate `(:refers-to ,id))
                                     (org-glance-graph--metas graph))))))

(cl-defun org-glance-table--property-key? (key)
  "Non-nil when KEY names a drawer-property column: the all-UPCASE tag.
Edge columns carry a `kind:' prefix, built-ins are lowercase."
  (string= key (upcase key)))

(cl-defun org-glance-table--custom-column (graph name &optional header)
  "Build GRAPH's custom column NAME, titled HEADER; NAME's case is its type.
All-upcase NAME is a property, else a kind slug; both persist via `prop'.
Case decides, never a graph scan, whose answer flips when a kind's last edge
goes -- so property \"AUTHOR\" and kind \"author\" coexist."
  (if (org-glance-table--property-key? name)
      (org-glance-table--property-column graph name header)
    (org-glance-table--edge-column graph name header)))

(cl-defun org-glance-table--add-column-prompt ()
  "Return a column for a property or relation kind this view's rows carry.
Empty input returns nil; `C-c +' calls it as `table-view-add-column-function'."
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
  "Return FILTER's schema key: its sorted tags `+'-joined, or \":none:\".
Keying on tags alone shares a tag's columns across all its views."
  (let ((tags (sort (mapcar #'symbol-name (org-glance-filter:tags filter)) #'string<)))
    (if tags (s-join "+" tags) ":none:")))

(cl-defun org-glance-table--schema-entry (graph filter)
  "Return GRAPH's saved schema plist for FILTER's tags, or nil.
`:columns' lists custom (PROP . HEADER) pairs, `:hidden' removed built-ins."
  (org-glance--eld-alist-ref (org-glance-table--schema-file graph)
                             (org-glance-table--schema-key filter)))

(cl-defun org-glance-table--schema-put (graph filter &key columns hidden)
  "Persist FILTER's per-tag schema in GRAPH: custom COLUMNS and HIDDEN keys.
COLUMNS is a (PROP . HEADER) list; an all-empty schema drops the entry."
  (org-glance--eld-alist-set
   (org-glance-table--schema-file graph)
   (org-glance-table--schema-key filter)
   (and (or columns hidden) (list :columns columns :hidden hidden))))

(cl-defun org-glance-table--compose-columns (graph base hidden pairs)
  "Return BASE minus HIDDEN keys, plus GRAPH's custom columns for PAIRS.
PAIRS is a (NAME . HEADER) list; Title is never hidden (invariant 15)."
  (let ((hidden (cl-remove-if #'org-glance-table--mandatory-column? hidden)))
    (append (cl-remove-if (lambda (c) (member (alist-get 'key c) hidden)) base)
            (mapcar (lambda (pair)
                      (org-glance-table--custom-column graph (car pair) (cdr pair)))
                    pairs))))

(cl-defun org-glance-table--apply-schema (graph filter columns)
  "Return COLUMNS under GRAPH's per-tag schema for FILTER, unchanged if none."
  (let ((entry (org-glance-table--schema-entry graph filter)))   ; one eld read
    (org-glance-table--compose-columns graph columns
                                       (plist-get entry :hidden)
                                       (plist-get entry :columns))))

(cl-defun org-glance-table--persist-schema ()
  "Save this view's custom and hidden columns under its tags (invariant 16).
Runs on `table-view-schema-changed-hook'; transient views skip (invariant 17)."
  (when (and org-glance-view--graph
             (not (org-glance-filter:transient? org-glance-table--spec)))
    (let ((snap (org-glance-table--layout-snapshot)))
      (with-demoted-errors "org-glance: table schema save failed: %S"
        (org-glance-table--schema-put org-glance-view--graph org-glance-table--spec
                                      :columns (plist-get snap :columns)
                                      :hidden (plist-get snap :hidden))))))

;;; Scoped reference layouts (invariants 17-18)

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
  "Return CONTEXT's (ANCHOR-TAGS . ROW-TAGS) in GRAPH.
Each is a sorted list of distinct downcased strings."
  (let* ((anchor (plist-get context :anchor))
         (meta (org-glance-graph:live-meta graph anchor))
         (row-metas (org-glance-graph--metas
                     graph (org-glance-table--related-ids graph anchor))))
    (cons (org-glance--sorted-distinct
           (and meta (org-glance-headline-metadata:tag-strings meta)))
          (org-glance--sorted-distinct
           (cl-loop for m in row-metas append (org-glance-headline-metadata:tag-strings m))))))

(cl-defun org-glance-table--refs-tag-pairs (anchor-tags row-tags)
  "Return the tag pairs ANCHOR-TAGS x ROW-TAGS as (FROM . TO) (invariant 18).
The single source of pair order: anchor tag first."
  (cl-loop for a in anchor-tags append
           (cl-loop for r in row-tags collect (cons a r))))

(cl-defun org-glance-table--refs-resolve (graph context)
  "Return GRAPH's scoped layout entry for CONTEXT, or nil (invariant 18).
The anchor's own entry wins, else the latest `:applied' matching tag pair."
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
  "Return BASE filtered, extended and ordered per GRAPH's scoped ENTRY."
  (org-glance-table--reorder-columns
   (org-glance-table--compose-columns graph base (plist-get entry :hidden)
                                      (plist-get entry :columns))
   (plist-get entry :order)))

(cl-defun org-glance-table--layout-snapshot ()
  "Return this buffer's full layout as one persistable plist, unstamped.
`:columns' custom (PROP . HEADER) pairs, `:hidden' built-in keys missing from
the view (`Relation' counts in a relation view), `:order' every live key,
`:sort' the sort chain.  Scoped entries store it whole."
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
A reference view prompts for a scope, its anchor or a tag pair (invariant 18); a
persistent view saves config and schema now; other views signal `user-error'."
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
  "Delete headline ID from GRAPH (`D') after a referrer-aware confirmation."
  (setq id (org-glance-table--require-row-id id))
  (let ((line (line-number-at-pos)))
    (when (org-glance-material:delete graph id)
      (org-glance-table--finish id line "Headline deleted"))))

(cl-defun org-glance-table--act-planning (graph id kind)
  "Set, or with `C-u' clear, KIND planning of headline ID in GRAPH (`s', `d')."
  (setq id (org-glance-table--require-row-id id))
  (let ((line (line-number-at-pos))
        (remove current-prefix-arg))
    (org-glance-material:set-planning graph id kind remove)
    (org-glance-table--finish id line "%s %s" (capitalize (symbol-name kind))
                              (if remove "cleared" "set"))))

(cl-defun org-glance-table--act-duplicate (graph id)
  "Copy headline ID in GRAPH under a fresh id (`C-c p')."
  (setq id (org-glance-table--require-row-id id))
  (let ((line (line-number-at-pos))
        (new (org-glance-material:duplicate graph id)))
    (org-glance-table--finish new line "Headline copied")))

(cl-defun org-glance-table--act-edit (graph id)
  "Edit the cell at point of headline ID in GRAPH (`i').
State, tags and planning reuse `C-c C-t', `:' (`C-u' removes) and
`org-read-date'; title, priority and property cells prompt pre-filled; derived
columns signal `user-error'."
  (setq id (org-glance-table--require-row-id id))
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
  "Open one of ID's occurrence snapshots in GRAPH read-only (`l')."
  (setq id (org-glance-table--require-row-id id))
  (org-glance-view:pick-occurrence graph id))

(cl-defun org-glance-table--act-deltag (graph id spec)
  "Untag headline ID in GRAPH from SPEC's tag after confirmation (`-').
The headline leaves the view; several tags prompt for one; unsaved edits abort."
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
  "Filter or narrow the table (`/'); `C-u /' clears the substring filter.
Without a prefix, run `table-view-filter-or-narrow'."
  (interactive)
  (if current-prefix-arg
      (table-view-filter "")
    (call-interactively #'table-view-filter-or-narrow)))

(cl-defun org-glance-table--act-refresh ()
  "Clear the `/' filter and narrowing, then re-fill from the graph (`g').
Marks survive."
  (setq table-view--filter nil
        table-view--narrowed nil)
  (org-glance-table--reload (current-buffer)))

(cl-defun org-glance-table--handlers (graph spec)
  "Return the action-command handler alist for GRAPH's table under SPEC."
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
                            (org-glance-table:visit-relations
                             graph (org-glance-table--require-row-id id))))
        (cons "tag"      (lambda (id _row) (org-glance-table--act-tag graph id)))
        (cons "crypt"    (lambda (id _row) (org-glance-table--act-crypt graph id)))
        (cons "history"  (lambda (id _row) (org-glance-table--act-history graph id)))
        (cons "edit"      (lambda (id _row) (org-glance-table--act-edit graph id)))
        (cons "duplicate" (lambda (id _row) (org-glance-table--act-duplicate graph id)))
        (cons "delete"   (lambda (id _row) (org-glance-table--act-delete graph id)))
        (cons "schedule" (lambda (id _row) (org-glance-table--act-planning graph id 'schedule)))
        (cons "deadline" (lambda (id _row) (org-glance-table--act-planning graph id 'deadline)))))

(cl-defun org-glance-table--visit-spec (graph spec &key saved ref-entry context)
  "Return GRAPH's display spec for SPEC, its columns resolved by view kind.
A relation view (CONTEXT) takes REF-ENTRY's whole set, else defaults, both with
`Relation' (invariant 18); any other view restores SAVED's column order."
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
CONTEXT, a (:anchor ID :dir relations) plist, makes it a relation view with the
scoped layout (invariant 18) and the `Relation' column."
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
      (if-let* ((sort (or (plist-get ref-entry :sort) (plist-get saved :sort))))
          (table-view-set-sort sort)
        (table-view-apply-sort))
      (setq org-glance-table--config-snapshot (org-glance-table--current-config))
      (add-hook 'post-command-hook #'org-glance-table--persist-config nil t)
      (org-glance-view:fill-frame from-view))
    buf))

(cl-defun org-glance-table:visit-relations (graph id)
  "Open GRAPH's table of every headline related to ID, in both directions.
Bound to `@' (table, overview) and `C-c @' (material).  Edges come from
last-saved metadata; DONE headlines stay visible."
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
  "Add a property or relation-kind column to the table (`C-c +').
Prompt with `org-glance-table--add-column-prompt'; it persists per tag."
  (interactive)
  (org-glance-table--ensure)
  (call-interactively #'table-view-add-column))

(cl-defun org-glance-table:remove-column (&optional arg)
  "Remove the table column at point (`C-c -'); the removal persists per tag.
With ARG (`C-u C-c -') or off a column, prompt; Title stays (invariant 15)."
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
  "Completing-read a removable column key of this view, or nil (invariant 15)."
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
  "Configure this table's sole filter tag without a prompt (`C').
An unfiltered or multi-tag view falls back to the tag prompt."
  (interactive)
  (org-glance-tag-config-edit (org-glance-filter:sole-tag org-glance-table--spec)))

;;;###autoload
(cl-defun org-glance-table (&optional tag)
  "Browse the graph as a sortable, badge-coloured table filtered by TAG.
Interactively, prompt for TAG (empty for none), overlaid on
`org-glance-filter-spec' like `org-glance-overview'.  `^' sorts by the column
at point (repeat toggles, `C-u ^' adds a tie-breaker); RET, j and e act on the
row at point; `m' marks it."
  (interactive (list (org-glance-view:completing-read-tag "Table tag (empty for all): ")))
  (org-glance-ensure-init)
  (org-glance-table:visit org-glance-graph
                          (org-glance-filter:merge org-glance-filter-spec tag)))

(provide 'org-glance-table)
;;; org-glance-table.el ends here
