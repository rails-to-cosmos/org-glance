;;; table-view.el --- Declarative, backend-agnostic table view -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@gmail.com>
;; Maintainer: Dmitry Akatov <akatovda@gmail.com>
;; URL: https://github.com/rails-to-cosmos/table-view
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, data, tools
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;;; Commentary:
;; A tiny, backend-agnostic core that renders a declarative table
;; description -- columns, actions, default sort -- and dispatches keys to
;; consumer-registered command handlers.
;;
;; Responsibilities:
;;   * render a spec as an aligned, org-table-styled read-only view
;;   * colour `badge' cells from the column's declared palette
;;   * build a keymap from the declared actions, dispatch by command name
;;   * own a row store keyed by id: set-rows / upsert-row / delete-row
;;   * client-side sort on sortable columns
;;   * interactive substring filter (/)
;;   * mark rows (m) and run `bulk' actions on the marked set
;;   * optional server-side pagination via a `page-fn': one page in memory,
;;     sort and filter pushed down, marks and bulk that span pages
;;
;; A consumer provides: a parsed spec, a handler alist (command-name -> FN of
;; ID ROW), and either a `fill-fn' (BUFFER -> populates all rows via the
;; mutators below) or, for server-side pagination, a `page-fn' (REQUEST ->
;; fetches one page and delivers it with `table-view-set-page').
;;
;; See examples/ for runnable demos:
;;   minimal.el       — inline rows from a JSON spec
;;   fill-function.el — populate via a fill function (Emacs subprocesses)
;;   upsert.el        — streaming row updates via a timer
;;   multi-sort.el    — column navigation + multi-column (C-u ^) sorting
;;   sort-methods.el  — per-column sort methods (values / compare) + default sort
;;   delete.el        — row deletion gated on a custom pre-delete step
;;   bulk.el          — marking (m), narrowing (/), and bulk actions (bulk: t)
;;   paginate.el      — server-side pagination over a fake backend (page-fn)
;;   org-links.el     — Org links in cells, followed by C-c C-o or mouse
;;
;; Keybindings in table-view-mode:
;;   g   — clear filter/narrow & refresh, preserving the sort order
;;   ^   — sort by the column at point, repeat toggles asc/desc;
;;         off a column, cycle through every column and direction
;;   C-u ^ — add the column at point as a secondary (tie-breaker) sort key;
;;           a following run of `^' then toggles that key's direction
;;   m   — toggle mark on the current row;  u — unmark it;  U — unmark all
;;   /   — narrow to the marked rows, or filter by substring when none marked
;;   n/p — next/previous data row (stops on the last / first row)
;;   f/b — forward/backward: by column on a table line (header or row),
;;         by char elsewhere
;;   C-c C-o — follow the Org link at point (cells may hold [[TARGET][DESC]]);
;;             links are also mouse-clickable
;;   M-left/M-right — move the column at point left/right (org-table style)
;;   > / . — next page, < / , — previous page (paged buffers)
;;   M-> / M-< — last / first page;  M-g — go to page (offset paging)
;;   q   — quit

;;; Code:

(require 'cl-lib)
(require 'subr-x)                        ; when-let, string-empty-p
(require 'org)                           ; org-link-bracket-re, org-link-open-from-string

;;; Buffer-local state

(defvar-local table-view--spec nil
  "Parsed table spec (alist) for this buffer.")
(defvar-local table-view--rows nil
  "Current rows (list of alists), in insertion order before sorting.")
(defvar-local table-view--handlers nil
  "Alist of command-name (string) -> function called as (FN ID ROW).")
(defvar-local table-view--fill-fn nil
  "Function of one arg (this BUFFER) that (re)populates the rows, or nil.")
(defvar-local table-view--sort-keys nil
  "Sort chain: list of (KEY . ASC) conses, highest priority first.
KEY is a column key (string); ASC is non-nil for ascending.  Rows equal
on one key are ordered by the next; an empty list renders load order.")
(defvar-local table-view--sorted nil
  "Non-nil once an explicit sort has been applied to the current row set.
Reset whenever the full row set is replaced (`table-view-set-rows'), so
the hint line never claims a sort the displayed rows are not actually in.")
(defvar-local table-view--filter nil
  "Current filter string, or nil.  When set, only rows with at least one
cell matching the string (case-insensitive substring) are rendered.")
(defvar-local table-view--marks nil
  "List of marked row ids.")
(defvar-local table-view--narrowed nil
  "Non-nil when the view is narrowed to the marked rows.")
(defvar-local table-view--mark-cache nil
  "Alist of id -> ROW for every marked row.
In paged mode a marked row leaves `table-view--rows' when you turn the
page, so its payload is cached here; this is what lets marks, narrow,
and bulk span pages.")

;;; Pagination state
;;
;; When a `page-fn' is supplied, `table-view--rows' holds only the CURRENT
;; page fetched from the consumer's backend; sort and filter are pushed down
;; into each page request instead of running client-side.  A buffer with no
;; page-fn leaves all of this nil/0 and behaves exactly as before.

(defvar-local table-view--page-fn nil
  "Consumer fetcher, or nil.  Called as (FN REQUEST); REQUEST is a plist
\(see `table-view--build-request') and the fetcher delivers the page with
`table-view-set-page' (or `table-view-page-error' on failure).")
(defvar-local table-view--page-size nil
  "Rows per page in paged mode.")
(defvar-local table-view--strategy 'offset
  "Paging strategy: `offset' (OFFSET/LIMIT, random access + totals) or
`keyset' (opaque cursors, DB-stable, forward/back only).")
(defvar-local table-view--offset 0
  "Row offset of the current page (offset strategy).")
(defvar-local table-view--page-index 0
  "Current 0-based page number, for the indicator.")
(defvar-local table-view--total nil
  "Server's total matching-row count under the active filter, or nil when
the backend does not report one.")
(defvar-local table-view--has-next nil
  "Non-nil when another page follows the current one.")
(defvar-local table-view--page-cursor nil
  "Cursor that produced the current page (keyset strategy).")
(defvar-local table-view--next-cursor nil
  "Cursor for the following page, or nil at the end (keyset strategy).")
(defvar-local table-view--prev-cursor nil
  "Cursor for the preceding page, or nil at the start (keyset strategy).")
(defvar-local table-view--page-loading nil
  "Non-nil while a page fetch is in flight.")
(defvar-local table-view--page-error nil
  "Message from the last failed fetch, or nil.")
(defvar-local table-view--pending nil
  "Identity of the in-flight page request (a plist), committed by
`table-view-set-page' and discarded by `table-view-page-error', so a
failed fetch never advances the visible page position.")

(defun table-view--paged-p ()
  "Non-nil when this buffer fetches rows a page at a time via a `page-fn'."
  (and table-view--page-fn t))

(defun table-view--offset-p ()
  "Non-nil when the paging strategy is offset-based (not keyset)."
  (eq table-view--strategy 'offset))

;;; Spec accessors

(defun table-view--columns (spec) (alist-get 'columns spec))
(defun table-view--actions (spec) (alist-get 'actions spec))

(defun table-view--column (spec key)
  "Return the column alist whose key is KEY in SPEC."
  (seq-find (lambda (c) (equal (alist-get 'key c) key))
            (table-view--columns spec)))

(defun table-view--own-spec (spec)
  "Return SPEC with a private, reorderable copy of its `columns' list.
Column moves reorder the buffer's own copy, never a spec shared between
buffers."
  (cons (cons 'columns (copy-sequence (alist-get 'columns spec)))
        spec))

(defun table-view--cell (row key)
  "Raw value of cell KEY (a string column key) in ROW."
  (alist-get (intern key) (alist-get 'cells row)))

(defun table-view--str (val)
  "Render cell VAL as a display string."
  (cond ((null val) "")
        ((stringp val) val)
        ((numberp val) (number-to-string val))
        (t (format "%s" val))))

(defun table-view--badge-color (col value)
  "Return the colour declared for VALUE in badge column COL, or nil."
  (alist-get 'color
             (seq-find (lambda (b) (equal (alist-get 'value b) value))
                       (alist-get 'badges col))))

;;; Org links
;;
;; A cell whose display string contains Org bracket links -- [[TARGET][DESC]]
;; or [[TARGET]] -- renders each link as its DESC (or TARGET), followable by
;; mouse or `C-c C-o'.  Parsing and following reuse Org's own `ol.el' (the
;; `org-link-bracket-re' regexp and `org-link-open-from-string'), rather than
;; redefining them.  Width, filtering, and sorting see the DESC, so a link
;; column lines up and searches by what is on screen, not the raw markup.

(defface table-view-link '((t :inherit link))
  "Face for Org links rendered in table cells.")

(defvar table-view-render-links t
  "When non-nil, cells render Org bracket links ([[TARGET][DESC]]) as
followable links showing DESC.  Set to nil to show cells verbatim.")

(defvar table-view-open-link-function #'table-view-open-link-default
  "Function of one argument, an Org-link TARGET string, that opens it.
Rebind to control what following a link does.")

(defun table-view-open-link-default (target)
  "Open Org-link TARGET.
Web and mail links go through `browse-url'; every other kind is handed to
Org (`org-link-open-from-string'), so `file:', `id:', and custom
`org-link-parameters' links open correctly."
  (if (string-match-p "\\`\\(?:https?\\|ftp\\|mailto\\):" target)
      (browse-url target)
    (org-link-open-from-string (format "[[%s]]" target))))

(defun table-view-open-link (&optional event)
  "Follow the table-view link at point, or the one clicked (EVENT).
Bound to `C-c C-o' and to mouse clicks on rendered link text."
  (interactive (list last-nonmenu-event))
  (if-let ((pos (if (mouse-event-p event) (posn-point (event-end event)) (point)))
           (target (get-text-property pos 'table-view-link)))
      (funcall table-view-open-link-function target)
    (message "No link at point")))

(defvar table-view--link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'table-view-open-link)
    (define-key map [mouse-2] #'table-view-open-link)
    map)
  "Keymap on rendered link text; a mouse click follows the link.")

(defun table-view--map-links (s fn)
  "Return S with each Org bracket link replaced by (FN TARGET DESC).
Matches with Org's own `org-link-bracket-re'; DESC falls back to TARGET for a
link written without one ([[t]]).  Returns S unchanged when link rendering is
off or S has no link."
  (if (or (not table-view-render-links) (not (string-search "[[" s)))
      s
    (replace-regexp-in-string
     org-link-bracket-re
     (lambda (m)
       (funcall fn (match-string 1 m) (or (match-string 2 m) (match-string 1 m))))
     s t t)))                            ; FIXEDCASE + LITERAL: keep target/props verbatim

(defun table-view--link-string (target desc)
  "Propertize DESC as a followable link to TARGET."
  (propertize desc
              'face 'table-view-link
              'mouse-face 'highlight
              'help-echo (concat "mouse-1, C-c C-o: open " target)
              'table-view-link target
              'keymap table-view--link-keymap
              'follow-link t))

(defun table-view--linkify (s)
  "Return S with Org links rendered as propertized, followable descriptions."
  (table-view--map-links s #'table-view--link-string))

(defun table-view--delink (s)
  "Return S with Org links reduced to plain descriptions (no properties).
Used for width, filtering, and sorting, so they see the displayed text."
  (table-view--map-links s (lambda (_target desc) desc)))

(defun table-view--links-p (col)
  "Non-nil when column COL renders Org links.
Badge columns show their values verbatim (a badge is a categorical label,
not a link), so links are handled for every OTHER column type."
  (not (equal (alist-get 'type col) "badge")))

(defun table-view--cell-text (col val)
  "Plain displayed text of VAL in column COL.
Org links are reduced to their descriptions for link-rendering columns, so
that width and filtering see exactly what is on screen; badge values are
returned verbatim.  This keeps measurement, filtering, and display aligned."
  (let ((s (table-view--str val)))
    (if (table-view--links-p col) (table-view--delink s) s)))

(defun table-view--sort-value (col val)
  "VAL prepared for comparison in column COL.
A string uses its displayed text (`table-view--cell-text', so an Org-link
column sorts by its descriptions); numbers and other non-strings pass through
unchanged for numeric/custom comparators.  Applied once, before the
comparator, so every comparator kind agrees with the display."
  (if (stringp val) (table-view--cell-text col val) val))

(defun table-view--compute-cells (rows spec)
  "Return ROWS with SPEC's computed columns' cells materialised.
A column may declare a `value-fn': a function of (ID ROW) returning that
column's cell value.  For every such column, each row that lacks a cell for it
gets one filled in by calling the function; a row that already carries the cell
is left untouched, so a backend that supplies the value directly always wins.
The computed value is stored on the row like any other cell, so sorting,
filtering and width computation see it -- a computed column is otherwise a
first-class column.  Non-destructive: unchanged rows are returned as-is and
changed rows get a fresh `cells' alist, so the caller's ROWS are never mutated."
  (let ((vcols (seq-filter (lambda (c) (alist-get 'value-fn c))
                           (table-view--columns spec))))
    (if (null vcols)
        rows
      (mapcar
       (lambda (row)
         (let ((cells (alist-get 'cells row))
               (id (alist-get 'id row))
               (added nil))
           (dolist (col vcols)
             (let ((sym (intern (alist-get 'key col))))
               (unless (assq sym cells)
                 (push (cons sym (funcall (alist-get 'value-fn col) id row)) added))))
           (if (null added)
               row
             (cons (cons 'cells (append added cells))
                   (seq-remove (lambda (kv) (eq (car kv) 'cells)) row)))))
       rows))))

(defun table-view--strip-cell (rows key)
  "Return ROWS with any cell for column KEY removed.
Non-destructive; a row without that cell is returned as-is.  Used to drop a
stale computed cell so a (re)added `value-fn' column recomputes it instead of
`table-view--compute-cells' finding the old value present and skipping it."
  (let ((sym (intern key)))
    (mapcar (lambda (row)
              (let ((cells (alist-get 'cells row)))
                (if (assq sym cells)
                    (cons (cons 'cells (assq-delete-all sym (copy-alist cells)))
                          (seq-remove (lambda (kv) (eq (car kv) 'cells)) row))
                  row)))
            rows)))

(defun table-view--strip-cell-everywhere (key)
  "Drop column KEY's cell from `table-view--rows' and the paged mark-cache."
  (setq table-view--rows (table-view--strip-cell table-view--rows key)
        table-view--mark-cache
        (mapcar (lambda (c) (cons (car c) (car (table-view--strip-cell (list (cdr c)) key))))
                table-view--mark-cache)))

(defun table-view--materialise-cells ()
  "Fill `value-fn' columns' cells in `table-view--rows' AND the mark-cache.
The mark-cache holds row snapshots a paged/narrowed view renders from, so a
column added at runtime must be materialised there too, not just in the rows."
  (setq table-view--rows (table-view--compute-cells table-view--rows table-view--spec)
        table-view--mark-cache
        (mapcar (lambda (c)
                  (cons (car c) (car (table-view--compute-cells (list (cdr c)) table-view--spec))))
                table-view--mark-cache)))

;;; Sorting

(defvar table-view-comparators nil
  "Alist of NAME (string) -> less-than predicate for a column's `compare'.
Each predicate takes two raw cell values and returns non-nil when the
first sorts before the second.  A column's `compare' may name one of
these, name a built-in (\"number\", \"string\", \"natural\"), or hold a
predicate function directly.  Consumers extend this with `push'.")

(defun table-view--as-number (val)
  "VAL as a number: numbers as-is, strings via `string-to-number', else 0."
  (cond ((numberp val) val)
        ((stringp val) (string-to-number val))
        (t 0)))

(defun table-view--number-lessp (a b)
  "Numeric order of A and B, coercing number-strings and nil."
  (< (table-view--as-number a) (table-view--as-number b)))

(defun table-view--natural-lessp (a b)
  "Number-aware string order of A and B, so \"2\" < \"10\" and \"x2\" < \"x10\"."
  (string-version-lessp (table-view--str a) (table-view--str b)))

(defun table-view--string-lessp (a b)
  "Lexicographic order of A and B as display strings."
  (string< (table-view--str a) (table-view--str b)))

(defun table-view--value-order (col)
  "Ordered list of column COL's declared values, or nil.
Taken from `values' when present, else the badge palette order (so
badge columns keep sorting by palette order).  `values' is a plain
ordered list of expected values; colours stay in `badges'."
  (or (alist-get 'values col)
      (and (equal (alist-get 'type col) "badge")
           (mapcar (lambda (b) (alist-get 'value b)) (alist-get 'badges col)))))

(defun table-view--categorical-lessp (order)
  "Return a predicate ordering values by position in ORDER, unlisted last.
Values are matched as display strings, so numeric cells compare against
string entries in ORDER."
  (let* ((keys (mapcar #'table-view--str order))
         (n (length keys)))
    (lambda (a b)
      (< (or (cl-position (table-view--str a) keys :test #'equal) n)
         (or (cl-position (table-view--str b) keys :test #'equal) n)))))

(defun table-view--comparator (col)
  "Return a less-than predicate over raw cell values for column COL.
Resolution order:
  1. an explicit `compare' -- a predicate function, a built-in name
     (\"number\"/\"numeric\", \"string\"/\"lexicographic\",
     \"natural\"/\"version\"), or a name in `table-view-comparators';
  2. else an ordered `values'/badge domain (categorical, unlisted last);
  3. else `type' \"number\" (numeric);
  4. else lexicographic."
  (let ((compare (alist-get 'compare col))
        (order (table-view--value-order col)))
    (cond
     ((functionp compare) compare)
     ((member compare '("number" "numeric")) #'table-view--number-lessp)
     ((member compare '("string" "lexicographic")) #'table-view--string-lessp)
     ((member compare '("natural" "version")) #'table-view--natural-lessp)
     ((and (stringp compare) (cdr (assoc compare table-view-comparators)))
      (cdr (assoc compare table-view-comparators)))
     (order (table-view--categorical-lessp order))
     ((equal (alist-get 'type col) "number") #'table-view--number-lessp)
     (t #'table-view--string-lessp))))

(defun table-view--sort-rows ()
  "Sort `table-view--rows' in place by the current sort chain.
No-op when the chain is empty.  Rows equal on every key keep their
relative order (the sort is stable).  Sorting is explicit: it runs only
from the sort commands (`^' and `g' when a sort is already active),
never from row updates, so operating on a row updates it in place
without moving it."
  (when table-view--sort-keys
    ;; In paged mode ordering is the server's job (the loaded page is only a
    ;; slice); re-sorting it here would reorder just those rows and lie about
    ;; the global order.  The chain still counts as an active sort.
    (unless (table-view--paged-p)
      (let ((tests (mapcar
                    (lambda (ka)
                      (let ((col (table-view--column table-view--spec (car ka))))
                        (list (table-view--comparator col)
                              (car ka)          ; column key
                              (cdr ka)          ; ascending?
                              col)))            ; column (for link-aware value prep)
                    table-view--sort-keys)))
        (setq table-view--rows
              (sort (copy-sequence table-view--rows)
                    (lambda (a b)
                      (cl-loop for (less key asc col) in tests
                               for va = (table-view--sort-value col (table-view--cell a key))
                               for vb = (table-view--sort-value col (table-view--cell b key))
                               do (cond ((funcall less va vb) (cl-return asc))
                                        ((funcall less vb va) (cl-return (not asc))))
                               finally return nil))))))
    (setq table-view--sorted t)))

(defun table-view--sort-description ()
  "Render the sort chain as e.g. \"name asc -> pid desc\"."
  (mapconcat (lambda (ka)
               (format "%s %s" (car ka) (if (cdr ka) "asc" "desc")))
             table-view--sort-keys " -> "))

(defun table-view--parse-sort (sort)
  "Return the default sort chain declared by a spec's SORT value.
SORT is either a single {column, ascending} alist or a list of them for
a multi-column default; the result is a list of (KEY . ASC), highest
priority first.  A missing `ascending' defaults to ascending."
  (let ((specs (if (alist-get 'column sort)
                   (list sort)          ; single {column, ascending}
                 sort)))                ; list of them, or nil
    (delq nil
          (mapcar (lambda (s)
                    (when-let ((col (alist-get 'column s)))
                      (cons col (if (assq 'ascending s)
                                    (and (alist-get 'ascending s) t)
                                  t))))
                  specs))))

;;; Filtering

(defun table-view--row-matches-p (row filter)
  "Non-nil if any cell in ROW contains FILTER (case-insensitive substring)."
  (let ((pat (downcase filter)))
    (cl-some (lambda (col)
               (string-match-p
                (regexp-quote pat)
                (downcase (table-view--cell-text
                           col (table-view--cell row (alist-get 'key col))))))
             (table-view--columns table-view--spec))))

(defun table-view--marked-p (id)
  "Non-nil when the row with ID is marked."
  (and (member id table-view--marks) t))

(defun table-view--marks-active-p ()
  "Non-nil when any row is marked, so the mark column is shown."
  (and table-view--marks t))

(defun table-view--prune-marks ()
  "Discard the mark of any row missing from `table-view--rows'.
Also widen the view when nothing stays marked.  In paged mode a marked
row legitimately leaves the loaded page on a page turn, so marks are
never pruned by absence there -- only cleared explicitly (`U' / `g')."
  (unless (table-view--paged-p)
    (let ((ids (mapcar (lambda (r) (alist-get 'id r)) table-view--rows)))
      (setq table-view--marks
            (cl-remove-if-not (lambda (m) (member m ids)) table-view--marks))))
  (unless table-view--marks (setq table-view--narrowed nil)))

(defun table-view--visible-rows ()
  "Rows to render.
Paged, narrowed: the cached marked rows (they span pages).  Paged, not
narrowed: the fetched page as-is (the server already sorted and filtered
it).  Otherwise: `table-view--rows' narrowed to the marked subset (when
narrowed), then restricted to the current filter."
  (if (table-view--paged-p)
      (if table-view--narrowed
          (mapcar #'cdr (reverse table-view--mark-cache))
        table-view--rows)
    (let ((rows table-view--rows))
      (when table-view--narrowed
        (setq rows (cl-remove-if-not
                    (lambda (r) (table-view--marked-p (alist-get 'id r))) rows)))
      (when (and table-view--filter (not (string-empty-p table-view--filter)))
        (setq rows (cl-remove-if-not
                    (lambda (r) (table-view--row-matches-p r table-view--filter)) rows)))
      rows)))

;;; Rendering

(defun table-view--cell-width (col row)
  "Screen width of COL's cell in ROW (its displayed text, links reduced)."
  (string-width (table-view--cell-text col (table-view--cell row (alist-get 'key col)))))

(defun table-view--widths (spec rows)
  "Return alist of column-key -> display width for SPEC over ROWS."
  (mapcar
   (lambda (col)
     (let ((w (string-width (alist-get 'header col))))
       (dolist (row rows)
         (setq w (max w (table-view--cell-width col row))))
       (cons (alist-get 'key col) w)))
   (table-view--columns spec)))

(defun table-view--pad (s width align)
  "Pad string S to WIDTH, right-justified when ALIGN is \"right\"."
  (let ((gap (max 0 (- width (string-width s)))))
    (if (equal align "right")
        (concat (make-string gap ?\s) s)
      (concat s (make-string gap ?\s)))))

(defun table-view--badge-string (col s)
  "Return S coloured per badge column COL's palette.
S is returned unchanged when its value has no declared colour."
  (if-let ((color (table-view--badge-color col s)))
      (propertize s 'face (list :foreground color :weight 'bold))
    s))

(defun table-view--cell-string (col row widths)
  "Return the padded, styled cell string for COL in ROW.
Badge columns are coloured; other cells have their Org links rendered.  The
whole cell carries the `table-view-col' text property (its column key) so
column navigation can locate cell boundaries."
  (let* ((key (alist-get 'key col))
         (s (table-view--str (table-view--cell row key)))
         (styled (if (table-view--links-p col)
                     (table-view--linkify s)
                   (table-view--badge-string col s))))
    (propertize
     (table-view--pad styled (alist-get key widths nil nil #'equal)
                      (alist-get 'align col))
     'table-view-col key)))

(defun table-view--row-string (spec row widths cell-fn)
  "Build one \"| ... |\" line for ROW, each cell via CELL-FN.
Prepends the mark-gutter column when any row is marked."
  (concat "| "
          (and (table-view--marks-active-p)
               (concat (if (table-view--marked-p (alist-get 'id row)) "*" " ") " | "))
          (mapconcat (lambda (col) (funcall cell-fn col row widths))
                     (table-view--columns spec) " | ")
          " |"))

(defun table-view--header-string (spec widths)
  (concat "| "
          (and (table-view--marks-active-p) "  | ")
          (mapconcat (lambda (col)
                       (propertize
                        (table-view--pad
                         (propertize (alist-get 'header col) 'face 'bold)
                         (alist-get (alist-get 'key col) widths nil nil #'equal)
                         (alist-get 'align col))
                        'table-view-col (alist-get 'key col)))
                     (table-view--columns spec) " | ")
          " |"))

(defun table-view--rule-string (spec widths)
  (concat "|"
          (and (table-view--marks-active-p) "---+")
          (mapconcat (lambda (col)
                       (make-string
                        (+ 2 (alist-get (alist-get 'key col) widths nil nil #'equal)) ?-))
                     (table-view--columns spec) "+")
          "|"))

(defun table-view--hint-string ()
  "A one-line status/help string: current sort + declared action keys.
Shows \"unsorted\" until an explicit sort has been applied, since rows
render in load order and only reorder on `^'."
  (format "sort: %s%s%s%s    %s"
          (if (and table-view--sorted table-view--sort-keys)
              (table-view--sort-description)
            "unsorted (^)")
          (cond ((null table-view--filter) "")
                ;; Paged: the whole-set counts aren't known client-side.
                ((table-view--paged-p) (format "    filter: %s" table-view--filter))
                (t (format "    filter: %s (%d/%d)"
                           table-view--filter
                           (length (table-view--visible-rows))
                           (length table-view--rows))))
          (cond (table-view--narrowed
                 (format "    narrowed: %d marked" (length table-view--marks)))
                (table-view--marks
                 (format "    marked: %d" (length table-view--marks)))
                (t ""))
          (table-view--page-segment)
          (mapconcat (lambda (a) (format "%s:%s"
                                         (alist-get 'key a) (alist-get 'label a)))
                     (table-view--actions table-view--spec) "  ")))

(defun table-view--goto-id (id)
  "Move point to the start of the row whose id is ID; non-nil on success."
  (let ((pos (point-min)) (found nil))
    (while (and (not found) pos)
      (if (equal (get-text-property pos 'table-view-id) id)
          (progn (goto-char pos) (setq found t))
        (setq pos (next-single-property-change pos 'table-view-id))))
    found))

(defun table-view--render ()
  "Re-render the current buffer from spec + rows.
Renders rows in their current order; sorting is explicit (see
`table-view--sort-rows'), so a row updated by an action stays in place.
Keeps point on the same row id when possible, else falls back to the
same line number."
  (let* ((spec table-view--spec)
         (rows (table-view--visible-rows))
         (widths (table-view--widths spec rows))
         (inhibit-read-only t)
         (line (line-number-at-pos))
         (id (get-text-property (point) 'table-view-id)))
    (erase-buffer)
    (insert (propertize (or (alist-get 'title spec) "Table")
                        'face '(:weight bold :height 1.1))
            "\n")
    (insert (propertize (table-view--hint-string) 'face 'shadow) "\n\n")
    (insert (table-view--header-string spec widths) "\n")
    (insert (table-view--rule-string spec widths) "\n")
    (if (null rows)
        (insert (propertize "  (no rows)\n" 'face 'shadow))
      (dolist (row rows)
        (let ((start (point)))
          (insert (table-view--row-string spec row widths #'table-view--cell-string) "\n")
          (put-text-property start (point) 'table-view-id (alist-get 'id row))
          (put-text-property start (point) 'table-view-row row))))
    (unless (and id (table-view--goto-id id))
      (goto-char (point-min))
      (forward-line (1- line)))))

;;; Marks and bulk

(defun table-view-marked-rows (&optional buffer)
  "The marked rows of BUFFER (or the current buffer).
Client buffers return them in row order.  Paged buffers return the cached
marked rows, which span every page visited -- so a bulk action operates on
the whole selection, not just the rows currently on screen."
  (with-current-buffer (or buffer (current-buffer))
    (if (table-view--paged-p)
        (mapcar #'cdr (reverse table-view--mark-cache))
      (cl-remove-if-not (lambda (r) (table-view--marked-p (alist-get 'id r)))
                        table-view--rows))))

(defun table-view-current-or-marked-rows (&optional buffer)
  "The marked rows of BUFFER, or the single row at point when none are marked.
This is what a `bulk' action's handler receives."
  (with-current-buffer (or buffer (current-buffer))
    (or (table-view-marked-rows)
        (let ((row (get-text-property (point) 'table-view-row)))
          (and row (list row))))))

;;; Dispatch

(defun table-view--dispatch (command &optional bulk)
  "Invoke the registered handler for COMMAND.
When BULK is non-nil the handler is called with the operative row list
\(the marked rows, or the row at point when none are marked); otherwise
it is called with the id and row at point."
  (let ((handler (cdr (assoc command table-view--handlers))))
    (cond
     ((not handler) (message "table-view: no handler for %s" command))
     (bulk (funcall handler (table-view-current-or-marked-rows)))
     (t (funcall handler
                 (get-text-property (point) 'table-view-id)
                 (get-text-property (point) 'table-view-row))))))

;;; Navigation

(defun table-view--rows-region ()
  "Return (FIRST . LAST), the line-start positions of the first and last
data rows, or nil when there are no rows.  Rows are the lines the renderer
tags with `table-view-id'."
  (when-let ((first (text-property-not-all (point-min) (point-max) 'table-view-id nil)))
    (let ((last first) (pos first))
      (while (setq pos (next-single-property-change pos 'table-view-id))
        (when (get-text-property pos 'table-view-id)
          (setq last pos)))
      (cons first last))))

(defun table-view--move-row (dir)
  "Move point one data row in DIR (1 down, -1 up), preserving the column.
Point never leaves the data rows: on the last row a further `n' stays put,
on the first row a further `p' stays just below the header separator.  From
above the rows `n' enters the first row (and from below, `p' the last).
When the preserved column falls on the leading \"|\" (e.g. entering the table
from the title line at column 0), point snaps to the first cell rather than
the separator."
  (when-let ((region (table-view--rows-region)))
    (let ((col (current-column))
          (start (point))
          (start-bol (line-beginning-position))
          (first (car region))
          (last (cdr region)))
      (forward-line dir)
      (let ((bol (line-beginning-position)))
        (cond
         ((get-text-property bol 'table-view-id))            ; landed on a row: keep
         ((and (> dir 0) (< start-bol first)) (goto-char first)) ; enter from above
         ((and (< dir 0) (> start-bol last)) (goto-char last))   ; enter from below
         (t (goto-char start))))                              ; at a boundary: stay
      (move-to-column col)
      (unless (get-text-property (point) 'table-view-col)
        (when-let ((starts (table-view--cell-starts)))
          (goto-char (car starts)))))))

(defun table-view-next-line (&optional n)
  "Move down among the table's data rows, stopping on the last row.
With prefix N repeat; a negative N moves up (see `table-view-previous-line')."
  (interactive "p")
  (let ((n (or n 1)))
    (if (< n 0)
        (table-view-previous-line (- n))
      (dotimes (_ n) (table-view--move-row 1)))))

(defun table-view-previous-line (&optional n)
  "Move up among the table's data rows, stopping on the first row -- just
below the header separator.  With prefix N repeat; a negative N moves down."
  (interactive "p")
  (let ((n (or n 1)))
    (if (< n 0)
        (table-view-next-line (- n))
      (dotimes (_ n) (table-view--move-row -1)))))

(defun table-view--cell-starts ()
  "Buffer positions where each cell begins on the current line, left to right.
Cells are the regions the renderer tags with the `table-view-col' text
property; the bordering \"|\" separators are not tagged.  Both the header
row and data rows carry these tags."
  (let* ((eol (line-end-position))
         (pos (line-beginning-position))
         (starts '()))
    (while (< pos eol)
      (let ((next (or (next-single-property-change pos 'table-view-col nil eol) eol)))
        (when (get-text-property pos 'table-view-col)
          (push pos starts))
        (setq pos next)))
    (nreverse starts)))

(defun table-view--on-cells-p ()
  "Non-nil when the current line has table cells to move between.
True on the header row and on data rows (both carry `table-view-col');
nil on the title, hint, rule, and blank lines."
  (and (table-view--cell-starts) t))

(defun table-view--goto-column (dir)
  "Move to the start of the adjacent cell in DIR (1 forward, -1 backward).
Return non-nil when point actually moves."
  (let* ((starts (table-view--cell-starts))
         (pt (point))
         (target (if (> dir 0)
                     (seq-find (lambda (s) (> s pt)) starts)
                   (seq-find (lambda (s) (< s pt)) (reverse starts)))))
    (when target
      (goto-char target)
      t)))

(defun table-view-forward-column (&optional n)
  "Move to the start of the Nth following cell on the current line (default 1).
With a negative N move backward.  Works on the header row and data rows,
stops at the line's first or last cell instead of leaving it, and never
signals."
  (interactive "p")
  (let ((dir (if (< (or n 1) 0) -1 1)))
    (dotimes (_ (abs (or n 1)))
      (table-view--goto-column dir))))

(defun table-view-backward-column (&optional n)
  "Move to the start of the Nth preceding cell on the current line (default 1).
See `table-view-forward-column'."
  (interactive "p")
  (table-view-forward-column (- (or n 1))))

(defun table-view-forward (&optional n)
  "Move forward N times, by column on a table line and by character elsewhere.
On the header row or a data row this steps between cells; off the table
it falls back to `forward-char'."
  (interactive "p")
  (if (table-view--on-cells-p)
      (table-view-forward-column n)
    (forward-char n)))

(defun table-view-backward (&optional n)
  "Move backward N times, by column on a table line and by character elsewhere.
On the header row or a data row this steps between cells; off the table
it falls back to `backward-char'."
  (interactive "p")
  (if (table-view--on-cells-p)
      (table-view-backward-column n)
    (backward-char n)))

;;; Column reordering

(defun table-view--goto-cell (col)
  "Move point to the start of COL's cell on the current line.
Return non-nil when the cell is found."
  (let ((eol (line-end-position)) (found nil))
    (goto-char (line-beginning-position))
    (while (and (not found) (< (point) eol))
      (if (equal (get-text-property (point) 'table-view-col) col)
          (setq found t)
        (goto-char (or (next-single-property-change (point) 'table-view-col nil eol)
                       eol))))
    found))

(defun table-view--move-column (dir)
  "Swap the column at point with its neighbour in DIR (-1 left, 1 right).
Point follows the moved column.  Return non-nil on success, nil at an
edge or off any column."
  (let* ((col (get-text-property (point) 'table-view-col))
         (cols (table-view--columns table-view--spec))
         (keys (mapcar (lambda (c) (alist-get 'key c)) cols))
         (idx (and col (cl-position col keys :test #'equal)))
         (dest (and idx (+ idx dir))))
    (cond
     ((not idx)
      (message "Point is not on a column")
      nil)
     ((or (< dest 0) (>= dest (length cols)))
      (message "Column %s is already at the %s edge"
               col (if (< dir 0) "left" "right"))
      nil)
     (t
      (let ((new (copy-sequence cols))
            (line (line-number-at-pos)))
        (cl-rotatef (nth idx new) (nth dest new))
        (setf (alist-get 'columns table-view--spec) new)
        (table-view--render)
        (goto-char (point-min))
        (forward-line (1- line))
        (table-view--goto-cell col)
        t)))))

(defun table-view-move-column-right (&optional n)
  "Move the column at point one position to the right, org-table style.
Point follows the column.  With a numeric prefix N repeat N times (a
negative N moves left).  Works on the header row and on data rows."
  (interactive "p")
  (let ((dir (if (< (or n 1) 0) -1 1)))
    (catch 'edge
      (dotimes (_ (abs (or n 1)))
        (unless (table-view--move-column dir) (throw 'edge nil))))))

(defun table-view-move-column-left (&optional n)
  "Move the column at point one position to the left, org-table style.
See `table-view-move-column-right'."
  (interactive "p")
  (table-view-move-column-right (- (or n 1))))

;;; Column schema (add / remove columns at runtime)

(defvar-local table-view-add-column-function nil
  "Function returning a column alist to add, or nil to cancel the add.
Called with no arguments by an interactive `table-view-add-column' so a
consumer can prompt for a domain-specific column -- e.g. one whose cells are
computed from a record field via a `value-fn' (see `table-view--compute-cells').
The returned column is an alist shaped like an entry of a spec's `columns'.
When this is nil, `table-view-add-column' falls back to prompting for a header
and adding an empty text column.")

(defvar table-view-schema-changed-hook nil
  "Normal hook run after a column is added or removed.
Runs in the table buffer once `table-view-add-column' or
`table-view-remove-column' has changed the column set and re-rendered.  A
consumer adds a buffer-local handler (`add-hook' with LOCAL non-nil) to
persist the new schema, reading the live columns from `table-view--spec'.")

(defun table-view--column-keys ()
  "Keys of the current spec's columns, in display order."
  (mapcar (lambda (c) (alist-get 'key c)) (table-view--columns table-view--spec)))

(defun table-view--fresh-column-key (header)
  "A column key derived from HEADER, made unique among the current columns."
  (let* ((slug (downcase (replace-regexp-in-string "[^[:alnum:]]+" "-" header)))
         (base (if (string-empty-p slug) "col" slug))
         (keys (table-view--column-keys))
         (key base) (n 1))
    (while (member key keys)
      (setq key (format "%s-%d" base (cl-incf n))))
    key))

(defun table-view-add-column (&optional column index)
  "Add COLUMN to the current table-view buffer, materialise its cells, re-render.
COLUMN is a column alist (as in a spec's `columns'); it may carry a `value-fn'
of (ID ROW) so its cells are computed for the loaded rows (see
`table-view--compute-cells').  It is inserted at INDEX (0-based) or, by default,
appended; if a column with the same key already exists it is replaced in place.

Called interactively (COLUMN nil) it builds the column via
`table-view-add-column-function' when that is set, else prompts for a header and
adds an empty text column.  After the change it runs
`table-view-schema-changed-hook' so a consumer may persist the schema.  Returns
the column that was added, or nil when the add was cancelled."
  (interactive)
  (let ((col (or column
                 (if table-view-add-column-function
                     (funcall table-view-add-column-function)
                   (let ((header (string-trim (read-string "New column header: "))))
                     (unless (string-empty-p header)
                       `((key . ,(table-view--fresh-column-key header))
                         (header . ,header) (type . "text") (align . "left"))))))))
    (when col
      (let* ((key (alist-get 'key col))
             (cols (table-view--columns table-view--spec)))
        (setf (alist-get 'columns table-view--spec)
              (cond
               ((cl-find key cols :test #'equal :key (lambda (c) (alist-get 'key c)))
                (mapcar (lambda (c) (if (equal (alist-get 'key c) key) col c)) cols))
               ((and index (<= 0 index (length cols)))
                (append (seq-take cols index) (list col) (seq-drop cols index)))
               (t (append cols (list col)))))
        ;; A (re)added `value-fn' column is authoritative: drop any stale cell for
        ;; its key first, so `table-view--compute-cells' recomputes it instead of
        ;; keeping the value a prior column (or a prior definition) left behind.
        (when (alist-get 'value-fn col)
          (table-view--strip-cell-everywhere key))
        (table-view--materialise-cells)
        (table-view--render)
        (run-hooks 'table-view-schema-changed-hook)
        (when (called-interactively-p 'interactive)
          (message "Added column: %s" (alist-get 'header col)))
        col))))

(defun table-view-remove-column (&optional key)
  "Remove the column named KEY from the current table-view buffer and re-render.
Interactively (KEY nil) target the column at point, else prompt for one.  Also
drops the column from the active sort, then runs the buffer's
`table-view-schema-changed-hook' so a consumer may persist the schema.  Returns
KEY when a column was actually removed."
  (interactive)
  (let* ((cols (table-view--columns table-view--spec))
         (key (or key
                  (get-text-property (point) 'table-view-col)
                  (and cols (completing-read "Remove column: "
                                             (table-view--column-keys) nil t)))))
    (cond
     ((null key) (message "No column to remove") nil)
     ((not (cl-find key cols :test #'equal :key (lambda (c) (alist-get 'key c))))
      (message "No such column: %s" key) nil)
     ((= (length cols) 1) (message "Cannot remove the last column") nil)
     (t
      (setf (alist-get 'columns table-view--spec)
            (cl-remove key cols :test #'equal :key (lambda (c) (alist-get 'key c))))
      (setq table-view--sort-keys
            (cl-remove key table-view--sort-keys :test #'equal :key #'car))
      ;; Drop the removed column's now-orphaned cell so a later same-key re-add
      ;; recomputes from its own `value-fn' rather than resurrecting this value.
      (table-view--strip-cell-everywhere key)
      (table-view--render)
      (run-hooks 'table-view-schema-changed-hook)
      (when (called-interactively-p 'interactive)
        (message "Removed column: %s" key))
      key))))

;;; Keymap

(defvar table-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'table-view-next-line)
    (define-key map "p" #'table-view-previous-line)
    (define-key map "f" #'table-view-forward)
    (define-key map "b" #'table-view-backward)
    (define-key map "g" #'table-view-sort)
    (define-key map "/" #'table-view-filter-or-narrow)
    (define-key map "m" #'table-view-mark-toggle)
    (define-key map "u" #'table-view-unmark)
    (define-key map "U" #'table-view-unmark-all)
    (define-key map "^" #'table-view-sort-cycle)
    (define-key map (kbd "C-c C-o") #'table-view-open-link)
    (define-key map (kbd "M-<right>") #'table-view-move-column-right)
    (define-key map (kbd "M-<left>")  #'table-view-move-column-left)
    (define-key map "q" #'quit-window)
    map)
  "Base keymap for `table-view-mode'; action keys overlay it per buffer.")

(define-derived-mode table-view-mode special-mode "Table"
  "Generic, read-only declarative table view."
  (setq truncate-lines t)
  (setq-local cursor-type 'box))

(defun table-view--install-action-keys (spec)
  "Build a buffer-local keymap binding the declared action keys.
In paged buffers it first binds the page-navigation keys (`>' / `.' next
page, `<' / `,' previous page, `M->' / `M-<' last / first page, `M-g' go
to page); these are buffer-local so ordinary tables keep `M-<' / `M->' as
beginning / end-of-buffer.  Action keys are bound last, so a consumer key
shadows a page key rather than the reverse."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map table-view-mode-map)
    (when (table-view--paged-p)
      (define-key map ">" #'table-view-next-page)
      (define-key map "." #'table-view-next-page)   ; alias for >
      (define-key map "<" #'table-view-prev-page)
      (define-key map "," #'table-view-prev-page)   ; alias for <
      (define-key map (kbd "M->") #'table-view-last-page)
      (define-key map (kbd "M-<") #'table-view-first-page)
      (define-key map (kbd "M-g") #'table-view-goto-page))
    (dolist (action (table-view--actions spec))
      (let ((command (alist-get 'command action))
            (bulk (and (alist-get 'bulk action) t)))
        (define-key map (kbd (alist-get 'key action))
                    (lambda ()
                      (interactive)
                      (table-view--dispatch command bulk)))))
    (use-local-map map)))

;;; Interactive sort commands

(defmacro table-view--save-point-location (&rest body)
  "Run BODY, then restore point to the line and column it is on now.
Unlike saving raw point, this survives the header/hint text changing
width during the re-render BODY performs, so the cursor stays put on
screen instead of drifting or following its row."
  (declare (indent 0) (debug t))
  (let ((line (make-symbol "line"))
        (col (make-symbol "col")))
    `(let ((,line (line-number-at-pos))
           (,col (current-column)))
       ,@body
       (goto-char (point-min))
       (forward-line (1- ,line))
       (move-to-column ,col))))

(defun table-view--sortable-keys ()
  "Keys of the columns the user may sort by.
Columns are sortable by default; a column opts out with `sortable' set
to a false value."
  (delq nil (mapcar (lambda (c)
                      (let ((s (assq 'sortable c)))
                        (when (or (null s) (cdr s))
                          (alist-get 'key c))))
                    (table-view--columns table-view--spec))))

(defun table-view-filter (pattern)
  "Filter rows to those with any cell matching PATTERN, landing on the first
match.  Empty PATTERN clears the filter.  In client buffers this filters the
loaded rows; in paged buffers the pattern is pushed to the server and the
first matching page is fetched, so the filter applies across the whole
dataset rather than just the loaded page."
  (interactive "sFilter: ")
  (setq table-view--filter (if (string-empty-p pattern) nil pattern))
  (if (table-view--paged-p)
      (progn
        (table-view--refetch-first)          ; lands on the first row
        (message (if table-view--filter
                     (format "Filter: %s" table-view--filter)
                   "Filter cleared")))
    (table-view--render)
    (table-view--goto-first-row)
    (if table-view--filter
        (message "Filter: %s (%d/%d rows)"
                 table-view--filter
                 (length (table-view--visible-rows))
                 (length table-view--rows))
      (message "Filter cleared"))))

(defun table-view-sort ()
  "Refresh the view, preserving the current ordering.
In a client buffer this clears any filter/narrow and refreshes, keeping
the sort order and point's on-screen location.  In a paged buffer it
re-fetches the CURRENT page from the server -- keeping the sort, filter,
and page position -- which doubles as recovering from a fetch error.
Begin sorting with `^'."
  (interactive)
  (if (table-view--paged-p)
      (progn
        (table-view--refetch-current)
        (message "Refreshed"))
    (table-view--save-point-location
      (setq table-view--filter nil table-view--narrowed nil)
      (when table-view--sorted
        (table-view--sort-rows))
      (table-view--render))
    (message (if table-view--sorted
                 (format "Sort: %s" (table-view--sort-description))
               "Unsorted"))))

(defun table-view-mark-toggle ()
  "Toggle the mark on the row at point, then move to the next row.
Marked rows show a `*' in a gutter column and are the operand of a
`bulk' action; see `table-view-current-or-marked-rows'."
  (interactive)
  (let ((id (get-text-property (point) 'table-view-id))
        (row (get-text-property (point) 'table-view-row)))
    (if (null id)
        (message "Point is not on a row")
      (if (member id table-view--marks)
          (setq table-view--marks (delete id table-view--marks)
                table-view--mark-cache
                (cl-remove id table-view--mark-cache :key #'car :test #'equal))
        (setq table-view--marks (cons id table-view--marks))
        (when row (push (cons id row) table-view--mark-cache)))
      (table-view--prune-marks)         ; widen if that was the last mark
      (table-view--render)
      (forward-line 1))))

(defun table-view-unmark ()
  "Unmark the row at point (a no-op when it is not marked), then move down.
Complements `m' (mark/unmark toggle) and `U' (unmark all); mirrors dired's
`u'."
  (interactive)
  (let ((id (get-text-property (point) 'table-view-id)))
    (if (null id)
        (message "Point is not on a row")
      (when (member id table-view--marks)
        (setq table-view--marks (delete id table-view--marks)
              table-view--mark-cache
              (cl-remove id table-view--mark-cache :key #'car :test #'equal))
        (table-view--prune-marks))      ; widen if that was the last mark
      (table-view--render)
      (forward-line 1))))

(defun table-view-unmark-all ()
  "Remove every mark (and its cached payload), widening a narrowed view."
  (interactive)
  (setq table-view--marks nil table-view--narrowed nil table-view--mark-cache nil)
  (table-view--render)
  (message "Marks cleared"))

(defun table-view-narrow-toggle ()
  "Toggle narrowing the view to the marked rows."
  (interactive)
  (if (null table-view--marks)
      (message "No marked rows")
    (table-view--save-point-location
      (setq table-view--narrowed (not table-view--narrowed))
      (table-view--render))
    (message (if table-view--narrowed
                 (format "Narrowed to %d marked" (length table-view--marks))
               "Widened"))))

(defun table-view-filter-or-narrow ()
  "Narrow to the marked rows when any are marked, else prompt for a filter.
Bound to `/': while rows are marked it toggles the narrow-to-marked view;
otherwise it runs the substring filter (`table-view-filter')."
  (interactive)
  (if table-view--marks
      (table-view-narrow-toggle)
    (call-interactively #'table-view-filter)))

(defun table-view--sort-advance ()
  "Collapse to a single column and advance it through the walk-through.
The cycle visits every sortable column ascending then descending,
wrapping around; it continues from the current column when the sort is
already a single column, else it starts at the first column ascending."
  (let ((keys (table-view--sortable-keys)))
    (when keys
      (let* ((states (mapcan (lambda (k) (list (cons k t) (cons k nil))) keys))
             (cur (and table-view--sorted
                       (= 1 (length table-view--sort-keys))
                       (car table-view--sort-keys)))
             (idx (and cur (cl-position cur states :test #'equal)))
             (next (nth (mod (1+ (or idx -1)) (length states)) states)))
        (setq table-view--sort-keys (list next))))))

(defun table-view--commit-order ()
  "Apply the current sort chain to the view.
Client buffers re-sort the rows in place and re-render, keeping point on
screen.  Paged buffers push the new order to the server and re-fetch the
first page -- an old offset or cursor is meaningless under a new order --
landing on the first row."
  (if (table-view--paged-p)
      (progn
        (setq table-view--sorted (and table-view--sort-keys t))
        ;; Sort resets to page 1, but keep the cursor where it is on screen
        ;; (e.g. on the column header just clicked), like the client path.
        (table-view--refetch-first (table-view--preserve-landing)))
    (table-view--save-point-location
      (table-view--sort-rows)
      (table-view--render))))

(defun table-view--apply-sort-selection (col secondary)
  "Mutate `table-view--sort-keys' for a `^' press on column COL.
With SECONDARY (a `C-u' prefix), add COL as the next lower-priority
tie-breaker, or flip it in place when already chained.  Otherwise make
COL the sole sort key, toggling its direction when it already is.  With
no COL, step the single-column walk-through."
  (cond
   (secondary
    (setq table-view--sort-keys
          (if (assoc col table-view--sort-keys)
              (mapcar (lambda (ka)
                        (if (equal (car ka) col) (cons col (not (cdr ka))) ka))
                      table-view--sort-keys)
            (append table-view--sort-keys (list (cons col t))))))
   (col
    (setq table-view--sort-keys
          (if (and table-view--sorted
                   (= 1 (length table-view--sort-keys))
                   (equal (caar table-view--sort-keys) col))
              (list (cons col (not (cdar table-view--sort-keys))))
            (list (cons col t)))))
   (t
    (table-view--sort-advance))))

(defun table-view--toggle-sort-key (col)
  "Flip COL's direction in the sort chain and apply it."
  (setq table-view--sort-keys
        (mapcar (lambda (ka)
                  (if (equal (car ka) col) (cons col (not (cdr ka))) ka))
                table-view--sort-keys))
  (table-view--commit-order)
  (message "Sort: %s" (table-view--sort-description)))

(defun table-view--secondary-toggle-map (col)
  "A one-key transient map: `^' flips COL's direction in the sort chain."
  (let ((map (make-sparse-keymap)))
    (define-key map "^" (lambda () (interactive) (table-view--toggle-sort-key col)))
    map))

(defun table-view--arm-secondary-toggle (col)
  "Arm the transient `^'-toggles-COL map after `C-u \\[table-view-sort-cycle]'.
It stays active while `^' is pressed and ends on any other key, so a run
of `^' keeps flipping the just-added secondary key COL's direction."
  (set-transient-map (table-view--secondary-toggle-map col) t))

(defun table-view-sort-cycle (&optional secondary)
  "Sort the table via `^'.
With point on a sortable column (a data cell or its header), sort by that
column, collapsing any multi-column chain to it; pressing `^' again on
that already-sorted column toggles ascending/descending.  With point off
any column, walk through every sortable column ascending then descending,
one per press.

With a prefix argument (\\[universal-argument]) and point on a sortable
column, instead ADD that column to the sort chain as the next
lower-priority tie-breaker (so rows equal on the earlier keys are ordered
by it); if it is already in the chain, flip its direction in place.  A run
of plain `^' right after such a `C-u ^' keeps toggling that key.  Point
keeps its on-screen location across the re-sort."
  (interactive "P")
  (let ((col (get-text-property (point) 'table-view-col))
        (sortable (table-view--sortable-keys)))
    (cond
     ((null sortable)
      (message "No sortable columns"))
     ((and col (not (member col sortable)))
      (message "Column %s is not sortable" col))
     ((and secondary (not col))
      (message "Point is not on a column"))
     (t
      (table-view--apply-sort-selection col secondary)
      (table-view--commit-order)
      (message "Sort: %s" (table-view--sort-description))
      ;; C-u ^ arms a run of plain `^' to keep flipping this key.
      (when secondary (table-view--arm-secondary-toggle col))))))

;;; Pagination

(defun table-view--page-segment ()
  "The paging-status segment of the hint line, or \"\" when not paged."
  (cond
   ((not (table-view--paged-p)) "")
   (table-view--page-loading "    loading…")
   (table-view--page-error (format "    error: %s (g refreshes)" table-view--page-error))
   ((zerop (length table-view--rows)) "    (empty)")
   ((table-view--offset-p)
    (let* ((n (length table-view--rows))
           (start (1+ table-view--offset))
           (end (+ table-view--offset n)))
      (if table-view--total
          (format "    page %d/%d · %d-%d of %d"
                  (1+ table-view--page-index)
                  (max 1 (ceiling table-view--total table-view--page-size))
                  start end table-view--total)
        (format "    page %d · %d-%d%s"
                (1+ table-view--page-index) start end
                (if table-view--has-next " · more…" "")))))
   (t                                   ; keyset
    (format "    page %d · %d rows%s"
            (1+ table-view--page-index) (length table-view--rows)
            (if table-view--has-next " · more…" "")))))

(defun table-view--build-request ()
  "The page-fn request plist for the pending page under the live query.
Always carries the buffer, page size, the sort chain, and the active
filter (nil when none); plus `:offset' for offset paging or
`:cursor'/`:direction' for keyset paging."
  (append
   (list :buffer (current-buffer)
         :limit  table-view--page-size
         :sort   table-view--sort-keys
         :filter (and table-view--filter
                      (not (string-empty-p table-view--filter))
                      table-view--filter))
   (if (table-view--offset-p)
       (list :offset (or (plist-get table-view--pending :offset) 0))
     (list :cursor    (plist-get table-view--pending :cursor)
           :direction (or (plist-get table-view--pending :direction) 'forward)))))

(defun table-view--goto-first-row ()
  "Move point to the first data row, or to point-min when there are none."
  (goto-char (point-min))
  (let ((pos (text-property-not-all (point-min) (point-max) 'table-view-id nil)))
    (when pos (goto-char pos))))

(defun table-view--land-point (landing)
  "Place point after a fetch, per the LANDING plist.
`:restore' (LINE . COL) restores an on-screen position (used by sort and
filter, so the cursor stays put across the re-fetch); a string `:point'
is a row id (falling back to the first row); anything else lands on the
first row."
  (let ((restore (plist-get landing :restore))
        (where (plist-get landing :point)))
    (cond
     (restore
      (goto-char (point-min))
      (forward-line (1- (car restore)))
      (move-to-column (cdr restore)))
     ((and (stringp where) (table-view--goto-id where)))
     (t (table-view--goto-first-row)))))

(defun table-view--preserve-landing ()
  "A landing plist keeping point on its current on-screen line and column."
  (list :restore (cons (line-number-at-pos) (current-column))))

(defun table-view--fetch-page (pending)
  "Record PENDING as the in-flight request, show a loading state, call the page-fn.
PENDING is a plist naming the target page (`:offset', or `:cursor' /
`:direction' / `:index') and where point lands once it arrives (`:point',
a row id or the symbol `first').  The position is committed only when
`table-view-set-page' delivers, so a failed fetch never advances the page."
  (unless (table-view--paged-p)
    (user-error "Pagination is not enabled in this buffer"))
  (setq table-view--pending pending
        table-view--page-loading t
        table-view--page-error nil)
  (table-view--render)
  (funcall table-view--page-fn (table-view--build-request)))

(defun table-view--refetch-first (&optional landing)
  "Fetch the first page under the current sort/filter.
LANDING is a plist controlling where point ends up once it arrives
\(`:restore' or `:point'); it defaults to the first row."
  (table-view--fetch-page
   (append (if (table-view--offset-p)
               (list :offset 0 :index 0)
             (list :cursor nil :direction 'forward :index 0))
           (or landing (list :point 'first)))))

(defun table-view--refetch-current ()
  "Re-fetch the CURRENT page, keeping the sort, filter, and page position.
Point returns to the row it is on when the page comes back, else the
first row.  This is what `g' does in a paged buffer -- a plain refresh
that also clears a fetch error by retrying the visible page."
  (let ((id (get-text-property (point) 'table-view-id)))
    (table-view--fetch-page
     (if (table-view--offset-p)
         (list :offset table-view--offset :point (or id 'first))
       (list :cursor table-view--page-cursor :direction 'forward
             :index table-view--page-index :point (or id 'first))))))

(defun table-view--paging-unavailable ()
  "Reason page navigation is currently blocked, or nil when it is allowed.
Paging mutates the underlying page, which is invisible while narrowed to
the marked rows, so it is refused there until the view is widened."
  (cond ((not (table-view--paged-p)) "Pagination not enabled")
        (table-view--narrowed "Widen with / before paging")))

;; Page turns keep point on its current on-screen line and column (so a
;; column can be scanned straight across pages); when the new page is shorter
;; the position clamps to the last row.

(defun table-view-next-page ()
  "Fetch the next page (paged buffers), keeping point where it is on screen."
  (interactive)
  (let ((blocked (table-view--paging-unavailable))
        (landing (table-view--preserve-landing)))
    (cond
     (blocked (message "%s" blocked))
     ((not table-view--has-next) (message "Already on the last page"))
     ((table-view--offset-p)
      (table-view--fetch-page
       (append (list :offset (+ table-view--offset table-view--page-size)) landing)))
     (table-view--next-cursor
      (table-view--fetch-page
       (append (list :cursor table-view--next-cursor :direction 'forward
                     :index (1+ table-view--page-index))
               landing)))
     (t (message "Already on the last page")))))

(defun table-view-prev-page ()
  "Fetch the previous page (paged buffers), keeping point where it is on screen."
  (interactive)
  (let ((blocked (table-view--paging-unavailable))
        (landing (table-view--preserve-landing)))
    (cond
     (blocked (message "%s" blocked))
     ((table-view--offset-p)
      (if (<= table-view--offset 0)
          (message "Already on the first page")
        (table-view--fetch-page
         (append (list :offset (max 0 (- table-view--offset table-view--page-size)))
                 landing))))
     (table-view--prev-cursor
      (table-view--fetch-page
       (append (list :cursor table-view--prev-cursor :direction 'backward
                     :index (max 0 (1- table-view--page-index)))
               landing)))
     (t (message "Already on the first page")))))

(defun table-view-first-page ()
  "Fetch the first page (paged buffers), keeping point where it is on screen."
  (interactive)
  (let ((blocked (table-view--paging-unavailable)))
    (if blocked (message "%s" blocked)
      (table-view--refetch-first (table-view--preserve-landing)))))

(defun table-view-last-page ()
  "Fetch the last page (offset paging with a known total only)."
  (interactive)
  (let ((blocked (table-view--paging-unavailable))
        (landing (table-view--preserve-landing)))
    (cond
     (blocked (message "%s" blocked))
     ((not (table-view--offset-p)) (message "Last page is not available in keyset mode"))
     ((not table-view--total) (message "Total is unknown; page to the end with >"))
     (t (let ((last (* (/ (max 0 (1- table-view--total)) table-view--page-size)
                       table-view--page-size)))
          (table-view--fetch-page (append (list :offset last) landing)))))))

(defun table-view-goto-page (n)
  "Jump to page N (1-based; offset paging only), keeping point on screen."
  (interactive "nGo to page: ")
  (let ((blocked (table-view--paging-unavailable))
        (landing (table-view--preserve-landing)))
    (cond
     (blocked (message "%s" blocked))
     ((not (table-view--offset-p)) (message "Goto-page needs offset paging"))
     (t (let* ((idx (max 0 (1- n)))
               (idx (if table-view--total
                        (min idx (/ (max 0 (1- table-view--total)) table-view--page-size))
                      idx)))
          (table-view--fetch-page
           (append (list :offset (* idx table-view--page-size)) landing)))))))

;;; Public API

(defun table-view-parse (json-string)
  "Parse JSON-STRING into the alist shape the core expects."
  (json-parse-string json-string
                     :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun table-view-set-rows (buffer rows)
  "Replace all rows in table-view BUFFER (a buffer or name) with ROWS."
  (let ((buf (get-buffer buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq table-view--rows (table-view--compute-cells (copy-sequence rows)
                                                          table-view--spec)
              table-view--sorted nil)
        (table-view--prune-marks)
        (table-view--render)))))

(cl-defun table-view-set-page (buffer rows &key total (has-next 'unset)
                                      next-cursor prev-cursor offset)
  "Deliver ROWS as the current page of paged table-view BUFFER.
Call this from a `page-fn' once a page has been fetched (synchronously or
asynchronously).  Keyword metadata, all optional:

  :total        total matching-row count under the active filter, or nil
                when the backend does not report one -- offset paging then
                shows \"page K/N of T\" only when it is known;
  :has-next     force whether a page follows; when omitted it is derived
                from :total (offset) or :next-cursor (keyset), falling back
                to \"a full page implies there may be more\";
  :next-cursor / :prev-cursor  opaque cursors for keyset paging;
  :offset       the offset this page was fetched at (offset paging); the
                pending request's offset is used when omitted.

Commits the requested page position (so a page turn only takes effect on a
successful delivery), refreshes any cached marked rows that reappear in
ROWS, and lands point on the first row (or the requested row id)."
  (let ((buf (get-buffer buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((pending table-view--pending))
          (setq table-view--rows (table-view--compute-cells (copy-sequence rows)
                                                            table-view--spec))
          (if (table-view--offset-p)
              (setq table-view--offset (or offset (plist-get pending :offset) 0)
                    table-view--page-index
                    (if (and table-view--page-size (> table-view--page-size 0))
                        (/ table-view--offset table-view--page-size)
                      0))
            (setq table-view--page-cursor (plist-get pending :cursor)
                  table-view--page-index (or (plist-get pending :index) 0)
                  table-view--next-cursor next-cursor
                  table-view--prev-cursor prev-cursor))
          (setq table-view--total total
                table-view--has-next
                (cond ((not (eq has-next 'unset)) (and has-next t))
                      ((table-view--offset-p)
                       (if total
                           (< (+ table-view--offset (length table-view--rows)) total)
                         (and table-view--page-size
                              (= (length table-view--rows) table-view--page-size))))
                      (t (and next-cursor t)))
                table-view--page-loading nil
                table-view--page-error nil)
          ;; Keep marked-row snapshots fresh when a marked row reappears here.
          (dolist (r table-view--rows)
            (let ((cell (assoc (alist-get 'id r) table-view--mark-cache)))
              (when cell (setcdr cell r))))
          (setq table-view--pending nil)
          (table-view--render)
          (table-view--land-point pending))))))

(defun table-view-page-error (buffer message)
  "Report that fetching a page of table-view BUFFER failed with MESSAGE.
Keeps the previously loaded page visible and shows MESSAGE in the hint
line; the page position is unchanged, so `g' refreshes that page (and
clears the error), and the navigation can be retried with the page keys."
  (let ((buf (get-buffer buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq table-view--page-loading nil
              table-view--page-error (format "%s" message)
              table-view--pending nil)
        (table-view--render)))))

(defun table-view-page-request (&optional buffer)
  "The live paging query of paged BUFFER, or nil when it is not paged.
A plist of :sort (the chain), :filter (or nil), :strategy, and :page-size
-- enough for a bulk handler to push a whole-result server operation
instead of enumerating rows it has not loaded.  This is the seam a future
\"apply to all matching\" bulk action will build on."
  (with-current-buffer (or buffer (current-buffer))
    (when (table-view--paged-p)
      (list :sort table-view--sort-keys
            :filter (and table-view--filter
                         (not (string-empty-p table-view--filter))
                         table-view--filter)
            :strategy table-view--strategy
            :page-size table-view--page-size))))

(defun table-view-upsert-row (buffer row)
  "Add ROW to table-view BUFFER (a buffer or name), or replace its id-match."
  (let ((buf (get-buffer buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq row (car (table-view--compute-cells (list row) table-view--spec)))
        (let ((id (alist-get 'id row)) (found nil))
          (setq table-view--rows
                (mapcar (lambda (r)
                          (if (equal (alist-get 'id r) id)
                              (progn (setq found t) row)
                            r))
                        table-view--rows))
          (unless found
            (setq table-view--rows (nconc table-view--rows (list row))))
          (table-view--render))))))

(defun table-view-delete-row (buffer id)
  "Remove the row whose id is ID from table-view BUFFER (a buffer or name).
Re-renders and moves point to the following row, or to the previous one
when the deleted row was last.  Returns non-nil when a row was removed.

Deletion is otherwise up to the consumer: bind an action to a handler
that does any pre-delete work (removing files, database rows, ...) and
calls this only on success, so the row survives if that work fails."
  (let ((buf (get-buffer buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (cl-find id table-view--rows
                       :key (lambda (r) (alist-get 'id r)) :test #'equal)
          (let* ((visible (table-view--visible-rows))
                 (vpos (cl-position id visible
                                    :key (lambda (r) (alist-get 'id r)) :test #'equal))
                 (target (and vpos
                              (alist-get 'id (or (nth (1+ vpos) visible)
                                                 (and (> vpos 0) (nth (1- vpos) visible)))))))
            (setq table-view--rows
                  (cl-remove id table-view--rows
                             :key (lambda (r) (alist-get 'id r)) :test #'equal)
                  table-view--marks (delete id table-view--marks)
                  table-view--mark-cache
                  (cl-remove id table-view--mark-cache :key #'car :test #'equal))
            (table-view--prune-marks)
            (table-view--render)
            (when target (table-view--goto-id target))
            t))))))

(defun table-view-refresh (buffer)
  "Refresh table-view BUFFER (a buffer or name).
Client buffers re-invoke the registered `fill-fn'.  Paged buffers re-fetch
the current page, keeping point on the same row when it comes back."
  (let ((buf (get-buffer buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (cond
         ((table-view--paged-p) (table-view--refetch-current))
         (table-view--fill-fn (funcall table-view--fill-fn buf)))))))

(defun table-view-display (buffer spec handlers &optional fill-fn page-fn)
  "Render SPEC into BUFFER, install HANDLERS, and populate.  Returns the buffer.
SPEC is a parsed alist (see `table-view-parse').  HANDLERS is an alist of
command-name (string) -> (FN ID ROW).

Data source (FILL-FN and PAGE-FN are mutually exclusive; PAGE-FN wins):
  * FILL-FN, a function of BUFFER that populates ALL rows via
    `table-view-set-rows' / `table-view-upsert-row'; or
  * PAGE-FN, server-side pagination: a function of a REQUEST plist that
    fetches ONE page and delivers it with `table-view-set-page' (or
    `table-view-page-error').  When PAGE-FN is non-nil the buffer is
    paged: `table-view--rows' holds only the current page, sort and
    filter are pushed into each request, and totals come from the
    server.  SPEC's `pagination' block sets `page-size' (default 50) and
    `strategy' (`offset', the default, for OFFSET/LIMIT with random
    access and totals, or `keyset' for opaque forward/back cursors).

SPEC's `sort' (a single {column, ascending}, or a list of them for a
multi-column default) seeds the sort chain: in a client buffer it is
applied to SPEC's own rows on open; in a paged buffer it is sent with the
first page request.  Rows arriving later via FILL-FN start unsorted."
  (let ((buf (get-buffer-create buffer)))
    (with-current-buffer buf
      (table-view-mode)
      (setq table-view--spec (table-view--own-spec spec)
            table-view--rows (table-view--compute-cells (alist-get 'rows spec)
                                                        table-view--spec)
            table-view--handlers handlers
            table-view--fill-fn fill-fn
            table-view--page-fn page-fn)
      (let ((pg (alist-get 'pagination spec)))
        (when (or page-fn pg)
          (let ((ps (alist-get 'page-size pg)))
            (setq table-view--page-size (if (and (integerp ps) (> ps 0)) ps 50)))
          (setq table-view--strategy (pcase (alist-get 'strategy pg)
                                       ((or "keyset" 'keyset) 'keyset)
                                       (_ 'offset)))))
      (setq table-view--sort-keys (table-view--parse-sort (alist-get 'sort spec)))
      (if (table-view--paged-p)
          (setq table-view--sorted (and table-view--sort-keys t))
        (when (and table-view--sort-keys table-view--rows)
          (table-view--sort-rows)))       ; apply the declared default sort
      (table-view--install-action-keys spec)
      (table-view--render))
    (switch-to-buffer buf)
    (with-current-buffer buf
      (cond ((table-view--paged-p) (table-view--refetch-first))  ; fetch page 1
            (fill-fn (funcall fill-fn buf))))
    buf))

(provide 'table-view)
;;; table-view.el ends here
