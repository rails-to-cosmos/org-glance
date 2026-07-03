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
;;   * own a row store keyed by id: set-rows / upsert-row (streaming fill)
;;   * client-side sort on sortable columns
;;   * interactive substring filter (/)
;;
;; A consumer provides: a parsed spec, a `fill-fn' (BUFFER -> populates via
;; the mutators below), and a handler alist (command-name -> FN of ID ROW).
;;
;; See examples/ for runnable demos:
;;   minimal.el       — inline rows from a JSON spec
;;   fill-function.el — populate via a fill function (Emacs subprocesses)
;;   upsert.el        — streaming row updates via a timer
;;   multi-sort.el    — column navigation + multi-column (C-u ^) sorting
;;   sort-methods.el  — per-column sort methods (values / compare) + default sort
;;
;; Keybindings in table-view-mode:
;;   g   — clear filter & refresh, preserving the current sort order
;;   ^   — sort by the column at point, repeat toggles asc/desc;
;;         off a column, cycle through every column and direction
;;   C-u ^ — add the column at point as a secondary (tie-breaker) sort key;
;;           a following run of `^' then toggles that key's direction
;;   /   — filter rows by substring
;;   n/p — next/previous line
;;   f/b — forward/backward: by column on a table line (header or row),
;;         by char elsewhere
;;   M-left/M-right — move the column at point left/right (org-table style)
;;   q   — quit

;;; Code:

(require 'cl-lib)

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
    (let ((tests (mapcar
                  (lambda (ka)
                    (list (table-view--comparator
                           (table-view--column table-view--spec (car ka)))
                          (car ka)          ; column key
                          (cdr ka)))        ; ascending?
                  table-view--sort-keys)))
      (setq table-view--rows
            (sort (copy-sequence table-view--rows)
                  (lambda (a b)
                    (cl-loop for (less key asc) in tests
                             for va = (table-view--cell a key)
                             for vb = (table-view--cell b key)
                             do (cond ((funcall less va vb) (cl-return asc))
                                      ((funcall less vb va) (cl-return (not asc))))
                             finally return nil))))
      (setq table-view--sorted t))))

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
                    (let ((col (alist-get 'column s)))
                      (when col
                        (cons col (if (assq 'ascending s)
                                      (and (alist-get 'ascending s) t)
                                    t)))))
                  specs))))

;;; Filtering

(defun table-view--row-matches-p (row filter)
  "Non-nil if any cell in ROW contains FILTER (case-insensitive substring)."
  (let ((pat (downcase filter)))
    (cl-some (lambda (col)
               (string-match-p
                (regexp-quote pat)
                (downcase (table-view--str
                           (table-view--cell row (alist-get 'key col))))))
             (table-view--columns table-view--spec))))

(defun table-view--visible-rows ()
  "Rows to render: all if no filter, otherwise only matching ones."
  (if (and table-view--filter (not (string-empty-p table-view--filter)))
      (cl-remove-if-not
       (lambda (row) (table-view--row-matches-p row table-view--filter))
       table-view--rows)
    table-view--rows))

;;; Rendering

(defun table-view--widths (spec rows)
  "Return alist of column-key -> display width for SPEC over ROWS."
  (mapcar
   (lambda (col)
     (let* ((key (alist-get 'key col))
            (w (string-width (alist-get 'header col))))
       (dolist (row rows)
         (setq w (max w (string-width (table-view--str (table-view--cell row key))))))
       (cons key w)))
   (table-view--columns spec)))

(defun table-view--pad (s width align)
  "Pad string S to WIDTH, right-justified when ALIGN is \"right\"."
  (let ((gap (max 0 (- width (string-width s)))))
    (if (equal align "right")
        (concat (make-string gap ?\s) s)
      (concat s (make-string gap ?\s)))))

(defun table-view--cell-string (col row widths)
  "Return the padded, possibly coloured cell string for COL in ROW.
The whole cell carries the `table-view-col' text property (its column
key) so column navigation can locate cell boundaries."
  (let* ((key (alist-get 'key col))
         (val (table-view--cell row key))
         (s (table-view--str val)))
    (when (equal (alist-get 'type col) "badge")
      (let ((color (table-view--badge-color col s)))
        (when color
          (setq s (propertize s 'face (list :foreground color :weight 'bold))))))
    (propertize
     (table-view--pad s (alist-get key widths nil nil #'equal) (alist-get 'align col))
     'table-view-col key)))

(defun table-view--row-string (spec row widths cell-fn)
  "Build one \"| ... |\" line for ROW, each cell via CELL-FN."
  (concat "| "
          (mapconcat (lambda (col) (funcall cell-fn col row widths))
                     (table-view--columns spec) " | ")
          " |"))

(defun table-view--header-string (spec widths)
  (concat "| "
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
          (mapconcat (lambda (col)
                       (make-string
                        (+ 2 (alist-get (alist-get 'key col) widths nil nil #'equal)) ?-))
                     (table-view--columns spec) "+")
          "|"))

(defun table-view--hint-string ()
  "A one-line status/help string: current sort + declared action keys.
Shows \"unsorted\" until an explicit sort has been applied, since rows
render in load order and only reorder on `^'."
  (format "sort: %s%s    %s"
          (if (and table-view--sorted table-view--sort-keys)
              (table-view--sort-description)
            "unsorted (^)")
          (if table-view--filter
              (format "    filter: %s (%d/%d)"
                      table-view--filter
                      (length (table-view--visible-rows))
                      (length table-view--rows))
            "")
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

;;; Dispatch

(defun table-view--dispatch (command)
  "Invoke the registered handler for COMMAND on the row at point."
  (let ((handler (cdr (assoc command table-view--handlers)))
        (id (get-text-property (point) 'table-view-id))
        (row (get-text-property (point) 'table-view-row)))
    (if handler
        (funcall handler id row)
      (message "table-view: no handler for %s" command))))

;;; Navigation

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

;;; Keymap

(defvar table-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "f" #'table-view-forward)
    (define-key map "b" #'table-view-backward)
    (define-key map "g" #'table-view-sort)
    (define-key map "/" #'table-view-filter)
    (define-key map "^" #'table-view-sort-cycle)
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
  "Build a buffer-local keymap binding the declared action keys."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map table-view-mode-map)
    (dolist (action (table-view--actions spec))
      (let ((command (alist-get 'command action)))
        (define-key map (kbd (alist-get 'key action))
                    (lambda ()
                      (interactive)
                      (table-view--dispatch command)))))
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
  (delq nil (mapcar (lambda (c)
                      (when (eq t (alist-get 'sortable c)) (alist-get 'key c)))
                    (table-view--columns table-view--spec))))

(defun table-view-filter (pattern)
  "Filter rows to those with any cell matching PATTERN.
Empty PATTERN clears the filter.  Point keeps its on-screen location
(line and column) across the re-render."
  (interactive "sFilter: ")
  (table-view--save-point-location
    (setq table-view--filter (if (string-empty-p pattern) nil pattern))
    (table-view--render))
  (if table-view--filter
      (message "Filter: %s (%d/%d rows)"
               table-view--filter
               (length (table-view--visible-rows))
               (length table-view--rows))
    (message "Filter cleared")))

(defun table-view-sort ()
  "Clear any filter and refresh the view, preserving the current ordering.
An unsorted table stays in load order; a sorted table keeps its sort.
Point keeps its on-screen location (line and column) across the refresh.
Begin sorting with `^'."
  (interactive)
  (table-view--save-point-location
    (setq table-view--filter nil)
    (when table-view--sorted
      (table-view--sort-rows))
    (table-view--render))
  (message (if table-view--sorted
               (format "Sort: %s" (table-view--sort-description))
             "Unsorted")))

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

(defun table-view--toggle-sort-key (col)
  "Flip COL's direction in the sort chain, re-sort, and re-render."
  (table-view--save-point-location
    (setq table-view--sort-keys
          (mapcar (lambda (ka)
                    (if (equal (car ka) col) (cons col (not (cdr ka))) ka))
                  table-view--sort-keys))
    (table-view--sort-rows)
    (table-view--render))
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
      (table-view--save-point-location
        (cond
         ;; C-u ^ on a column -> add it as a tie-breaker, or flip it in place.
         (secondary
          (setq table-view--sort-keys
                (if (assoc col table-view--sort-keys)
                    (mapcar (lambda (ka)
                              (if (equal (car ka) col) (cons col (not (cdr ka))) ka))
                            table-view--sort-keys)
                  (append table-view--sort-keys (list (cons col t))))))
         ;; ^ on a column -> single-column sort (toggle if already the sole key).
         (col
          (setq table-view--sort-keys
                (if (and table-view--sorted
                         (= 1 (length table-view--sort-keys))
                         (equal (caar table-view--sort-keys) col))
                    (list (cons col (not (cdar table-view--sort-keys))))
                  (list (cons col t)))))
         ;; ^ off any column -> step the single-column walk-through.
         (t
          (table-view--sort-advance)))
        (table-view--sort-rows)
        (table-view--render))
      (message "Sort: %s" (table-view--sort-description))
      ;; C-u ^ arms a run of plain `^' to keep flipping this key.
      (when secondary (table-view--arm-secondary-toggle col))))))

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
        (setq table-view--rows (copy-sequence rows)
              table-view--sorted nil)
        (table-view--render)))))

(defun table-view-upsert-row (buffer row)
  "Add ROW to table-view BUFFER (a buffer or name), or replace its id-match."
  (let ((buf (get-buffer buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
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

(defun table-view-refresh (buffer)
  "Re-invoke the registered `fill-fn' for table-view BUFFER (a buffer or name)."
  (let ((buf (get-buffer buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when table-view--fill-fn
          (funcall table-view--fill-fn buf))))))

(defun table-view-display (buffer spec handlers &optional fill-fn)
  "Render SPEC into BUFFER, install HANDLERS, and run FILL-FN to populate.
SPEC is a parsed alist (see `table-view-parse').  HANDLERS is an alist of
command-name (string) -> (FN ID ROW).  FILL-FN, if non-nil, is a function
of one argument (BUFFER) that populates rows via `table-view-set-rows' /
`table-view-upsert-row'.  Returns the buffer.

SPEC's `sort' (a single {column, ascending}, or a list of them for a
multi-column default) is applied as the default sort when SPEC itself
supplies rows.  Rows arriving later via FILL-FN / `table-view-set-rows'
start unsorted."
  (let ((buf (get-buffer-create buffer)))
    (with-current-buffer buf
      (table-view-mode)
      (setq table-view--spec (table-view--own-spec spec)
            table-view--rows (alist-get 'rows spec)
            table-view--handlers handlers
            table-view--fill-fn fill-fn)
      (setq table-view--sort-keys (table-view--parse-sort (alist-get 'sort spec)))
      (when (and table-view--sort-keys table-view--rows)
        (table-view--sort-rows))          ; apply the declared default sort
      (table-view--install-action-keys spec)
      (table-view--render))
    (switch-to-buffer buf)
    (when fill-fn (funcall fill-fn buf))
    buf))

(provide 'table-view)
;;; table-view.el ends here
