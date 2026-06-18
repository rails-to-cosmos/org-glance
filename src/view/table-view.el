;;; table-view.el --- Generic declarative table view -*- lexical-binding: t; -*-

;; VENDORED into org-glance from the standalone `table-view' project (the
;; `repos' dashboard's table core).  org-glance consumes it from
;; `org-glance-table.el', building the spec in pure elisp (no Haskell backend).
;; Kept namespaced and backend-agnostic so it can still graduate into its own
;; package; the ONLY org-glance-local change is the `table-view-remove-row'
;; primitive below (a consumer needs to drop a row whose backing record left the
;; view's filter) -- upstream it if this graduates.
;;
;; A tiny, backend-agnostic core that renders a declarative table
;; description -- columns, actions, default sort -- and dispatches keys to
;; consumer-registered command handlers.  The schema is produced by a
;; backend (see Repos.Table on the Haskell side); the rows are supplied by
;; a consumer `fill-fn'.  The core knows nothing about what a row means.
;;
;; Responsibilities:
;;   * render a spec as an aligned, org-table-styled read-only view
;;   * colour `badge' cells from the column's declared palette
;;   * build a keymap from the declared actions, dispatch by command name
;;   * own a row store keyed by id: set-rows / upsert-row (streaming fill)
;;   * client-side sort on sortable columns
;;
;; A consumer provides: a parsed spec, a `fill-fn' (BUFFER -> populates via
;; the mutators below), and a handler alist (command-name -> FN of ID ROW).

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
(defvar-local table-view--sort-key nil
  "Column key (string) currently sorted by, or nil.")
(defvar-local table-view--sort-asc t
  "Non-nil when the current sort is ascending.")
(defvar-local table-view--sorted nil
  "Non-nil once an explicit sort has been applied to the current row set.
Reset whenever the full row set is replaced (`table-view-set-rows'), so
the hint line never claims a sort the displayed rows are not actually in.")

;;; Spec accessors

(defun table-view--columns (spec) (alist-get 'columns spec))
(defun table-view--actions (spec) (alist-get 'actions spec))

(defun table-view--column (spec key)
  "Return the column alist whose key is KEY in SPEC."
  (seq-find (lambda (c) (equal (alist-get 'key c) key))
            (table-view--columns spec)))

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

(defun table-view--comparator (col)
  "Return a less-than predicate over raw cell values for column COL."
  (pcase (alist-get 'type col)
    ("number"
     (lambda (a b) (< (or a 0) (or b 0))))
    ("badge"
     ;; Sort by declared palette order (which doubles as priority).
     (let ((order (mapcar (lambda (b) (alist-get 'value b))
                          (alist-get 'badges col)))
           (n (length (alist-get 'badges col))))
       (lambda (a b)
         (< (or (cl-position a order :test #'equal) n)
            (or (cl-position b order :test #'equal) n)))))
    (_
     (lambda (a b) (string< (table-view--str a) (table-view--str b))))))

(defun table-view--sort-rows ()
  "Sort `table-view--rows' in place by the current sort column.
No-op when no sort column is selected.  Sorting is explicit: it runs
only from the sort commands (`g', `^', `~'), never from row updates,
so operating on a row updates it in place without moving it."
  (let ((key table-view--sort-key))
    (when key
      (let* ((col (table-view--column table-view--spec key))
             (less (table-view--comparator col))
             (sorted (sort (copy-sequence table-view--rows)
                           (lambda (a b)
                             (funcall less
                                      (table-view--cell a key)
                                      (table-view--cell b key))))))
        (setq table-view--rows
              (if table-view--sort-asc sorted (nreverse sorted)))
        (setq table-view--sorted t)))))

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
  "Return the padded, possibly coloured cell string for COL in ROW."
  (let* ((key (alist-get 'key col))
         (val (table-view--cell row key))
         (s (table-view--str val)))
    (when (equal (alist-get 'type col) "badge")
      (let ((color (table-view--badge-color col s)))
        (when color
          (setq s (propertize s 'face (list :foreground color :weight 'bold))))))
    (table-view--pad s (alist-get key widths nil nil #'equal) (alist-get 'align col))))

(defun table-view--row-string (spec row widths cell-fn)
  "Build one \"| ... |\" line for ROW, each cell via CELL-FN."
  (concat "| "
          (mapconcat (lambda (col) (funcall cell-fn col row widths))
                     (table-view--columns spec) " | ")
          " |"))

(defun table-view--header-string (spec widths)
  (concat "| "
          (mapconcat (lambda (col)
                       (table-view--pad
                        (propertize (alist-get 'header col) 'face 'bold)
                        (alist-get (alist-get 'key col) widths nil nil #'equal)
                        (alist-get 'align col)))
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
render in load order and only reorder on `g'/`^'/`~'."
  (format "sort: %s    %s"
          (if (and table-view--sorted table-view--sort-key)
              (format "%s %s" table-view--sort-key
                      (if table-view--sort-asc "asc" "desc"))
            "unsorted (g)")
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
         (rows table-view--rows)
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

;;; Keymap

(defvar table-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "g" #'table-view-sort)
    (define-key map "^" #'table-view-cycle-sort)
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

(defun table-view--sortable-keys ()
  (delq nil (mapcar (lambda (c)
                      (when (eq t (alist-get 'sortable c)) (alist-get 'key c)))
                    (table-view--columns table-view--spec))))

(defun table-view-sort ()
  "Sort the table by the current sort column and re-render.
With no active sort column, fall back to the spec's default sort
column.  This is the primary key that reorders rows; updates from row
actions leave rows in place until `g' is pressed."
  (interactive)
  (unless table-view--sort-key
    (setq table-view--sort-key
          (alist-get 'column (alist-get 'sort table-view--spec))))
  (table-view--sort-rows)
  (table-view--render)
  (message "Sort: %s %s"
           (or table-view--sort-key "-")
           (if table-view--sort-asc "asc" "desc")))

(defun table-view--column-at-point ()
  "Return the column key at point, or nil if point is not on a table line."
  (let ((col (current-column))
        (line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (when (string-prefix-p "|" line)
      (let ((widths (table-view--widths table-view--spec table-view--rows))
            (pos 2)
            (result nil))
        (dolist (c (table-view--columns table-view--spec))
          (let* ((key (alist-get 'key c))
                 (w (alist-get key widths nil nil #'equal)))
            (when (and (>= col pos) (< col (+ pos w)))
              (setq result key))
            (setq pos (+ pos w 3))))
        result))))

(defun table-view-cycle-sort ()
  "Context-aware sort.
On a table column: sort by that column, toggling direction if already active.
Before the table: cycle through all sortable columns."
  (interactive)
  (let ((col-key (table-view--column-at-point)))
    (if col-key
        (let ((c (table-view--column table-view--spec col-key)))
          (when (and c (eq t (alist-get 'sortable c)))
            (if (equal table-view--sort-key col-key)
                (setq table-view--sort-asc (not table-view--sort-asc))
              (setq table-view--sort-key col-key
                    table-view--sort-asc t))
            (table-view--sort-rows)
            (table-view--render)
            (message "Sort: %s %s" col-key
                     (if table-view--sort-asc "asc" "desc"))))
      (let* ((keys (table-view--sortable-keys))
             (idx (cl-position table-view--sort-key keys :test #'equal))
             (next (and keys (nth (mod (1+ (or idx -1)) (length keys)) keys))))
        (when next
          (setq table-view--sort-key next table-view--sort-asc t)
          (table-view--sort-rows)
          (table-view--render)
          (message "Sort: %s asc" next))))))

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

(defun table-view-remove-row (buffer id)
  "Drop the row whose id is ID from table-view BUFFER (a buffer or name).
A no-op when no row has that id.  The mirror of `table-view-upsert-row' for a
consumer whose backing record left the view (e.g. a row that no longer matches a
live filter); like upsert it leaves the surviving rows in place and re-renders."
  (let ((buf (get-buffer buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq table-view--rows
              (cl-remove id table-view--rows
                         :key (lambda (r) (alist-get 'id r)) :test #'equal))
        (table-view--render)))))

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
`table-view-upsert-row'.  Returns the buffer."
  (let ((buf (get-buffer-create buffer)))
    (with-current-buffer buf
      (table-view-mode)
      (setq table-view--spec spec
            table-view--rows (alist-get 'rows spec)
            table-view--handlers handlers
            table-view--fill-fn fill-fn)
      (let ((sort (alist-get 'sort spec)))
        (setq table-view--sort-key (alist-get 'column sort)
              table-view--sort-asc (if (assq 'ascending sort)
                                       (and (alist-get 'ascending sort) t)
                                     t)))
      (table-view--install-action-keys spec)
      (table-view--render))
    (switch-to-buffer buf)
    (when fill-fn (funcall fill-fn buf))
    buf))

(provide 'table-view)
;;; table-view.el ends here
