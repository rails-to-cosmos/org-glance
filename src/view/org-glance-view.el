;; -*- lexical-binding: t -*-

;;; org-glance-view.el --- shared coherence for graph views

;;; Commentary:
;; Both halves of view coherence (invariant 10): PUSH flag-stale, PULL refresh.

;;; Code:

(require 'cl-lib)
(require 'org-glance-core)
(require 'table-view)
(require 'org-glance-utils)
(require 'org-glance-graph)

(defvar-local org-glance-view--graph nil
  "The graph this buffer is a view of; its presence marks an org-glance view.")

(defvar-local org-glance-view--stale nil
  "Non-nil when this view is behind the store (the `glance:stale' lighter).")

(defvar-local org-glance-view--stale-fn nil
  "Nullary predicate of the current view: non-nil when it is behind the store.")

(defvar-local org-glance-view--reload-fn nil
  "Nullary thunk of the current view: re-fill it from the graph.")

(defvar-local org-glance-view--mtime nil
  "Store mtime snapshot taken at this view's last fill (its freshness anchor).")

(cl-defun org-glance-view:snapshot-mtime (path)
  "Record PATH's mtime as the current view's freshness anchor."
  (setq-local org-glance-view--mtime (org-glance--file-mtime path)))

(cl-defun org-glance-view:stale-vs-file? (path)
  "Non-nil when the view's fill predates PATH's last change.
A missing snapshot or PATH counts as stale (invariant 10)."
  (let ((mtime (org-glance--file-mtime path)))
    (or (null org-glance-view--mtime)
        (null mtime)
        (time-less-p org-glance-view--mtime mtime))))

(cl-defun org-glance-view:completing-read-tag (&optional (prompt "Tag (empty for all): "))
  "Prompt with PROMPT for a tag from the graph's headlines; empty input = nil."
  (org-glance-ensure-init)
  (let ((choice (completing-read prompt (org-glance-graph:tags org-glance-graph))))
    (unless (string-empty-p choice) choice)))

(defcustom org-glance-view-fill-frame t
  "When non-nil, opening an overview or table view fills the frame.
The view's window becomes the sole one; nil leaves the layout untouched."
  :group 'org-glance
  :type 'boolean)

(cl-defun org-glance-view:fill-frame (&optional already-in-view)
  "Delete the other windows when `org-glance-view-fill-frame' is non-nil.
Call right after displaying a view; act only when it is in the selected window
and ALREADY-IN-VIEW is nil (non-nil for a filter change or `T' in a view).  A
`delete-other-windows' signal is only messaged, even under `debug-on-error'."
  (when (and org-glance-view-fill-frame
             (not already-in-view)
             (eq (window-buffer) (current-buffer)))
    (condition-case err
        (delete-other-windows)
      (error (message "org-glance: fill-frame skipped: %S" err)))))

(cl-defun org-glance-view:register (graph &key stale-fn reload-fn)
  "Mark the current buffer a fresh view of GRAPH and wire its coherence.
:STALE-FN is a nullary predicate, non-nil when behind the store; :RELOAD-FN
re-fills the view.  Refresh on every display or selection (invariant 10)."
  (setq-local org-glance-view--graph graph
              org-glance-view--stale nil
              org-glance-view--stale-fn stale-fn
              org-glance-view--reload-fn reload-fn)
  (add-hook 'window-buffer-change-functions #'org-glance-view--refresh-when-stale nil t)
  (add-hook 'window-selection-change-functions #'org-glance-view--refresh-when-stale nil t))

(cl-defun org-glance-view--mark-stale ()
  "Flag the current view buffer as behind the store (lighter only; no rebuild)."
  (unless org-glance-view--stale
    (setq org-glance-view--stale t)
    (force-mode-line-update)))

(cl-defun org-glance-view:mark-fresh ()
  "Clear the current view's stale flag, after a refresh re-filled it."
  (when org-glance-view--stale
    (setq org-glance-view--stale nil)
    (force-mode-line-update)))

(cl-defun org-glance-view:mark-graph-stale (graph)
  "Flag every open view buffer of GRAPH stale, touching no disk, rows or point.
Each view clears the flag on its next refresh (display boundary or `g')."
  (dolist (buffer (buffer-list))
    (when (eq (buffer-local-value 'org-glance-view--graph buffer) graph)
      (with-current-buffer buffer (org-glance-view--mark-stale)))))

(cl-defun org-glance-view--refresh-when-stale (&optional window)
  "Re-fill WINDOW's (or the current) view iff it is behind the store.
A modified file-backed buffer is only flagged stale (invariant 11); reload
errors are demoted (invariant 9)."
  (with-current-buffer (if (windowp window) (window-buffer window) (current-buffer))
    (when (and org-glance-view--stale-fn
               (funcall org-glance-view--stale-fn))
      (if (and buffer-file-name (buffer-modified-p))
          (org-glance-view--mark-stale)
        (with-demoted-errors "org-glance: view refresh failed: %S"
          (funcall org-glance-view--reload-fn))))))

(cl-pushnew '(org-glance-view--stale " glance:stale") mode-line-misc-info :test #'equal)

(cl-defun org-glance-view:pick-occurrence (graph id)
  "Completing-read one of ID's occurrence snapshots in GRAPH; open it read-only.
The buffer is named by the headline's title (else ID) and the stamp."
  (let ((title (org-glance-graph:title-or-id graph id))
        (occurrences (org-glance-graph:occurrences graph id)))
    (unless occurrences
      (user-error "No occurrence history for this headline (see `org-glance-repeat-history-depth')"))
    (let* ((stamp (completing-read "Occurrence: " (mapcar #'car occurrences) nil t))
           (path (alist-get stamp occurrences nil nil #'string=))
           (buf (get-buffer-create (format "*org-glance-occurrence: %s [%s]*" title stamp))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (f-read-text path 'utf-8))
          (org-mode)
          (goto-char (point-min)))
        (read-only-mode 1))
      (switch-to-buffer buf))))

(cl-defun org-glance-view:column-at-point ()
  "Return the `table-view' column key under point, or nil."
  (get-text-property (point) 'table-view-col))

(cl-defun org-glance-view:point-context ()
  "Return point's position in a `table-view' buffer as (ID LINE COL).
Capture it before a refill; restore with `org-glance-view:restore-point'."
  (list (get-text-property (point) 'table-view-id)
        (line-number-at-pos)
        (org-glance-view:column-at-point)))

(cl-defun org-glance-view:restore-point (id line &optional col)
  "Return point to row ID, else to screen LINE; COL re-lands on that cell.
Every table refill restores the (row, cell) pair (invariant 24)."
  (unless (and id (table-view--goto-id id))
    (goto-char (point-min))
    (forward-line (1- line)))
  (when col (table-view--goto-cell col)))

(cl-defun org-glance-view:display-table (graph name spec handlers fill-fn
                                               &key stale-fn reload-fn)
  "Display a `table-view' buffer NAME over GRAPH and return it.
SPEC, HANDLERS and FILL-FN go to `table-view-display'; `default-directory' is
GRAPH's root; the seeded sort applies; the frame fills unless entered from
another view.  STALE-FN and RELOAD-FN register pull refresh, else `g' only."
  (let* ((from-view (and org-glance-view--graph t))
         (buf (table-view-display name spec handlers fill-fn)))
    (with-current-buffer buf
      (setq default-directory (file-name-as-directory (org-glance-graph:directory graph)))
      (when stale-fn
        (org-glance-view:register graph :stale-fn stale-fn :reload-fn reload-fn))
      (table-view-apply-sort)
      (org-glance-view:fill-frame from-view))
    buf))

(defconst org-glance-view--stale-message
  "Headline no longer in graph (view is stale; press `g' to refresh)"
  "The `user-error' both staleness guards below raise.")

(cl-defun org-glance-view:live-headline (graph id)
  "Return the live `org-glance-headline' for ID in GRAPH; `user-error' if gone."
  (or (org-glance-graph:headline graph id)
      (user-error "%s" org-glance-view--stale-message)))

(cl-defun org-glance-view:live-metadata (graph id)
  "Return the live headline metadata for ID in GRAPH; `user-error' if gone."
  (or (org-glance-graph:live-meta graph id)
      (user-error "%s" org-glance-view--stale-message)))

(provide 'org-glance-view)
;;; org-glance-view.el ends here
