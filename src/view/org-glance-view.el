;; -*- lexical-binding: t -*-

;;; org-glance-view.el --- shared coherence for graph views

;;; Commentary:
;; Overviews and tables are read-only projections of the graph that can fall
;; behind the store when it advances (a materialized save appends to
;; `headlines.jsonl').  This module owns BOTH halves of keeping them coherent,
;; once, for every view type:
;;
;;  - PUSH (cheap, on the save hot path): `org-glance-view:mark-graph-stale'
;;    flags every open view of a graph stale -- a boolean per view, no I/O, no
;;    rebuild, it never touches point -- so the save stays "blob + WAL" and the
;;    `glance:stale' lighter shows until the view refreshes.
;;  - PULL (at a SAFE boundary): a view registered with `org-glance-view:register'
;;    re-fills itself when its window is (re)displayed or selected -- where point
;;    is being re-established anyway -- via `org-glance-view--refresh-when-stale'.
;;    A FILE-backed buffer with unsaved edits is never reverted (it is only
;;    flagged), so no user data is discarded.
;;
;; Each view supplies only what genuinely differs: a STALE-FN (its freshness
;; test -- an org-file cache vs an in-memory fill) and a RELOAD-FN (its rebuild).
;; The window-hook wiring, the modified-buffer guard and the error demotion live
;; here, so a new view type adds only a `register' call.

;;; Code:

(require 'cl-lib)
(require 'cl-macs)

(defvar-local org-glance-view--graph nil
  "The graph this buffer is a view of; its presence marks an org-glance view.")

(defvar-local org-glance-view--stale nil
  "Non-nil when this view is behind the store; shown as the `glance:stale'
lighter.")

(defvar-local org-glance-view--stale-fn nil
  "Nullary predicate of the current view: non-nil when it is behind the store.")

(defvar-local org-glance-view--reload-fn nil
  "Nullary thunk of the current view: re-fill it from the graph.")

;;; Display

(defcustom org-glance-view-fill-frame t
  "When non-nil, opening an overview or table view fills the frame.
The view's window becomes the sole one (via `delete-other-windows'), so a graph
view takes over the frame instead of sharing it with whatever windows were open.
Set to nil to leave the existing window layout untouched."
  :group 'org-glance
  :type 'boolean)

(cl-defun org-glance-view:fill-frame (&optional already-in-view)
  "Delete the other windows when `org-glance-view-fill-frame' is non-nil.
Call right after a view buffer has been displayed.  Acts only on a FRESH open:
  - the buffer must be the one shown in the selected window (a no-op otherwise:
    a view opened programmatically or in a stubbed test -- so it never deletes
    windows around an unrelated buffer);
  - ALREADY-IN-VIEW must be nil.  Pass it non-nil when the visit is a
    re-navigation from WITHIN a graph view (a filter change, or the `T' toggle),
    so re-filtering or switching views leaves a deliberate split alone.
Filling the frame is cosmetic, so a `delete-other-windows' signal (a quirky
side/atomic window arrangement) is caught -- it must never break opening a view,
even under `debug-on-error' (hence a plain `condition-case', not
`with-demoted-errors', which re-raises while debugging)."
  (when (and org-glance-view-fill-frame
             (not already-in-view)
             (eq (window-buffer) (current-buffer)))
    (condition-case err
        (delete-other-windows)
      (error (message "org-glance: fill-frame skipped: %S" err)))))

(cl-defun org-glance-view:register (graph &key stale-fn reload-fn)
  "Mark the current buffer a (fresh) view of GRAPH and wire its coherence.
:STALE-FN is a nullary predicate (non-nil = behind the store); :RELOAD-FN
re-fills the view from the graph.  Installs the display-boundary refresh on both
window hooks, so the view pulls itself current whenever it is (re)displayed or
selected."
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
  "Flag every open view buffer of GRAPH stale.
A boolean per view -- no disk write, no rebuild, no point movement -- so it is
safe on the save hot path; each view clears the flag when it next refreshes at a
display boundary (`org-glance-view--refresh-when-stale') or on `g'.
`buffer-local-value' filters without entering every live buffer; only the
matching views are visited."
  (dolist (buffer (buffer-list))
    (when (eq (buffer-local-value 'org-glance-view--graph buffer) graph)
      (with-current-buffer buffer (org-glance-view--mark-stale)))))

(cl-defun org-glance-view--refresh-when-stale (&optional window)
  "Re-fill WINDOW's (or the current) view iff it is behind the store.
The lazy half of coherence: runs at a display boundary, where point is being
re-established anyway.  A FILE-backed buffer with unsaved edits is never
reverted -- it is only flagged stale, so no user data is discarded; a non-file
projection (no `buffer-file-name') reloads freely.  A reload must never break
the save that flagged it, so its errors are demoted."
  (with-current-buffer (if (windowp window) (window-buffer window) (current-buffer))
    (when (and org-glance-view--stale-fn
               (funcall org-glance-view--stale-fn))
      (if (and buffer-file-name (buffer-modified-p))
          (org-glance-view--mark-stale)
        (with-demoted-errors "org-glance: view refresh failed: %S"
          (funcall org-glance-view--reload-fn))))))

;; One global, guarded mode-line element.  The native `(VARIABLE THEN)' construct
;; reads the buffer-local flag directly -- no per-redisplay funcall -- so it shows
;; ` glance:stale' only in a stale view and nothing elsewhere (idempotent across
;; reloads).
(cl-pushnew '(org-glance-view--stale " glance:stale") mode-line-misc-info :test #'equal)

(provide 'org-glance-view)
;;; org-glance-view.el ends here
