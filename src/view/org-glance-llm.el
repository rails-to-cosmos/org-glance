;;; org-glance-llm.el --- LLM session for a headline (agnostic-llm) -*- lexical-binding: t; -*-

;;; Commentary:
;; The `l' action: pick a headline and open the `agnostic-llm' menu pinned to
;; the headline's content-addressable data directory, so the CLI's per-directory
;; context accumulates there.  The `*llm:…*' session buffer is named for the
;; headline's title (see `org-glance-llm--label'), not the data dir's hash.
;;
;; A headline's session is identified by that data DIR -- a full-hash path unique
;; to the headline, kept as the session buffer's `default-directory'.  Reopening
;; the headline switches to the live buffer instead of prompting again; the title
;; label is only cosmetic, so two same-titled headlines never share a session.

;;; Code:

(require 'cl-lib)
(require 'table-view)
(require 'org-glance-core)
(require 'org-glance-utils)
(require 'org-glance-graph)
(require 'org-glance-filter)
(require 'org-glance-tag-config)
(require 'org-glance-material)
(require 'org-glance-property-index)
(require 'org-glance-view)

;; `agnostic-llm-menu' is a required dependency, but loaded lazily through its
;; autoload only when the command runs, so org-glance never pulls in vterm at
;; load or byte-compile time.
(declare-function agnostic-llm-menu "agnostic-llm" (&optional root label))

(cl-defun org-glance-llm--slug (title)
  "Downcased dash-separated slug of TITLE, or nil when it has no word chars.
Non-alphanumeric runs become one dash, edges trimmed (\"Buy milk (2L)!\" ->
\"buy-milk-2l\")."
  (let ((slug (string-trim
               (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase (or title "")))
               "-+" "-+")))
    (unless (string-empty-p slug) slug)))

(cl-defun org-glance-llm--buffer-name (label)
  "The `*llm:...*' session buffer name for LABEL."
  (format "*llm:%s*" label))

(cl-defun org-glance-llm--session-buffer? (buf)
  "Non-nil when BUF is a `*llm:…*' session buffer."
  (string-prefix-p "*llm:" (buffer-name buf)))

(cl-defun org-glance-llm--session-buffer (dir)
  "The live agnostic-llm session buffer rooted at DIR, or nil.
DIR is a headline's content-addressable data dir -- a full-hash path unique to
the headline -- so matching a `*llm:…*' buffer's `default-directory' against it
reuses that session, independent of the (cosmetic) title label."
  (cl-find-if (lambda (buf)
                (and (org-glance-llm--session-buffer? buf)
                     (ignore-errors
                       (file-equal-p (buffer-local-value 'default-directory buf)
                                     dir))))
              (buffer-list)))

(cl-defun org-glance-llm--label (metadata dir)
  "The `*llm:...*' label for METADATA's session rooted at DIR.
METADATA's title slug (`--slug'), reading nicer than DIR's content-hash leaf.
Falls back to that leaf when the title slugs to nothing, and appends it to
disambiguate when a session for a DIFFERENT headline already holds the slug."
  (let* ((tail (file-name-nondirectory (directory-file-name dir)))
         (slug (or (org-glance-llm--slug (org-glance-headline-metadata:title metadata))
                   tail))
         (buf (get-buffer (org-glance-llm--buffer-name slug))))
    (if (and buf (not (file-equal-p (buffer-local-value 'default-directory buf)
                                    dir)))
        (format "%s-%s" slug tail)
      slug)))

(cl-defun org-glance-llm--dir (graph id)
  "Session directory for headline ID in GRAPH.
The headline's `ORG_GLANCE_PROJECT_DIR' drawer property when set (a user-chosen
project dir; see `org-glance-material:set-project-dir'), else its
content-addressable data dir."
  (let ((override (org-glance-property-index:property
                   graph id org-glance-project-dir-property)))
    ;; Directory-valued: always trailing-slashed, as `default-directory' expects.
    (file-name-as-directory
     (if (org-glance--present-string? override)
         (expand-file-name (string-trim override))
       (org-glance-graph:headline-data-path graph id)))))

;;;###autoload
(cl-defun org-glance-llm ()
  "Choose a headline and open its `agnostic-llm' session.
Prompt for a headline like `org-glance-materialize'.  If that headline's session
buffer is already live, switch to it.  Otherwise materialize the headline and,
from its blob buffer, open `agnostic-llm-menu' with the session directory and
buffer name overridden.  The session directory is the headline's
`ORG_GLANCE_PROJECT_DIR' property when set (see `org-glance-llm--dir'), else its
content-addressable data dir; the label is the title slug
(`org-glance-llm--label').  The menu highlights the override."
  (interactive)
  (org-glance-ensure-init)
  (let* ((graph org-glance-graph)
         (metadata (org-glance-material:pick-metadata graph))
         (id (org-glance-headline-metadata:id metadata))
         (dir (org-glance-llm--dir graph id))
         (label (org-glance-llm--label metadata dir))
         (existing (org-glance-llm--session-buffer dir)))
    (if (buffer-live-p existing)
        (switch-to-buffer existing)
      (make-directory dir t)
      ;; Materialize the headline, then launch the menu pinned to DIR (its
      ;; project dir, or the data dir) as the session root.
      (switch-to-buffer (org-glance-material:open graph id))
      (agnostic-llm-menu dir label))))

;;; Sessions table (`L' in the transient)
;;
;; One row per LLM session -- a state machine over its lifetime: `running'
;; (live buffer, live process), `exited' (live buffer, dead process), and
;; `stopped' (no buffer, but the provider recorded a transcript for the
;; headline's session dir).  Rows come from every live `*llm:…*' buffer plus
;; every headline whose session dir has a recorded transcript.  Like the
;; entry command, `agnostic-llm' (and thus vterm) loads lazily at run time.

(declare-function agnostic-llm "agnostic-llm" (&optional user-root label))
(declare-function agnostic-llm--provider-get "agnostic-llm" (key))
(declare-function agnostic-llm--session-dir "agnostic-llm" (dir))
(declare-function agnostic-llm--session-file "agnostic-llm" (dir))
(declare-function agnostic-llm--prompt-history-files "agnostic-llm" (&optional root))
(declare-function agnostic-llm--prompt-preview "agnostic-llm" (file))
(defvar agnostic-llm--root-override)

(cl-defun org-glance-llm--buffer-label (buf)
  "LABEL of BUF's `*llm:LABEL*' name (uniquifying `<N>' suffix dropped)."
  (let ((name (buffer-name buf)))
    (if (string-match "\\`\\*llm:\\(.*?\\)\\*\\(<[0-9]+>\\)?\\'" name)
        (match-string 1 name)
      name)))

(cl-defun org-glance-llm--buffer-dir (buf)
  "BUF's session root: its `agnostic-llm--root-override', else its
`default-directory', as a directory name."
  (file-name-as-directory
   (expand-file-name
    (or (and (local-variable-p 'agnostic-llm--root-override buf)
             (buffer-local-value 'agnostic-llm--root-override buf))
        (buffer-local-value 'default-directory buf)))))

(cl-defun org-glance-llm--live-buffers ()
  "Every live `*llm:…*' session buffer."
  (cl-remove-if-not #'org-glance-llm--session-buffer? (buffer-list)))

(cl-defun org-glance-llm--state (buf)
  "Session state for BUF (nil = no buffer): running / exited / stopped."
  (let ((proc (and buf (get-buffer-process buf))))
    (cond ((null buf) "stopped")
          ((and proc (process-live-p proc)) "running")
          (t "exited"))))

(cl-defun org-glance-llm--session-row (dir buf transcript title id)
  "Row for the session at DIR: live BUF (or nil), newest TRANSCRIPT file (or
nil), TITLE, owning headline ID (or nil)."
  `((id . ,dir)
    (headline . ,id)
    (cells . ((title . ,(or title ""))
              (state . ,(org-glance-llm--state buf))
              (buffer . ,(if buf (buffer-name buf) ""))
              (last . ,(if transcript
                           (format-time-string
                            "%Y-%m-%d %H:%M"
                            (file-attribute-modification-time
                             (file-attributes transcript)))
                         ""))
              (prompt . ,(if-let ((prompt (org-glance-llm--last-prompt dir)))
                             (truncate-string-to-width prompt 48 nil nil "…")
                           ""))
              (dir . ,(abbreviate-file-name dir))))))

(cl-defun org-glance-llm--last-prompt (dir)
  "One-line preview of DIR's newest saved prompt, or nil."
  (when-let ((file (car (agnostic-llm--prompt-history-files dir))))
    (agnostic-llm--prompt-preview file)))

(cl-defun org-glance-llm--session-rows (graph &optional keep?)
  "One row per LLM session among GRAPH's headlines passing KEEP? (nil = all).
A headline contributes when its session dir has a live `*llm:…*' buffer or a
recorded provider transcript; orphan live sessions (no owning headline) appear
only in the unfiltered view.  The provider store is listed ONCE; each headline
dir is then membership-checked by its encoded name.  KEEP? also bounds the
per-headline property-index lookups (`org-glance-llm--dir'), which on a cold
index cost a blob parse each -- the reason the unfiltered whole-graph view is
the slow path."
  (let* ((store (agnostic-llm--provider-get :session-dir))
         (recorded (and (file-directory-p store)
                        (directory-files store nil
                                         directory-files-no-dot-files-regexp t)))
         (buf-by-dir (make-hash-table :test 'equal))
         rows)
    (dolist (buf (org-glance-llm--live-buffers))
      (puthash (org-glance-llm--buffer-dir buf) buf buf-by-dir))
    (dolist (meta (if keep?
                      (cl-remove-if-not keep? (org-glance-graph--metas graph))
                    (org-glance-graph--metas graph)))
      (let* ((id (org-glance-headline-metadata:id meta))
             (dir (org-glance-llm--dir graph id))
             (buf (gethash dir buf-by-dir))
             (transcript (and (member (file-name-nondirectory
                                       (agnostic-llm--session-dir dir))
                                      recorded)
                              (agnostic-llm--session-file dir))))
        (when (or buf transcript)
          (remhash dir buf-by-dir)
          (push (org-glance-llm--session-row
                 dir buf transcript
                 (org-glance-headline-metadata:title meta) id)
                rows))))
    ;; live sessions not owned by any headline -- unfiltered view only
    (unless keep?
      (maphash (lambda (dir buf)
                 (push (org-glance-llm--session-row
                        dir buf (agnostic-llm--session-file dir)
                        (org-glance-llm--buffer-label buf) nil)
                       rows))
               buf-by-dir))
    (nreverse rows)))

(defconst org-glance-llm--sessions-spec
  '((title . "org-glance llm sessions")
    (columns . (((key . "state")  (header . "State")  (type . "badge") (sortable . t) (align . "left")
                 (badges . (((value . "running") (color . "#9ece6a"))
                            ((value . "exited")  (color . "#e0af68"))
                            ((value . "stopped") (color . "#565f89")))))
                ((key . "title")  (header . "Title")  (type . "text") (sortable . t) (align . "left"))
                ((key . "buffer") (header . "Buffer") (type . "text") (sortable . t) (align . "left"))
                ((key . "last")   (header . "Last")   (type . "text") (sortable . t) (align . "left"))
                ((key . "prompt") (header . "Prompt") (type . "text") (sortable . nil) (align . "left"))
                ((key . "dir")    (header . "Dir")    (type . "text") (sortable . t) (align . "left"))))
    (actions . (((key . "RET") (command . "open")        (label . "Open"))
                ((key . "m")   (command . "materialize") (label . "Materialize"))
                ((key . "k")   (command . "kill")        (label . "Kill"))
                ((key . "g")   (command . "refresh")     (label . "Refresh"))))
    (sort . ((column . "state") (ascending . t))))
  "The `table-view' spec for the LLM sessions table.
A constant: unlike `org-glance-table--spec' it depends on no graph state,
and `table-view-display' never mutates a passed spec.")

(cl-defun org-glance-llm--act-open (graph dir row)
  "Pop to DIR's running session; else (re)start it, continuing its transcript.
The label: the owning headline's title slug, else the old buffer's label,
else DIR's leaf."
  (let ((buf (org-glance-llm--session-buffer dir)))
    (if (and (buffer-live-p buf)
             (process-live-p (get-buffer-process buf)))
        (pop-to-buffer buf)
      (let* ((id (alist-get 'headline row))
             (meta (and id (org-glance-graph:get-headline graph id)))
             (label (cond ((org-glance-headline-metadata? meta)
                           (org-glance-llm--label meta dir))
                          (buf (org-glance-llm--buffer-label buf))
                          (t (file-name-nondirectory (directory-file-name dir))))))
        (agnostic-llm dir label)))))

(cl-defun org-glance-llm--act-kill (dir)
  "Kill DIR's live session buffer, after confirming."
  (let ((buf (org-glance-llm--session-buffer dir)))
    (cond ((not (buffer-live-p buf)) (user-error "No live session buffer"))
          ((y-or-n-p (format "Kill %s? " (buffer-name buf)))
           (let ((kill-buffer-query-functions nil))
             (kill-buffer buf))
           (table-view-refresh (current-buffer))
           (table-view-apply-sort)))))

(cl-defun org-glance-llm--act-materialize (graph row)
  "Materialize the row's owning headline."
  (let ((id (alist-get 'headline row)))
    (unless id (user-error "This session belongs to no headline"))
    (switch-to-buffer (org-glance-material:open graph id))))

(cl-defun org-glance-llm-sessions:visit (graph &optional filter)
  "Open GRAPH's LLM sessions table for FILTER, one buffer per description.
Honours the same filter language as the overview and table."
  (let* ((spec (org-glance-filter:normalize-spec filter))
         ;; Bind the filter's done split exactly like `org-glance-table:visit',
         ;; so a `:done' clause agrees with the tag's own todo cycle.
         (cycle (org-glance-tag-config:cycle-for-filter graph spec))
         (org-done-keywords (if cycle (org-glance-tag-config:done-keywords cycle)
                              (org-glance--done-keywords)))
         (keep? (and spec (org-glance-filter:predicate spec)))
         (name (format "*org-glance-llm-sessions: %s*"
                       (org-glance-filter:describe spec)))
         (fill-fn (lambda (buf)
                    (with-current-buffer buf
                      (table-view-set-rows
                       buf (org-glance-llm--session-rows graph keep?)))))
         (handlers (list (cons "open" (lambda (id row) (org-glance-llm--act-open graph id row)))
                         (cons "materialize" (lambda (_id row) (org-glance-llm--act-materialize graph row)))
                         (cons "kill" (lambda (id _row) (org-glance-llm--act-kill id)))
                         (cons "refresh" (lambda (_id _row)
                                           (table-view-refresh (current-buffer))
                                           (table-view-apply-sort))))))
    (org-glance-view:display-table graph name
                                   org-glance-llm--sessions-spec handlers fill-fn)))

;;;###autoload
(cl-defun org-glance-llm-sessions (&optional tag)
  "Table of the tag's LLM sessions, running, exited, or stopped.
Prompt for a tag (empty input = all) and overlay it on the ambient
`org-glance-filter-spec' -- exactly like `org-glance-table'.  Rows: matching
headlines with a live `*llm:…*' buffer or a recorded provider transcript
\(plus, in the unfiltered view, live sessions owned by no headline).  RET
pops to a running session or (re)starts a stopped one; `m' materializes the
owning headline; `k' kills a live buffer.  Loads `agnostic-llm' (and vterm)
lazily, like `org-glance-llm'."
  (interactive (list (org-glance-view:completing-read-tag
                      "LLM sessions tag (empty for all): ")))
  (org-glance-ensure-init)
  (require 'agnostic-llm)
  (org-glance-llm-sessions:visit org-glance-graph
                                 (org-glance-filter:merge org-glance-filter-spec tag)))

(provide 'org-glance-llm)
;;; org-glance-llm.el ends here
