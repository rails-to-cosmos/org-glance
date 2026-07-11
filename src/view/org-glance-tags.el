;;; org-glance-tags.el --- all-tags overview (a table-view of tags) -*- lexical-binding: t; -*-

;;; Commentary:
;; A table-view whose rows are TAGS (not headlines): count, todo-state
;; breakdown, the tag's configured cycle, and event-tracked created/modified
;; timestamps (see `org-glance-tag-metrics').  Row actions: open the tag's
;; overview (o/RET), add a tag (+), remove a tag (-, a guarded non-destructive
;; retag).  Bound to "t" in `org-glance-transient'.  Reuses the table-view
;; package and `org-glance-view' coherence like `org-glance-table'.

;;; Code:

(require 'cl-lib)
(require 's)
(require 'org)
(require 'table-view)
(require 'org-glance-utils)
(require 'org-glance-tag)
(require 'org-glance-graph)
(require 'org-glance-tag-config)
(require 'org-glance-tag-metrics)
(require 'org-glance-filter)
(require 'org-glance-view)
(require 'org-glance-overview)
(require 'org-glance-table)
(require 'org-glance-material)
(require 'org-glance-capture)

(require 'org-glance-core)

(defvar-local org-glance-tags--mtime nil
  "Mtime of `headlines.jsonl' at the tags buffer's last fill (staleness snapshot).")

;;; Formatting

(cl-defun org-glance-tags--format-time (ts)
  "Format timestamp TS as `YYYY-MM-DD HH:MM', or empty when nil."
  (if ts (format-time-string "%Y-%m-%d %H:%M" ts) ""))

(cl-defun org-glance-tags--colorize-state (state)
  "STATE (a string) propertized with its todo-state colour.
Uses the same palette as the headline table's state badges."
  (propertize state 'face
              (list :foreground (org-glance-table--state-color state) :weight 'bold)))

(cl-defun org-glance-tags--format-states (states)
  "Format STATES (alist STATE -> COUNT) as coloured `STATE N ...', sorted."
  (if states
      (mapconcat (lambda (c) (concat (org-glance-tags--colorize-state (car c))
                                     " " (number-to-string (cdr c))))
                 (cl-sort (copy-sequence states) #'string< :key #'car)
                 "  ")
    ""))

(cl-defun org-glance-tags--format-cycle (cycle)
  "Format CYCLE (a `#+TODO:'-style string) with each keyword coloured.
The `|' active/done separator is left plain."
  (if (org-glance--present-string? cycle)
      (mapconcat (lambda (tok)
                   (if (string= tok "|") tok (org-glance-tags--colorize-state tok)))
                 (split-string cycle)
                 " ")
    ""))

;;; Spec / rows

(cl-defun org-glance-tags--spec ()
  "The `table-view' spec for the all-tags overview (rows are tags)."
  '((title . "org-glance tags")
    (columns . (((key . "tag")      (header . "Tag")      (type . "text") (sortable . t)   (align . "left"))
                ((key . "count")    (header . "N")        (type . "text") (sortable . t)   (align . "right"))
                ((key . "states")   (header . "States")   (type . "text") (sortable . nil) (align . "left"))
                ((key . "config")   (header . "Cycle")    (type . "text") (sortable . nil) (align . "left"))
                ((key . "created")  (header . "Created")  (type . "text") (sortable . t)   (align . "left"))
                ((key . "modified") (header . "Modified") (type . "text") (sortable . t)   (align . "left"))))
    (actions . (((key . "RET") (command . "table")    (label . "Table"))
                ((key . "o")   (command . "overview") (label . "Overview"))
                ((key . "+")   (command . "add")      (label . "Add tag"))
                ((key . "-")   (command . "remove")   (label . "Remove tag"))
                ((key . "g")   (command . "refresh")  (label . "Refresh"))))
    (sort . ((column . "tag") (ascending . t)))))

(cl-defun org-glance-tags--row (graph tag plist)
  "Build a `table-view' row for TAG (a string) from its metrics PLIST.
The row id is the tag string; the Cycle cell comes from the tag's config."
  (let ((cfg (ignore-errors
               (org-glance-tag-config:resolve graph (org-glance-tag:from-string tag)))))
    `((id . ,tag)
      (cells . ((tag      . ,tag)
                (count    . ,(number-to-string (or (plist-get plist :count) 0)))
                (states   . ,(org-glance-tags--format-states (plist-get plist :states)))
                (config   . ,(org-glance-tags--format-cycle
                              (and cfg (org-glance-tag-config:todo cfg))))
                (created  . ,(org-glance-tags--format-time (plist-get plist :created)))
                (modified . ,(org-glance-tags--format-time (plist-get plist :modified))))))))

(cl-defun org-glance-tags--rows (graph)
  "All tag rows for GRAPH, one per live tag."
  (cl-loop for entry in (org-glance-tag-metrics:all graph)
           collect (org-glance-tags--row graph (car entry) (cdr entry))))

;;; Coherence (non-file projection, mirrors `org-glance-table')

(cl-defun org-glance-tags--stale? (graph)
  "Non-nil when the tags buffer is behind GRAPH's store."
  (let ((src (org-glance-graph:headline-meta-path graph)))
    (or (null org-glance-tags--mtime)
        (time-less-p org-glance-tags--mtime (org-glance--file-mtime src)))))

(cl-defun org-glance-tags--reload (buffer)
  "Re-fill BUFFER from the graph, re-apply the sort, and keep point in place."
  (when-let ((buf (get-buffer buffer)))
    (with-current-buffer buf
      (let ((id (get-text-property (point) 'table-view-id))
            (line (line-number-at-pos)))
        (table-view-refresh buf)
        (table-view-apply-sort)
        (org-glance-view:mark-fresh)
        (unless (and id (table-view--goto-id id))
          (goto-char (point-min))
          (forward-line (1- line)))))))

;;; Actions

(cl-defun org-glance-tags--tag-filter (tag)
  "Normalised single-tag filter spec for TAG (a string)."
  (org-glance-filter:normalize-spec
   (list :tags (list (org-glance-tag:from-string tag)))))

(cl-defun org-glance-tags--act-table (graph tag)
  "Open TAG's headline table (`org-glance-table' mode) from GRAPH."
  (org-glance-table:visit graph (org-glance-tags--tag-filter tag)))

(cl-defun org-glance-tags--act-overview (graph tag)
  "Open the overview of TAG (a string) from GRAPH."
  (org-glance-overview:visit graph (org-glance-tags--tag-filter tag)))

(cl-defun org-glance-tags--act-add (_graph)
  "Add a tag by capturing a first headline that carries it.
A tag exists only by carrying a headline, so this runs the standard capture
(prompting for the tag -- new tags allowed -- and a title)."
  (call-interactively #'org-glance-capture))

(cl-defun org-glance-tags--retag-remove (graph tag ids)
  "Drop TAG (a symbol) off each headline in IDS via materialize + save.
Skip an id whose blob buffer already has unsaved edits.  Return (CHANGED .
SKIPPED)."
  (let ((tag-string (format "%s" tag))
        (changed 0) (skipped 0))
    (dolist (id ids)
      (let* ((path (org-glance-graph:content-path graph id))
             (existing (find-buffer-visiting path)))
        (if (and existing (buffer-modified-p existing))
            (cl-incf skipped)
          (let ((buffer (org-glance-material:open graph id)))
            (unwind-protect
                (with-current-buffer buffer
                  (goto-char (point-min))
                  (org-set-tags (remove tag-string (org-get-tags nil t)))
                  (let ((inhibit-message t)) (save-buffer))
                  (cl-incf changed))
              (unless existing
                (org-glance--discard-buffer buffer)))))))
    (cons changed skipped)))

(cl-defun org-glance-tags--act-remove (graph tag-string)
  "Remove tag TAG-STRING from GRAPH: drop it off each headline, after confirming.
Non-destructive -- multi-tagged headlines stay alive under their other tags; the
tag vanishes once no live headline carries it."
  (let* ((metas (cl-remove-if-not
                 (lambda (m) (member tag-string
                                     (mapcar (lambda (x) (format "%s" x))
                                             (org-glance-headline-metadata:tags m))))
                 (org-glance-graph:headlines graph)))
         (ids (mapcar #'org-glance-headline-metadata:id metas))
         (multi (cl-some (lambda (m) (> (length (org-glance-headline-metadata:tags m)) 1))
                         metas)))
    (cond
     ((null ids) (message "org-glance: no live headlines carry `%s'" tag-string))
     ((yes-or-no-p (format "Remove tag `%s' from %d headline(s)%s? "
                           tag-string (length ids)
                           (if multi
                               " (multi-tagged headlines stay under their other tags)"
                             "")))
      (let ((res (org-glance-tags--retag-remove graph tag-string ids)))
        (org-glance-tags--reload (current-buffer))
        (message "Removed `%s' from %d headline(s)%s"
                 tag-string (car res)
                 (if (> (cdr res) 0)
                     (format " (%d skipped: unsaved edits)" (cdr res))
                   "")))))))

;;; Visit + entry point

(cl-defun org-glance-tags:visit (graph)
  "Open GRAPH's all-tags overview in the single `*org-glance-tags*' buffer."
  (let* ((from-view (and org-glance-view--graph t))
         (src (org-glance-graph:headline-meta-path graph))
         (fill-fn (lambda (buf)
                    (with-current-buffer buf
                      (table-view-set-rows buf (org-glance-tags--rows graph))
                      (setq org-glance-tags--mtime (org-glance--file-mtime src)))))
         (handlers (list (cons "table"    (lambda (id _row) (org-glance-tags--act-table graph id)))
                         (cons "overview" (lambda (id _row) (org-glance-tags--act-overview graph id)))
                         (cons "add"      (lambda (_id _row) (org-glance-tags--act-add graph)))
                         (cons "remove"   (lambda (id _row) (org-glance-tags--act-remove graph id)))
                         (cons "refresh"  (lambda (_id _row) (org-glance-tags--reload (current-buffer))))))
         (buf (table-view-display "*org-glance-tags*" (org-glance-tags--spec) handlers fill-fn)))
    (with-current-buffer buf
      (setq default-directory (file-name-as-directory (org-glance-graph:directory graph)))
      (org-glance-view:register graph
                                :stale-fn  (lambda () (org-glance-tags--stale? graph))
                                :reload-fn (lambda () (org-glance-tags--reload (current-buffer))))
      (table-view-apply-sort)
      (org-glance-view:fill-frame from-view))
    buf))

;;;###autoload
(cl-defun org-glance-tags ()
  "Open the all-tags overview: a table of tags with per-tag metrics."
  (interactive)
  (org-glance-ensure-init)
  (org-glance-tags:visit org-glance-graph))

(provide 'org-glance-tags)
;;; org-glance-tags.el ends here
