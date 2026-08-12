;;; org-glance-tags.el --- all-tags overview (a table-view of tags) -*- lexical-binding: t; -*-


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


(cl-defun org-glance-tags--format-time (ts)
  "Format timestamp TS as `YYYY-MM-DD HH:MM', or empty when nil."
  (if ts (format-time-string "%Y-%m-%d %H:%M" ts) ""))

(cl-defun org-glance-tags--format-states (states)
  "Format STATES (alist STATE -> COUNT) as coloured `STATE N ...', sorted."
  (if states
      (mapconcat (lambda (c) (concat (org-glance-table--colorize-state (car c))
                                     " " (number-to-string (cdr c))))
                 (cl-sort (copy-sequence states) #'string< :key #'car)
                 "  ")
    ""))

(cl-defun org-glance-tags--format-cycle (cycle)
  "Format CYCLE (a `#+TODO:'-style string) with each keyword coloured.
The `|' active/done separator is left plain."
  (if (org-glance--present-string? cycle)
      (mapconcat (lambda (tok)
                   (if (string= tok "|") tok (org-glance-table--colorize-state tok)))
                 (split-string cycle)
                 " ")
    ""))


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
                ((key . "+")   (command . "add")      (label . "Add"))
                ((key . "-")   (command . "remove")   (label . "Remove"))
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



(cl-defun org-glance-tags--tag-filter (tag)
  "TAG's view filter: the tag overlaid on the ambient spec.
Same merge as the o/table entry points, so RET here and the picker open
the SAME view (the dashboard previously bypassed the ambient filter and
showed e.g. archived rows the picker hides)."
  (org-glance-filter:merge org-glance-filter-spec
                           (org-glance-tag:from-string tag)))

(cl-defun org-glance-tags--act-table (graph tag)
  "Open TAG's headline table (`org-glance-table' mode) from GRAPH."
  (org-glance-table:visit graph (org-glance-tags--tag-filter tag)))

(cl-defun org-glance-tags--act-overview (graph tag)
  "Open the overview of TAG (a string) from GRAPH."
  (org-glance-overview:visit graph (org-glance-tags--tag-filter tag)))

(cl-defun org-glance-tags--retag-remove (graph tag-string ids)
  "Drop TAG-STRING off each headline in IDS via `org-glance-material:retag'.
An id whose blob buffer has unsaved edits (retag's `user-error') is skipped.
Return (CHANGED . SKIPPED)."
  (let ((changed 0) (skipped 0))
    (dolist (id ids)
      (condition-case nil
          (when (org-glance-material:retag graph id tag-string :remove t)
            (cl-incf changed))
        (user-error (cl-incf skipped))))
    (cons changed skipped)))

(cl-defun org-glance-tags--act-remove (graph tag-string)
  "Remove tag TAG-STRING from GRAPH: drop it off each headline, after confirming.
Non-destructive -- multi-tagged headlines stay alive under their other tags; the
tag vanishes once no live headline carries it."
  (let* ((metas (cl-remove-if-not
                 (lambda (m) (member tag-string
                                     (org-glance-headline-metadata:tag-strings m)))
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
        (org-glance-table--reload (current-buffer))
        (message "Removed `%s' from %d headline(s)%s"
                 tag-string (car res)
                 (if (> (cdr res) 0)
                     (format " (%d skipped: unsaved edits)" (cdr res))
                   "")))))))


(cl-defun org-glance-tags:visit (graph)
  "Open GRAPH's all-tags overview in the single `*org-glance-tags*' buffer."
  (let ((src (org-glance-graph:headline-meta-path graph))
        (handlers (list (cons "table"    (lambda (id _row) (org-glance-tags--act-table graph id)))
                        (cons "overview" (lambda (id _row) (org-glance-tags--act-overview graph id)))
                        ;; a tag exists only on a headline, so "add" is capture
                        (cons "add"      (lambda (_id _row)
                                           (call-interactively #'org-glance-capture)))
                        (cons "remove"   (lambda (id _row) (org-glance-tags--act-remove graph id)))
                        (cons "refresh"  (lambda (_id _row) (org-glance-table--reload (current-buffer)))))))
    (org-glance-view:display-table
     graph "*org-glance-tags*" (org-glance-tags--spec) handlers
     (lambda (buf)
       (with-current-buffer buf
         (table-view-set-rows buf (org-glance-tags--rows graph))
         (org-glance-view:snapshot-mtime src)))
     :stale-fn  (lambda () (org-glance-view:stale-vs-file? src))
     :reload-fn (lambda () (org-glance-table--reload (current-buffer))))))

;;;###autoload
(cl-defun org-glance-tags ()
  "Open the all-tags overview: a table of tags with per-tag metrics."
  (interactive)
  (org-glance-ensure-init)
  (org-glance-tags:visit org-glance-graph))

(provide 'org-glance-tags)
