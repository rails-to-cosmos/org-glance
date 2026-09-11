;; -*- lexical-binding: t -*-

;;; org-glance-overview.el --- graph-backed overview + agenda

;;; Code:

(require 'cl-lib)
(require 'f)
(require 's)
(require 'org)
(require 'org-agenda)
(require 'transient)

(require 'org-glance-graph)
(require 'org-glance-filter)
(require 'org-glance-tag-config)
(require 'org-glance-material)
(require 'org-glance-view)

(require 'org-glance-core)
(declare-function org-glance-table:visit "org-glance-table")
(declare-function org-glance-table:visit-relations "org-glance-table" (graph id))
(declare-function org-glance-capture "org-glance-capture")
(declare-function org-glance-capture:completing-read-tag "org-glance-capture")

(defconst org-glance-overview:header
  "#    -*- mode: org; mode: org-glance-overview -*-\n#+TITLE: org-glance overview\n\n"
  "Prop-line header written at the top of the overview file.")

(cl-defun org-glance-overview:spec-key (filter)
  "Return a deterministic cache key for FILTER.
\"all\" for the empty filter, nil for a transient one, else the first 12 hex
chars of the SHA-1 of its `org-glance-filter:identity'.
`org-glance-overview:cached-file' rebuilds on a prefix collision."
  (let ((spec (org-glance-filter:normalize-spec filter)))
    (cond
     ((null spec) "all")
     ((org-glance-filter:transient? spec) nil)   ; :where / relation views: never cached
     (t (substring (secure-hash 'sha1 (org-glance-filter:identity spec))
                   0 12)))))

(cl-defun org-glance-overview--spec-sidecar (file)
  "Path of the SPEC identity sidecar stored next to cache FILE."
  (f-join (f-dirname file) "SPEC"))

(cl-defun org-glance-overview--spec-owns-cache? (filter file)
  "Non-nil when FILE's SPEC sidecar records exactly FILTER's identity."
  (let ((sidecar (org-glance-overview--spec-sidecar file)))
    (and (f-exists? sidecar)
         (string= (org-glance-filter:identity filter)
                  (s-trim (f-read-text sidecar 'utf-8))))))

(cl-defun org-glance-overview:render-headline (graph metadata)
  "Render METADATA as one self-sufficient org heading (invariant 20).
Relation titles resolve live from GRAPH, falling back to a gone target's id."
  (let ((state (org-glance-headline-metadata:state metadata))
        (priority (org-glance-headline-metadata:priority metadata))
        (schedule (org-glance-headline-metadata:schedule metadata))
        (deadline (org-glance-headline-metadata:deadline metadata)))
    (concat "* "
            (if (org-glance--present-string? state) (concat state " ") "")
            (if (integerp priority) (format "[#%c] " priority) "")
            (org-glance-headline-metadata:title metadata)
            (if-let* ((tags (org-glance-headline-metadata:tag-strings metadata)))
                (format "  :%s:" (s-join ":" tags)) "")
            "\n"
            ;; Planning keywords parse ONLY on the line right after the heading.
            (let ((planning
                   (concat (when (org-glance--present-string? deadline)
                             (concat "DEADLINE: " deadline " "))
                           (when (org-glance--present-string? schedule)
                             (concat "SCHEDULED: " schedule)))))
              (unless (string-empty-p planning)
                (concat (s-trim-right planning) "\n")))
            ":PROPERTIES:\n:ORG_GLANCE_ID: " (org-glance-headline-metadata:id metadata) "\n:END:\n"
            ;; Verbatim interval: agenda shows the span (invariant 20).
            (pcase (org-glance-headline-metadata:range metadata)
              (`(,from ,to) (concat from "--" to "\n"))
              (_ ""))
            (apply #'concat
                   (cl-loop for (target . kind) in (org-glance-headline-metadata:relations metadata)
                            collect (concat "- "
                                            (org-glance--edge->string
                                             target kind (org-glance-graph:title-or-id graph target))
                                            "\n")))
            (apply #'concat
                   (cl-loop for link in (org-glance-headline-metadata:links metadata)
                            collect (concat "- " link "\n"))))))

(cl-defun org-glance-overview:render (graph &optional filter)
  "Render GRAPH's live headlines matching FILTER as org text.
FILTER is nil (all), a bare tag, or a plist (`org-glance-filter:predicate').
Prefix the pragmas FILTER's tags agree on; select by their cycle's done-set."
  (cl-check-type graph org-glance-graph)
  (let* ((cycle (org-glance-tag-config:cycle-for-filter graph filter))
         (org-done-keywords (if cycle
                                (org-glance-tag-config:done-keywords cycle)
                              org-done-keywords))
         (keep? (org-glance-filter:predicate filter)))
    ;; Join ONCE: `cl-loop ... concat' re-copies the accumulator -- O(N^2).
    (apply #'concat
           org-glance-overview:header
           (or (org-glance-tag-config:preamble-for-filter graph filter) "")
           (cl-loop for meta in (org-glance-graph:headlines graph)
                    when (funcall keep? meta)
                    collect (org-glance-overview:render-headline graph meta)))))

(cl-defun org-glance-overview:file (graph)
  "Path to GRAPH's unfiltered overview file (inside the hidden store)."
  (cl-check-type graph org-glance-graph)
  (f-join (org-glance-graph:store-path graph) "overview.org"))

(cl-defun org-glance-overview:cache-path (graph)
  "Directory holding GRAPH's filtered (cached) overviews."
  (cl-check-type graph org-glance-graph)
  (f-join (org-glance-graph:store-path graph) "overviews"))

(cl-defun org-glance-overview:spec-cache-file (graph filter)
  "Return the file backing FILTER's overview under GRAPH.
`org-glance-overview:file' for the empty filter, a shared `transient.org' for
a transient one, else `<cache-path>/<key>/overview.org'."
  (let ((key (org-glance-overview:spec-key filter)))
    (cond
     ((null key) (f-join (org-glance-overview:cache-path graph) "transient.org"))
     ((string= key "all") (org-glance-overview:file graph))
     (t (f-join (org-glance-overview:cache-path graph) key "overview.org")))))

(cl-defun org-glance-overview--fresher-than? (file-mtime src-mtime)
  "Non-nil if SRC-MTIME is absent or FILE-MTIME is STRICTLY newer."
  (or (null src-mtime)
      (time-less-p src-mtime file-mtime)))

(cl-defun org-glance-overview:fresh? (graph file)
  "Non-nil if FILE exists and is newer than every source it renders from.
The sources: GRAPH's `headlines.jsonl', its `EXTERNAL.jsonl' (an outside edit
invalidates before it is folded in) and the per-tag config files."
  (cl-check-type graph org-glance-graph)
  (when-let* ((mtime (org-glance--file-mtime file)))
    (cl-every (lambda (src) (org-glance-overview--fresher-than? mtime src))
              (list (org-glance--file-mtime
                     (org-glance-graph:headline-meta-path graph))
                    (org-glance--file-mtime
                     (org-glance-graph:external-path graph))
                    (org-glance-tag-config:source-mtime graph)))))

(cl-defun org-glance-overview--header-current? (file)
  "Non-nil if FILE begins with the prop-line of `org-glance-overview:header'."
  (let ((prop-line (car (s-lines org-glance-overview:header))))
    (with-temp-buffer
      (insert-file-contents file nil 0 (+ 16 (length prop-line)))
      (looking-at-p (regexp-quote prop-line)))))

(cl-defun org-glance-overview:write (graph &optional filter)
  "Regenerate FILTER's overview file for GRAPH unconditionally; return its path.
A keyed cache directory also gets a SPEC sidecar recording FILTER's identity."
  (let ((key (org-glance-overview:spec-key filter))
        (file (org-glance-overview:spec-cache-file graph filter)))
    (f-mkdir-full-path (f-dirname file))
    (unless (member key '(nil "all"))
      (f-write-text (concat (org-glance-filter:identity filter) "\n") 'utf-8
                    (org-glance-overview--spec-sidecar file)))
    (f-write-text (org-glance-overview:render graph filter) 'utf-8 file)
    file))

(cl-defun org-glance-overview:cached-file (graph &optional filter)
  "Return FILTER's overview file for GRAPH, rebuilding only when stale.
A hit -- fresh, current header, and a SPEC sidecar naming FILTER unless the key
is \"all\" -- skips reading and rendering; a transient filter re-renders."
  (let ((key (org-glance-overview:spec-key filter))
        (file (org-glance-overview:spec-cache-file graph filter)))
    (cond
     ((null key)                                       ; :where -- never cache
      (org-glance-overview:write graph filter))
     ((and (org-glance-overview:fresh? graph file)
           (org-glance-overview--header-current? file)
           (or (string= key "all")                     ; "all" never collides
               (org-glance-overview--spec-owns-cache? filter file)))
      file)                                            ; hit -- no read, no render
     (t (org-glance-overview:write graph filter)))))

(defvar org-glance-overview-mode-map (make-sparse-keymap)
  "Keymap for `org-glance-overview-mode'.")

(define-minor-mode org-glance-overview-mode
  "Read-only browser over the graph."
  :global nil
  :init-value nil
  :keymap org-glance-overview-mode-map
  :after-hook (read-only-mode +1)
  (when org-glance-overview-mode
    ;; org needs tab-width 8 to parse node properties (invariant 12).
    (setq tab-width 8 indent-tabs-mode nil)))

(define-key org-glance-overview-mode-map (kbd "n") #'org-next-visible-heading)
(define-key org-glance-overview-mode-map (kbd "p") #'org-previous-visible-heading)
(define-key org-glance-overview-mode-map (kbd "f") #'org-forward-heading-same-level)
(define-key org-glance-overview-mode-map (kbd "b") #'org-backward-heading-same-level)
(define-key org-glance-overview-mode-map (kbd ",") #'beginning-of-buffer)
(define-key org-glance-overview-mode-map (kbd "<") #'beginning-of-buffer)
(define-key org-glance-overview-mode-map (kbd ".") #'end-of-buffer)
(define-key org-glance-overview-mode-map (kbd ">") #'end-of-buffer)
(define-key org-glance-overview-mode-map (kbd "TAB") #'org-cycle)
(define-key org-glance-overview-mode-map (kbd "RET") #'org-glance-overview:materialize)
(define-key org-glance-overview-mode-map (kbd "j") #'org-glance-overview:open)
(define-key org-glance-overview-mode-map (kbd "!") #'org-glance-overview:open)  ; dired execute rhyme
(define-key org-glance-overview-mode-map (kbd "e") #'org-glance-overview:extract)
(define-key org-glance-overview-mode-map (kbd "a") #'org-glance-agenda)
(define-key org-glance-overview-mode-map (kbd "g") #'org-glance-overview:refresh)
(define-key org-glance-overview-mode-map (kbd "O") #'org-glance-overview:table)
(define-key org-glance-overview-mode-map (kbd "@") #'org-glance-overview:relations)
(define-key org-glance-overview-mode-map (kbd "C") #'org-glance-overview:configure-tag)
(define-key org-glance-overview-mode-map (kbd "+") #'org-glance-overview:capture)
(define-key org-glance-overview-mode-map (kbd "l") #'org-glance-overview:history)
(define-key org-glance-overview-mode-map (kbd "D") #'org-glance-overview:delete)
(define-key org-glance-overview-mode-map (kbd "C-c C-t") #'org-glance-overview:todo)
(define-key org-glance-overview-mode-map (kbd "C-c C-s") #'org-glance-overview:schedule)
(define-key org-glance-overview-mode-map (kbd "C-c C-d") #'org-glance-overview:deadline)
(define-key org-glance-overview-mode-map (kbd "q") #'quit-window)

(defvar-local org-glance-overview--spec nil
  "Normalised filter spec this overview buffer renders; nil for all headlines.")

(cl-defun org-glance-overview:id-at-point ()
  "ORG_GLANCE_ID of the headline at point, or signal a `user-error'."
  (or (save-excursion
        (org-back-to-heading t)
        (org-entry-get nil "ORG_GLANCE_ID"))
      (user-error "No headline at point")))

(cl-defun org-glance-overview:materialize ()
  "Materialize the headline at point."
  (interactive)
  (switch-to-buffer (org-glance-material:open org-glance-graph (org-glance-overview:id-at-point))))

(cl-defun org-glance-overview:delete ()
  "Delete the headline at point (tombstone; referrer-aware confirmation)."
  (interactive)
  (when (org-glance-material:delete org-glance-graph (org-glance-overview:id-at-point))
    (org-glance-overview:refresh)))

(cl-defun org-glance-overview:history ()
  "Open one of the occurrence snapshots of the headline at point, read-only."
  (interactive)
  (org-glance-view:pick-occurrence org-glance-graph (org-glance-overview:id-at-point)))

(cl-defun org-glance-overview--headline-at-point ()
  "Return the live `org-glance-headline' at point; error if it was deleted."
  (org-glance-view:live-headline org-glance-graph (org-glance-overview:id-at-point)))

(cl-defun org-glance-overview:open ()
  "Open a link inside the headline at point."
  (interactive)
  (org-glance-material:open-link (org-glance-overview--headline-at-point)))

(cl-defun org-glance-overview:extract ()
  "Extract a key-value pair from the headline at point."
  (interactive)
  (org-glance-material:extract (org-glance-overview--headline-at-point)))

(cl-defun org-glance-overview:todo (&optional arg)
  "Advance the TODO state of the headline at point exactly like `C-c C-t'.
Pass ARG to `org-todo' in the material buffer, LOGBOOK notes included; once
the change commits, refresh and return point to the headline."
  (interactive "P")
  (let ((id (org-glance-overview:id-at-point)))
    (org-glance-material:change-todo-live
     org-glance-graph id arg
     (lambda (state)
       (org-glance-overview--refresh-to-id id)
       (message "State: %s" (if (s-present? state) state "(none)"))))))

(cl-defun org-glance-overview--refresh-to-id (id)
  "Refresh the overview and return point to ID's heading."
  (org-glance-overview:refresh)
  (goto-char (point-min))
  (when (re-search-forward (format "^:ORG_GLANCE_ID: %s$" (regexp-quote id)) nil t)
    (org-back-to-heading t)))

(cl-defun org-glance-overview--set-planning (kind remove)
  "Set (or REMOVE) KIND planning of the headline at point; refresh, keep point."
  (let ((id (org-glance-overview:id-at-point)))
    (org-glance-material:set-planning org-glance-graph id kind remove)
    (org-glance-overview--refresh-to-id id)
    (message "%s %s" (capitalize (symbol-name kind)) (if remove "cleared" "set"))))

(cl-defun org-glance-overview:schedule (&optional arg)
  "Set the schedule of the headline at point, like `C-c C-s'; ARG clears it."
  (interactive "P")
  (org-glance-overview--set-planning 'schedule arg))

(cl-defun org-glance-overview:deadline (&optional arg)
  "Set the deadline of the headline at point, like `C-c C-d'; ARG clears it."
  (interactive "P")
  (org-glance-overview--set-planning 'deadline arg))

(cl-defun org-glance-overview:visit (graph &optional filter)
  "Open GRAPH's overview for FILTER read-only, serving the cache when fresh."
  (let* ((from-view (and org-glance-view--graph t))   ; re-navigation from within a view?
         (spec (org-glance-filter:normalize-spec filter))
         (file (org-glance-overview:cached-file graph spec))
         (existing (get-file-buffer file)))
    (cond
     ;; Re-read in place: `find-file' would raise a "changed on disk" prompt.
     ((eq existing (current-buffer))
      (unless (verify-visited-file-modtime existing)
        (let ((inhibit-read-only t)) (revert-buffer t t t))))
     (t (when existing (kill-buffer existing))
        (find-file file)))
    (setq-local org-glance-overview--spec spec
                ;; This file lives in the store; act from the graph ROOT.
                default-directory (file-name-as-directory (org-glance-graph:directory graph)))
    (org-glance-overview-mode +1)
    (org-glance-view:register
     graph
     :stale-fn  (lambda () (and org-glance-graph buffer-file-name (org-glance-overview--stale?)))
     :reload-fn #'org-glance-overview:refresh)
    (org-glance-view:fill-frame from-view)
    (current-buffer)))

(cl-defun org-glance-overview:refresh ()
  "Rebuild the current overview from the graph, ignoring the cache."
  (interactive)
  (org-glance-overview:write org-glance-graph org-glance-overview--spec)
  (let ((inhibit-read-only t))
    (revert-buffer t t t))
  (org-glance-view:mark-fresh))

(cl-defun org-glance-overview:table ()
  "Open the table view with the same filter as the current overview."
  (interactive)
  (org-glance-table:visit org-glance-graph org-glance-overview--spec))

(cl-defun org-glance-overview:relations ()
  "Open the relation table of the headline at point (`@'), both directions."
  (interactive)
  (org-glance-table:visit-relations org-glance-graph
                                    (org-glance-overview:id-at-point)))

(cl-defun org-glance-overview:capture ()
  "Capture a headline pre-tagged with this overview's tags."
  (interactive)
  (org-glance-capture (or (org-glance-filter:tags org-glance-overview--spec)
                          (org-glance-capture:completing-read-tag))
                      ""))

(cl-defun org-glance-overview:configure-tag ()
  "Configure this overview's sole filter tag, skipping the tag prompt (`C').
An unfiltered or multi-tag overview falls back to the prompt."
  (interactive)
  (org-glance-tag-config-edit
   (org-glance-filter:sole-tag org-glance-overview--spec)))

;;;

;;;###autoload
(cl-defun org-glance-overview (&optional tag)
  "Browse the graph, optionally filtered by TAG, in the default view.
TAG, a bare tag or a filter plist (`org-glance-filter:predicate'), overlays
the ambient `org-glance-filter-spec'; interactively, read it (empty = none).
`org-glance-overview-default-view' names the view; `O' there toggles it."
  (interactive (list (org-glance-view:completing-read-tag "Overview tag (empty for all): ")))
  (org-glance-ensure-init)
  (org-glance-overview:visit-default org-glance-graph
                                     (org-glance-filter:merge org-glance-filter-spec tag)))

(cl-defun org-glance-overview:visit-default (graph filter)
  "Open FILTER's view of GRAPH per `org-glance-overview-default-view'."
  (if (org-glance-overview--default-table?)
      (org-glance-table:visit graph filter)
    (org-glance-overview:visit graph filter)))


(cl-defun org-glance-overview--revisit (spec)
  "Re-visit the current overview with the (already composed) filter SPEC."
  (org-glance-ensure-init)
  (org-glance-overview:visit org-glance-graph spec))

(cl-defun org-glance-overview:filter-by-state ()
  "Narrow the current overview by todo state (active / done / all / a state)."
  (interactive)
  (org-glance-ensure-init)
  (org-glance-overview--revisit
   (org-glance-filter:set-state org-glance-overview--spec
                                (org-glance-filter:read-state org-glance-graph))))

(cl-defun org-glance-overview:filter-by-substring ()
  "Narrow the current overview to titles containing a substring."
  (interactive)
  (let ((needle (read-string "Title contains: ")))
    (when (string-empty-p needle) (user-error "No substring given"))
    (org-glance-overview--revisit
     (org-glance-filter:set-substring org-glance-overview--spec needle))))

(cl-defun org-glance-overview:filter-clear ()
  "Drop all filters: visit the unfiltered overview."
  (interactive)
  (org-glance-ensure-init)
  (org-glance-overview:visit org-glance-graph nil))

(transient-define-prefix org-glance-overview-filter ()
  "Narrow the current overview by an additional criterion."
  ["Filter overview by"
   ("s" "Todo state" org-glance-overview:filter-by-state)
   ("/" "Title substring" org-glance-overview:filter-by-substring)
   ("c" "Clear (show all)" org-glance-overview:filter-clear)])

(define-key org-glance-overview-mode-map (kbd "/") #'org-glance-overview-filter)

;; Invariant 10: a save flags views stale; each refills at a display boundary.

(cl-defun org-glance-overview--stale? ()
  "Non-nil when the current overview buffer may show outdated results."
  (or (not (verify-visited-file-modtime (current-buffer))) ; file changed under us
      (not (org-glance-overview:fresh? org-glance-graph buffer-file-name))))

;;;###autoload
(cl-defun org-glance-agenda ()
  "Show an `org-agenda' over the headlines `org-glance-filter-spec' admits."
  (interactive)
  (org-glance-ensure-init)
  ;; A dedicated file: `a' must never rewrite the file an open overview visits.
  (let* ((file (f-join (org-glance-overview:cache-path org-glance-graph) "agenda.org"))
         (org-agenda-files (list file))
         (org-agenda-start-on-weekday nil)
         (org-agenda-overriding-header "org-glance agenda"))
    (f-mkdir-full-path (f-dirname file))
    (f-write-text (org-glance-overview:render org-glance-graph org-glance-filter-spec) 'utf-8 file)
    (org-agenda-list nil "-7d" 21)))

(provide 'org-glance-overview)
;;; org-glance-overview.el ends here
