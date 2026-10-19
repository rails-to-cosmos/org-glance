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
(require 'org-glance-cache)
(require 'org-glance-filter)
(require 'org-glance-tag-config)
(require 'org-glance-material)
(require 'org-glance-view)

(declare-function org-glance-cache:headlines-for-filter
                  "org-glance-cache" (graph filter))

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
  "Return the publication manifest path for anchor or artifact FILE."
  (let ((directory (file-name-directory file)))
    (when (equal (file-name-nondirectory (directory-file-name directory)) "artifacts")
      (setq directory (file-name-directory (directory-file-name directory))))
    (expand-file-name "SPEC" directory)))

(cl-defun org-glance-overview--spec-owns-cache? (filter file &optional graph)
  "Non-nil when GRAPH's current manifest certifies immutable FILE for FILTER."
  (and file (equal file (org-glance-overview--admit graph filter))))

(defun org-glance-overview--manifest (graph filter)
  "Read FILTER's artifact manifest in GRAPH, returning nil if incomplete."
  (ignore-errors
    (let* ((anchor (org-glance-overview:spec-cache-file graph filter))
           (receipt (json-parse-string
                     (f-read-text (org-glance-overview--spec-sidecar anchor) 'utf-8))))
      (when (and (vectorp receipt) (= (length receipt) 3)
                 (equal (aref receipt 0) 2) (stringp (aref receipt 1))
                 (stringp (aref receipt 2))
                 (string-match-p "\\`[0-9a-f]\\{64\\}\\'" (aref receipt 2)))
        (cons receipt (expand-file-name (concat "artifacts/" (aref receipt 2) ".org")
                                       (file-name-directory anchor)))))))

(defun org-glance-overview--admit (graph filter)
  "Resolve FILTER to one immutable artifact certified by GRAPH's inputs."
  (when-let* ((manifest (org-glance-overview--manifest graph filter)))
    (let ((dependency (org-glance-overview--dependency graph filter))
          (file (cdr manifest)))
      (when (and (equal dependency (aref (car manifest) 1))
                 (file-exists-p file)
                 (org-glance-overview--header-current? file))
        file))))

(defun org-glance-overview--publish (graph filter receipt text)
  "Publish prepared TEXT and RECEIPT as one immutable GRAPH artifact for FILTER."
  (unless (equal receipt (org-glance-overview--dependency graph filter))
    (user-error "Overview inputs changed during preparation; refresh and retry"))
  (let* ((anchor (org-glance-overview:spec-cache-file graph filter))
         (digest (secure-hash 'sha256 (encode-coding-string text 'utf-8-unix)))
         (file (expand-file-name (concat "artifacts/" digest ".org")
                                 (file-name-directory anchor))))
    (f-mkdir-full-path (f-dirname file))
    (condition-case nil
        (org-glance--atomic-write file text nil)
      (file-already-exists
       (unless (equal digest (org-glance-cache--file-digest file))
         (user-error "Immutable Overview artifact is corrupt; clear its local cache"))))
    (unless (equal receipt (org-glance-overview--dependency graph filter))
      (user-error "Overview inputs changed before publication; refresh and retry"))
    (when (org-glance-overview:spec-key filter)
      (org-glance--atomic-write
       (org-glance-overview--spec-sidecar anchor)
       (decode-coding-string (json-serialize (vector 2 receipt digest)) 'utf-8 t)))
    file))

(cl-defun org-glance-overview:render-headline (graph metadata &optional titles)
  "Render METADATA as one self-sufficient org heading (invariant 20).
Relation titles resolve from TITLES or GRAPH, falling back to a gone target's
id."
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
                                             target kind
                                             (if titles
                                                 (or (gethash target titles) target)
                                               (org-glance-graph:title-or-id
                                                graph target)))
                                            "\n")))
            (apply #'concat
                   (cl-loop for link in (org-glance-headline-metadata:links metadata)
                            collect (concat "- " link "\n"))))))

(cl-defun org-glance-overview:render (graph &optional filter)
  "Render GRAPH's live headlines matching FILTER as org text.
FILTER is nil (all), a bare tag, or a plist (`org-glance-filter:predicate').
Prefix the pragmas FILTER's tags agree on; select by their cycle's done-set."
  (cl-check-type graph org-glance-graph)
  (let* ((json (org-glance-config:current graph))
         (prepared (and json (org-glance-overview--json-rows graph filter json)))
         (metas (if json (car prepared) (org-glance-cache:headlines-for-filter graph filter)))
         (titles (make-hash-table :test #'equal :size (length metas)))
         (cycle (unless json (org-glance-tag-config:cycle-for-filter graph filter metas)))
         (org-done-keywords (if cycle
                                (org-glance-tag-config:done-keywords cycle)
                              org-done-keywords))
         (keep? (if json #'always (org-glance-filter:predicate filter))))
    (dolist (meta metas)
      (let ((id (org-glance-headline-metadata:id meta)))
        (unless (gethash id titles)
          (puthash id (org-glance-headline-metadata:title meta) titles))))
    (dolist (meta metas)
      (dolist (relation (org-glance-headline-metadata:relations meta))
        (let ((target (car relation)))
          (unless (gethash target titles)
            (when-let* ((target-meta (org-glance-cache:headline graph target)))
              (when (org-glance-headline-metadata? target-meta)
                (puthash target
                         (org-glance-headline-metadata:title target-meta)
                         titles)))))))
    ;; Join ONCE: `cl-loop ... concat' re-copies the accumulator -- O(N^2).
    (apply #'concat
           org-glance-overview:header
           (if json (cdr prepared)
             (or (org-glance-tag-config:preamble-for-filter graph filter t metas) ""))
           (cl-loop for meta in metas
                    when (funcall keep? meta)
                    collect (org-glance-overview:render-headline
                             graph meta titles)))))

(defun org-glance-overview--json-rows (graph filter settings)
  "Resolve GRAPH's FILTER against SETTINGS and return (ROWS . ORG-HEADER)."
  (let ((contexts (make-hash-table :test #'equal))
        (export-contexts (make-hash-table :test #'equal))
        (spec (org-glance-filter:normalize-spec filter))
        selected scopes)
    (dolist (row (org-glance-cache:overview-rows graph filter))
      (let* ((meta (car row)) (declarations (cdr row))
             (tags (org-glance-headline-metadata:tag-strings meta))
             (key (list tags (mapcar #'caddr declarations)))
             (context
              (or (gethash key contexts)
                  (let* ((file (org-glance-tag-config:declaration-keywords declarations))
                         (cycles (org-glance-config:native-keywords settings file tags declarations))
                         (header (mapconcat (lambda (cycle)
                                              (concat "#+TODO: " (string-join (cdr cycle) " ") "\n"))
                                            cycles ""))
                         (done (cdr (org-glance-config:keywords settings file tags)))
                         (predicate (org-glance-filter:predicate
                                     (if (or (not (plist-member spec :done))
                                             (plist-member spec :done-keywords)) spec
                                       (plist-put (copy-sequence spec) :done-keywords done)))))
                    (puthash key (cons header predicate) contexts)))))
        (when (funcall (cdr context) meta)
          (push meta selected)
          (unless (gethash (car context) export-contexts)
            (puthash (car context) t export-contexts)
            (push (cons (format "%s [%s]" (org-glance-headline-metadata:id meta)
                                (or (org-glance-headline-metadata:hash meta) "recovery"))
                        (org-glance-org-presentation--make :header (car context))) scopes)))))
    (cons (nreverse selected)
          (if scopes (org-glance-config:compatible-header (nreverse scopes))
            (mapconcat (lambda (cycle) (concat "#+TODO: " (string-join (cdr cycle) " ") "\n"))
                       (org-glance-config:native-keywords settings nil nil) "")))))

(defun org-glance-overview--dependency (graph filter)
  "Return FILTER's private Overview receipt for GRAPH's current context."
  (let ((json (org-glance-config:current graph)))
    (prin1-to-string
     (list "overview/2" (org-glance-filter:identity filter)
           (org-glance-config:cache-key graph) (org-glance-config:presentation-key graph)
           (unless json (org-glance-tag-config:source-mtime graph))
           (mapcar (lambda (path)
                     (when-let* ((attributes (file-attributes path)))
                       (list (file-attribute-modification-time attributes)
                             (file-attribute-size attributes))))
                   (list (org-glance-cache:path graph)
                         (concat (org-glance-cache:path graph) "-wal")
                         (org-glance-graph:headline-meta-path graph)
                         (org-glance-graph:external-path graph)))))))

(cl-defun org-glance-overview:file (graph)
  "Return GRAPH's unfiltered local Overview publication anchor."
  (cl-check-type graph org-glance-graph)
  (f-join (org-glance-overview:cache-path graph) "all" "overview.org"))

(cl-defun org-glance-overview:cache-path (graph)
  "Return GRAPH's git-ignored local Overview publication directory."
  (cl-check-type graph org-glance-graph)
  (org-glance-graph:cache-file graph "overviews"))

(cl-defun org-glance-overview:spec-cache-file (graph filter)
  "Return FILTER's publication anchor under GRAPH.
Exports themselves are immutable artifacts resolved through the SPEC manifest."
  (let ((key (org-glance-overview:spec-key filter)))
    (cond
     ((null key) (f-join (org-glance-overview:cache-path graph) "transient" "overview.org"))
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
                    (unless (org-glance-config:current graph)
                      (org-glance-tag-config:source-mtime graph))))))

(cl-defun org-glance-overview--header-current? (file)
  "Non-nil if FILE begins with the prop-line of `org-glance-overview:header'."
  (let ((prop-line (car (s-lines org-glance-overview:header))))
    (with-temp-buffer
      (insert-file-contents file nil 0 (+ 16 (length prop-line)))
      (looking-at-p (regexp-quote prop-line)))))

(cl-defun org-glance-overview:write (graph &optional filter)
  "Render FILTER for GRAPH and return its immutable local artifact path.
Persistent filters atomically publish a manifest binding inputs to the artifact."
  (let ((receipt (org-glance-overview--dependency graph filter))
        (text (org-glance-overview:render graph filter)))
    (org-glance-overview--publish graph filter receipt text)))

(cl-defun org-glance-overview--heading-region (id)
  "Return the current buffer region for ID's heading, or nil."
  (goto-char (point-min))
  (when (re-search-forward
         (format "^:ORG_GLANCE_ID:[ \t]+%s[ \t]*$" (regexp-quote id)) nil t)
    (re-search-backward "^\\* " nil t)
    (let ((beg (point)))
      (forward-line 1)
      (if (re-search-forward "^\\* " nil t)
          (cons beg (match-beginning 0))
        (cons beg (point-max))))))

(cl-defun org-glance-overview--splice (graph file ids)
  "Publish a copy-on-write splice of IDS from unfiltered overview FILE.
Return non-nil only when every replacement is structurally safe."
  (when (and (f-exists? file) ids)
    (let ((receipt (org-glance-overview--dependency graph nil)))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((safe t))
          (dolist (id ids)
            (when safe
              (let ((region (org-glance-overview--heading-region id))
                    (meta (org-glance-cache:headline graph id)))
                (if (and region
                         (org-glance-headline-metadata? meta)
                         (null (org-glance-cache:incoming-relations graph id)))
                    (progn
                      (delete-region (car region) (cdr region))
                      (goto-char (car region))
                      (insert (org-glance-overview:render-headline graph meta)))
                  (setq safe nil)))))
          (when safe
            (org-glance-overview--publish graph nil receipt (buffer-string))
            t))))))

(cl-defun org-glance-overview:update (graph filter changed)
  "Refresh FILTER's cached Overview after CHANGED commits.
Return `incremental' for a safe bounded splice and `full' after regeneration."
  (let ((file (cdr (org-glance-overview--manifest graph filter))))
    (if (and (not (org-glance-config:current graph))
             (null (org-glance-filter:normalize-spec filter))
             file (listp changed)
             (condition-case nil
                 (org-glance-overview--splice graph file changed)
               (error nil)))
        'incremental
      (org-glance-overview:write graph filter)
      'full)))

(cl-defun org-glance-overview:cached-file (graph &optional filter)
  "Return FILTER's certified immutable artifact for GRAPH, rebuilding if stale.
A manifest hit fetches no Rows; transient filters always render."
  (or (and (org-glance-overview:spec-key filter)
           (org-glance-overview--admit graph filter))
      (org-glance-overview:write graph filter)))

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
  "Refresh the current Overview from its pending graph changes."
  (interactive)
  (let ((id (ignore-errors (org-glance-overview:id-at-point))))
    (let ((file
           (if (org-glance-overview:spec-key org-glance-overview--spec)
               (progn
                 (org-glance-overview:update org-glance-graph org-glance-overview--spec
                                             org-glance-view--pending-changes)
                 (org-glance-overview:cached-file org-glance-graph org-glance-overview--spec))
             (org-glance-overview:write org-glance-graph org-glance-overview--spec))))
      (unless (equal file buffer-file-name) (set-visited-file-name file t)))
    (let ((inhibit-read-only t))
      (revert-buffer t t t))
    (org-set-regexps-and-options)
    (when-let* ((region (and id (org-glance-overview--heading-region id))))
      (goto-char (car region))))
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
      (if (org-glance-overview:spec-key org-glance-overview--spec)
          (not (org-glance-overview--spec-owns-cache?
                org-glance-overview--spec buffer-file-name org-glance-graph))
        (not (org-glance-overview:fresh? org-glance-graph buffer-file-name)))))

;;;###autoload
(cl-defun org-glance-agenda ()
  "Show an `org-agenda' over the headlines `org-glance-filter-spec' admits."
  (interactive)
  (org-glance-ensure-init)
  (let* ((file (org-glance-overview:write org-glance-graph org-glance-filter-spec))
         (org-agenda-files (list file))
         (org-agenda-start-on-weekday nil)
         (org-agenda-overriding-header "org-glance agenda"))
    (org-agenda-list nil "-7d" 21)))

(provide 'org-glance-overview)
;;; org-glance-overview.el ends here
