;; -*- lexical-binding: t -*-

;;; org-glance-overview.el --- graph-backed overview + agenda

;;; Commentary:
;; A read-only browser over the graph: one org file (under the store) rendered
;; from headline *metadata* (cheap -- no per-headline content parse), one heading
;; per matching headline with its state/title/tags/planning + ORG_GLANCE_ID.
;;
;; FILTERING.  Every entry point takes an optional FILTER, interpreted by three
;; pure helpers: `:spec-predicate' (spec -> closure on metadata), `:spec-key'
;; (spec -> stable, filesystem-safe cache dir name, or nil = uncacheable) and
;; `:fresh?' (is a cache file newer than the graph?).  A FILTER is either nil
;; (all headlines, the historical behaviour), a bare tag symbol/string (legacy
;; shorthand for `(:tags (TAG))'), or a plist spec -- see `:spec-predicate' for
;; the recognised keys.  `plist-member' (not `plist-get') gates each clause so
;; `(:state nil)' (headlines with no todo state) is distinct from omitting
;; `:state' (no constraint).
;;
;; CACHING.  The unfiltered overview lives at `<store>/overview.org'; each
;; cacheable filter gets its own directory `<store>/overviews/<key>/overview.org'
;; (<key> = a short hash of the canonical filter; the SPEC sidecar inside
;; records the filter readably).
;; A cached file is served untouched -- no JSONL read, no render -- while it is
;; newer than the append-only `headlines.jsonl' (`:fresh?'), and rebuilt
;; otherwise.  An uncacheable `:where' filter renders every time to a transient
;; file so it never clobbers a real cache.
;;
;; Browse with `org-glance-overview'; act on the headline at point.

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
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

(defvar org-glance-graph)
(declare-function org-glance-initialized? "org-glance")
(declare-function org-glance-table:visit "org-glance-table")
(declare-function org-glance-capture "org-glance-capture")
(declare-function org-glance-capture:completing-read-tag "org-glance-capture")

(defconst org-glance-overview:header
  "#    -*- mode: org; mode: org-glance-overview -*-\n#+TITLE: org-glance overview\n\n"
  "Prop-line header written at the top of the overview file.")

;;; Per-filter on-disk cache key
;;
;; The filter LANGUAGE -- the table, `normalize-spec', `predicate', `identity'
;; and the refinement helpers -- lives in `org-glance-filter'.  The overview
;; layers a per-filter on-disk cache on top: the key below is a short hash of a
;; filter's canonical identity, so cache directory names stay byte-stable across
;; versions and machines.

(cl-defun org-glance-overview:spec-key (filter)
  "Return a compact, deterministic cache key for FILTER.
\"all\" for the empty filter; nil when FILTER is uncacheable (carries `:where').
The key is the first 12 hex chars of the SHA-1 of the canonical spec identity
(`org-glance-filter:identity'), so specs differing only in key order or tag
order map to the same key, every machine derives the same name for the same
filter, and the name is filesystem-safe by construction.  The prefix is still
LOSSY: distinct specs can (astronomically rarely) share a key; the SPEC sidecar
check in `cached-file' turns such a collision into a rebuild, never into serving
the wrong overview.  The sidecar, not the name, is the human-readable record of
which filter a cache directory holds."
  (let ((spec (org-glance-filter:normalize-spec filter)))
    (cond
     ((null spec) "all")
     ((plist-member spec :where) nil)
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

;;; Rendering (from metadata, not content)

(cl-defun org-glance-overview:tag-string (metadata)
  (when-let ((tags (append (org-glance-headline-metadata:tags metadata) nil)))
    (format "  :%s:" (s-join ":" (mapcar (lambda (x) (format "%s" x)) tags)))))

(cl-defun org-glance-overview:render-headline (metadata)
  "Render METADATA as one org heading + planning + ORG_GLANCE_ID property."
  (let ((state (org-glance-headline-metadata:state metadata))
        (schedule (org-glance-headline-metadata:schedule metadata))
        (deadline (org-glance-headline-metadata:deadline metadata)))
    (concat "* "
            (if (org-glance--present-string? state) (concat state " ") "")
            (org-glance-headline-metadata:title metadata)
            (or (org-glance-overview:tag-string metadata) "")
            "\n"
            (when (org-glance--present-string? schedule)
              (concat "SCHEDULED: " schedule "\n"))
            (when (org-glance--present-string? deadline)
              (concat "DEADLINE: " deadline "\n"))
            ":PROPERTIES:\n:ORG_GLANCE_ID: " (org-glance-headline-metadata:id metadata) "\n:END:\n")))

(cl-defun org-glance-overview:render (graph &optional filter)
  "Render GRAPH's live headlines matching FILTER as org text.
FILTER is nil (all), a bare tag, or a filter plist -- see
`org-glance-filter:predicate'.

When FILTER resolves to a single tag with a configured `:TODO_KEYWORDS:' cycle
(see `org-glance-tag-config'), a `#+TODO:' file keyword for it is emitted in the
header -- so org, on opening the cached file (overview OR agenda, both of which
read this text), cycles and faces those states natively -- and the cycle's
done-set is bound while the `:done'/`active?' predicate is built, so selection
is correct for the tag's keywords without any spec/cache-key change."
  (cl-check-type graph org-glance-graph)
  (let* ((cycle (org-glance-tag-config:cycle-for-filter graph filter))
         (org-done-keywords (if cycle
                                (org-glance-tag-config:done-keywords cycle)
                              org-done-keywords))
         (keep? (org-glance-filter:predicate filter)))
    ;; Collect the per-headline strings and join ONCE (`apply #'concat'); a
    ;; `cl-loop ... concat' re-copies the growing accumulator each step -- O(N^2)
    ;; in the output size (seconds at 10^4 headlines).
    (apply #'concat
           org-glance-overview:header
           (if cycle (concat "#+TODO: " cycle "\n") "")
           (cl-loop for meta in (org-glance-graph:headlines graph)
                    when (funcall keep? meta)
                    collect (org-glance-overview:render-headline meta)))))

;;; Cache paths + freshness

(cl-defun org-glance-overview:file (graph)
  "Path to GRAPH's unfiltered overview file (inside the hidden store)."
  (cl-check-type graph org-glance-graph)
  (f-join (org-glance-graph:store-path graph) "overview.org"))

(cl-defun org-glance-overview:cache-path (graph)
  "Directory holding GRAPH's filtered (cached) overviews."
  (cl-check-type graph org-glance-graph)
  (f-join (org-glance-graph:store-path graph) "overviews"))

(cl-defun org-glance-overview:spec-cache-file (graph filter)
  "File backing FILTER's overview under GRAPH.
The empty filter uses the legacy unfiltered file; an uncacheable `:where' filter
uses a shared transient file (so it never clobbers a real cache); every other
filter gets `<cache-path>/<key>/overview.org'."
  (let ((key (org-glance-overview:spec-key filter)))
    (cond
     ((null key) (f-join (org-glance-overview:cache-path graph) "transient.org"))
     ((string= key "all") (org-glance-overview:file graph))
     (t (f-join (org-glance-overview:cache-path graph) key "overview.org")))))

(cl-defun org-glance-overview--mtime (path)
  (file-attribute-modification-time (file-attributes path)))

(cl-defun org-glance-overview--fresher-than? (file src)
  "Non-nil if SRC is absent or FILE is STRICTLY newer than SRC.
Strict, so a same-second source treats the cache as stale and rebuilds: serving
stale content is the only real bug, a rebuild is just a perf cost.  A future
source (clock skew / restored backup) also rebuilds."
  (or (not (f-exists? src))
      (time-less-p (org-glance-overview--mtime src)
                   (org-glance-overview--mtime file))))

(cl-defun org-glance-overview:fresh? (graph file)
  "Non-nil if FILE exists and is newer than every source it is rendered from.
The sources: GRAPH's `headlines.jsonl' (content) and the tag-config `tags.org'
(the `#+TODO:' header + per-tag done-set render depends on), so editing a tag's
cycle invalidates existing overview caches like a content change."
  (cl-check-type graph org-glance-graph)
  (and (f-exists? file)
       (org-glance-overview--fresher-than? file (org-glance-graph:headline-meta-path graph))
       (org-glance-overview--fresher-than? file (org-glance-tag-config:file graph))))

(cl-defun org-glance-overview--header-current? (file)
  "Non-nil if FILE starts with the current `org-glance-overview:header'.
A cache rendered by an older org-glance (e.g. with the pre-rename
`org-glance-overview-v2' prop-line mode) must be rebuilt, not served --
its prop-line would try to enable a mode that no longer exists."
  (let ((prop-line (car (s-lines org-glance-overview:header))))
    (with-temp-buffer
      ;; `insert-file-contents' into an empty buffer leaves point at BOB.
      (insert-file-contents file nil 0 (+ 16 (length prop-line)))
      (looking-at-p (regexp-quote prop-line)))))

(cl-defun org-glance-overview:write (graph &optional filter)
  "Unconditionally (re)generate FILTER's overview file for GRAPH; return its path.
Used by the agenda and by `g' (refresh), which must always rebuild.  A keyed
cache directory also gets its SPEC identity sidecar, claiming the (hashed,
lossy) directory name for exactly this filter and recording the filter
readably."
  (let ((key (org-glance-overview:spec-key filter))
        (file (org-glance-overview:spec-cache-file graph filter)))
    (f-mkdir-full-path (f-dirname file))
    (unless (member key '(nil "all"))
      (f-write-text (concat (org-glance-filter:identity filter) "\n") 'utf-8
                    (org-glance-overview--spec-sidecar file)))
    (f-write-text (org-glance-overview:render graph filter) 'utf-8 file)
    file))

(cl-defun org-glance-overview:cached-file (graph &optional filter)
  "Path to FILTER's overview file for GRAPH, rebuilding only when stale.
A hit requires the file to be fresh AND, for keyed directories, the SPEC
sidecar to record exactly FILTER's identity -- directory names are truncated
hashes, so a (rare) name collision between distinct filters rebuilds instead
of serving the other filter's overview.  On a hit the file is returned without
reading
`headlines.jsonl' or rendering.  An uncacheable `:where' filter always
re-renders."
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

;;; Browser

(defvar org-glance-overview-mode-map (make-sparse-keymap)
  "Keymap for `org-glance-overview-mode'.")

(define-minor-mode org-glance-overview-mode
  "Read-only browser over the graph."
  :global nil
  :init-value nil
  :keymap org-glance-overview-mode-map
  :after-hook (read-only-mode +1)
  (when org-glance-overview-mode
    ;; org requires tab-width 8 to parse (`id-at-point' reads node properties).
    ;; Coherence (the display-boundary refresh) is wired by `org-glance-view:register'
    ;; in `org-glance-overview:visit'.
    (setq tab-width 8 indent-tabs-mode nil)))

;; Movement mirrors the v1 overview (n/p between headings) plus f/b across
;; same-level siblings; actions act on the headline at point.
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
(define-key org-glance-overview-mode-map (kbd "o") #'org-glance-overview:open)
(define-key org-glance-overview-mode-map (kbd "e") #'org-glance-overview:extract)
(define-key org-glance-overview-mode-map (kbd "a") #'org-glance-agenda)
(define-key org-glance-overview-mode-map (kbd "g") #'org-glance-overview:refresh)
(define-key org-glance-overview-mode-map (kbd "T") #'org-glance-overview:table)
(define-key org-glance-overview-mode-map (kbd "+") #'org-glance-overview:capture)
(define-key org-glance-overview-mode-map (kbd "C-c C-t") #'org-glance-overview:todo)
(define-key org-glance-overview-mode-map (kbd "q") #'quit-window)

(defvar-local org-glance-overview--spec nil
  "Normalised filter spec the current overview buffer was generated with
(nil = all headlines).")

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

(cl-defun org-glance-overview--headline-at-point ()
  "The live `org-glance-headline' for the heading at point.
The overview is a cached snapshot that can outlive the graph, so a heading may
name a headline that has since been deleted; error clearly rather than passing
nil into the material layer."
  (or (org-glance-graph:headline org-glance-graph (org-glance-overview:id-at-point))
      (user-error "Headline no longer in graph (overview is stale; press `g' to refresh)")))

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
Runs org's own `org-todo' (ARG passed through) in the headline's materialized
blob buffer -- interactive LOGBOOK notes and all (see
`org-glance-material:change-todo-live') -- then refreshes this overview and
returns point to the headline once the change (and any note) is committed."
  (interactive "P")
  (let ((id (org-glance-overview:id-at-point)))
    (org-glance-material:change-todo-live
     org-glance-graph id arg
     (lambda (state)
       (org-glance-overview:refresh)
       (goto-char (point-min))
       (when (re-search-forward (format "^:ORG_GLANCE_ID: %s$" (regexp-quote id)) nil t)
         (org-back-to-heading t))
       (message "State: %s" (if (s-present? state) state "(none)"))))))

(cl-defun org-glance-overview:visit (graph &optional filter)
  "Open GRAPH's overview for FILTER read-only, serving the cache when fresh."
  (let* ((from-view (and org-glance-view--graph t))   ; re-navigation from within a view?
         (spec (org-glance-filter:normalize-spec filter))
         (file (org-glance-overview:cached-file graph spec))
         (existing (get-file-buffer file)))
    (cond
     ;; Re-invoked from inside the very buffer that visits FILE: `cached-file'
     ;; may have just rebuilt it on disk, so re-read in place rather than letting
     ;; `find-file' raise a "changed on disk" prompt.
     ((eq existing (current-buffer))
      (unless (verify-visited-file-modtime existing)
        (let ((inhibit-read-only t)) (revert-buffer t t t))))
     (t (when existing (kill-buffer existing))
        (find-file file)))
    (setq-local org-glance-overview--spec spec
                ;; Run directory-relative actions (capture, dired, shell, relative
                ;; links) from the graph's ROOT, not the hidden `.org-glance' cache
                ;; subdir this buffer's file happens to live in.
                default-directory (file-name-as-directory (org-glance-graph:directory graph)))
    (org-glance-overview-mode +1)
    (org-glance-view:register
     graph
     ;; STALE-FN: guard the global graph + visited file the cache freshness needs.
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

(cl-defun org-glance-overview:capture ()
  "Capture a headline pre-tagged with this overview's tags."
  (interactive)
  (org-glance-capture (or (org-glance-filter:tags org-glance-overview--spec)
                          (org-glance-capture:completing-read-tag))
                      ""))

(cl-defun org-glance-overview:tags (graph)
  "Distinct tags across GRAPH's live headlines, sorted."
  (org-glance-graph:tags graph))

(cl-defun org-glance-overview:completing-read-tag ()
  "Prompt for a tag from the graph's headlines; empty input means \"all\"."
  (cl-assert (org-glance-initialized?))
  (let ((choice (completing-read "Overview tag (empty for all): "
                                 (org-glance-overview:tags org-glance-graph))))
    (unless (string-empty-p choice) choice)))

(defcustom org-glance-overview-default-view 'table
  "Which view `org-glance-overview' opens by default.
`table' is the sortable `org-glance-table' dashboard; `org' is the org-text
overview file.  Either view toggles to the other with `T', so this only sets the
landing view for `org-glance-overview' (and the `org-glance-overview:' link)."
  :group 'org-glance
  :type '(choice (const :tag "Table dashboard" table)
                 (const :tag "Org-text overview" org)))

(cl-defun org-glance-overview (&optional tag)
  "Browse the graph, optionally filtered, in the default view.
Interactively, prompt for a tag (empty input = no tag constraint) and overlay it
on the ambient `org-glance-filter-spec' (default: active headlines).
The landing view is `org-glance-overview-default-view' (the table dashboard by
default); press `T' there to toggle to the other view.  TAG may be a bare tag
(symbol/string) or a full filter plist -- see `org-glance-filter:predicate'."
  (interactive (list (org-glance-overview:completing-read-tag)))
  (cl-assert (org-glance-initialized?))
  (let ((filter (org-glance-filter:merge org-glance-filter-spec tag)))
    (if (eq org-glance-overview-default-view 'table)
        (org-glance-table:visit org-glance-graph filter)
      (org-glance-overview:visit org-glance-graph filter))))

;;; Interactive filter refinement (the `/' transient)
;;
;; Each command composes a clause onto the CURRENT buffer's filter spec
;; (re-applying a dimension replaces it) and re-visits -- the new overview gets,
;; or re-uses, its own cache like any other filter, and the previous (less
;; filtered) buffer stays around, so narrowing is non-destructive.  The clause
;; builders (`org-glance-filter:set-state' / `:set-substring' / `:read-state')
;; are shared with the `org-glance-form-action' transient so the two filter UIs
;; stay consistent.

(cl-defun org-glance-overview--revisit (spec)
  "Re-visit the current overview with the (already composed) filter SPEC."
  (cl-assert (org-glance-initialized?))
  (org-glance-overview:visit org-glance-graph spec))

(cl-defun org-glance-overview:filter-by-state ()
  "Narrow the current overview by todo state (active / done / all / a state)."
  (interactive)
  (cl-assert (org-glance-initialized?))
  (org-glance-overview--revisit
   (org-glance-filter:set-state org-glance-overview--spec
                                (org-glance-filter:read-state org-glance-graph))))

(cl-defun org-glance-overview:filter-by-substring ()
  "Narrow the current overview to headlines whose title contains a substring."
  (interactive)
  (let ((needle (read-string "Title contains: ")))
    (when (string-empty-p needle) (user-error "No substring given"))
    (org-glance-overview--revisit
     (org-glance-filter:set-substring org-glance-overview--spec needle))))

(cl-defun org-glance-overview:filter-clear ()
  "Drop all filters: visit the unfiltered overview."
  (interactive)
  (cl-assert (org-glance-initialized?))
  (org-glance-overview:visit org-glance-graph nil))

(transient-define-prefix org-glance-overview-filter ()
  "Narrow the current overview by an additional criterion."
  ["Filter overview by"
   ("s" "Todo state" org-glance-overview:filter-by-state)
   ("/" "Title substring" org-glance-overview:filter-by-substring)
   ("c" "Clear (show all)" org-glance-overview:filter-clear)])

(define-key org-glance-overview-mode-map (kbd "/") #'org-glance-overview-filter)

;;; View coherence: an overview buffer must never show outdated results
;;
;; PULL, at a SAFE boundary, driven by `org-glance-view' (which owns the shared
;; window-hook wiring + the modified-buffer guard for every view type).  A
;; materialized save only appends to the WAL (which makes every overview cache
;; `stale?') and FLAGS open views via `org-glance-view:mark-graph-stale' (a cheap
;; boolean + the `glance:stale' lighter) -- it does NOT rewrite any overview.
;; Each overview rebuilds itself lazily when its window is (re)displayed or
;; selected -- where point is being re-established anyway -- catching its own saves
;; AND every other mutation (capture, delete, reindex, compaction, another Emacs)
;; by the same freshness check.  This file supplies only the two view-specific
;; pieces `org-glance-view:register' takes: the STALE-FN (`--stale?', below) and
;; the RELOAD-FN (`:refresh').

(cl-defun org-glance-overview--stale? ()
  "Non-nil when the current overview buffer may show outdated results.
The view's STALE-FN (see `org-glance-view:register'): the cache file changed
under us, or predates a source it renders from (`org-glance-overview:fresh?')."
  (or (not (verify-visited-file-modtime (current-buffer))) ; file changed under us
      (not (org-glance-overview:fresh? org-glance-graph buffer-file-name))))

;;; Agenda

(cl-defun org-glance-agenda ()
  "Show an `org-agenda' over the graph's scheduled/deadline headlines.
Honours the ambient `org-glance-filter-spec' (default: active headlines)."
  (interactive)
  (cl-assert (org-glance-initialized?))
  ;; Render to a dedicated file rather than overview.org, so pressing `a' from an
  ;; open overview never rewrites the file that buffer is visiting.
  (let* ((file (f-join (org-glance-overview:cache-path org-glance-graph) "agenda.org"))
         (org-agenda-files (list file))
         (org-agenda-start-on-weekday nil)
         (org-agenda-overriding-header "org-glance agenda"))
    (f-mkdir-full-path (f-dirname file))
    (f-write-text (org-glance-overview:render org-glance-graph org-glance-filter-spec) 'utf-8 file)
    (org-agenda-list nil "-7d" 21)))

(provide 'org-glance-overview)
;;; org-glance-overview.el ends here
