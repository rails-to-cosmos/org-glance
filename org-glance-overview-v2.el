;; -*- lexical-binding: t -*-

;;; org-glance-overview-v2.el --- v2 graph-backed overview + agenda

;;; Commentary:
;; A read-only browser over the v2 graph: one org file (under the store) rendered
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
;; cacheable filter gets its own directory `<store>/overviews/<key>/overview.org'.
;; A cached file is served untouched -- no JSONL read, no render -- while it is
;; newer than the append-only `headlines.jsonl' (`:fresh?'), and rebuilt
;; otherwise.  An uncacheable `:where' filter renders every time to a transient
;; file so it never clobbers a real cache.
;;
;; Browse with `org-glance-overview-v2'; act on the headline at point.  Gated by
;; `org-glance-use-graph-v2'; see MIGRATION-PLAN.md Phase 2.

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'f)
(require 's)
(require 'org)
(require 'org-agenda)

(require 'org-glance-graph-v2)
(require 'org-glance-material-v2)

(defvar org-glance-graph-v2)
(declare-function org-glance-initialized?-v2 "org-glance")

(defconst org-glance-overview-v2:header
  "#    -*- mode: org; mode: org-glance-overview-v2 -*-\n#+TITLE: org-glance overview\n\n"
  "Prop-line header written at the top of the v2 overview file.")

;;; Filters
;;
;; The whole filter language is interpreted by `:--normalize-spec' (the single
;; coercion point) plus the three pure helpers below.  render/write/visit stay
;; dumb and just call them.

(defconst org-glance-overview-v2:filter-keys
  '(:tags :state :done :id :title :hash :priority
          :linked :propertized :encrypted :schedule :deadline :where)
  "Recognised keys in a normalised overview filter spec (`:tag' folds into
`:tags').")

(cl-defun org-glance-overview-v2:--normalize-spec (filter)
  "Coerce FILTER into a canonical plist spec (or nil for \"all\").
nil/() stays nil; a bare tag symbol/string becomes `(:tags (TAG))'; a plist is
returned with any `:tag' folded into `:tags' so downstream code only ever sees
`:tags'.  Signals an error on an unrecognised key."
  (cond
   ((null filter) nil)
   ((symbolp filter) (list :tags (list (format "%s" filter))))
   ((stringp filter) (list :tags (list filter)))
   ((listp filter)
    (let (out tag tags tags-seen)
      (cl-loop for (k v) on filter by #'cddr
               do (cond ((eq k :tag) (setq tag v))
                        ((eq k :tags) (setq tags v tags-seen t))
                        (t (setq out (plist-put out k v)))))
      (when (or tags-seen tag)
        ;; An empty tag list is no constraint -- drop it so `(:tags nil)' folds
        ;; to the "all" key and shares the unfiltered cache (like `(:tag nil)').
        (when-let ((all (append (cond ((null tags) nil)
                                      ((listp tags) tags)
                                      (t (list tags)))
                                (when tag (list tag)))))
          (setq out (plist-put out :tags all))))
      (cl-loop for (k _v) on out by #'cddr
               unless (memq k org-glance-overview-v2:filter-keys)
               do (error "Unrecognised overview filter key: %S" k))
      out))
   (t (error "Invalid overview filter: %S" filter))))

(cl-defun org-glance-overview-v2:spec-predicate (filter)
  "Return a predicate (lambda (METADATA) -> generalized-boolean) for FILTER.
A nil/empty filter yields a predicate that accepts every headline.  Each present
clause must hold (logical AND).  See `org-glance-overview-v2:filter-keys'."
  (let ((spec (org-glance-overview-v2:--normalize-spec filter))
        (clauses nil))
    (when (plist-member spec :tags)
      ;; Tags are stored interned + downcased, so compare case-insensitively
      ;; (mirrors v1's `org-glance:tag-filter').
      (let ((want (mapcar (lambda (x) (downcase (format "%s" x))) (plist-get spec :tags))))
        (push (lambda (m)
                (let ((have (mapcar (lambda (x) (downcase (format "%s" x)))
                                    (append (org-glance-headline-metadata-v2:tags m) nil))))
                  (cl-every (lambda (tag) (member tag have)) want)))
              clauses)))
    (when (plist-member spec :state)
      ;; A headline with no todo keyword carries state "" (not nil); treat a nil
      ;; or "" filter value alike, so `(:state nil)' means "stateless".
      (let ((want (or (plist-get spec :state) "")))
        (push (lambda (m) (equal want (or (org-glance-headline-metadata-v2:state m) ""))) clauses)))
    (when (plist-member spec :done)
      ;; `done?' reads the buffer-local `org-done-keywords', which is nil outside
      ;; an Org buffer; capture the user's set once so the filter is deterministic
      ;; regardless of which buffer the render runs in.
      (let ((want (and (plist-get spec :done) t))
            (done-keywords (org-glance--done-keywords)))
        (push (lambda (m)
                (let ((org-done-keywords done-keywords))
                  (eq want (org-glance-headline-metadata-v2:done? m))))
              clauses)))
    (when (plist-member spec :id)
      (let ((want (plist-get spec :id)))
        (push (lambda (m) (equal want (org-glance-headline-metadata-v2:id m))) clauses)))
    (when (plist-member spec :title)
      (let ((want (plist-get spec :title)))
        (push (lambda (m) (equal want (org-glance-headline-metadata-v2:title m))) clauses)))
    (when (plist-member spec :hash)
      (let ((want (plist-get spec :hash)))
        (push (lambda (m) (equal want (org-glance-headline-metadata-v2:hash m))) clauses)))
    (when (plist-member spec :priority)
      (let ((want (plist-get spec :priority)))
        (push (lambda (m) (eql want (org-glance-headline-metadata-v2:priority m))) clauses)))
    (cl-loop for (key . acc) in (list (cons :linked #'org-glance-headline-metadata-v2:linked?)
                                      (cons :propertized #'org-glance-headline-metadata-v2:propertized?)
                                      (cons :encrypted #'org-glance-headline-metadata-v2:encrypted?))
             when (plist-member spec key)
             do (let ((want (and (plist-get spec key) t)) (acc acc))
                  (push (lambda (m) (eq want (and (funcall acc m) t))) clauses)))
    (cl-loop for (key . acc) in (list (cons :schedule #'org-glance-headline-metadata-v2:schedule)
                                      (cons :deadline #'org-glance-headline-metadata-v2:deadline))
             when (plist-member spec key)
             do (let ((want (plist-get spec key)) (acc acc))
                  (push (lambda (m)
                          (let ((present (let ((v (funcall acc m)))
                                           (and (stringp v) (not (string-empty-p v))))))
                            (pcase want
                              (:present present)
                              (:absent (not present))
                              (_ (error "%S filter expects :present or :absent, got %S" key want)))))
                        clauses)))
    (when (plist-member spec :where)
      (let ((fn (plist-get spec :where)))
        (cl-check-type fn function)
        (push fn clauses)))
    (if clauses
        (lambda (m) (cl-every (lambda (clause) (funcall clause m)) clauses))
      (lambda (_m) t))))

(cl-defun org-glance-overview-v2:--key-value (key value)
  "Human-readable printed form of one canonical filter VALUE under KEY.
Used only for the greppable slug; ambiguity here is harmless (a sha1 over an
unambiguous encoding disambiguates -- see `org-glance-overview-v2:spec-key')."
  (cond
   ((eq key :tags) (s-join "," (mapcar (lambda (x) (downcase (format "%s" x))) value)))
   ((eq value t) "t")
   ((null value) "nil")
   ((keywordp value) (substring (symbol-name value) 1))
   ((numberp value) (number-to-string value))
   (t (format "%s" value))))

(cl-defun org-glance-overview-v2:--canonical-pairs (spec)
  "Order-independent (KEYWORD . VALUE) alist for normalised SPEC.
Keys sorted; tag lists sorted and downcased; other values kept as their Lisp
data so that `prin1' renders them unambiguously."
  (sort (cl-loop for (k v) on spec by #'cddr
                 collect (cons k (if (eq k :tags)
                                     (sort (mapcar (lambda (x) (downcase (format "%s" x))) v) #'string<)
                                   v)))
        (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))

(cl-defun org-glance-overview-v2:--fs-safe (readable hash-input)
  "One filesystem-safe path segment: a greppable slug from READABLE + a sha1.
READABLE is lossy/cosmetic; HASH-INPUT must be an unambiguous encoding of the
filter, so two distinct filters can never map to the same segment."
  (concat (s-truncate 64 (replace-regexp-in-string "[^[:alnum:]_.=&,-]" "_" readable) "")
          "-"
          (secure-hash 'sha1 hash-input)))

(cl-defun org-glance-overview-v2:spec-key (filter)
  "Return a stable, filesystem-safe cache key for FILTER.
\"all\" for the empty filter; nil when FILTER is uncacheable (carries `:where').
Two specs that differ only in key order or tag order map to the same key; two
specs that differ in any value never do (the sha1 is over an unambiguous form)."
  (let ((spec (org-glance-overview-v2:--normalize-spec filter)))
    (cond
     ((null spec) "all")
     ((plist-member spec :where) nil)
     (t (let ((pairs (org-glance-overview-v2:--canonical-pairs spec)))
          (if (null pairs)
              "all"
            (org-glance-overview-v2:--fs-safe
             (s-join "&" (cl-loop for (k . v) in pairs
                                  collect (concat (substring (symbol-name k) 1) "="
                                                  (org-glance-overview-v2:--key-value k v))))
             (prin1-to-string pairs))))))))

;;; Rendering (from metadata, not content)

(cl-defun org-glance-overview-v2:tag-string (metadata)
  (when-let ((tags (append (org-glance-headline-metadata-v2:tags metadata) nil)))
    (format "  :%s:" (s-join ":" (mapcar (lambda (x) (format "%s" x)) tags)))))

(cl-defun org-glance-overview-v2:render-headline (metadata)
  "Render METADATA as one org heading + planning + ORG_GLANCE_ID property."
  (let ((state (org-glance-headline-metadata-v2:state metadata))
        (schedule (org-glance-headline-metadata-v2:schedule metadata))
        (deadline (org-glance-headline-metadata-v2:deadline metadata)))
    (concat "* "
            (if (and (stringp state) (not (string-empty-p state))) (concat state " ") "")
            (org-glance-headline-metadata-v2:title metadata)
            (or (org-glance-overview-v2:tag-string metadata) "")
            "\n"
            (when (and (stringp schedule) (not (string-empty-p schedule)))
              (concat "SCHEDULED: " schedule "\n"))
            (when (and (stringp deadline) (not (string-empty-p deadline)))
              (concat "DEADLINE: " deadline "\n"))
            ":PROPERTIES:\n:ORG_GLANCE_ID: " (org-glance-headline-metadata-v2:id metadata) "\n:END:\n")))

(cl-defun org-glance-overview-v2:render (graph &optional filter)
  "Render GRAPH's live headlines matching FILTER as org text.
FILTER is nil (all), a bare tag, or a filter plist -- see
`org-glance-overview-v2:spec-predicate'."
  (cl-check-type graph org-glance-graph-v2)
  (let ((keep? (org-glance-overview-v2:spec-predicate filter)))
    (concat org-glance-overview-v2:header
            (cl-loop for meta in (org-glance-graph-v2:headlines graph)
                     when (funcall keep? meta)
                     concat (org-glance-overview-v2:render-headline meta)))))

;;; Cache paths + freshness

(cl-defun org-glance-overview-v2:file (graph)
  "Path to GRAPH's unfiltered overview file (inside the hidden store)."
  (cl-check-type graph org-glance-graph-v2)
  (f-join (org-glance-graph-v2:store-path graph) "overview.org"))

(cl-defun org-glance-overview-v2:cache-path (graph)
  "Directory holding GRAPH's filtered (cached) overviews."
  (cl-check-type graph org-glance-graph-v2)
  (f-join (org-glance-graph-v2:store-path graph) "overviews"))

(cl-defun org-glance-overview-v2:spec-cache-file (graph filter)
  "File backing FILTER's overview under GRAPH.
The empty filter uses the legacy unfiltered file; an uncacheable `:where' filter
uses a shared transient file (so it never clobbers a real cache); every other
filter gets `<cache-path>/<key>/overview.org'."
  (let ((key (org-glance-overview-v2:spec-key filter)))
    (cond
     ((null key) (f-join (org-glance-overview-v2:cache-path graph) "transient.org"))
     ((string= key "all") (org-glance-overview-v2:file graph))
     (t (f-join (org-glance-overview-v2:cache-path graph) key "overview.org")))))

(cl-defun org-glance-overview-v2:--mtime (path)
  (file-attribute-modification-time (file-attributes path)))

(cl-defun org-glance-overview-v2:fresh? (graph file)
  "Non-nil if FILE exists and is newer than GRAPH's `headlines.jsonl'.
The comparison is strict, so a same-second cache is treated as stale and
rebuilt: serving stale content is the only real bug, a rebuild is just a perf
cost.  A future-dated source (clock skew / restored backup) also rebuilds."
  (cl-check-type graph org-glance-graph-v2)
  (let ((src (org-glance-graph-v2:headline-meta-path graph)))
    (and (f-exists? file)
         (or (not (f-exists? src))
             (time-less-p (org-glance-overview-v2:--mtime src)
                          (org-glance-overview-v2:--mtime file))))))

(cl-defun org-glance-overview-v2:write (graph &optional filter)
  "Unconditionally (re)generate FILTER's overview file for GRAPH; return its path.
Used by the agenda and by `g' (refresh), which must always rebuild."
  (let ((file (org-glance-overview-v2:spec-cache-file graph filter)))
    (f-mkdir-full-path (f-dirname file))
    (f-write-text (org-glance-overview-v2:render graph filter) 'utf-8 file)
    file))

(cl-defun org-glance-overview-v2:cached-file (graph &optional filter)
  "Path to FILTER's overview file for GRAPH, rebuilding only when stale.
On a cache hit the file is returned without reading `headlines.jsonl' or
rendering.  An uncacheable `:where' filter always re-renders."
  (let ((key (org-glance-overview-v2:spec-key filter))
        (file (org-glance-overview-v2:spec-cache-file graph filter)))
    (cond
     ((null key)                                       ; :where -- never cache
      (org-glance-overview-v2:write graph filter))
     ((org-glance-overview-v2:fresh? graph file) file) ; hit -- no read, no render
     (t (org-glance-overview-v2:write graph filter)))))

;;; Browser

(defvar org-glance-overview-v2-mode-map (make-sparse-keymap)
  "Keymap for `org-glance-overview-v2-mode'.")

(define-minor-mode org-glance-overview-v2-mode
  "Read-only browser over the v2 graph."
  :global nil
  :init-value nil
  :keymap org-glance-overview-v2-mode-map
  :after-hook (read-only-mode +1)
  (when org-glance-overview-v2-mode
    ;; org requires tab-width 8 to parse (`id-at-point' reads node properties).
    (setq tab-width 8 indent-tabs-mode nil)))

;; Movement mirrors the v1 overview (n/p between headings) plus f/b across
;; same-level siblings; actions act on the headline at point.
(define-key org-glance-overview-v2-mode-map (kbd "n") #'org-next-visible-heading)
(define-key org-glance-overview-v2-mode-map (kbd "p") #'org-previous-visible-heading)
(define-key org-glance-overview-v2-mode-map (kbd "f") #'org-forward-heading-same-level)
(define-key org-glance-overview-v2-mode-map (kbd "b") #'org-backward-heading-same-level)
(define-key org-glance-overview-v2-mode-map (kbd ",") #'beginning-of-buffer)
(define-key org-glance-overview-v2-mode-map (kbd "<") #'beginning-of-buffer)
(define-key org-glance-overview-v2-mode-map (kbd ".") #'end-of-buffer)
(define-key org-glance-overview-v2-mode-map (kbd ">") #'end-of-buffer)
(define-key org-glance-overview-v2-mode-map (kbd "TAB") #'org-cycle)
(define-key org-glance-overview-v2-mode-map (kbd "RET") #'org-glance-overview-v2:materialize)
(define-key org-glance-overview-v2-mode-map (kbd "m") #'org-glance-overview-v2:materialize)
(define-key org-glance-overview-v2-mode-map (kbd "o") #'org-glance-overview-v2:open)
(define-key org-glance-overview-v2-mode-map (kbd "e") #'org-glance-overview-v2:extract)
(define-key org-glance-overview-v2-mode-map (kbd "a") #'org-glance-agenda-v2)
(define-key org-glance-overview-v2-mode-map (kbd "g") #'org-glance-overview-v2:refresh)
(define-key org-glance-overview-v2-mode-map (kbd "q") #'quit-window)

(defvar-local org-glance-overview-v2--spec nil
  "Normalised filter spec the current overview buffer was generated with
(nil = all headlines).")

(cl-defun org-glance-overview-v2:id-at-point ()
  "ORG_GLANCE_ID of the headline at point, or signal a `user-error'."
  (or (save-excursion
        (org-back-to-heading t)
        (org-entry-get nil "ORG_GLANCE_ID"))
      (user-error "No headline at point")))

(cl-defun org-glance-overview-v2:materialize ()
  "Materialize the headline at point."
  (interactive)
  (switch-to-buffer (org-glance-material-v2:open org-glance-graph-v2 (org-glance-overview-v2:id-at-point))))

(cl-defun org-glance-overview-v2:--headline-at-point ()
  "The live `org-glance-headline-v2' for the heading at point.
The overview is a cached snapshot that can outlive the graph, so a heading may
name a headline that has since been deleted; error clearly rather than passing
nil into the material layer."
  (or (org-glance-graph-v2:headline org-glance-graph-v2 (org-glance-overview-v2:id-at-point))
      (user-error "Headline no longer in graph (overview is stale; press `g' to refresh)")))

(cl-defun org-glance-overview-v2:open ()
  "Open a link inside the headline at point."
  (interactive)
  (org-glance-material-v2:open-link (org-glance-overview-v2:--headline-at-point)))

(cl-defun org-glance-overview-v2:extract ()
  "Extract a key-value pair from the headline at point."
  (interactive)
  (org-glance-material-v2:extract (org-glance-overview-v2:--headline-at-point)))

(cl-defun org-glance-overview-v2:visit (graph &optional filter)
  "Open GRAPH's overview for FILTER read-only, serving the cache when fresh."
  (let* ((spec (org-glance-overview-v2:--normalize-spec filter))
         (file (org-glance-overview-v2:cached-file graph spec))
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
    (setq-local org-glance-overview-v2--spec spec)
    (org-glance-overview-v2-mode +1)
    (current-buffer)))

(cl-defun org-glance-overview-v2:refresh ()
  "Rebuild the current overview from the graph, ignoring the cache."
  (interactive)
  (org-glance-overview-v2:write org-glance-graph-v2 org-glance-overview-v2--spec)
  (let ((inhibit-read-only t))
    (revert-buffer t t t)))

(cl-defun org-glance-overview-v2 (&optional tag)
  "Browse the v2 graph, optionally filtered.
TAG may be a bare tag (symbol/string) or a full filter plist -- see
`org-glance-overview-v2:spec-predicate'."
  (interactive)
  (cl-assert (org-glance-initialized?-v2))
  (org-glance-overview-v2:visit org-glance-graph-v2 tag))

;;; Agenda

(cl-defun org-glance-agenda-v2 ()
  "Show an `org-agenda' over the v2 graph's scheduled/deadline headlines."
  (interactive)
  (cl-assert (org-glance-initialized?-v2))
  ;; Render to a dedicated file rather than overview.org, so pressing `a' from an
  ;; open overview never rewrites the file that buffer is visiting.
  (let* ((file (f-join (org-glance-overview-v2:cache-path org-glance-graph-v2) "agenda.org"))
         (org-agenda-files (list file))
         (org-agenda-start-on-weekday nil)
         (org-agenda-overriding-header "org-glance v2 agenda"))
    (f-mkdir-full-path (f-dirname file))
    (f-write-text (org-glance-overview-v2:render org-glance-graph-v2) 'utf-8 file)
    (org-agenda-list nil "-7d" 21)))

(provide 'org-glance-overview-v2)
;;; org-glance-overview-v2.el ends here
