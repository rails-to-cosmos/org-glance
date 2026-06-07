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
;; cacheable filter gets its own directory `<store>/overviews/<key>/overview.org'
;; (<key> = a short hash of the canonical filter; the SPEC sidecar inside
;; records the filter readably).
;; A cached file is served untouched -- no JSONL read, no render -- while it is
;; newer than the append-only `headlines.jsonl' (`:fresh?'), and rebuilt
;; otherwise.  An uncacheable `:where' filter renders every time to a transient
;; file so it never clobbers a real cache.
;;
;; Browse with `org-glance-overview-v2'; act on the headline at point.

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'f)
(require 's)
(require 'org)
(require 'org-agenda)
(require 'transient)

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

(defconst org-glance-overview-v2:filter-table
  ;; KEY -> plist:
  ;;   :match    clause kind built by `--match-clause' (nil = the clause is
  ;;             structural and hand-built in `spec-predicate': :done is
  ;;             parameterised by :done-keywords; :where is a raw predicate;
  ;;             :done-keywords contributes no clause of its own)
  ;;   :accessor metadata accessor the clause tests
  ;;   :canon    canonicalisation kind shared by the SPEC identity and the
  ;;             cache key (see `--canon-value'; nil = as-is)
  `((:tags           :match member-all     :canon tags
                     :accessor ,#'org-glance-headline-metadata-v2:tags)
    (:state          :match state-equal
                     :accessor ,#'org-glance-headline-metadata-v2:state)
    (:done)
    (:done-keywords  :canon string-list)
    (:id             :match equal          :accessor ,#'org-glance-headline-metadata-v2:id)
    (:title          :match equal          :accessor ,#'org-glance-headline-metadata-v2:title)
    (:title-contains :match substring      :canon downcase
                     :accessor ,#'org-glance-headline-metadata-v2:title)
    (:hash           :match equal          :accessor ,#'org-glance-headline-metadata-v2:hash)
    (:priority       :match eql            :accessor ,#'org-glance-headline-metadata-v2:priority)
    (:linked         :match bool           :accessor ,#'org-glance-headline-metadata-v2:linked?)
    (:propertized    :match bool           :accessor ,#'org-glance-headline-metadata-v2:propertized?)
    (:encrypted      :match bool           :accessor ,#'org-glance-headline-metadata-v2:encrypted?)
    (:schedule       :match present-absent :accessor ,#'org-glance-headline-metadata-v2:schedule)
    (:deadline       :match present-absent :accessor ,#'org-glance-headline-metadata-v2:deadline)
    (:where))
  "The single source of truth for the overview filter language.
Drives `filter-keys', `spec-predicate' and `--canonical-pairs' together --
previously each new key meant four lockstep edits, two of them
silently forgettable (a missing predicate clause made the filter match
EVERYTHING; a missing canonicalisation case fragmented the cache).  Adding a
plain value-key = one row here.  `:title' is exact; `:title-contains' is a
case-insensitive substring.")

(defconst org-glance-overview-v2:filter-keys
  (mapcar #'car org-glance-overview-v2:filter-table)
  "Recognised keys in a normalised overview filter spec (`:tag' folds into
`:tags').  Derived from `org-glance-overview-v2:filter-table'.")

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
      ;; :done-keywords only parameterises :done; without :done it changes
      ;; nothing, so drop it (avoids a no-op filter and a fragmented cache).
      (when (and (plist-member out :done-keywords) (not (plist-member out :done)))
        (cl-remf out :done-keywords))
      (cl-loop for (k _v) on out by #'cddr
               unless (memq k org-glance-overview-v2:filter-keys)
               do (error "Unrecognised overview filter key: %S" k))
      out))
   (t (error "Invalid overview filter: %S" filter))))

(cl-defun org-glance-overview-v2:--match-clause (kind accessor want)
  "Build one predicate clause testing ACCESSOR's value against WANT, per KIND."
  (pcase kind
    ;; Tags are stored interned + downcased, so compare case-insensitively
    ;; (mirrors v1's `org-glance:tag-filter').
    ('member-all (let ((want (mapcar (lambda (x) (downcase (format "%s" x))) want)))
                   (lambda (m)
                     (let ((have (mapcar (lambda (x) (downcase (format "%s" x)))
                                         (append (funcall accessor m) nil))))
                       (cl-every (lambda (tag) (member tag have)) want)))))
    ;; A headline with no todo keyword carries state "" (not nil); treat a nil
    ;; or "" filter value alike, so `(:state nil)' means "stateless".
    ('state-equal (let ((want (or want "")))
                    (lambda (m) (equal want (or (funcall accessor m) "")))))
    ('equal (lambda (m) (equal want (funcall accessor m))))
    ('eql (lambda (m) (eql want (funcall accessor m))))
    ('bool (let ((want (and want t)))
             (lambda (m) (eq want (and (funcall accessor m) t)))))
    ;; Case-insensitive substring (the interactive `/' refinement).
    ('substring (let ((needle (downcase (format "%s" want))))
                  (lambda (m) (s-contains? needle (downcase (or (funcall accessor m) ""))))))
    ('present-absent (lambda (m)
                       (let ((present (let ((v (funcall accessor m)))
                                        (and (stringp v) (not (string-empty-p v))))))
                         (pcase want
                           (:present present)
                           (:absent (not present))
                           (_ (error "Planning filter expects :present or :absent, got %S" want))))))
    (_ (error "org-glance: filter key has no :match kind: %S" kind))))

(cl-defun org-glance-overview-v2:spec-predicate (filter)
  "Return a predicate (lambda (METADATA) -> generalized-boolean) for FILTER.
A nil/empty filter yields a predicate that accepts every headline.  Each present
clause must hold (logical AND).  See `org-glance-overview-v2:filter-table'."
  (let ((spec (org-glance-overview-v2:--normalize-spec filter))
        (clauses nil))
    ;; Structural specials the table cannot express:
    (when (plist-member spec :done)
      ;; `done?' reads the buffer-local `org-done-keywords', nil outside an Org
      ;; buffer; capture a concrete set once so the filter is deterministic.  A
      ;; per-overview `:done-keywords' wins over the global default chain.
      (let ((want (and (plist-get spec :done) t))
            (done-keywords (or (plist-get spec :done-keywords)
                               (org-glance--done-keywords))))
        (push (lambda (m)
                (let ((org-done-keywords done-keywords))
                  (eq want (org-glance-headline-metadata-v2:done? m))))
              clauses)))
    (when (plist-member spec :where)
      (let ((fn (plist-get spec :where)))
        (cl-check-type fn function)
        (push fn clauses)))
    ;; Everything else is table-driven: one row per key.
    (cl-loop for (key . props) in org-glance-overview-v2:filter-table
             for kind = (plist-get props :match)
             when (and kind (plist-member spec key))
             do (push (org-glance-overview-v2:--match-clause
                       kind (plist-get props :accessor) (plist-get spec key))
                      clauses))
    (if clauses
        (lambda (m) (cl-every (lambda (clause) (funcall clause m)) clauses))
      (lambda (_m) t))))

(cl-defun org-glance-overview-v2:--canon-value (key value)
  "Canonical form of VALUE under KEY, per the table's :canon kind.
Shared by the SPEC identity and the cache key (a hash of that identity)."
  (pcase (plist-get (alist-get key org-glance-overview-v2:filter-table) :canon)
    ('tags (sort (mapcar (lambda (x) (downcase (format "%s" x))) value) #'string<))
    ('string-list (sort (mapcar (lambda (x) (format "%s" x)) value) #'string<))
    ('downcase (downcase (format "%s" value)))
    (_ value)))

(cl-defun org-glance-overview-v2:--canonical-pairs (spec)
  "Order-independent (KEY . VALUE) alist for normalised SPEC.
Keys sorted; values canonicalised per the table so that `prin1' renders them
unambiguously."
  (sort (cl-loop for (k v) on spec by #'cddr
                 collect (cons k (org-glance-overview-v2:--canon-value k v)))
        (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))

(cl-defun org-glance-overview-v2:spec-key (filter)
  "Return a compact, deterministic cache key for FILTER.
\"all\" for the empty filter; nil when FILTER is uncacheable (carries `:where').
The key is the first 12 hex chars of the SHA-1 of the canonical spec identity
(`org-glance-overview-v2:--spec-identity'), so specs differing only in key
order or tag order map to the same key, every machine derives the same name
for the same filter, and the name is filesystem-safe by construction.  The
prefix is still LOSSY: distinct specs can (astronomically rarely) share a key;
the SPEC sidecar check in `cached-file' turns such a collision into a rebuild,
never into serving the wrong overview.  The sidecar, not the name, is the
human-readable record of which filter a cache directory holds."
  (let ((spec (org-glance-overview-v2:--normalize-spec filter)))
    (cond
     ((null spec) "all")
     ((plist-member spec :where) nil)
     (t (substring (secure-hash 'sha1 (org-glance-overview-v2:--spec-identity spec))
                   0 12)))))

(cl-defun org-glance-overview-v2:--spec-identity (filter)
  "Unambiguous printed identity of FILTER's canonical form.
This -- not the lossy directory name -- is what makes two filters \"the same\"."
  (prin1-to-string (org-glance-overview-v2:--canonical-pairs
                    (org-glance-overview-v2:--normalize-spec filter))))

(cl-defun org-glance-overview-v2:--spec-sidecar (file)
  "Path of the SPEC identity sidecar stored next to cache FILE."
  (f-join (f-dirname file) "SPEC"))

(cl-defun org-glance-overview-v2:--spec-owns-cache? (filter file)
  "Non-nil when FILE's SPEC sidecar records exactly FILTER's identity."
  (let ((sidecar (org-glance-overview-v2:--spec-sidecar file)))
    (and (f-exists? sidecar)
         (string= (org-glance-overview-v2:--spec-identity filter)
                  (s-trim (f-read-text sidecar 'utf-8))))))

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
Used by the agenda and by `g' (refresh), which must always rebuild.  A keyed
cache directory also gets its SPEC identity sidecar, claiming the (hashed,
lossy) directory name for exactly this filter and recording the filter
readably."
  (let ((key (org-glance-overview-v2:spec-key filter))
        (file (org-glance-overview-v2:spec-cache-file graph filter)))
    (f-mkdir-full-path (f-dirname file))
    (unless (member key '(nil "all"))
      (f-write-text (concat (org-glance-overview-v2:--spec-identity filter) "\n") 'utf-8
                    (org-glance-overview-v2:--spec-sidecar file)))
    (f-write-text (org-glance-overview-v2:render graph filter) 'utf-8 file)
    file))

(cl-defun org-glance-overview-v2:cached-file (graph &optional filter)
  "Path to FILTER's overview file for GRAPH, rebuilding only when stale.
A hit requires the file to be fresh AND, for keyed directories, the SPEC
sidecar to record exactly FILTER's identity -- directory names are truncated
hashes, so a (rare) name collision between distinct filters rebuilds instead
of serving the other filter's overview.  On a hit the file is returned without
reading
`headlines.jsonl' or rendering.  An uncacheable `:where' filter always
re-renders."
  (let ((key (org-glance-overview-v2:spec-key filter))
        (file (org-glance-overview-v2:spec-cache-file graph filter)))
    (cond
     ((null key)                                       ; :where -- never cache
      (org-glance-overview-v2:write graph filter))
     ((and (org-glance-overview-v2:fresh? graph file)
           (or (string= key "all")                     ; "all" never collides
               (org-glance-overview-v2:--spec-owns-cache? filter file)))
      file)                                            ; hit -- no read, no render
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
    (setq tab-width 8 indent-tabs-mode nil)
    ;; The lazy half of the no-outdated-results invariant: re-check freshness
    ;; whenever this buffer is (re)displayed or its window gets selected.
    (add-hook 'window-buffer-change-functions #'org-glance-overview-v2:--refresh-when-stale nil t)
    (add-hook 'window-selection-change-functions #'org-glance-overview-v2:--refresh-when-stale nil t)))

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

(cl-defun org-glance-overview-v2:tags (graph)
  "Distinct tags across GRAPH's live headlines, sorted."
  (org-glance-graph-v2:tags graph))

(cl-defun org-glance-overview-v2:states (graph)
  "Distinct non-empty todo states across GRAPH's live headlines, sorted."
  (sort (cl-remove-duplicates
         (cl-loop for meta in (org-glance-graph-v2:headlines graph)
                  for state = (org-glance-headline-metadata-v2:state meta)
                  when (and (stringp state) (not (string-empty-p state)))
                  collect state)
         :test #'string=)
        #'string<))

(cl-defun org-glance-overview-v2:completing-read-tag ()
  "Prompt for a tag from the graph's headlines; empty input means \"all\"."
  (cl-assert (org-glance-initialized?-v2))
  (let ((choice (completing-read "Overview tag (empty for all): "
                                 (org-glance-overview-v2:tags org-glance-graph-v2))))
    (unless (string-empty-p choice) choice)))

(cl-defun org-glance-overview-v2 (&optional tag)
  "Browse the v2 graph, optionally filtered.
Interactively, prompt for a tag (empty input = all headlines); the rendered
overview is cached per filter and served from the cache while the graph is
unchanged.  TAG may be a bare tag (symbol/string) or a full filter plist -- see
`org-glance-overview-v2:spec-predicate'."
  (interactive (list (org-glance-overview-v2:completing-read-tag)))
  (cl-assert (org-glance-initialized?-v2))
  (org-glance-overview-v2:visit org-glance-graph-v2 tag))

;;; Interactive filter refinement (the `/' transient)
;;
;; Each command composes one clause onto the CURRENT buffer's filter spec
;; (re-applying a dimension replaces its previous clause) and visits the
;; resulting overview -- which gets, or re-uses, its own cache like any other
;; filter.  The previous (less filtered) overview buffer stays around, so
;; narrowing is non-destructive.

(cl-defun org-glance-overview-v2:--refine (key value)
  "Re-visit the current overview with KEY VALUE composed onto its filter."
  (cl-assert (org-glance-initialized?-v2))
  (org-glance-overview-v2:visit
   org-glance-graph-v2
   (plist-put (copy-sequence org-glance-overview-v2--spec) key value)))

(cl-defun org-glance-overview-v2:filter-by-state ()
  "Narrow the current overview to headlines in a given todo state."
  (interactive)
  (cl-assert (org-glance-initialized?-v2))
  (let ((state (completing-read "Todo state: "
                                (org-glance-overview-v2:states org-glance-graph-v2))))
    (when (string-empty-p state) (user-error "No state given"))
    (org-glance-overview-v2:--refine :state state)))

(cl-defun org-glance-overview-v2:filter-by-substring ()
  "Narrow the current overview to headlines whose title contains a substring."
  (interactive)
  (let ((needle (read-string "Title contains: ")))
    (when (string-empty-p needle) (user-error "No substring given"))
    (org-glance-overview-v2:--refine :title-contains needle)))

(cl-defun org-glance-overview-v2:filter-clear ()
  "Drop all filters: visit the unfiltered overview."
  (interactive)
  (cl-assert (org-glance-initialized?-v2))
  (org-glance-overview-v2:visit org-glance-graph-v2 nil))

(transient-define-prefix org-glance-overview-v2-filter ()
  "Narrow the current overview by an additional criterion."
  ["Filter overview by"
   ("s" "Todo state" org-glance-overview-v2:filter-by-state)
   ("/" "Title substring" org-glance-overview-v2:filter-by-substring)
   ("c" "Clear (show all)" org-glance-overview-v2:filter-clear)])

(define-key org-glance-overview-v2-mode-map (kbd "/") #'org-glance-overview-v2-filter)

;;; View coherence: an overview buffer must never show outdated results
;;
;; Two complementary mechanisms enforce the invariant:
;;
;; 1. EAGER, SURGICAL push on the edit path: the material layer announces every
;;    metadata refresh (`org-glance-material-v2:sync-functions'); every open
;;    overview buffer of the graph patches JUST the affected headline in place
;;    and persists the patch to its cache file -- which thereby stays fresh and
;;    byte-identical to a full re-render.  A headline newly entering a filtered
;;    view falls back to a full rebuild of that buffer (its position depends on
;;    the whole record stream, so an in-place patch cannot be exact).
;;
;; 2. LAZY pull at the display boundary: whenever an overview buffer is (re)
;;    shown or its window selected, it re-checks freshness against the store
;;    and rebuilds if anything else mutated the graph meanwhile (capture,
;;    delete, reindex, compaction, another Emacs).

(cl-defun org-glance-overview-v2:--heading-region (id)
  "Start/end cons of ID's heading in the current overview buffer, or nil."
  (org-with-wide-buffer
   (goto-char (point-min))
   (when (re-search-forward (format "^:ORG_GLANCE_ID: %s$" (regexp-quote id)) nil t)
     (org-back-to-heading t)
     (cons (point) (progn (org-end-of-subtree t t) (point))))))

(cl-defun org-glance-overview-v2:--save-quietly ()
  "Persist the current (patched) overview buffer to its cache file.
Bypasses `save-buffer' and its hooks; the cache file ends up newer than the
store, i.e. fresh."
  (write-region (point-min) (point-max) buffer-file-name nil 'silent)
  (set-visited-file-modtime)
  (set-buffer-modified-p nil))

(cl-defun org-glance-overview-v2:--patch-headline (metadata)
  "Update METADATA's heading in the current overview buffer, in place.
Replace it when it still matches the buffer's filter, drop it when it no longer
does, or rebuild the whole view when it newly enters it."
  (let* ((id (org-glance-headline-metadata-v2:id metadata))
         (matches? (funcall (org-glance-overview-v2:spec-predicate org-glance-overview-v2--spec)
                            metadata))
         (region (org-glance-overview-v2:--heading-region id))
         (inhibit-read-only t))
    (cond
     ((and region matches?)             ; still visible -> replace in place
      (save-excursion
        (delete-region (car region) (cdr region))
        (goto-char (car region))
        (insert (org-glance-overview-v2:render-headline metadata)))
      (org-glance-overview-v2:--save-quietly))
     (region                            ; filtered out now -> drop the heading
      (save-excursion (delete-region (car region) (cdr region)))
      (org-glance-overview-v2:--save-quietly))
     (matches?                          ; newly visible -> exact position needs
      (org-glance-overview-v2:refresh))))) ; the full stream: rebuild this view

(cl-defun org-glance-overview-v2:on-headline-update (graph metadata)
  "Patch METADATA's heading into every open overview buffer of GRAPH.
Registered on `org-glance-material-v2:sync-functions'; a view update must never
break the save that triggered it, so per-buffer errors are demoted."
  (let ((store (org-glance-graph-v2:store-path graph)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (bound-and-true-p org-glance-overview-v2-mode)
                   buffer-file-name
                   (f-ancestor-of? store buffer-file-name))
          (with-demoted-errors "org-glance: overview update failed: %S"
            (org-glance-overview-v2:--patch-headline metadata)))))))

(add-hook 'org-glance-material-v2:sync-functions #'org-glance-overview-v2:on-headline-update)

(cl-defun org-glance-overview-v2:--stale? ()
  "Non-nil when the current overview buffer may show outdated results."
  (or (not (verify-visited-file-modtime (current-buffer))) ; file changed under us
      (not (org-glance-overview-v2:fresh? org-glance-graph-v2 buffer-file-name))))

(cl-defun org-glance-overview-v2:--refresh-when-stale (&optional window)
  "Rebuild WINDOW's (or the current) overview iff it no longer reflects the store.
The lazy half of the no-outdated-results invariant: runs whenever the buffer is
(re)displayed or its window selected."
  (with-current-buffer (if (windowp window) (window-buffer window) (current-buffer))
    (when (and (bound-and-true-p org-glance-overview-v2-mode)
               org-glance-graph-v2
               buffer-file-name
               (org-glance-overview-v2:--stale?))
      (with-demoted-errors "org-glance: overview refresh failed: %S"
        (org-glance-overview-v2:refresh)))))

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
