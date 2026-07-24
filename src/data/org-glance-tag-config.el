;; -*- lexical-binding: t -*-
;; `org-glance-tag-config' -- a separate, optional per-tag configuration store.
;;
;; Tags are DISCOVERED from captured headlines (`org-glance-tag'); they need no
;; declaration.  A tag's CONFIG is optional metadata that bounds / describes a
;; tag: its capture skeleton and its todo-keyword cycle.  This config lives
;; OUTSIDE the content graph, so it never reaches tag discovery, overviews or
;; the capture picker.
;;
;; ONE Org file per tag, at `<store>/.org-glance/config/tags/<tag>.org', named
;; for the tag.  The file header carries the todo cycle as an org-native
;; `#+TODO:' keyword; the body (from the first `*') is the org-capture skeleton:
;;
;;   #+TITLE: Book
;;   #+TODO:  TODO READING | READ ABANDONED
;;
;;   * Book
;;   *** Notes
;;       %?
;;
;; The first heading's text is a placeholder; capture overwrites it with the
;; instance title + tags and prepends
;; the `#+TODO:' as a file keyword.  `#+TITLE:' is the human label for the file.
;;
;; A legacy single-file `config/tags.org' (level-1 `:TAG:' headlines) is split
;; into per-tag files at graph open (`--migrate-on-open') and backed up.
;;
;; Resolution is an O(1) lookup into a cache rebuilt only when the config
;; directory changes (mtime+size+names of its `*.org', mirroring the graph's
;; store snapshot).  Absent dir / absent file / absent dimension all degrade to
;; nil, so an unconfigured tag captures and renders byte-for-byte as before.

(require 's)
(require 'org)
(require 'cl-lib)
(require 'f)

(require 'org-glance-tag)
(require 'org-glance-headline)
(require 'org-glance-graph)
(require 'org-glance-filter)

(require 'org-glance-core)

(defcustom org-glance-tag-config-dir nil
  "Override directory for the per-tag configuration files.
Nil (default) means `config/tags/' under the current graph's store.  A string
sets an explicit directory (used for tests and non-standard layouts)."
  :group 'org-glance
  :type '(choice (const :tag "Default (store config/tags/)" nil) directory))

(cl-defstruct (org-glance-tag-config (:predicate org-glance-tag-config?)
                                     (:constructor org-glance-tag-config)
                                     (:conc-name org-glance-tag-config:))
  "A tag's optional configuration, projected from its `config/tags/<tag>.org'."
  (tag nil :read-only t :type symbol)                   ; the configured tag (= file name)
  (title nil :read-only t :type (or null string))       ; `#+TITLE:' human label, or nil
  (todo nil :read-only t :type (or null string))        ; `#+TODO:'-style cycle, or nil
  (template nil :read-only t :type (or null string)))   ; capture entry (from the first `*')

;;; Config file location

(cl-defun org-glance-tag-config:dir (graph)
  "Resolved config directory of GRAPH (may not exist).
`org-glance-tag-config-dir' overrides; else `config/tags/' under GRAPH's store.
Nil when neither is available (uninitialised, no override) -- callers then see
an empty config set."
  (or (and (stringp org-glance-tag-config-dir) org-glance-tag-config-dir)
      (and (org-glance-graph? graph)
           (org-glance-graph:config-file graph "tags"))))

(cl-defun org-glance-tag-config:file (graph tag)
  "Path of TAG's config file under GRAPH's config directory, or nil.
TAG is a tag symbol; the file is `<dir>/<tag>.org'."
  (when-let ((dir (org-glance-tag-config:dir graph)))
    (f-join dir (concat (org-glance-tag:to-string tag) ".org"))))

(cl-defun org-glance-tag-config:source-mtime (graph)
  "Newest mtime across GRAPH's per-tag config files, or nil when none exist.
Overview caches compare against this: editing any tag's cycle invalidates
them like a content change (a directory mtime alone misses in-file edits)."
  (when-let ((dir (org-glance-tag-config:dir graph)))
    (when (f-directory? dir)
      (cl-loop for path in (directory-files dir t "\\.org\\'")
               for m = (org-glance--file-mtime path)
               with newest = nil
               do (when (or (null newest) (time-less-p newest m)) (setq newest m))
               finally return newest))))

;;; Parse: config/tags/<tag>.org -> tag-symbol -> config

(cl-defun org-glance-tag-config--file-keyword (key)
  "Value of file keyword KEY (e.g. \"TITLE\", \"TODO\") in the current buffer.
Returns the trimmed value of the first `#+KEY:' line, or nil when absent or
blank."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s:[ \t]*\\(.*\\)$" (regexp-quote key)) nil t)
      (let ((v (s-trim (match-string-no-properties 1))))
        (unless (string-empty-p v) v)))))

(cl-defun org-glance-tag-config--entry ()
  "The capture entry of the current buffer: text from the first `*' heading to
EOF (trimmed), or nil when the buffer has no heading."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\*+ " nil t)
      (s-trim-right (buffer-substring-no-properties (line-beginning-position)
                                                    (point-max))))))

(cl-defun org-glance-tag-config--parse-file (path tag)
  "Parse config file PATH for TAG (a symbol) into an `org-glance-tag-config'."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents path))
    (org-glance-tag-config
     :tag tag
     :title (org-glance-tag-config--file-keyword "TITLE")
     ;; NB the cycle is a `#+TODO:' FILE keyword now (org-native), replacing the
     ;; old `:TODO_KEYWORDS:' drawer.
     :todo (org-glance-tag-config--file-keyword "TODO")
     :template (org-glance-tag-config--entry))))

(cl-defun org-glance-tag-config--tag-of-file (path)
  "The tag symbol a config file PATH configures: its basename, sans `.org'."
  (org-glance-tag:from-string (f-base path)))

(cl-defun org-glance-tag-config--parse (dir)
  "Parse DIR into a tag-symbol -> `org-glance-tag-config' hash (empty if absent).
Each `<tag>.org' contributes one config keyed by its file name."
  (let ((by-tag (make-hash-table :test 'eq)))
    (when (and dir (f-directory? dir))
      (dolist (path (directory-files dir t "\\.org\\'"))
        (let ((tag (org-glance-tag-config--tag-of-file path)))
          (when (and (org-glance-tag? tag) (not (gethash tag by-tag)))
            (puthash tag (org-glance-tag-config--parse-file path tag) by-tag)))))
    by-tag))

;;; In-memory cache (directory snapshot, mirroring the graph's read cache)

(defvar org-glance-tag-config--cache nil
  "Module cache plist (:dir D :snapshot S :by-tag HASH), or nil (cold).")

(cl-defun org-glance-tag-config--snapshot (dir)
  "A value identifying DIR's state for cache validity, or nil.
The sorted (NAME MTIME SIZE) list of its `*.org' files, so an edit, an added or
a removed config all invalidate."
  (when (and dir (f-directory? dir))
    (cl-loop for path in (sort (directory-files dir t "\\.org\\'") #'string<)
             for attrs = (file-attributes path)
             collect (list (f-filename path)
                           (file-attribute-modification-time attrs)
                           (file-attribute-size attrs)))))

(cl-defun org-glance-tag-config--invalidate ()
  "Drop the in-memory config cache, so the next resolve re-reads the files.
Called after our own writes (the `-edit' save, the migration), so we never
depend on mtime granularity for in-process edits."
  (setq org-glance-tag-config--cache nil))

(cl-defun org-glance-tag-config--by-tag (graph)
  "GRAPH's cached tag -> config hash, rebuilt iff its dir or snapshot changed."
  (let* ((dir (org-glance-tag-config:dir graph))
         (snapshot (org-glance-tag-config--snapshot dir))
         (cache org-glance-tag-config--cache))
    (unless (and cache
                 (equal (plist-get cache :dir) dir)
                 (equal (plist-get cache :snapshot) snapshot))
      (setq org-glance-tag-config--cache
            (list :dir dir :snapshot snapshot
                  :by-tag (org-glance-tag-config--parse dir))))
    (plist-get org-glance-tag-config--cache :by-tag)))

;;; Resolution

(cl-defun org-glance-tag-config:resolve (graph tag)
  "Resolve TAG to its `org-glance-tag-config' in GRAPH, or nil if it has none.
A nil result is the identity: TAG captures with the default template and renders
with the global todo keywords -- the graceful-degradation path."
  (cl-check-type tag org-glance-tag)
  (gethash tag (org-glance-tag-config--by-tag graph)))

(cl-defun org-glance-tag-config:cycle->keywords (cycle)
  "The `org-todo-keywords' value for a tag CYCLE string: one `:sequence'."
  (list (cons 'sequence (split-string cycle))))

(cl-defun org-glance-tag-config:cycle->keywords-or (cycle default)
  "CYCLE's `org-todo-keywords' form, or DEFAULT when CYCLE is nil."
  (if cycle (org-glance-tag-config:cycle->keywords cycle) default))

(cl-defun org-glance-tag-config:done-keywords-for-filter (graph filter)
  "The done-keyword set FILTER's views should honour in GRAPH.
The single configured tag's cycle when FILTER names one, else the global
done set (`org-glance--done-keywords').  Bind to `org-done-keywords' while
building a `:done' predicate or a badge split."
  (if-let ((cycle (org-glance-tag-config:cycle-for-filter graph filter)))
      (org-glance-tag-config:done-keywords cycle)
    (org-glance--done-keywords)))

(cl-defun org-glance-tag-config:done-keywords (todo-spec)
  "The done keywords of TODO-SPEC (everything after the last `|'), or nil.
Derived by org itself from the verbatim cycle string, so the active/done split
is exactly the one a `#+TODO:' header produces -- single source of truth."
  (when (org-glance--present-string? todo-spec)
    (with-temp-buffer
      (let ((org-todo-keywords (org-glance-tag-config:cycle->keywords todo-spec)))
        (delay-mode-hooks (org-mode))
        (copy-sequence org-done-keywords)))))

(cl-defun org-glance-tag-config:cycle-for-filter (graph filter)
  "The single todo cycle configured for FILTER's tags, or nil.
Returns a cycle only when EXACTLY ONE distinct cycle is configured across the
filtered tags: merging keyword sequences is order-sensitive and corrupts the
active/done split, so 0 or >1 distinct cycles fall back to the global keywords."
  ;; `org-glance-filter:tags' already returns canonical (downcased) tag symbols,
  ;; so each resolves directly; configs without a `#+TODO:' cycle drop out.
  (let ((cycles (cl-remove-duplicates
                 (delq nil (mapcar (lambda (tag)
                                     (when-let ((c (org-glance-tag-config:resolve graph tag)))
                                       (org-glance-tag-config:todo c)))
                                   (org-glance-filter:tags filter)))
                 :test #'string=)))
    (when (= 1 (length cycles))
      (car cycles))))

;;; Render: a config -> an `org-capture' entry template string

(defconst org-glance-tag-config--render-strip
  (append '("TAG" "TODO_KEYWORDS") org-glance-headline:hash-ignore-properties)
  "Drawer keys stripped from a rendered capture instance.
The current format keeps none of these; they are stripped defensively so a
migrated or hand-edited entry that retained an old `:TAG:'/`:TODO_KEYWORDS:'
drawer key still renders clean.  The cycle is applied as a `#+TODO:' FILE
keyword, never an instance drawer property.")

(cl-defun org-glance-tag-config:render (config title tags)
  "Render an `org-capture' entry template for an instance of CONFIG.
TITLE pre-fills the entry heading; TAGS are the instance org tags.  The
config's capture entry (its `%^{...}' prompts, property defaults and body
skeleton) is preserved verbatim; residual config drawer keys are stripped.
A `%?' is appended to the heading only when the skeleton carries none of its
own (in its body OR a kept drawer property); a well-formed skeleton therefore
yields exactly one (org-capture honours only the first)."
  (cl-check-type config org-glance-tag-config)
  (cl-check-type title string)
  (let ((template (org-glance-tag-config:template config))
        (tags (org-glance-tag:as-list tags)))
    (unless (org-glance--present-string? template)
      (user-error "org-glance: tag config for `%s' has no capture entry (add a `*' heading)"
                  (org-glance-tag-config:tag config)))
    (with-temp-buffer
      (insert template)
      (org-glance--org-mode)
      (dolist (property org-glance-tag-config--render-strip)
        (goto-char (point-min))
        (org-entry-delete nil property))
      (goto-char (point-min))
      ;; Append `%?' to the heading only if the skeleton has NO capture point of
      ;; its own.  Scan the WHOLE entry (body AND kept drawer properties -- a
      ;; preserved `:NOTE: %?' counts); the title is inserted below, so it can't
      ;; be mistaken for the skeleton's own marker.
      (let ((skeleton-has-point (s-contains? "%?" (buffer-substring-no-properties
                                                   (point-min) (point-max)))))
        (org-edit-headline (concat title (if skeleton-has-point "" "%?")))
        (goto-char (point-min))
        (org-set-tags (mapcar #'org-glance-tag:to-string tags)))
      (s-trim-right (buffer-substring-no-properties (point-min) (point-max))))))

;;; Migration: legacy single-file config/tags.org -> per-tag files

(cl-defun org-glance-tag-config--migrate-on-open (graph)
  "Split a legacy single-file `config/tags.org' into per-tag files, once.
`org-glance-graph-after-open-functions' hook (error-demoted by core,
invariant 9): each level-1 `:TAG:' headline becomes `config/tags/<tag>.org'
-- its heading text is the `#+TITLE:', its `:TODO_KEYWORDS:' the `#+TODO:',
its subtree minus those keys the capture entry.  An already-authored per-tag
file is never clobbered.  The legacy file is renamed `tags.org.bak' (kept,
never deleted -- migration discipline)."
  (when-let* ((legacy (org-glance-graph:config-file graph "tags.org"))
              ((f-exists? legacy))
              (dir (org-glance-tag-config:dir graph)))
    (f-mkdir-full-path dir)
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents legacy))
      (org-glance--org-mode)
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (org-back-to-heading t)
        (let* ((tag-raw (org-entry-get nil "TAG"))
               (tag (and tag-raw (org-glance-tag:from-string tag-raw)))
               (todo (org-entry-get nil "TODO_KEYWORDS"))
               (title (org-get-heading t t t t))
               (beg (point))
               (end (save-excursion (org-end-of-subtree t t) (point))))
          (when (and (org-glance-tag? tag)
                     (not (f-exists? (org-glance-tag-config:file graph tag))))
            (let* ((subtree (buffer-substring-no-properties beg end))
                   (entry (with-temp-buffer
                            (insert subtree)
                            (org-glance--org-mode)
                            (goto-char (point-min))
                            (org-entry-delete nil "TAG")
                            (org-entry-delete nil "TODO_KEYWORDS")
                            (s-trim-right (buffer-substring-no-properties
                                           (point-min) (point-max))))))
              (f-write-text
               (concat "#+TITLE: " (if (org-glance--present-string? title)
                                       title (symbol-name tag)) "\n"
                       (when (org-glance--present-string? todo)
                         (concat "#+TODO:  " (s-trim todo) "\n"))
                       "\n" entry "\n")
               'utf-8 (org-glance-tag-config:file graph tag))))
          (goto-char end))))
    (rename-file legacy (concat legacy ".bak") t)
    (org-glance-tag-config--invalidate)))

(add-hook 'org-glance-graph-after-open-functions
          #'org-glance-tag-config--migrate-on-open)

;;; Authoring

(defconst org-glance-tag-config--stub-header
  "# This file is TAG's config (its name is the tag).  The `#+TODO:' cycle is
# org-native (edit or delete it to use the global keywords).  The body below
# the first `*' is the org-capture skeleton: the heading is filled with the
# captured title + tags; use %^{...} prompts and one %?.
"
  "Comment header written at the top of a freshly-stubbed per-tag config.")

(cl-defun org-glance-tag-config--stub (tag)
  "Worked-example contents for a freshly-created config file of TAG."
  (concat "#+TITLE: " (capitalize (symbol-name tag)) "\n"
          "#+TODO:  TODO | DONE\n\n"
          org-glance-tag-config--stub-header
          "\n* " (capitalize (symbol-name tag)) "\n*** Notes\n    %?\n"))

(cl-defun org-glance-tag-config--edit-candidates (graph)
  "Sorted tags to offer for configuration: GRAPH's live tags + existing configs."
  (org-glance--sorted-distinct
   (append (and (org-glance-graph? graph) (org-glance-graph:tags graph))
           (let ((dir (org-glance-tag-config:dir graph)))
             (and dir (f-directory? dir)
                  (mapcar #'f-base (directory-files dir nil "\\.org\\'")))))))

(cl-defun org-glance-tag-config--lint ()
  "Advisory lint of the current per-tag config buffer; never blocks the save.
Flags a residual `:TODO_KEYWORDS:'/`:TAG:' drawer (superseded by `#+TODO:'
and the file name) and a missing capture entry.  Shows issues via
`display-warning' and returns them as a list of strings."
  (let (issues)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*:TODO_KEYWORDS:" nil t)
        (push "`:TODO_KEYWORDS:' drawer is ignored -- set the cycle with a `#+TODO:' file keyword"
              issues))
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*:TAG:" nil t)
        (push "`:TAG:' drawer is ignored -- the file name is the tag" issues))
      (goto-char (point-min))
      (unless (re-search-forward "^\\*+ " nil t)
        (push "no capture entry -- add a `*' heading (org-capture needs one)" issues)))
    (setq issues (nreverse issues))
    (when issues
      (display-warning 'org-glance
                       (concat "tag config issues:\n- " (mapconcat #'identity issues "\n- "))
                       :warning))
    issues))

(cl-defun org-glance-tag-config--on-save ()
  "After-save hook for a config buffer: invalidate the read cache, then lint."
  (org-glance-tag-config--invalidate)
  (org-glance-tag-config--lint))

;;;###autoload
(cl-defun org-glance-tag-config-edit (&optional tag)
  "Open a tag's configuration file for editing, creating a stub if absent.
Prompts for TAG (completing over live tags + existing configs); `C' in the
transient."
  (interactive)
  (let* ((graph (and (org-glance-initialized?) org-glance-graph))
         (tag-str (or tag (completing-read "Configure tag: "
                                           (org-glance-tag-config--edit-candidates graph))))
         (tag-sym (org-glance-tag:from-string (org-glance-tag:validate-string tag-str)))
         (path (or (org-glance-tag-config:file graph tag-sym)
                   (user-error "org-glance: not initialised and no `org-glance-tag-config-dir' set"))))
    (unless (f-exists? path)
      (f-mkdir-full-path (f-dirname path))
      (f-write-text (org-glance-tag-config--stub tag-sym) 'utf-8 path))
    (find-file path)
    (add-hook 'after-save-hook #'org-glance-tag-config--on-save nil t)))

(provide 'org-glance-tag-config)
