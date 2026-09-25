;; -*- lexical-binding: t -*-
;; `org-glance-tag-config' -- optional per-tag config (capture skeleton, todo
;; cycle) in one Org file per tag.  It lives OUTSIDE the content graph, so it
;; never reaches tag discovery, overviews or the capture picker.

(require 's)
(require 'org)
(require 'cl-lib)
(require 'f)

(require 'org-glance-tag)
(require 'org-glance-headline)
(require 'org-glance-graph)
(require 'org-glance-filter)
(require 'org-glance-utils)

(require 'org-glance-core)

(defcustom org-glance-tag-config-dir nil
  "Directory of per-tag config files, or nil for the store's `config/tags/'."
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

(defconst org-glance-tag-config:fields
  '((tag       nil        nil)     ; supplied by the file NAME
    (title     "TITLE"    nil)     ; human label; never emitted into a buffer
    (todo      "TODO"     t)       ; the todo cycle: capture + overview emit it
    (template  nil        nil))    ; the body from the first `*'
  "Per-tag config schema rows (SLOT PRAGMA EMIT?), checked against the struct.
PRAGMA: SLOT's `#+PRAGMA:' keyword, or nil; EMIT?: rendered buffers emit it.")

(cl-defun org-glance-tag-config--check-fields (slots fields)
  "Signal an error unless FIELDS lists struct SLOTS in order; else return t."
  (org-glance--check-struct-field-order
   :slots slots :fields fields :subject "tag-config"))

(org-glance-tag-config--check-fields
 (cdr (cl-struct-slot-info 'org-glance-tag-config))
 org-glance-tag-config:fields)

(cl-defun org-glance-tag-config--pragma-slots (&optional emitting)
  "Return (SLOT . PRAGMA) for each slot read from a `#+PRAGMA:' keyword.
With EMITTING, only those a rendered buffer emits."
  (cl-loop for (slot pragma emit) in org-glance-tag-config:fields
           when (and pragma (or (not emitting) emit))
           collect (cons slot pragma)))

(cl-defun org-glance-tag-config:dir (graph)
  "Return GRAPH's tag config directory, possibly nonexistent, or nil if none.
`org-glance-tag-config-dir' overrides the store's `config/tags/'."
  (or (and (stringp org-glance-tag-config-dir) org-glance-tag-config-dir)
      (and (org-glance-graph? graph)
           (org-glance-graph:config-file graph "tags"))))

(cl-defun org-glance-tag-config:file (graph tag)
  "Return tag symbol TAG's config file `<dir>/<tag>.org' in GRAPH, or nil."
  (when-let* ((dir (org-glance-tag-config:dir graph)))
    (f-join dir (concat (org-glance-tag:to-string tag) ".org"))))

(cl-defun org-glance-tag-config:source-mtime (graph)
  "Return the newest mtime of GRAPH's tag config files, or nil if none exist.
Overview caches compare against it, so a cycle edit invalidates them."
  (cl-loop for (_name mtime _size) in (org-glance-tag-config--snapshot
                                       (org-glance-tag-config:dir graph))
           with newest = nil
           do (when (or (null newest) (time-less-p newest mtime)) (setq newest mtime))
           finally return newest))

(cl-defun org-glance-tag-config--file-keyword (key)
  "Return the buffer's first `#+KEY:' value trimmed, or nil if absent or blank."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s:[ \t]*\\(.*\\)$" (regexp-quote key)) nil t)
      (let ((v (s-trim (match-string-no-properties 1))))
        (unless (string-empty-p v) v)))))

(cl-defun org-glance-tag-config--entry ()
  "Return the buffer's capture entry, its first heading to EOF, or nil if none."
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
    (apply #'org-glance-tag-config
           :tag tag
           :template (org-glance-tag-config--entry)
           (cl-loop for (slot . pragma) in (org-glance-tag-config--pragma-slots)
                    append (list (intern (concat ":" (symbol-name slot)))
                                 (org-glance-tag-config--file-keyword pragma))))))

(cl-defun org-glance-tag-config--tag-of-file (path)
  "Return the tag symbol config file PATH configures: its base name."
  (org-glance-tag:from-string (f-base path)))

(cl-defun org-glance-tag-config--parse (dir)
  "Parse DIR's `<tag>.org' files into a tag -> config hash (empty without DIR)."
  (let ((by-tag (make-hash-table :test 'eq)))
    (when (and dir (f-directory? dir))
      (dolist (path (directory-files dir t "\\.org\\'"))
        (let ((tag (org-glance-tag-config--tag-of-file path)))
          (when (and (org-glance-tag? tag) (not (gethash tag by-tag)))
            (puthash tag (org-glance-tag-config--parse-file path tag) by-tag)))))
    by-tag))

(defvar org-glance-tag-config--cache nil
  "Module cache plist (:dir D :snapshot S :by-tag HASH), or nil (cold).")

(cl-defun org-glance-tag-config--snapshot (dir)
  "Return DIR's cache key: its sorted `*.org' files' (NAME MTIME SIZE), or nil."
  (when (and dir (f-directory? dir))
    (cl-loop for path in (sort (directory-files dir t "\\.org\\'") #'string<)
             for attrs = (file-attributes path)
             collect (list (f-filename path)
                           (file-attribute-modification-time attrs)
                           (file-attribute-size attrs)))))

(cl-defun org-glance-tag-config--invalidate ()
  "Drop the in-memory config cache, so the next resolve re-reads the files.
Called after in-process writes, which mtime granularity could hide."
  (setq org-glance-tag-config--cache nil))

(cl-defun org-glance-tag-config--by-tag (graph)
  "Return GRAPH's tag -> config hash, re-parsed on a dir or snapshot change."
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

(cl-defun org-glance-tag-config:resolve (graph tag)
  "Resolve TAG to its `org-glance-tag-config' in GRAPH, or nil if it has none.
Nil means TAG uses the default capture template and global todo keywords."
  (cl-check-type tag org-glance-tag)
  (gethash tag (org-glance-tag-config--by-tag graph)))

(cl-defun org-glance-tag-config:cycle->keywords (cycle)
  "Return the `org-todo-keywords' value for tag CYCLE string: one `sequence'."
  (list (cons 'sequence (split-string cycle))))

(cl-defun org-glance-tag-config:cycle->keywords-or (cycle default)
  "Return CYCLE's `org-todo-keywords' form, or DEFAULT when CYCLE is nil."
  (if cycle (org-glance-tag-config:cycle->keywords cycle) default))

(cl-defun org-glance-tag-config:done-keywords-for-filter (graph filter)
  "Return the done keywords FILTER's views honour in GRAPH.
Those of FILTER's sole configured cycle, else `org-glance--done-keywords'.
Bind `org-done-keywords' to it to build a `:done' predicate or a badge split."
  (if-let* ((cycle (org-glance-tag-config:cycle-for-filter graph filter)))
      (org-glance-tag-config:done-keywords cycle)
    (org-glance--done-keywords)))

(cl-defun org-glance-tag-config:done-keywords (todo-spec)
  "Return the done keywords of TODO-SPEC, or nil when it is blank.
Org derives them, so the split matches what a `#+TODO:' header produces."
  (when (org-glance--present-string? todo-spec)
    (with-temp-buffer
      (let ((org-todo-keywords (org-glance-tag-config:cycle->keywords todo-spec)))
        (delay-mode-hooks (org-mode))
        (copy-sequence org-done-keywords)))))

(cl-defun org-glance-tag-config--sole-value (graph filter slot)
  "Return SLOT's value across FILTER's tags in GRAPH, if exactly one exists.
Else nil: merging keyword sequences would corrupt the active/done split."
  (let ((values (cl-remove-duplicates
                 (delq nil (mapcar (lambda (tag)
                                     (when-let* ((c (org-glance-tag-config:resolve graph tag)))
                                       (cl-struct-slot-value 'org-glance-tag-config slot c)))
                                   (org-glance-filter:tags filter)))
                 :test #'string=)))
    (when (= 1 (length values))
      (car values))))

(cl-defun org-glance-tag-config:cycle-for-filter (graph filter)
  "Return the sole distinct todo cycle across FILTER's tags in GRAPH, or nil."
  (org-glance-tag-config--sole-value graph filter 'todo))

(cl-defun org-glance-tag-config:preamble (config)
  "Return CONFIG's emittable pragmas as `#+KEY: VALUE' lines, or nil."
  (org-glance-tag-config--preamble-lines
   (cl-loop for (slot . pragma) in (org-glance-tag-config--pragma-slots t)
            collect (cons pragma (cl-struct-slot-value 'org-glance-tag-config
                                                       slot config)))))

(cl-defun org-glance-tag-config:preamble-for-filter (graph filter)
  "Return the `#+KEY: VALUE' lines FILTER's tags in GRAPH agree on, or nil.
A pragma agrees when it has exactly one distinct value (`--sole-value')."
  (org-glance-tag-config--preamble-lines
   (cl-loop for (slot . pragma) in (org-glance-tag-config--pragma-slots t)
            collect (cons pragma (org-glance-tag-config--sole-value graph filter slot)))))

(cl-defun org-glance-tag-config--preamble-lines (pairs)
  "Return PAIRS ((PRAGMA . VALUE)...) as `#+PRAGMA: VALUE' lines, or nil.
Blank values drop out."
  (let ((lines (cl-loop for (pragma . value) in pairs
                        when (org-glance--present-string? value)
                        collect (concat "#+" pragma ": " value "\n"))))
    (when lines (apply #'concat lines))))

(defconst org-glance-tag-config--render-strip
  (append '("TAG" "TODO_KEYWORDS") org-glance-headline:hash-ignore-properties)
  "Drawer keys stripped from a rendered capture instance.
Legacy `:TAG:'/`:TODO_KEYWORDS:' keys a migrated or hand-edited entry retained;
the cycle lives only in a `#+TODO:' file keyword.")

(cl-defun org-glance-tag-config:render (config title tags)
  "Render an `org-capture' template for an instance of CONFIG.
TITLE replaces the heading text, TAGS its tags, and `--render-strip' keys go;
the rest stays verbatim.  The heading gets a `%?' unless one sits below it.
Signal a `user-error' when CONFIG has no capture entry."
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
      ;; `org-edit-headline' overwrites a `%?' in the heading, so only a
      ;; capture point BELOW it counts.
      (let ((skeleton-has-point (save-excursion
                                  (end-of-line)
                                  (s-contains? "%?" (buffer-substring-no-properties
                                                     (point) (point-max))))))
        (org-edit-headline (concat title (if skeleton-has-point "" "%?")))
        (goto-char (point-min))
        (org-set-tags (mapcar #'org-glance-tag:to-string tags)))
      (s-trim-right (buffer-substring-no-properties (point-min) (point-max))))))

(cl-defun org-glance-tag-config--migrate-on-open (graph)
  "Split GRAPH's legacy `config/tags.org' into per-tag files, once.
On `org-glance-graph-after-open-functions' (error-demoted, invariant 9), each
level-1 `:TAG:' headline becomes `<tag>.org': heading text as `#+TITLE:',
`:TODO_KEYWORDS:' as `#+TODO:', subtree minus both keys as capture entry.
Existing per-tag files are kept; the legacy file becomes `tags.org.bak'."
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

(cl-defun org-glance-tag-config--stub (tag)
  "Return the stub contents of a new config file for TAG.
Comments listing the optional pragmas, none active, then a bare capture entry."
  (let ((name (symbol-name tag)))
    (concat
     "# Config for the `" name "' tag (the file name is the tag).\n"
     "# Below the comments is the org-capture template only.\n"
     "#\n"
     "# Optional pragmas to add ABOVE the `*' heading (each overrides a default):\n"
     "#   #+TITLE:    a human label for the tag (default: the tag name).\n"
     "#   #+TODO:     a per-tag todo cycle, e.g. `TODO DOING | DONE'\n"
     "#               (absent = the global `org-todo-keywords').\n"
     "#   #+CATEGORY: agenda category, default the tag name (planned).\n"
     "#   #+AUTHOR:   planned.\n"
     "#\n"
     "# The heading below is the skeleton: its text is filled with the captured\n"
     "# title + tags.  Add `%^{...}' prompts and keep exactly one `%?'.\n"
     "\n"
     "* %?\n")))

(cl-defun org-glance-tag-config--edit-candidates (graph)
  "Return the sorted tags to offer for configuration: GRAPH's tags and configs."
  (org-glance--sorted-distinct
   (append (and (org-glance-graph? graph) (org-glance-graph:tags graph))
           (let ((dir (org-glance-tag-config:dir graph)))
             (and dir (f-directory? dir)
                  (mapcar #'f-base (directory-files dir nil "\\.org\\'")))))))

(cl-defun org-glance-tag-config--lint ()
  "Warn about, and return as strings, the current tag config buffer's issues.
Flags residual `:TODO_KEYWORDS:'/`:TAG:' keys and a missing capture entry."
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
  "Invalidate the config cache, then lint; buffer-local on `after-save-hook'."
  (org-glance-tag-config--invalidate)
  (org-glance-tag-config--lint))

;;;###autoload
(cl-defun org-glance-tag-config-edit (&optional tag)
  "Open TAG's config file, creating a stub if absent; `C' in the transient.
With TAG nil, prompt for it among live tags and existing configs."
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
