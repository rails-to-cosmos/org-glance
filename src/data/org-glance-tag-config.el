;; -*- lexical-binding: t -*-
;; `org-glance-tag-config' -- a separate, optional per-tag configuration store.
;;
;; Tags are DISCOVERED from captured headlines (`org-glance-tag'); they need no
;; declaration.  A tag's CONFIG, by contrast, is optional metadata that bounds /
;; describes a tag: its capture skeleton, its todo-keyword cycle, superclass
;; `requires', a version.  This config lives OUTSIDE the content graph, in one
;; hand-editable Org file at `<dir>/.org-glance/config/tags.org', so it never
;; reaches tag discovery, overviews or the capture picker (the reserved `:class:'
;; tag this replaces did all three).
;;
;; Each config is ONE level-1 headline keyed by a `:TAG:' drawer property (NOT an
;; org tag -- the headline carries no tags, so a stray scan surfaces nothing):
;;
;;   * Book
;;   :PROPERTIES:
;;   :TAG:            book
;;   :TODO_KEYWORDS:  TODO READING | READ ABANDONED
;;   :REQUIRES:       author
;;   :VERSION:        1
;;   :END:
;;   *** Notes
;;       %?
;;
;; Resolution is an O(1) lookup into a cache rebuilt only when the file's
;; mtime+size change (the graph's snapshot cache, in single-file form).  Absent
;; file / absent `:TAG:' / absent dimension all degrade to nil, so an unconfigured
;; tag captures and renders byte-for-byte as before.

(require 's)
(require 'org)
(require 'cl-lib)
(require 'cl-macs)
(require 'f)

(require 'org-glance-tag)
(require 'org-glance-headline)
(require 'org-glance-graph)
(require 'org-glance-filter)

;; Defined in org-glance.el (require cycle); referenced only at runtime.
(declare-function org-glance-initialized? "org-glance")
(defvar org-glance-graph)

(defcustom org-glance-tag-config-file nil
  "Override path for the tag-configuration Org file.
Nil (default) means `config/tags.org' under the current graph's store.  A string
sets an explicit path (used for tests and non-standard layouts)."
  :group 'org-glance
  :type '(choice (const :tag "Default (store config/tags.org)" nil) file))

(cl-defstruct (org-glance-tag-config (:predicate org-glance-tag-config?)
                                     (:constructor org-glance-tag-config)
                                     (:conc-name org-glance-tag-config:))
  "A tag's optional configuration, projected from its `tags.org' headline."
  (tag nil :read-only t :type symbol)                       ; the configured tag
  (todo nil :read-only t :type (or null string))            ; `#+TODO:'-style cycle, or nil
  (requires nil :read-only t :type list)                    ; superclass tags (Phase 2)
  (version nil :read-only t :type (or null integer))
  (headline nil :read-only t :type org-glance-headline))    ; source config headline

;;; Config file location

(cl-defun org-glance-tag-config:file (graph)
  "Resolved path of GRAPH's tag-config file (may not exist).
`org-glance-tag-config-file' overrides; else `config/tags.org' under GRAPH's
store.  Nil when neither is available (uninitialised, no override) -- callers
then see an empty config set."
  (or (and (stringp org-glance-tag-config-file) org-glance-tag-config-file)
      (and (org-glance-graph? graph)
           (f-join (org-glance-graph:store-path graph) "config" "tags.org"))))

;;; Parse: tags.org -> tag-symbol -> config

(cl-defun org-glance-tag-config--headlines (path)
  "The LEVEL-1 headlines in config file PATH, each a full-subtree headline.
Sub-headings (the capture skeleton's body) are part of their level-1 parent, not
separate configs."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents path))
    (org-glance--org-mode)
    (cl-loop for element in (org-element-map (org-element-parse-buffer 'headline) 'headline #'identity)
             when (= 1 (or (org-element-property :level element) 1))
             collect (org-glance-headline--from-element element))))

(cl-defun org-glance-tag-config--from-headline (headline)
  "Build a config from HEADLINE if it declares a `:TAG:' property, else nil.
All drawer keys are read in a single parse pass."
  (org-glance-headline:with-contents headline
    (let ((tag-raw (org-entry-get nil "TAG")))
      (when tag-raw
        (let ((tag (org-glance-tag:from-string tag-raw)))
          (when (org-glance-tag? tag)
            (cl-flet ((prop (key)
                        (when-let ((v (org-entry-get nil key)))
                          (let ((s (s-trim v))) (unless (string-empty-p s) s)))))
              (org-glance-tag-config
               :tag tag
               ;; NOT "TODO": org reserves it as the special todo-STATE property,
               ;; so `org-entry-get nil "TODO"' returns the heading's keyword, not
               ;; a drawer value.  `TODO_KEYWORDS' is an ordinary drawer property.
               :todo (prop "TODO_KEYWORDS")
               :requires (when-let ((r (prop "REQUIRES")))
                           (org-glance-tags:from-string r))
               :version (when-let ((v (prop "VERSION")))
                          (when (string-match-p "\\`[0-9]+\\'" v) (string-to-number v)))
               :headline headline))))))))

(cl-defun org-glance-tag-config--parse (path)
  "Parse PATH into a tag-symbol -> `org-glance-tag-config' hash (empty if absent).
On a duplicate `:TAG:' the first headline in file order wins."
  (let ((by-tag (make-hash-table :test 'eq)))
    (when (and path (f-exists? path))
      (dolist (headline (org-glance-tag-config--headlines path))
        (when-let ((config (org-glance-tag-config--from-headline headline)))
          (let ((tag (org-glance-tag-config:tag config)))
            (unless (gethash tag by-tag)
              (puthash tag config by-tag))))))
    by-tag))

;;; In-memory cache (single-file snapshot, mirroring the graph's read cache)

(defvar org-glance-tag-config--cache nil
  "Module cache plist (:path P :snapshot S :by-tag HASH), or nil (cold).")

(cl-defun org-glance-tag-config--snapshot (path)
  "A value identifying PATH's state for cache validity: (MTIME . SIZE), or nil."
  (when (and path (f-exists? path))
    (let ((attrs (file-attributes path)))
      (cons (file-attribute-modification-time attrs)
            (file-attribute-size attrs)))))

(cl-defun org-glance-tag-config--invalidate ()
  "Drop the in-memory config cache, so the next resolve re-reads the file.
Called after our own writes (the `-edit' save), so we never depend on mtime
granularity for in-process edits."
  (setq org-glance-tag-config--cache nil))

(cl-defun org-glance-tag-config--by-tag (graph)
  "GRAPH's cached tag -> config hash, rebuilt iff its path or snapshot changed."
  (let* ((path (org-glance-tag-config:file graph))
         (snapshot (org-glance-tag-config--snapshot path))
         (cache org-glance-tag-config--cache))
    (unless (and cache
                 (equal (plist-get cache :path) path)
                 (equal (plist-get cache :snapshot) snapshot))
      (setq org-glance-tag-config--cache
            (list :path path :snapshot snapshot
                  :by-tag (org-glance-tag-config--parse path))))
    (plist-get org-glance-tag-config--cache :by-tag)))

;;; Resolution

(cl-defun org-glance-tag-config:resolve (graph tag)
  "Resolve TAG to its `org-glance-tag-config' in GRAPH, or nil if it has none.
A nil result is the identity: TAG captures with the default template and renders
with the global todo keywords -- the graceful-degradation path."
  (cl-check-type tag org-glance-tag)
  (gethash tag (org-glance-tag-config--by-tag graph)))

(cl-defun org-glance-tag-config:done-keywords (todo-spec)
  "The done keywords of TODO-SPEC (everything after the last `|'), or nil.
Derived by org itself from the verbatim cycle string, so the active/done split
is exactly the one a `#+TODO:' header produces -- single source of truth."
  (when (and (stringp todo-spec) (not (string-empty-p todo-spec)))
    (with-temp-buffer
      (let ((org-todo-keywords (list (cons 'sequence (split-string todo-spec)))))
        (delay-mode-hooks (org-mode))
        (copy-sequence org-done-keywords)))))

(cl-defun org-glance-tag-config:cycle-for-filter (graph filter)
  "The single todo cycle configured for FILTER's tags, or nil.
Returns a cycle only when EXACTLY ONE distinct cycle is configured across the
filtered tags: merging keyword sequences is order-sensitive and corrupts the
active/done split, so 0 or >1 distinct cycles fall back to the global keywords."
  ;; `org-glance-filter:tags' already returns canonical (downcased) tag symbols,
  ;; so each resolves directly; configs without a `:TODO_KEYWORDS:' cycle drop out.
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
  '("TAG" "TODO_KEYWORDS" "REQUIRES" "VERSION" "ORG_GLANCE_ID" "ORG_GLANCE_HASH")
  "Config drawer keys stripped from a rendered capture instance.
The todo cycle is applied as a `#+TODO:' FILE keyword (capture/overview), not as
an instance drawer property.")

(cl-defun org-glance-tag-config:render (config title tags)
  "Render an `org-capture' entry template for an instance of CONFIG.
TITLE pre-fills the heading; TAGS are the instance org tags.  The config
headline's body skeleton, property defaults and `%^{...}' prompts are preserved
verbatim; config metadata (TAG/TODO_KEYWORDS/REQUIRES/VERSION, ids) is stripped.
A `%?' capture point is appended to the heading only when the skeleton carries
none of its own (in its body OR a kept drawer property); a well-formed skeleton
therefore yields exactly one (org-capture honours only the first)."
  (cl-check-type config org-glance-tag-config)
  (cl-check-type title string)
  (let ((tags (if (listp tags) tags (list tags))))
    (org-glance-headline:with-contents (org-glance-tag-config:headline config)
      (dolist (property org-glance-tag-config--render-strip)
        (goto-char (point-min))
        (org-entry-delete nil property))
      (goto-char (point-min))
      ;; Append `%?' to the heading only if the skeleton has NO capture point of
      ;; its own.  Scan the WHOLE buffer (body AND kept drawer properties -- a
      ;; preserved `:NOTE: %?' counts), not just the post-drawer body; the title is
      ;; inserted below, so it can't be mistaken for the skeleton's own marker.
      (let ((skeleton-has-point (s-contains? "%?" (buffer-substring-no-properties
                                                   (point-min) (point-max)))))
        (org-edit-headline (concat title (if skeleton-has-point "" "%?")))
        (goto-char (point-min))
        (org-set-tags (mapcar #'org-glance-tag:to-string tags)))
      (s-trim-right (buffer-substring-no-properties (point-min) (point-max))))))

;;; Authoring

;;;###autoload
(cl-defun org-glance-tag-config-edit ()
  "Open the tag-configuration file for editing, creating a stub if absent."
  (interactive)
  (let ((path (or (org-glance-tag-config:file (and (org-glance-initialized?) org-glance-graph))
                  (user-error "org-glance: not initialised and no `org-glance-tag-config-file' set"))))
    (unless (f-exists? path)
      (f-mkdir-full-path (f-dirname path))
      (f-write-text "#+TITLE: org-glance tag configuration\n\n" 'utf-8 path))
    (find-file path)
    (add-hook 'after-save-hook #'org-glance-tag-config--invalidate nil t)))

(provide 'org-glance-tag-config)
