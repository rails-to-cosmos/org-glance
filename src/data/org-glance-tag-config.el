;; -*- lexical-binding: t -*-
;; `org-glance-tag-config' -- system TODO cycle and optional per-tag config.

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
(declare-function org-glance-config:current "org-glance-config")
(declare-function org-glance-config:native-keywords "org-glance-config")
(declare-function org-glance-settings:tags "org-glance-config")
(declare-function org-glance-settings:visit "org-glance-settings" (graph &optional tag))
(declare-function org-glance-config-file:path "org-glance-config-file")



(cl-defstruct (org-glance-tag-config (:predicate org-glance-tag-config?)
                                     (:constructor org-glance-tag-config)
                                     (:conc-name org-glance-tag-config:))
  "A tag's optional configuration, projected from shared JSON settings."
  (tag nil :read-only t :type symbol)                   ; the configured tag
  (title nil :read-only t :type (or null string))       ; `#+TITLE:' human label, or nil
  (todo nil :read-only t :type (or null string))        ; `#+TODO:'-style cycle, or nil
  (template nil :read-only t :type (or null string)))   ; capture entry (from the first `*')

(defconst org-glance-tag-config:fields
  '((tag       nil        nil)     ; supplied by the tag identity
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

























(cl-defun org-glance-tag-config--by-tag (graph)
  "Return GRAPH's tag configurations projected from JSON settings."
  (let ((table (make-hash-table :test 'eq)))
    (when (org-glance-graph? graph)
      (dolist (entry (org-glance-settings:tags (org-glance-config:current graph)))
        (puthash (intern (car entry)) (cadr entry) table)))
    table))

(cl-defun org-glance-tag-config:resolve (graph tag)
  "Resolve TAG to its `org-glance-tag-config' in GRAPH, or nil if it has none.
Nil means TAG uses the default capture template and system TODO cycle."
  (cl-check-type tag org-glance-tag)
  (gethash tag (org-glance-tag-config--by-tag graph)))

(defun org-glance-tag-config--org-cycle (cycle)
  "Project active-only CYCLE with the shared default inactive keywords.
Org treats a sequence's final word as done even after a trailing separator."
  (let* ((tokens (split-string cycle))
         (separator (member "|" tokens)))
    (if (and separator (cdr separator)) cycle
      (string-join (append (remove "|" tokens) '("|")
                           (cdr (org-glance-tag-config:resolve-keywords nil nil))) " "))))

(cl-defun org-glance-tag-config:cycle->keywords (cycle)
  "Return `org-todo-keywords' sequences for newline-separated CYCLE strings."
  (mapcar (lambda (line) (cons 'sequence (split-string (org-glance-tag-config--org-cycle line))))
          (split-string cycle "\n" t)))

(cl-defun org-glance-tag-config:cycle->keywords-or (cycle default)
  "Return CYCLE's `org-todo-keywords' form, or DEFAULT when CYCLE is nil."
  (if cycle (org-glance-tag-config:cycle->keywords cycle) default))

(defun org-glance-tag-config:resolve-keywords (scopes recognition)
  "Resolve ordered SCOPES of (ACTIVE . INACTIVE), retaining RECOGNITION.
The first scope naming a keyword decides its meaning.
Unclaimed words are active."
  (let ((seen (make-hash-table :test 'equal)) active inactive)
    (dolist (scope (append '((("TODO") . ("DONE"))) scopes
                           (list (cons recognition nil))))
      (dolist (word (append (car scope) (cdr scope)))
        (unless (gethash word seen)
          (puthash word t seen)
          (if (member word (car scope)) (push word active) (push word inactive)))))
    (cons (nreverse active) (nreverse inactive))))

(defun org-glance-tag-config:todo-declarations (text)
  "Observe TEXT's TODO aliases as (START END VALUE) in source order.
Spans are zero-based and half-open.  Opaque blocks are excluded.
SETUPFILE is not followed: this observation owns only the supplied source."
  (with-temp-buffer
    (let* ((dos (string-match-p "\r\n" text))
           (starts (when dos
                     (let ((offset 0) (positions (list 0)))
                       (while (string-match "\n" text offset)
                         (setq offset (match-end 0))
                         (push offset positions))
                       (vconcat (nreverse positions)))))
           (line 0) (previous 1))
      (insert (if dos (replace-regexp-in-string "\r\n" "\n" text t t) text))
      (setq-local tab-width 8)
      (let ((major-mode 'org-mode) (case-fold-search t) declarations)
	(goto-char (point-min))
	(while (re-search-forward (org-make-options-regexp '("TODO" "SEQ_TODO" "TYP_TODO")) nil t)
          (let ((element (org-element-at-point)))
            (when starts
              (cl-incf line (count-lines previous (line-beginning-position)))
              (setq previous (line-beginning-position)))
            (when (org-element-type-p element 'keyword)
              (push (list (if starts (aref starts line) (1- (line-beginning-position)))
                          (if starts (if (< (1+ line) (length starts)) (aref starts (1+ line)) (length text))
                            (1- (line-beginning-position 2)))
                          (org-element-property :value element)) declarations))))
	(nreverse declarations)))))

(defun org-glance-tag-config:declaration-keywords (declarations)
  "Resolve observed DECLARATIONS with explicit active meaning in source order."
  (let (active inactive)
    (dolist (declaration declarations)
      (let ((done nil))
        (dolist (token (split-string (nth 2 declaration)))
          (if (equal token "|") (setq done t)
            (let ((word (car (org-remove-keyword-keys (list token)))))
              (if done (push word inactive) (push word active)))))))
    (let ((a (delete-dups (nreverse active))))
      (cons a (cl-remove-if (lambda (word) (member word a))
                            (delete-dups (nreverse inactive)))))))

(defun org-glance-tag-config:declared-keywords (text)
  "Read TEXT's source-owned TODO declarations with explicit active meaning."
  (org-glance-tag-config:declaration-keywords (org-glance-tag-config:todo-declarations text)))

(cl-defun org-glance-tag-config:done-keywords-for-filter (graph filter)
  "Return the done keywords FILTER's views honour in GRAPH.
Those of its system and tag cycles, else `org-glance--done-keywords'.
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

(cl-defun org-glance-tag-config--tags-for-filter
    (graph filter &optional (metas nil metas-supplied-p))
  "Return FILTER's tags, or every live tag in GRAPH when FILTER is nil."
  (if filter
      (org-glance-filter:tags filter)
    (and (org-glance-graph? graph)
         (mapcar #'org-glance-tag:from-string
                 (if metas-supplied-p
                     (org-glance-graph:tags graph metas)
                   (org-glance-graph:tags graph))))))

(cl-defun org-glance-tag-config--cycle-sources
    (graph filter &optional (metas nil metas-supplied-p))
  "Return effective TODO cycles and their JSON source for FILTER in GRAPH."
  (when (org-glance-graph? graph)
    (let* ((settings (org-glance-config:current graph))
           (tags (mapcar #'symbol-name
                         (if metas-supplied-p
                             (org-glance-tag-config--tags-for-filter graph filter metas)
                           (org-glance-tag-config--tags-for-filter graph filter))))
           (cycles (org-glance-config:native-keywords settings nil tags))
           (path (org-glance-config-file:path (org-glance-graph:directory graph))))
      (mapcar (lambda (cycle) (list (string-join (cdr cycle) " ") path)) cycles))))

(cl-defun org-glance-tag-config:cycles-for-filter
    (graph filter &optional (metas nil metas-supplied-p))
  "Return system then distinct tag TODO sequences for FILTER in GRAPH."
  (mapcar #'car
          (if metas-supplied-p
              (org-glance-tag-config--cycle-sources graph filter metas)
            (org-glance-tag-config--cycle-sources graph filter))))

(cl-defun org-glance-tag-config:cycle-for-filter
    (graph filter &optional (metas nil metas-supplied-p))
  "Return GRAPH's system and FILTER's tag TODO cycles, newline-separated.
Use precomputed METAS when supplied."
  (when-let* ((cycles
               (if metas-supplied-p
                   (org-glance-tag-config:cycles-for-filter graph filter metas)
                 (org-glance-tag-config:cycles-for-filter graph filter))))
    (mapconcat #'identity cycles "\n")))

(cl-defun org-glance-tag-config:preamble (config)
  "Return CONFIG's emittable pragmas as `#+KEY: VALUE' lines, or nil."
  (org-glance-tag-config--preamble-lines
   (cl-loop for (slot . pragma) in (org-glance-tag-config--pragma-slots t)
            collect (cons pragma (cl-struct-slot-value 'org-glance-tag-config
                                                       slot config)))))

(cl-defun org-glance-tag-config:preamble-for-filter
    (graph filter &optional source-comments (metas nil metas-supplied-p))
  "Return separate `#+TODO:' lines for system and FILTER's tags in GRAPH.
With SOURCE-COMMENTS, prefix each line with comments naming its config files.
Use precomputed METAS when supplied."
  (if source-comments
      (when-let* ((groups
                   (if metas-supplied-p
                       (org-glance-tag-config--cycle-sources graph filter metas)
                     (org-glance-tag-config--cycle-sources graph filter))))
        (apply
         #'concat
         (mapcar
          (lambda (group)
            (concat
             (mapconcat
              (lambda (source)
                (format "# TODO source: %s"
                        (if (org-glance-graph? graph)
                            (file-relative-name
                             source (org-glance-graph:store-path graph))
                          (abbreviate-file-name source))))
              (cdr group) "\n")
             "\n#+TODO: " (org-glance-tag-config--org-cycle (car group)) "\n"))
          groups)))
    (org-glance-tag-config--preamble-lines
     (mapcar (lambda (cycle) (cons "TODO" cycle))
             (org-glance-tag-config:cycles-for-filter graph filter)))))

(cl-defun org-glance-tag-config--preamble-lines (pairs)
  "Return PAIRS ((PRAGMA . VALUE)...) as `#+PRAGMA: VALUE' lines, or nil.
Blank values drop out."
  (let ((lines (cl-loop for (pragma . value) in pairs
                        when (org-glance--present-string? value)
                        collect (concat "#+" pragma ": "
                                        (if (equal pragma "TODO")
                                            (org-glance-tag-config--org-cycle value) value)
                                        "\n"))))
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



(cl-defun org-glance-tag-config--edit-candidates (graph)
  "Return the sorted live and configured tags in GRAPH."
  (org-glance--sorted-distinct
   (append (org-glance-graph:tags graph)
           (mapcar #'car (org-glance-settings:tags
                          (org-glance-config:current graph))))))

;;;###autoload
(cl-defun org-glance-tag-config-edit (&optional tag)
  "Open TAG's shared Settings table, prompting among live/configured tags."
  (interactive)
  (unless (org-glance-initialized?) (user-error "org-glance is not initialized"))
  (require 'org-glance-settings)
  (org-glance-settings:visit
   org-glance-graph
   (or tag (completing-read "Configure tag: "
                            (org-glance-tag-config--edit-candidates org-glance-graph)))))

(provide 'org-glance-tag-config)
