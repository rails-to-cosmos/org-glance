;; -*- lexical-binding: t -*-

;;; org-glance-material.el --- graph-backed selection + materialize/sync

;;; Commentary:
;; Command layer over the graph: materialize a headline's blob, sync on save.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'ol)
(require 's)

(require 'transient)   ; `org-glance-materialize' reads the -d switch
(require 'org-glance-utils)
(require 'org-glance-headline)
(require 'org-glance-graph)
(require 'org-glance-cache)
(require 'org-glance-filter)
(require 'org-glance-tag)
(require 'org-glance-tag-config)
(require 'org-glance-config)
(require 'org-glance-datetime-mode)
(require 'org-glance-view)
(require 'org-glance-clipboard)

(require 'org-glance-core)

(declare-function org-glance-cache:incoming-relations
                  "org-glance-cache" (graph id))
(defvar org-glance-material--reserved-lines)
(defvar org-glance-material--password-proof)
(declare-function org-src-source-buffer "org-src")
(declare-function diff-buffers "diff")

(declare-function org-glance-cache:material-path
                  "org-glance-cache" (graph id))

;;; Selection

(cl-defun org-glance-material:label (metadata)
  "Return the `completing-read' label \"[tags] title\" of headline METADATA."
  (cl-check-type metadata org-glance-headline-metadata)
  (let ((tags (org-glance-headline-metadata:tag-strings metadata)))
    (concat (if tags (format "[%s] " (s-join "," tags)) "")
            (org-glance--title-clean (org-glance-headline-metadata:title metadata)))))

(cl-defun org-glance-material--candidates (metas filter)
  "Return completion entries built from METAS accepted by FILTER."
  (let* ((metas (cl-remove-if-not (or filter #'always) metas))
         (labels (mapcar #'org-glance-material:label metas))
         (counts (make-hash-table :test #'equal)))
    (dolist (label labels)
      (puthash label (1+ (gethash label counts 0)) counts))
    (cl-mapcar (lambda (meta label)
                 (cons (if (> (gethash label counts 0) 1)
                           (format "%s ·%s" label
                                   (s-left 8 (org-glance-headline-metadata:id meta)))
                         label)
                       meta))
               metas labels)))

(cl-defun org-glance-material--selection (graph filter)
  "Return GRAPH's (TOTAL . CANDIDATES) accepted by FILTER.
Allow 64 MiB of allocation between collections during picker preparation."
  (cl-check-type graph org-glance-graph)
  (let* ((gc-cons-threshold (max gc-cons-threshold (* 64 1024 1024)))
         (org-done-keywords (org-glance--done-keywords))
         (metas (org-glance-cache:headlines graph)))
    (cons (length metas) (org-glance-material--candidates metas filter))))

(cl-defun org-glance-material:candidates (graph &optional filter)
  "Return indexed completion entries for GRAPH accepted by FILTER."
  (cdr (org-glance-material--selection graph filter)))

(cl-defun org-glance-material:completing-read (graph &key (prompt "Headline: ") filter)
  "Choose a live headline from GRAPH under PROMPT; return its metadata.
FILTER, if non-nil, is a predicate on the metadata."
  (cl-check-type graph org-glance-graph)
  (let* ((selection (org-glance-material--selection graph filter))
         (candidates (cdr selection)))
    (unless candidates
      (let ((total (car selection))
            (dir (org-glance-graph:directory graph)))
        (user-error
         (if (zerop total)
             (format "org-glance: no headlines in the graph at `%s' -- capture some, or run `M-x org-glance-reindex' if you upgraded"
                     dir)
           (format "org-glance: no headlines match the active filter `%s' (of %d in the graph at `%s') -- clear it with `c' in `org-glance-transient', or run `M-x org-glance-reindex' if you upgraded"
                   (org-glance-filter:describe org-glance-filter-spec) total dir)))))
    (cdr (assoc (completing-read prompt candidates nil t) candidates))))

;;; Materialized buffer

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Keymap for `org-glance-material-mode'.")

(define-minor-mode org-glance-material-mode
  "Minor mode for a materialized headline buffer."
  :lighter " glance"
  :global nil
  :group 'org-glance
  :keymap org-glance-material-mode-map
  (when org-glance-material-mode
    (setq tab-width 8 indent-tabs-mode nil)
    (org-glance-datetime-mode 1)
    ;; Appended: runs AFTER `org-check-running-clock', which queues that note.
    (add-hook 'kill-buffer-hook #'org-glance-material--cancel-pending-log-note t t)))

(define-key org-glance-material-mode-map (kbd "C-c #") #'org-glance-material:crypt)
(define-key org-glance-material-mode-map (kbd "C-c d") #'org-glance-material:set-project-dir)
(define-key org-glance-material-mode-map (kbd "C-c e") #'org-glance-material:extract-here)
(define-key org-glance-material-mode-map (kbd "C-c y") #'org-glance-clipboard:paste)
(define-key org-glance-material-mode-map (kbd "C-c j") #'org-glance-material:open-link-here)
(define-key org-glance-material-mode-map (kbd "C-c i") #'org-glance-material:set-interval)
(define-key org-glance-material-mode-map (kbd "@") #'org-glance-material:refer)

(defconst org-glance-project-dir-property "ORG_GLANCE_PROJECT_DIR"
  "Drawer property naming the project directory `org-glance-llm' opens in.
Set it with `org-glance-material:set-project-dir' (`C-c d').")

(defvar-local org-glance-material--graph nil
  "Graph backing the current materialized buffer.")

(defvar-local org-glance-material--id nil
  "ORG_GLANCE_ID of the headline materialized in the current buffer.")

(defvar-local org-glance-material--row-id nil
  "Exact projection Row used to open the current material buffer.")

(defvar org-glance-material--buffers (make-hash-table :test #'equal)
  "Logical editors keyed by canonical graph directory and headline id.")
(defvar-local org-glance-material--base nil
  "Selected immutable source and expected structural frontier.")
(defvar-local org-glance-material--pending nil
  "Publication receipt awaiting completion and buffer adoption.")
(defvar-local org-glance-material--receipt nil
  "Most recently completed publication receipt.")
(defvar-local org-glance-material--revision 0
  "Monotonic authored change revision, including undo and indirect edits.")
(defvar-local org-glance-material--display-change nil
  "Non-nil while changing the editor's presentation alone.")
(defvar-local org-glance-material--busy nil
  "Non-nil during publication or source replacement.")
(defvar org-glance-material--save-completed nil
  "Non-nil after the contents handler returns successfully for this save.")
(defvar-local org-glance-material--saving nil
  "Non-nil throughout Emacs's save hooks and material publication.")

(cl-defun org-glance-material:buffer (graph id)
  "Return GRAPH's live logical editor for ID without loading content."
  (let* ((key (cons (org-glance-graph:directory graph) id))
         (buffer (gethash key org-glance-material--buffers)))
    (if (buffer-live-p buffer) buffer
      (remhash key org-glance-material--buffers)
      nil)))

(defun org-glance-material--unregister ()
  "Remove the current logical editor from the registry."
  (let ((key (cons (org-glance-graph:directory org-glance-material--graph)
                   org-glance-material--id)))
    (when (eq (gethash key org-glance-material--buffers) (current-buffer))
      (remhash key org-glance-material--buffers))))

(defun org-glance-material--changed (&rest _)
  "Advance the logical editor's authored revision after a text change."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (unless org-glance-material--display-change
      (cl-incf org-glance-material--revision))))

(defvar-local org-glance-material--encrypted nil
  "Non-nil when this editor requires sealed crypt publication.")

(defvar-local org-glance-material--reference-graph nil
  "Graph whose metadata supplies this buffer's reference display.")

(cl-defun org-glance-material--reference-label (metadata)
  "Return METADATA's current state and rendered title."
  (string-join
   (delq nil (list (let ((state (org-glance-headline-metadata:state metadata)))
                     (unless (string-empty-p (or state "")) state))
                   (org-glance--title-clean
                    (org-glance-headline-metadata:title metadata))))
   " "))

(cl-defun org-glance-material:activate-reference (start end path bracketp)
  "Display PATH's current state and title between START and END.
BRACKETP enables projection in descriptive links; source bytes stay intact."
  (save-match-data
    (with-demoted-errors "org-glance: reference display failed: %S"
      (when (and bracketp org-link-descriptive)
        (unless (memq 'display font-lock-extra-managed-props)
          (setq-local font-lock-extra-managed-props
                      (cons 'display font-lock-extra-managed-props)))
        (when-let* ((graph (or org-glance-material--graph
                              org-glance-view--graph org-glance-graph))
                    (reference-graph (setq org-glance-material--reference-graph graph))
                    (meta (org-glance-cache:headline
                           reference-graph (car (split-string path "[?]"))))
                    ((org-glance-headline-metadata? meta)))
          (save-excursion
            (goto-char start)
            (when (looking-at org-link-bracket-re)
              (put-text-property
               (or (match-beginning 2) (+ start 2))
               (or (match-end 2) (- end 2))
               'display (org-glance-material--reference-label meta)))))))))

(cl-defun org-glance-material--references-committed (graph _changed)
  "Invalidate reference display properties in GRAPH's open Org buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and font-lock-mode
                 (eq graph org-glance-material--reference-graph))
        (font-lock-flush)))))

(add-hook 'org-glance-graph-after-commit-functions
          #'org-glance-material--references-committed)

(defun org-glance-material--outstanding-p (&optional buffer)
  "Return whether BUFFER's logical editor has edits or pending publication.
BUFFER defaults to the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (with-current-buffer (or (buffer-base-buffer) (current-buffer))
      (or org-glance-material--pending (buffer-modified-p)))))

(cl-defun org-glance-material--ensure ()
  "Signal a `user-error' unless the current buffer is a materialized headline."
  (unless (and org-glance-material--graph org-glance-material--id)
    (user-error "Not in a materialized headline buffer"))
  (when (buffer-base-buffer)
    (user-error "Use the logical Material editor for this command")))

(cl-defun org-glance-material:current ()
  "Return this materialized buffer's (GRAPH . ID), or signal a `user-error'.
Plugins use it instead of the buffer-local internals."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (org-glance-material--ensure)
    (cons org-glance-material--graph org-glance-material--id)))

(defvar-local org-glance-material--cycle nil
  "Per-tag `#+TODO:'-style cycle string for this buffer's headline, or nil.")

(defun org-glance-material--context-tags ()
  "Return the current Material's authored root tags."
  (save-excursion
    (goto-char (point-min))
    (unless (org-at-heading-p) (org-next-visible-heading 1))
    (org-get-tags nil t)))

(defun org-glance-material--reload-context (graph)
  "Refresh GRAPH's open Material buffers without changing their source or point."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq graph org-glance-material--graph)
        (setq-local org-glance-config--org-context
                    (org-glance-config:org-context graph #'org-glance-material--context-tags))
        (setq org-glance-material--cycle
              (org-glance-tag-config:cycle-for-filter
               graph (list :tags (mapcar #'org-glance-tag:from-string
                                         (org-glance-material--context-tags)))))
        (let ((org-todo-keywords (org-glance-tag-config:cycle->keywords-or
                                 org-glance-material--cycle org-todo-keywords)))
          (org-set-regexps-and-options))))))

(add-hook 'org-glance-config-after-reload-functions #'org-glance-material--reload-context)

(defvar-local org-glance-material--newline-default 'lf
  "Ending used for newly inserted Material lines.")

(defvar-local org-glance-material--drawer-lines nil
  "Original property drawer delimiters, with newline provenance.")

(defvar-local org-glance-material--newline-mixed nil
  "Non-nil when existing lines need endings different from the default.")

(defun org-glance-material--read-endings (source)
  "Load exact SOURCE into LF editing text with per-newline provenance."
  (setq org-glance-material--newline-default 'lf
        org-glance-material--newline-mixed nil)
  (when (string-match-p "\r\n" source)
    (let ((source source)
          (modified (buffer-modified-p))
          (position (point))
          (buffer-undo-list t)
          (inhibit-read-only t))
      (setq org-glance-material--newline-default
            (if (string-match "\r?\n" source)
                (if (eq (aref source (match-beginning 0)) ?\r) 'crlf 'lf)
              'lf))
      (erase-buffer)
      (insert source)
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
        (let ((crlf (eq (char-before (1- (point))) ?\r)))
          (when crlf (delete-region (- (point) 2) (1- (point))))
          (unless (eq (if crlf 'crlf 'lf) org-glance-material--newline-default)
            (setq org-glance-material--newline-mixed t)
            (add-text-properties
             (1- (point)) (point)
             (list 'org-glance-newline (if crlf 'crlf 'lf)
                   'rear-nonsticky '(org-glance-newline))))))
      (goto-char (min position (point-max)))
      (set-buffer-modified-p modified)))
  (setq-local buffer-file-coding-system 'utf-8-unix))

(defun org-glance-material--encode-endings (text)
  "Restore newline bytes in inserted annotation TEXT."
  (let ((start 0) (cursor 0) parts)
    (while (string-match "\n" text cursor)
      (setq cursor (match-end 0))
      (when (eq (or (get-text-property (1- cursor) 'org-glance-newline text)
                    org-glance-material--newline-default) 'crlf)
        (push (substring-no-properties text start (1- cursor)) parts)
        (push "\r\n" parts)
        (setq start cursor)))
    (apply #'concat (nreverse (cons (substring-no-properties text start) parts)))))

(cl-defun org-glance-material--serialize (&key (seal t))
  "Return encoded content, metadata, crypt proof and authored revision.
SEAL encrypts crypt blocks for publication; nil retains editing presentation."
  (when seal (org-glance-material--prepare-crypt))
  (let* ((revision org-glance-material--revision)
         (text (buffer-substring (point-min) (point-max)))
         (reserved org-glance-material--reserved-lines)
         (drawer org-glance-material--drawer-lines)
         (endings org-glance-material--newline-default)
         (encrypted org-glance-material--encrypted)
         (password (when (and seal encrypted
                              (cl-some (lambda (block)
                                         (not (org-glance--crypt-sealed? block)))
                                       (org-glance--crypt-block-regions)))
                     (org-glance-material--require-password)))
         (org-glance-config--pending-context org-glance-config--org-context)
         (org-todo-keywords (org-glance-tag-config:cycle->keywords-or
                            org-glance-material--cycle org-todo-keywords)))
    (with-temp-buffer
      (org-glance--org-mode)
      (org-glance-material--harden-buffer)
      (insert text)
      (setq-local org-glance-material--drawer-lines drawer
                  org-glance-material--newline-default endings)
      (let ((inhibit-read-only t))
        (when seal (org-glance-headline--crypt-upgrade-legacy))
        (goto-char (point-min))
        (when reserved
          (let ((splice (org-glance-material--drawer-splice
                         (apply #'concat reserved))))
            (goto-char (car splice))
            (insert (cdr splice))))
        (when password (org-glance--crypt-seal-blocks password)))
      (when (and seal encrypted
                 (let ((blocks (org-glance--crypt-block-regions)))
                   (or (null blocks)
                       (cl-some (lambda (block) (not (org-glance--crypt-sealed? block)))
                                blocks))))
        (user-error "Encrypted headline is not sealed; save refused"))
      (let* ((content (org-glance-material--encode-endings (buffer-string)))
             (headline (org-glance-headline--from-string content)))
        (list content (org-glance-headline:metadata headline)
              (org-glance-material--sealed-proof) revision)))))

(defun org-glance-material--adopt (receipt)
  "Advance this editor's base from completed RECEIPT without loading content."
  (let* ((version (org-glance-publication:version receipt))
         (frontier (org-glance-publication:frontier receipt))
         (concurrent (not (equal frontier (list (org-glance-version:id version))))))
    (setq org-glance-material--base
          (list :version (org-glance-version:id version)
                :digest (org-glance-version:content-sha256 version)
                :path (f-join (org-glance-version:directory version) "data.org")
                :frontier frontier :reconcile concurrent)
          org-glance-material--receipt receipt
          org-glance-material--pending nil
          org-glance-material--row-id org-glance-material--id)
    (when (listp buffer-undo-list)
      (setq buffer-undo-list
            (cl-delete-if (lambda (entry) (eq t (car-safe entry))) buffer-undo-list)))
    (set-buffer-modified-p
     (/= org-glance-material--revision (org-glance-publication:revision receipt)))
    (when concurrent
      (display-warning 'org-glance "Content saved with concurrent versions; reconcile before saving again"))
    (condition-case nil
        (org-glance-material--reload-context org-glance-material--graph)
      ((error quit)
       (org-glance-view--mark-stale)
       (display-warning 'org-glance "Material saved; display refresh failed")))))

(defun org-glance-material--write-version ()
  "Complete pending work and publish the current authored draft."
  (org-glance-material--ensure)
  (when buffer-file-name
    (user-error "Material saves require a virtual buffer; use content export"))
  (when org-glance-material--busy (user-error "Material operation already running"))
  (let ((org-glance-material--busy t))
    (when org-glance-material--pending
      (org-glance-material--adopt
       (org-glance-graph:publish-edit org-glance-material--pending)))
    (when (buffer-modified-p)
      (when (plist-get org-glance-material--base :reconcile)
        (user-error "Resolve concurrent versions before saving"))
      (save-excursion
        (org-glance-material--drop-hand-typed-reserved)
        (org-glance-material--normalize-tags))
      (let* ((serialized (org-glance-material--serialize))
             (revision (nth 3 serialized))
             (receipt (org-glance-graph:prepare-edit
                       org-glance-material--graph :id org-glance-material--id
                       :contents (car serialized) :metadata (cadr serialized)
                       :revision revision
                       :expected-frontier (plist-get org-glance-material--base :frontier)
                       :source-version (plist-get org-glance-material--base :version)
                       :source-digest (plist-get org-glance-material--base :digest))))
        (setq org-glance-material--pending receipt)
        (when (nth 2 serialized)
          (setq org-glance-material--password-proof (nth 2 serialized)))
        (condition-case err
            (org-glance-material--adopt (org-glance-graph:publish-edit receipt))
          ((error quit)
           (unless (file-exists-p (org-glance-version:directory
                                  (org-glance-publication:version receipt)))
             (setq org-glance-material--pending nil))
           (set-buffer-modified-p
            (or (not (eq receipt org-glance-material--receipt))
                (/= org-glance-material--revision
                    (org-glance-publication:revision receipt))))
           (signal (car err) (cdr err)))))))
  (setq org-glance-material--save-completed t)
  t)

(defun org-glance-material:save ()
  "Save the logical draft and return its completed receipt, or nil for no work."
  (interactive)
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (org-glance-material--ensure)
    (let ((previous org-glance-material--receipt))
      (save-buffer)
      (unless (eq previous org-glance-material--receipt)
        org-glance-material--receipt))))

(defun org-glance-material--save-buffer (save &rest args)
  "Run SAVE with ARGS while protecting a material editor's save lifecycle."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (if (not org-glance-material-mode) (apply save args)
      (when (or org-glance-material--saving org-glance-material--busy)
        (user-error "Material save already running"))
      (when buffer-file-name
        (user-error "Material editors save through their logical identity"))
      (when org-glance-material--pending
        (set-buffer-modified-p t))
      (let ((org-glance-material--saving t)
            (org-glance-material--save-completed nil))
        (condition-case err
            (apply save args)
          (error
           (if (not org-glance-material--save-completed)
               (signal (car err) (cdr err))
             (org-glance-view--mark-stale)
             (display-warning 'org-glance "Material saved; an after-save notification failed"))))))))

(advice-add 'basic-save-buffer :around #'org-glance-material--save-buffer)

(defun org-glance-material--offer-pending-saves (&rest _args)
  "Include pending material publications in native save selection."
  (maphash (lambda (_key buffer)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when org-glance-material--pending
                   (set-buffer-modified-p t)))))
           org-glance-material--buffers))

(advice-add 'save-some-buffers :before #'org-glance-material--offer-pending-saves)

(cl-defun org-glance-material:sync ()
  "Append sealed visited-file metadata for compatibility callers.
Virtual editors publish through `org-glance-material:save'.  Skip a visited
file whose id differs from this buffer's logical id."
  (when (and org-glance-material--graph org-glance-material--id buffer-file-name)
    (let* ((graph org-glance-material--graph)
           (id org-glance-material--id)
           (headline (let ((org-glance-config--pending-context org-glance-config--org-context)
                           (org-todo-keywords
                            (org-glance-tag-config:cycle->keywords-or
                             org-glance-material--cycle org-todo-keywords)))
                       (org-glance-headline--from-string
                        (f-read-text buffer-file-name 'utf-8-unix)))))
      (if (equal (org-glance-headline:id headline) id)
          (org-glance-graph:insert graph (list (org-glance-headline:metadata headline)))
        (message "org-glance: %s lost its ORG_GLANCE_ID drawer; metadata not updated" id)))))

;;; Repeated headlines

(defcustom org-glance-repeat-history-depth 0
  "Completed occurrences kept per repeating headline: newest N, or t for all.
0 disables history and the after-repeat trim; snapshots live in `occurrences/'."
  :group 'org-glance
  :type '(choice (natnum :tag "Keep newest N (0 disables)")
                 (const :tag "Unlimited" t)))

(defconst org-glance-repeat-history-depth-property "ORG_GLANCE_REPEAT_HISTORY_DEPTH"
  "Drawer property overriding `org-glance-repeat-history-depth' per headline.
An integer, or t/inf/unlimited for all; junk reads as 0, floats truncate.")

(cl-defun org-glance-material--property (key)
  "Return drawer property KEY of this materialized buffer's heading, or nil."
  (save-excursion
    (org-glance-material--goto-first-heading)
    (org-entry-get (point) key)))

(cl-defun org-glance-material--history-depth ()
  "Return this headline's history depth: its drawer property, else the option."
  (if-let* ((v (org-glance-material--property org-glance-repeat-history-depth-property)))
      (pcase (downcase (s-trim v))
        ((or "t" "inf" "unlimited") t)
        (n (truncate (string-to-number n))))   ; junk -> 0 -> disabled; 3.5 -> 3
    org-glance-repeat-history-depth))

(defvar-local org-glance-material--snapshotted nil
  "Non-nil when `snapshot-on-repeat' wrote an occurrence file.
`cleanup-after-repeat' trims only when set: a failed snapshot keeps the body.")

(cl-defun org-glance-material--occurrence-stamp (ts)
  "Return the sortable filename stamp of timestamp element TS, or of now."
  (format-time-string
   "%Y-%m-%dT%H%M"
   (if ts (org-time-string-to-time (org-element-property :raw-value ts))
     (current-time))))

(cl-defun org-glance-material:snapshot-on-repeat (&rest _)
  "Preserve the completed repetition as an occurrence snapshot.
`:before' advice on `org-auto-repeat-maybe'; sets `--snapshotted' on success."
  (setq org-glance-material--snapshotted nil)
  (when-let* ((depth (and org-glance-material-mode
                          org-glance-material--graph
                          (org-glance-material--history-depth)))
              (ts (and (or (eq depth t) (> depth 0))
                       (member (org-get-todo-state) org-done-keywords)
                       (car (org-glance-datetime-active-repeated-timestamps
                             'include-schedules 'include-deadlines)))))
    ;; invariant 14: an encrypted buffer never snapshots.
    (if org-glance-material--encrypted
        (message "org-glance: encrypted headline keeps no occurrence history")
      (with-demoted-errors "org-glance: occurrence snapshot failed: %S"   ; inv 9
        (let* ((graph org-glance-material--graph)
               (id org-glance-material--id)
               (dir (org-glance-graph:occurrences-path graph id)))
          (f-mkdir-full-path dir)
          (org-glance--atomic-write
           (f-join dir (concat (org-glance-material--occurrence-stamp ts) ".org"))
           (org-glance-material--encode-endings
            (buffer-substring (point-min) (point-max))))
          (setq org-glance-material--snapshotted t)
          (when (integerp depth)                     ; t = unlimited, no prune
            (cl-loop for (_stamp . path) in (nthcdr depth
                                                    (org-glance-graph:occurrences graph id))
                     do (ignore-errors (f-delete path)))))))))

(cl-defun org-glance-material:history ()
  "Choose one of this headline's occurrence snapshots and open it read-only."
  (interactive)
  (org-glance-material--ensure)
  (org-glance-view:pick-occurrence org-glance-material--graph org-glance-material--id))

(define-key org-glance-material-mode-map (kbd "C-c h") #'org-glance-material:history)

(cl-defun org-glance-material:cleanup-after-repeat (&rest _)
  "Trim the repeated materialized headline to its header and pinned blocks.
Runs `:after' `org-auto-repeat-maybe'; consumes the `--snapshotted' flag."
  (when (prog1 org-glance-material--snapshotted
          (setq org-glance-material--snapshotted nil))
    (save-excursion
      (goto-char (point-min))
      (let ((header (s-trim (buffer-substring
                             (point)
                             (save-excursion (org-end-of-meta-data) (point)))))
            (pinned (cl-loop while (search-forward "#+begin_pin" nil t)
                             collect (save-excursion
                                       (beginning-of-line)
                                       (buffer-substring
                                        (point)
                                        (progn (search-forward "#+end_pin" nil t)
                                               (point)))))))
        (delete-region (point-min) (point-max))
        (insert (s-join "\n\n" (cons header pinned)) "\n")
        (org-delete-property "LAST_REPEAT")))))   ; entry-delete finds the sole heading

(advice-add 'org-auto-repeat-maybe :before #'org-glance-material:snapshot-on-repeat '((depth . -90)))
(advice-add 'org-auto-repeat-maybe :after #'org-glance-material:cleanup-after-repeat)

;;; Encryption

;; SECURITY: plaintext and the password stay in memory (proposal
;; 2026-07-06-encrypted-materialize-gpg).
(defcustom org-glance-material-password-ttl 300
  "Seconds an encrypted buffer caches its password; 0 means the buffer's life.
Expiry makes the next save re-prompt; `org-glance-material:lock' forgets early."
  :group 'org-glance
  :type 'integer)

(defvar-local org-glance-material--password nil
  "Cached password of an encrypted materialized buffer, or nil when forgotten.")
(defvar-local org-glance-material--password-timer nil
  "Timer that forgets `org-glance-material--password' after the TTL.")
(defvar-local org-glance-material--password-proof nil
  "Sealed crypt-block body used to validate a re-entered password.")

(cl-defun org-glance-material--clear-password ()
  "Forget the cached password and cancel its expiry timer."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (setq-local org-glance-material--password nil)
    (when (timerp org-glance-material--password-timer)
      (cancel-timer org-glance-material--password-timer))
    (setq-local org-glance-material--password-timer nil)))

(cl-defun org-glance-material--set-password (pw)
  "Cache PW buffer-local and (re)arm the TTL timer that forgets it."
  (org-glance-material--clear-password)
  (setq-local org-glance-material--password pw)
  (when (> org-glance-material-password-ttl 0)
    (let ((buf (current-buffer)))
      (setq-local org-glance-material--password-timer
                  (run-at-time org-glance-material-password-ttl nil
                               (lambda ()
                                 (when (buffer-live-p buf)
                                   (with-current-buffer buf
                                     (org-glance-material--clear-password)))))))))

(cl-defun org-glance-material--require-password ()
  "Return the cached password, validating a prompted replacement when forgotten."
  (or org-glance-material--password
      (let ((password (read-passwd "Headline password: ")))
        (when (and org-glance-material--password-proof
                   (not (let ((inhibit-message t))
                          (condition-case nil
                              (aes-decrypt-buffer-or-string
                               org-glance-material--password-proof password)
                            (error nil)))))
          (user-error "Wrong password; headline was not saved"))
        (org-glance-material--set-password password)
        password)))

(cl-defun org-glance-material--sealed-proof ()
  "Return one sealed crypt-block body in the current buffer, or nil."
  (when-let* ((block (cl-find-if #'org-glance--crypt-sealed?
                                 (org-glance--crypt-block-regions))))
    (buffer-substring-no-properties (plist-get block :body-beg)
                                    (plist-get block :body-end))))

(cl-defun org-glance-material--harden-buffer ()
  "Keep an encrypted buffer's plaintext off disk: no auto-save/backup/lockfile."
  (let ((asf buffer-auto-save-file-name))
    (auto-save-mode -1)
    (when (and asf (file-exists-p asf)) (ignore-errors (delete-file asf))))
  (setq-local buffer-auto-save-file-name nil
              backup-inhibited t
              create-lockfiles nil))

(defun org-glance-material--harden-source-edit ()
  "Disable disk draft recovery in source editors belonging to Material."
  (when-let* ((source (ignore-errors (org-src-source-buffer)))
              ((buffer-live-p source))
              ((buffer-local-value 'org-glance-material-mode source)))
    (org-glance-material--harden-buffer)))

(add-hook 'org-src-mode-hook #'org-glance-material--harden-source-edit)

(cl-defun org-glance-material--encrypt-buffer ()
  "Seal crypt blocks for explicit presentation locking.
Prompt for the password when its TTL has expired."
  (when org-glance-material--encrypted
    (let ((inhibit-read-only t))
      (org-glance--crypt-seal-blocks (org-glance-material--require-password))
      (setq-local org-glance-material--password-proof
                  (org-glance-material--sealed-proof)))))

(cl-defun org-glance-material--decrypt-buffer ()
  "Unseal this hardened editor while preserving its authored dirty state.
Validate every block in scratch memory before replacing the presentation."
  (when org-glance-material--encrypted
    (org-glance-material--harden-buffer)
    (let* ((text (buffer-substring (point-min) (point-max)))
           (proof (org-glance-material--sealed-proof))
           (password (org-glance-material--require-password))
           (plaintext
            (with-temp-buffer
              (org-glance-material--harden-buffer)
              (insert text)
              (org-glance--org-mode)
              (org-glance-headline--crypt-upgrade-legacy)
              (setq proof (org-glance-material--sealed-proof))
              (org-glance--crypt-unseal-blocks password)
              (buffer-string)))
           (modified (buffer-modified-p))
           (org-glance-material--display-change t)
           (inhibit-read-only t))
      (save-excursion
        (replace-region-contents (point-min) (point-max) (lambda () plaintext)))
      (setq org-glance-material--password-proof proof)
      (set-buffer-modified-p modified))))

(cl-defun org-glance-material:lock ()
  "Forget this encrypted buffer's cached password now; the next save re-prompts.
The decrypted body stays in the buffer until then."
  (interactive)
  (if org-glance-material--encrypted
      (when (y-or-n-p "Forget the cached password (next save re-prompts)? ")
        (org-glance-material--clear-password)
        (message "org-glance: password forgotten"))
    (user-error "Not an encrypted materialized buffer")))

(cl-defun org-glance-material--purge-occurrences (graph id)
  "Delete ID's occurrence snapshots in GRAPH: plaintext of now-secret content.
Both encrypt paths call it (invariant 14); idempotent."
  (let ((dir (org-glance-graph:occurrences-path graph id)))
    (when (f-exists? dir)
      (f-delete dir t)
      (message "org-glance: plaintext occurrence history removed (headline is now encrypted)"))))

(cl-defun org-glance-material--wire-crypto ()
  "Mark the buffer encrypted and harden it; preserve save presentation."
  (setq-local org-glance-material--encrypted t)
  (org-glance-material--harden-buffer)
  (add-hook 'kill-buffer-hook #'org-glance-material--clear-password nil t))

(defun org-glance-material--prepare-crypt ()
  "Validate crypt markers and establish sealing state before serialization."
  (let* ((case-fold-search t)
         (blocks (org-glance--crypt-block-regions))
         (markers (how-many (concat org-glance--crypt-begin-re "\\|"
                                     org-glance--crypt-end-re)
                            (point-min) (point-max))))
    (unless (= markers (* 2 (length blocks)))
      (user-error "Crypt blocks must be complete and unnested; save refused"))
    (when (and blocks (not org-glance-material--encrypted))
      (org-glance-material--wire-crypto)
      (when (cl-some (lambda (block) (not (org-glance--crypt-sealed? block))) blocks)
        (org-glance-material--set-password (read-passwd "Headline password (confirm): " t)))
      (org-glance-material--purge-occurrences org-glance-material--graph
                                              org-glance-material--id))))

(cl-defun org-glance-material--maybe-decrypt (_meta buffer)
  "Prompt and decrypt BUFFER when its content is sealed.
Return non-nil when it decrypted; wire and harden the buffer first.  A wrong
password clears the cached password and preserves BUFFER."
  (with-current-buffer buffer
    (when (org-glance-headline--buffer-encrypted?)
      (org-glance-material--wire-crypto)
      (org-glance-material--set-password (read-passwd "Headline password: "))
      (condition-case err
          (org-glance-material--decrypt-buffer)
        (error (org-glance-material--clear-password)
               (signal (car err) (cdr err))))
      t)))

(cl-defun org-glance-material:crypt-region (beg end)
  "Wrap body region BEG..END in a `#+begin_crypt' block that seals on save.
The first block prompts for a confirmed password and wires the round-trip."
  (interactive "r")
  (org-glance-material--ensure)
  (when (< beg (car (org-glance-headline--body-region)))
    (user-error "Region must lie inside the headline body"))
  (when (>= beg end) (user-error "Nothing to encrypt"))
  (org-glance--crypt-wrap-region beg end)
  (unless org-glance-material--encrypted
    (org-glance-material--wire-crypto)
    (org-glance-material--set-password
     (read-passwd "Headline password (confirm): " t))
    (org-glance-material--purge-occurrences org-glance-material--graph
                                            org-glance-material--id))
  (deactivate-mark)
  (message "org-glance: region wrapped -- seals on save"))

(cl-defun org-glance-material:decrypt ()
  "Decrypt this headline's sealed crypt blocks in place, wiring the round-trip."
  (interactive)
  (org-glance-material--ensure)
  (unless (org-glance-material--maybe-decrypt nil (current-buffer))
    (message "org-glance: nothing sealed here")))

(cl-defun org-glance-material:seal ()
  "Seal this buffer's crypt blocks in place and forget the cached password.
Refuse unencrypted buffers or outstanding work (invariant 11)."
  (interactive)
  (org-glance-material--ensure)
  (unless org-glance-material--encrypted
    (user-error "Not an encrypted materialized buffer"))
  (when (org-glance-material--outstanding-p)
    (user-error "Save this buffer before sealing it"))
  (org-glance-material--encrypt-buffer)
  (set-buffer-modified-p nil)               ; sealed == the bytes on disk
  (org-glance-material--clear-password)
  (message "org-glance: sealed -- `C-c #' unseals"))

(cl-defun org-glance-material:crypt-unwrap ()
  "Remove the crypt block around point; its body becomes public on save.
A sealed body decrypts first; the last unwrap drops encryption entirely."
  (interactive)
  (org-glance-material--ensure)
  (let ((block (org-glance--crypt-block-at (point))))
    (unless block (user-error "Point is not inside a crypt block"))
    (when (org-glance--crypt-sealed? block)
      ;; invariant 14: harden BEFORE plaintext lands (as-is opens unhardened).
      (org-glance-material--wire-crypto)
      (org-glance--crypt-unseal-blocks (org-glance-material--require-password))
      (setq block (org-glance--crypt-block-at (point))))
    (org-glance--crypt-unwrap-block block)
    (when (and org-glance-material--encrypted
               (null (org-glance--crypt-block-regions)))
      (setq-local org-glance-material--encrypted nil)
      (setq-local org-glance-material--password-proof nil)
      (org-glance-material--clear-password)
      (message "org-glance: last crypt block unwrapped -- headline public on save"))))

(cl-defun org-glance-material:crypt ()
  "Run the crypt action for the context at point (`C-c #'); first match wins:
region -> wrap it; sealed buffer -> unseal; block at point -> unwrap;
decrypted buffer -> seal; plaintext -> wrap the whole body.  Sealed comes
before block: a sealed buffer's point sits inside its ciphertext block."
  (interactive)
  (org-glance-material--ensure)
  (cond
   ((use-region-p)
    (org-glance-material:crypt-region (region-beginning) (region-end)))
   ((org-glance-headline--buffer-encrypted?)
    (if org-glance-material--encrypted
        (org-glance-material--decrypt-buffer)
      (org-glance-material:decrypt)))
   ((org-glance--crypt-block-at (point)) (org-glance-material:crypt-unwrap))
   (org-glance-material--encrypted (org-glance-material:seal))
   (t (pcase-let ((`(,beg . ,end) (org-glance-headline--body-region)))
        (org-glance-material:crypt-region beg end)))))

(cl-defun org-glance-material:set-project-dir (dir)
  "Set the materialized headline's project directory (`C-c d') to DIR and save.
Store home paths with `~' and no trailing slash; `C-u' clears."
  (interactive
   (list (unless current-prefix-arg
           (expand-file-name
            (read-directory-name
             "Project dir: "
             (if-let* ((cur (org-glance-material--property
                            org-glance-project-dir-property)))
                 (file-name-as-directory cur)
               "./"))))))
  (org-glance-material--ensure)
  (let ((stored (when (org-glance--present-string? dir)
                  (directory-file-name
                   (abbreviate-file-name (expand-file-name dir))))))
    (save-excursion
      (org-glance-material--goto-first-heading)
      (if stored
          (org-entry-put nil org-glance-project-dir-property stored)
        (org-entry-delete nil org-glance-project-dir-property)))
    (let ((inhibit-message t)) (save-buffer))
    (message "Project dir %s" (or stored "cleared"))))

(defcustom org-glance-material-hidden-properties org-glance-headline:hash-ignore-properties
  "Uppercase drawer property keys org-glance manages in material buffers.
Supply these through the publication serializer (invariant 21)."
  :group 'org-glance
  :type '(repeat string))

(defvar-local org-glance-material--managed-keys nil
  "`org-glance-material-hidden-properties' as it stood when this buffer opened.
Fixed for the buffer's life; an option change applies to later buffers.")

(defvar-local org-glance-material--reserved-lines nil
  "Managed drawer lines removed from this buffer at open, verbatim.
The publication serializer supplies them on every save.")

(cl-defun org-glance-material--relation-stamp (graph id)
  "Return ID's committed source time as an Org timestamp, or nil."
  (let ((state (org-glance-cache:material-state graph id)))
    (when (and (plist-get state :created)
               (not (member (plist-get state :producer) '("legacy" "migration"))))
      (format-time-string "[%Y-%m-%d %a %H:%M]"
                          (date-to-time (plist-get state :created))))))

(cl-defun org-glance-material--relation-line
    (graph id &key kind direction stamp)
  "Return one logbook line for ID in DIRECTION in GRAPH.
KIND supplies its relation label and STAMP names the source snapshot time."
  (let ((label (if kind
                   (s-capitalize (org-glance--kind-pretty kind))
                 (if (eq direction 'out) "Refers to" "Referred by"))))
    (concat "- " label " "
            (org-link-make-string
             (org-glance--edge->link-path id (and (eq direction 'out) kind))
             (let ((meta (org-glance-cache:headline graph id)))
               (if (org-glance-headline-metadata? meta)
                   (org-glance-material--reference-label meta)
                 id)))
            (and stamp (concat " at " stamp))
            "\n")))

(cl-defun org-glance-material--relation-lines (graph id)
  "Return current outgoing and incoming relation log lines for ID in GRAPH."
  (let* ((meta (org-glance-cache:headline graph id))
         (outgoing (and (org-glance-headline-metadata? meta)
                        (org-glance-headline-metadata:relations meta)))
         (out-stamp (and outgoing
                         (org-glance-material--relation-stamp graph id))))
    (concat
     (apply #'concat
            (cl-loop for (target . kind) in outgoing
                     unless (equal target id)
                     collect (org-glance-material--relation-line
                              graph target :kind kind :direction 'out
                              :stamp out-stamp)))
     (apply #'concat
            (cl-loop for (source-id . kind)
                     in (org-glance-cache:incoming-relations graph id)
                     unless (equal source-id id)
                     when (org-glance-headline-metadata?
                           (org-glance-cache:headline graph source-id))
                     collect (org-glance-material--relation-line
                              graph source-id :kind kind :direction 'in
                              :stamp (org-glance-material--relation-stamp
                                      graph source-id)))))))

(cl-defun org-glance-material--own-logbook ()
  "Return the first heading's own LOGBOOK drawer element, or nil."
  (save-excursion
    (org-glance-material--goto-first-heading)
    (let* ((root (org-element-map (org-element-parse-buffer) 'headline #'identity nil t))
           (root-beg (org-element-property :begin root)))
      (org-element-map root 'drawer
        (lambda (drawer)
          (let ((owner (org-element-lineage drawer '(headline) t)))
            (when (and owner
                       (= root-beg (org-element-property :begin owner))
                       (string-equal-ignore-case
                        "LOGBOOK" (org-element-property :drawer-name drawer)))
              drawer)))
        nil t))))

(cl-defun org-glance-material--relation-insert-position ()
  "Return the position for a fresh LOGBOOK in this material buffer."
  (save-excursion
    (org-glance-material--goto-first-heading)
    (forward-line 1)
    (when (looking-at-p org-planning-line-re) (forward-line 1))
    (when-let* ((body (org-get-property-block)))
      (goto-char (cdr body))
      (forward-line 1))
    (point)))

(defun org-glance-material--relation-entry-key (line)
  "Return LINE's relation label and target, ignoring description and time."
  (when (string-match org-link-bracket-re line)
    (let ((label (substring line 0 (match-beginning 0)))
          (target (match-string 1 line)))
      (cons (string-trim label) target))))

(cl-defun org-glance-material--insert-relation-entries ()
  "Append missing relation entries as ordinary editable LOGBOOK text.
Preserve existing entries and record insertions in the normal undo history."
  (when (and org-glance-material--graph org-glance-material--id)
    (save-restriction
      (widen)
      (save-excursion
        (let* ((lines (split-string
                      (org-glance-material--relation-lines
                       org-glance-material--graph org-glance-material--id) "\n" t))
               (drawer (and lines (org-glance-material--own-logbook)))
               (existing (make-hash-table :test #'equal)))
          (when drawer
            (dolist (line (split-string
                          (buffer-substring-no-properties
                           (org-element-property :begin drawer)
                           (org-element-property :end drawer)) "\n" t))
              (puthash (org-glance-material--relation-entry-key line) t existing)))
          (setq lines (cl-delete-if
                       (lambda (line)
                         (gethash (org-glance-material--relation-entry-key line) existing))
                       lines))
          (when lines
            (goto-char (if drawer
                           (org-element-property :contents-end drawer)
                         (org-glance-material--relation-insert-position)))
            (unless (bolp) (insert "\n"))
            (unless drawer (insert ":LOGBOOK:\n"))
            (insert (string-join lines "\n") "\n")
            (unless drawer (insert ":END:\n"))))))))

(cl-defun org-glance-material--line-key (line)
  "Return the uppercase property key of drawer LINE, or nil."
  (when (string-match "^[ \t]*:\\([A-Za-z0-9_-]+\\):" line)
    (upcase (match-string 1 line))))

(cl-defun org-glance-material--reserved-line? ()
  "Non-nil when the line at point is a managed drawer property."
  (member (org-glance-material--line-key (thing-at-point 'line t))
          org-glance-material--managed-keys))

(cl-defun org-glance-material--remove-reserved-lines (&optional keep-delimiters)
  "Delete the managed lines of the heading's property drawer; return them.
Each line comes back verbatim, newline included, in drawer order.  A drawer
left with no property goes whole.
KEEP-DELIMITERS retains its source delimiters."
  (save-excursion
    (org-glance-material--goto-first-heading)
    (when-let* ((body (and org-glance-material--managed-keys
                           (org-at-heading-p)
                           (org-get-property-block))))
      (let ((end (copy-marker (cdr body)))
            lines)
        (when keep-delimiters
          (setq org-glance-material--drawer-lines
                (save-excursion
                  (goto-char (car body))
                  (cons (buffer-substring (line-beginning-position 0) (car body))
                        (progn
                          (goto-char (cdr body))
                          (buffer-substring (point) (line-beginning-position 2)))))))
        (goto-char (car body))
        (while (< (point) end)
          (if (org-glance-material--reserved-line?)
              (let ((beg (point)))
                (forward-line 1)
                (push (buffer-substring beg (point)) lines)
                (delete-region beg (point)))
            (forward-line 1)))
        (when (and lines (= (car body) end))    ; emptied: drop the drawer
          (goto-char end)
          (delete-region (line-beginning-position 0)
                         (line-beginning-position 2)))
        (set-marker end nil)
        (nreverse lines)))))

(cl-defun org-glance-material--strip-reserved ()
  "Stash managed drawer lines while preserving undo and the modified flag.
Run after loading a source; the serializer restores the stashed lines."
  (let ((buffer-undo-list t)
        (modified (buffer-modified-p)))
    (setq org-glance-material--drawer-lines nil)
    (setq-local org-glance-material--reserved-lines
                (org-glance-material--remove-reserved-lines t))
    (set-buffer-modified-p modified)))

(cl-defun org-glance-material--drawer-splice (lines)
  "Return (POS . TEXT) that puts LINES into the heading's property drawer.
Into the drawer when there is one, else a fresh drawer after the heading and its
planning line.  Point is on the heading."
  (if-let* ((body (org-get-property-block)))
      (cons (car body) lines)
    (forward-line 1)
    (when (looking-at-p org-planning-line-re) (forward-line 1))
    ;; a heading on the file's last line ends without a newline to follow
    (cons (point)
          (concat (unless (bolp) "\n")
                  (or (car org-glance-material--drawer-lines) ":PROPERTIES:\n")
                  lines
                  (or (cdr org-glance-material--drawer-lines) ":END:\n")
                  (when (and (< (point) (point-max))
                             org-glance-material--drawer-lines
                             (not (string-suffix-p "\n" (cdr org-glance-material--drawer-lines))))
                    "\n")))))

(cl-defun org-glance-material--normalize-tags ()
  "Canonicalize heading tags before saving, and warn when they change."
  (when org-glance-material--id
    (when (> (org-glance-tag:normalize-buffer) 0)
      (display-warning 'org-glance "heading tags downcased, deduplicated and sorted"))))

(defun org-glance-material--read-source (state)
  "Return STATE's verified snapshot content, refusing damaged or missing bytes."
  (let* ((path (plist-get state :path))
         (content (and path (f-read-text path 'utf-8-unix))))
    (unless (and content (plist-get state :verified) (plist-get state :available)
                 (equal (secure-hash 'sha256 content) (plist-get state :digest)))
      (user-error "No verified readable snapshot"))
    content))

(defun org-glance-material--load-source (state &optional initial)
  "Load STATE and reset the editor's undo baseline, preserving its modes.
INITIAL retains the verified initial text and any Org mode hook edits."
  (let ((content (if initial (buffer-string) (org-glance-material--read-source state)))
        (modified (and initial (buffer-modified-p))))
    (org-glance-material--clear-password)
    (setq org-glance-material--password-proof nil)
    (let ((org-glance-material--display-change t)
          (buffer-undo-list t)
          (inhibit-read-only t))
      (save-restriction
        (widen)
        (unless (equal content (buffer-string))
          (erase-buffer)
          (insert content))
        (org-glance-material--read-endings content)
        (org-glance-material--strip-reserved)))
    (setq org-glance-material--base state
          org-glance-material--encrypted (org-glance-headline--buffer-encrypted?)
          buffer-undo-list nil)
    (cl-incf org-glance-material--revision)
    (set-buffer-modified-p modified)
    (goto-char (point-min))
    (org-glance-material--reload-context org-glance-material--graph)
    (org-glance-material--insert-relation-entries)))

(cl-defun org-glance-material--revert (ignore-auto noconfirm)
  "Load the current snapshot, passing IGNORE-AUTO through indirect views.
NOCONFIRM permits discarding a dirty draft."
  (when (buffer-base-buffer)
    (cl-return-from org-glance-material--revert
      (with-current-buffer (buffer-base-buffer)
        (org-glance-material--revert ignore-auto noconfirm))))
  (org-glance-material--ensure)
  (when (or org-glance-material--busy org-glance-material--pending)
    (user-error "Complete the pending material save before reverting"))
  (when (or (not (buffer-modified-p)) noconfirm
            (yes-or-no-p "Discard unsaved material edits and revert? "))
    (let ((state (org-glance-cache:material-state
                  org-glance-material--graph org-glance-material--id)))
      (unless (equal "live" (plist-get state :state))
        (user-error "Headline is %s; visible content retained"
                    (or (plist-get state :state) "unavailable")))
      (let ((org-glance-material--busy t)
            (position (point)))
        (if (and (not (buffer-modified-p))
                 (equal (plist-get state :version)
                        (plist-get org-glance-material--base :version))
                 (equal (plist-get state :digest)
                        (plist-get org-glance-material--base :digest)))
            (progn
              (setq org-glance-material--base state)
              (org-glance-material--insert-relation-entries))
          (org-glance-material--load-source state)
          (goto-char (min position (point-max)))))
      (org-glance-view:mark-fresh))))

(defun org-glance-material:revert ()
  "Refresh this editor from its current verified snapshot with draft protection."
  (interactive)
  (org-glance-material--revert nil nil))

(defun org-glance-material--kill-query ()
  "Offer to save, discard or retain edits and pending publication."
  (if (or (buffer-base-buffer)
          (eq (current-buffer) org-glance--discarding-buffer)
          (not (org-glance-material--outstanding-p))) t
    (pcase (read-char-choice "Material draft: [s]ave, [d]iscard, [c]ancel? " '(?s ?d ?c))
      (?s (org-glance-material:save) (not (org-glance-material--outstanding-p)))
      (?d (set-buffer-modified-p nil) t)
      (_ nil))))

(defun org-glance-material--exit-query ()
  "Require discard consent for material drafts remaining after exit's save pass."
  (let ((dirty 0))
    (maphash (lambda (_key buffer)
               (when (and (buffer-live-p buffer)
                          (org-glance-material--outstanding-p buffer))
                 (cl-incf dirty)))
             org-glance-material--buffers)
    (or (= 0 dirty)
        (yes-or-no-p (format "Discard %d unsaved material draft(s) and exit Emacs? " dirty)))))

(add-hook 'kill-emacs-query-functions #'org-glance-material--exit-query)

(defun org-glance-material--file-command ()
  "Explain how virtual material editors save and export content."
  (interactive)
  (user-error "Use save-buffer for Material or org-glance-material:export for a copy"))

(defun org-glance-material--set-file (set-file &rest args)
  "Run SET-FILE with ARGS while refusing attachment of a material editor."
  (if (or org-glance-material-mode
          (and (buffer-base-buffer)
               (buffer-local-value 'org-glance-material-mode (buffer-base-buffer))))
      (org-glance-material--file-command)
    (apply set-file args)))

(advice-add 'set-visited-file-name :around #'org-glance-material--set-file)

(defun org-glance-material:export (file)
  "Write ordinary material content to FILE while retaining this editor."
  (interactive "FExport content to: ")
  (org-glance-material--ensure)
  (when (or org-glance-material--encrypted (org-glance--crypt-block-regions))
    (user-error "Crypt content export requires an explicit confidential workflow"))
  (when (and (file-exists-p file) (not (yes-or-no-p "Replace existing export? ")))
    (user-error "Export canceled"))
  (org-glance--atomic-write file (car (org-glance-material--serialize))))

(defun org-glance-material:diff ()
  "Compare ordinary content with a pinned preview and return the diff buffer.
Refuse crypt content and retain this editor's source reference."
  (interactive)
  (org-glance-material--ensure)
  (when (or org-glance-material--encrypted (org-glance--crypt-block-regions))
    (user-error "Crypt content comparison requires a confidential workflow"))
  (require 'diff)
  (let ((base (org-glance-material:preview
               org-glance-material--graph org-glance-material--id
               (plist-get org-glance-material--base :version)))
        (content (car (org-glance-material--serialize))))
    (with-temp-buffer
      (org-glance-material--harden-buffer)
      (insert content)
      (diff-buffers base (current-buffer) nil t))))

(defun org-glance-material--diff-buffer (diff &optional buffer)
  "Run DIFF for BUFFER, using Material's editing base for a virtual editor."
  (let ((target (get-buffer (or buffer (current-buffer)))))
    (with-current-buffer (or (buffer-base-buffer target) target)
      (if org-glance-material-mode (org-glance-material:diff)
        (funcall diff buffer)))))

(with-eval-after-load 'diff
  (advice-add 'diff-buffer-with-file :around #'org-glance-material--diff-buffer))

(define-key org-glance-material-mode-map [remap write-file] #'org-glance-material--file-command)
(define-key org-glance-material-mode-map [remap set-visited-file-name] #'org-glance-material--file-command)
(define-key org-glance-material-mode-map [remap diff-buffer-with-file] #'org-glance-material:diff)

(cl-defun org-glance-material--drop-hand-typed-reserved ()
  "Delete hand-typed managed drawer lines and warn once per dropped line."
  (dolist (line (org-glance-material--remove-reserved-lines))
    (display-warning
     'org-glance
     (format "%s is managed by org-glance; your line was dropped"
             (org-glance-material--line-key line)))))

(defun org-glance-material--create (graph id state)
  "Create GRAPH's logical editor for ID from verified STATE."
  (let ((content (org-glance-material--read-source state))
        (buffer (generate-new-buffer (format "*org-glance: %s*" id))))
    (condition-case err
        (with-current-buffer buffer
          (setq default-directory (file-name-as-directory (org-glance-graph:directory graph)))
          (org-glance-material--harden-buffer)
          (insert content)
          (set-buffer-modified-p nil)
          (org-mode)
          (org-glance-material--harden-buffer)
          (setq-local org-glance-material--graph graph
                      org-glance-material--id id
                      org-glance-material--row-id id
                      default-directory (file-name-as-directory
                                         (org-glance-graph:directory graph))
                      require-final-newline nil
                      buffer-offer-save 'always
                      org-glance-material--managed-keys org-glance-material-hidden-properties
                      revert-buffer-function #'org-glance-material--revert)
          (org-glance-material-mode 1)
          (org-glance-material--load-source state t)
          (org-cycle-set-startup-visibility)
          (buffer-enable-undo)
          (add-hook 'after-change-functions #'org-glance-material--changed nil t)
          (add-hook 'write-contents-functions #'org-glance-material--write-version nil t)
          (add-hook 'kill-buffer-query-functions #'org-glance-material--kill-query nil t)
          (add-hook 'kill-buffer-hook #'org-glance-material--unregister nil t)
          (add-hook 'kill-buffer-hook #'org-glance-material--clear-password nil t)
          (org-glance-view:register graph
                                   :stale-fn (lambda () org-glance-view--stale)
                                   :reload-fn #'org-glance-material:revert)
          (when-let* ((meta (org-glance-cache:headline graph id)))
            (when (org-glance-headline-metadata? meta)
              (rename-buffer (format "*org-glance: %s*"
                                     (org-glance-headline-metadata:title meta)) t)))
          (puthash (cons (org-glance-graph:directory graph) id)
                   buffer org-glance-material--buffers)
          buffer)
      ((error quit) (org-glance--discard-buffer buffer)
       (signal (car err) (cdr err))))))

(cl-defun org-glance-material:open (graph id &key decrypt)
  "Return GRAPH's logical editor for ID, reusing its buffer and draft.
Exact-version Row IDs open pinned read-only previews.  DECRYPT explicitly
unseals a logical editor; ordinary opens preserve sealed content."
  (when-let* ((existing (org-glance-material:buffer graph id)))
    (when decrypt (org-glance-material--maybe-decrypt nil existing))
    (cl-return-from org-glance-material:open existing))
  (let ((logical (org-glance-cache:logical-id graph id)))
    (unless (equal logical id)
      (cl-return-from org-glance-material:open
        (org-glance-material:preview graph logical
                                     (substring id (1+ (length logical))))))
    (let ((buffer (or (org-glance-material:buffer graph id)
                      (let ((state (org-glance-cache:material-state graph id)))
                        (unless (equal "live" (plist-get state :state))
                          (user-error "No current readable headline with id %s" id))
                        (org-glance-material--create graph id state)))))
      (when decrypt (org-glance-material--maybe-decrypt nil buffer))
      buffer)))

(defvar org-glance-material--previews (make-hash-table :test #'equal)
  "Pinned version previews keyed by graph, headline and version.")

(cl-defun org-glance-material:preview (graph id version)
  "Return a pinned read-only preview of VERSION of logical ID in GRAPH."
  (org-glance-graph:headline-data-path graph id)
  (let* ((key (list (org-glance-graph:directory graph) id version))
         (existing (gethash key org-glance-material--previews)))
    (if (buffer-live-p existing) existing
      (let* ((state (org-glance-cache:material-state graph id version))
             (path (plist-get state :path))
             (text (and path (f-read-text path 'utf-8-unix))))
        (unless (and text (plist-get state :verified) (plist-get state :available)
                     (equal (secure-hash 'sha256 text) (plist-get state :digest)))
          (user-error "Version %s is unavailable or damaged" version))
        (let ((buffer (generate-new-buffer (format "*org-glance-history: %s [%s]*" id version))))
          (with-current-buffer buffer
            (setq default-directory (file-name-as-directory (org-glance-graph:directory graph)))
            (org-glance-material--harden-buffer)
            (insert text)
            (org-mode)
            (org-glance-material--harden-buffer)
            (setq default-directory (file-name-as-directory (org-glance-graph:directory graph)))
            (local-set-key (kbd "C-c C-c")
                           (lambda () (interactive)
                             (switch-to-buffer (org-glance-material:restore graph id version))))
            (goto-char (point-min))
            (set-buffer-modified-p nil)
            (read-only-mode 1))
          (puthash key buffer org-glance-material--previews)
          buffer)))))

(cl-defun org-glance-material:restore (graph id version)
  "Seed GRAPH's logical editor for ID from VERSION with current parents.
Require consent before discarding an existing dirty draft.  Saving publishes
one child of every captured structural leaf."
  (let* ((existing (org-glance-material:buffer graph id))
         (state (org-glance-cache:material-state graph id version)))
    (when existing
      (with-current-buffer existing
        (when (or org-glance-material--busy org-glance-material--pending)
          (user-error "Complete the pending material operation first"))
        (when (and (buffer-modified-p)
                   (not (yes-or-no-p "Discard unsaved material edits and restore version? ")))
          (user-error "Restore canceled"))))
    (let ((buffer (or existing (org-glance-material--create graph id state))))
      (with-current-buffer buffer
        (when existing (org-glance-material--load-source state))
        (set-buffer-modified-p t))
      buffer)))

;;; TODO state change

(defvar org-log-setup)         ; org.el: non-nil while an interactive note is queued
(defvar org-log-note-how)      ; org.el: `note' (prompt) vs `time'/`state' (timestamp)
(defvar org-log-note-this-command) ; org.el: command that queued the note
(defvar org-log-note-marker)   ; org.el: where the queued note will be inserted
(declare-function org-add-log-note "org" (&optional purpose))

(cl-defun org-glance-material--cancel-pending-log-note ()
  "Drop a log note queued into this buffer, on `kill-buffer-hook'.
Its `org-add-log-note' would run after the kill and error on the dead marker;
`org-check-running-clock' queues one when it clocks out mid-kill."
  (when (and (bound-and-true-p org-log-setup)
             (markerp (bound-and-true-p org-log-note-marker))
             (eq (marker-buffer org-log-note-marker) (current-buffer)))
    (remove-hook 'post-command-hook #'org-add-log-note)
    (setq org-log-setup nil)
    (set-marker org-log-note-marker nil)
    (message "org-glance: pending log note dropped (its buffer was killed)")))

(cl-defun org-glance-material--goto-first-heading ()
  "Move point to the first heading of the current buffer."
  (goto-char (point-min))
  (unless (org-at-heading-p) (outline-next-heading)))

(defun org-glance-material--require-logical (id)
  "Refuse a version Row ID where a logical mutation is required."
  (when (string-match-p "/" id)
    (user-error "Restore the selected version before changing its logical headline")))

(cl-defun org-glance-material:change-todo-live (graph id arg finalize)
  "Advance ID's TODO state in GRAPH via `org-todo' with prefix ARG; persist it.
A buffer this opens is saved once the change commits (after any note), then
killed, and FINALIZE runs on the new state in the origin buffer.  An open
material buffer is edited in place; the user saves it."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (org-glance-material--require-logical id)
  (let* ((fresh (null (org-glance-material:buffer graph id)))
         (origin (current-buffer))
         (buf (org-glance-material:open graph id)))  ; user-errors if not live
    (if (not fresh)
        (progn
          (switch-to-buffer buf)
          (org-glance-material--goto-first-heading)
          (let ((current-prefix-arg arg)) (call-interactively #'org-todo)))
      (let ((owned nil) (commit-now nil))
        (cl-labels
            ((finish (state)
               (when (and (buffer-live-p buf) (not (org-glance-material--outstanding-p buf)))
                 (kill-buffer buf))
               (when (buffer-live-p origin)
                 (with-current-buffer origin (funcall finalize state))))
             (persist ()
               (with-current-buffer buf
                 (save-buffer)
                 (substring-no-properties (or (org-get-todo-state) "")))))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-glance-material--goto-first-heading)
                  (let ((current-prefix-arg arg) (org-log-setup nil))
                    (call-interactively #'org-todo)
                    (cond
                     (org-log-setup
                      (org-glance-material--on-next-log-note
                       (lambda ()
                         (let ((state (persist)))
                           (run-at-time 0 nil (lambda () (finish state))))))
                      (setq owned t))
                     ((org-glance-material--outstanding-p)
                      (setq owned t commit-now t)))))
                (when commit-now (finish (persist))))
            (unless (or owned (org-glance-material--outstanding-p buf))
              (org-glance--discard-buffer buf))))))))

(cl-defun org-glance-material--on-next-log-note (continuation)
  "Run CONTINUATION once the pending log note is stored or aborted.
Install self-removing `:after' advice on `org-store-log-note'; return it."
  (letrec ((adv (lambda (&rest _)
                  (advice-remove 'org-store-log-note adv)
                  (funcall continuation))))
    (advice-add 'org-store-log-note :after adv)
    adv))

(cl-defun org-glance-material:set-todo-bulk (graph ids state finalize)
  "Set each of IDS in GRAPH to TODO STATE via `org-todo', then call FINALIZE.
Rows run one at a time, org keeping one global note marker; an interactive
note settles before the next row.  Rows with unsaved edits (invariant 11),
dead ids, or a cycle rejecting STATE are skipped.  FINALIZE runs in the
origin buffer with (CHANGED SKIPPED): ids set, (id . reason) pairs skipped."
  (cl-check-type graph org-glance-graph)
  (cl-check-type state string)
  (let ((origin (current-buffer)) (queue (copy-sequence ids)) changed skipped)
    (cl-labels
        ((kill-fresh (buf existing)
           (when (and (not existing) (buffer-live-p buf)
                      (not (org-glance-material--outstanding-p buf)))
             (kill-buffer buf)))
         (resume (buf existing id)          ; run after an interactive note settles
           (condition-case err
               (progn
                 (with-current-buffer buf (save-buffer))
                 (push id changed))
             (error (push (cons id (error-message-string err)) skipped)))
           ;; Kill + advance OFF `org-store-log-note's extent (its window restore).
           (run-at-time 0 nil (lambda () (kill-fresh buf existing) (drive))))
         (change-row (id)                   ; -> t when it SUSPENDS on a note, else nil
           (let ((existing (org-glance-material:buffer graph id)))
             (cond
              ((and existing (org-glance-material--outstanding-p existing))
               (push (cons id "unsaved changes") skipped) nil)
              (t
               (let ((buf (ignore-errors
                            (org-glance-material--require-logical id)
                            (org-glance-material:open graph id)))
                     (suspended nil))
                 (if (not (buffer-live-p buf))
                     (progn (push (cons id "not live") skipped) nil)
                   (condition-case err
                       (with-current-buffer buf
                         (org-glance-material--goto-first-heading)
                         (let ((org-log-setup nil))
                           (org-todo state)
                           (cond
                            ((and org-log-setup (eq org-log-note-how 'note))
                             (let ((adv (org-glance-material--on-next-log-note
                                         (lambda () (resume buf existing id)))))
                               (unwind-protect
                                   (let ((this-command org-log-note-this-command))
                                     (org-add-log-note)  ; pops `*Org Note*'
                                     (setq suspended t))
                                 (unless suspended
                                   (advice-remove 'org-store-log-note adv)))))
                            (org-log-setup
                             (save-window-excursion
                               (let ((this-command org-log-note-this-command))
                                 (org-add-log-note)))
                             (with-current-buffer buf (save-buffer))
                             (push id changed))
                            (t (when (org-glance-material--outstanding-p) (save-buffer))
                               (push id changed)))))
                     (error (push (cons id (error-message-string err)) skipped)))
                   (unless suspended (kill-fresh buf existing))
                   suspended))))))
         (drive ()
           (let ((suspended nil))
             (while (and queue (not suspended))
               (setq suspended (change-row (pop queue))))
             (unless suspended
               (when (buffer-live-p origin)
                 (with-current-buffer origin
                   (funcall finalize (reverse changed) (reverse skipped))))))))
      (drive))))

(cl-defun org-glance-material:retag (graph id tag &key remove)
  "Add TAG to headline ID in GRAPH, or drop it when REMOVE is non-nil.
Edit and save ID's material buffer; signal a `user-error' when an open one
has unsaved edits (invariant 11).  Return non-nil when the tags changed."
  (cl-check-type graph org-glance-graph)
  (cl-check-type tag string)
  (unless remove (setq tag (org-glance-tag:validate-string tag)))
  (let ((existing (org-glance-material--assert-clean graph id)))
    (let ((buffer (org-glance-material:open graph id))
          (changed nil))
      (unwind-protect
          (with-current-buffer buffer
            (org-glance-material--goto-first-heading)
            (let* ((tags (org-get-tags nil t))
                   (want (org-glance--downcased-string tag))
                   (new (if remove
                            (cl-remove want tags
                                       :key #'org-glance--downcased-string :test #'string=)
                          (if (cl-member want tags
                                         :key #'org-glance--downcased-string :test #'string=)
                              tags
                            (append tags (list tag))))))
              (unless (equal tags new)
                (org-set-tags new)
                (let ((inhibit-message t)) (save-buffer))
                (setq changed t))))
        (when (and (not existing) (not (org-glance-material--outstanding-p buffer)))
          (org-glance--discard-buffer buffer)))
      changed)))

(cl-defun org-glance-material--assert-clean (graph id)
  "Return GRAPH's editor for ID; refuse an unsaved draft or pending operation."
  (org-glance-material--require-logical id)
  (let ((existing (org-glance-material:buffer graph id)))
    (when existing
      (with-current-buffer existing
        (when (or (org-glance-material--outstanding-p) org-glance-material--busy)
          (user-error "org-glance: %s has unsaved edits or pending work" id))))
    existing))

(cl-defun org-glance-material--replace-headline (graph id transform &key sealed)
  "Apply TRANSFORM to GRAPH's clean headline ID and save its logical editor.
Return the transformed headline.  SEALED supplies ciphertext to crypt
transforms.  Preserve existing editors and refuse unsaved edits; retain any
draft whose publication fails."
  (let* ((existing (org-glance-material--assert-clean graph id))
         (buffer (org-glance-material:open graph id)))
    (unwind-protect
        (with-current-buffer buffer
          (save-restriction
            (widen)
            (let* ((encrypted org-glance-material--encrypted)
                   (serialized (org-glance-material--serialize :seal sealed))
                   (headline (org-glance-headline--from-string (car serialized)))
                   (new (funcall transform headline))
                   (content (org-glance-headline:contents new))
                   (managed org-glance-material--managed-keys)
                   reserved drawer endings mixed)
              (with-temp-buffer
                (org-glance-material--harden-buffer)
                (insert content)
                (org-glance--org-mode)
                (setq-local org-glance-material--managed-keys managed)
                (org-glance-material--read-endings content)
                (org-glance-material--strip-reserved)
                (setq content (buffer-string)
                      reserved org-glance-material--reserved-lines
                      drawer org-glance-material--drawer-lines
                      endings org-glance-material--newline-default
                      mixed org-glance-material--newline-mixed))
              (save-excursion
                (replace-region-contents (point-min) (point-max) (lambda () content)))
              (setq org-glance-material--reserved-lines reserved
                    org-glance-material--drawer-lines drawer
                    org-glance-material--newline-default endings
                    org-glance-material--newline-mixed mixed
                    org-glance-material--encrypted
                    (or (org-glance-headline--buffer-encrypted?)
                        (and (not sealed) encrypted (org-glance--crypt-block-regions) t)))
              (when sealed
                (org-glance-material--clear-password)
                (setq org-glance-material--password-proof nil))
              (save-buffer)
              new)))
      (when (and (not existing) (not (org-glance-material--outstanding-p buffer)))
        (org-glance--discard-buffer buffer)))))

(cl-defun org-glance-material:delete (graph id)
  "Tombstone headline ID in GRAPH after confirmation; return t when deleted.
The prompt names referrers and unsaved edits; ID's open buffer is discarded
and views flagged stale."
  (org-glance-material--require-logical id)
  (let* ((title (org-glance-graph:title-or-id graph id))
         (buf (org-glance-material:buffer graph id))
         (referrers
          (cl-loop for meta in (org-glance-graph:headlines graph)
                   when (assoc id (org-glance-headline-metadata:relations meta))
                   collect (org-glance-headline-metadata:title meta)))
         (prompt
          (concat
           (if referrers
               (format "Delete \"%s\"? %d headline(s) reference it (%s) -- their links will dangle"
                       title (length referrers)
                       (s-join ", " (mapcar (lambda (r) (s-truncate 30 r)) referrers)))
             (format "Delete \"%s\"?" title))
           (if (and buf (org-glance-material--outstanding-p buf))
               " (its open buffer has UNSAVED edits or pending publication, which will be discarded)"
             "")
           " ")))
    (when (yes-or-no-p prompt)
      ;; invariant 11: tombstone FIRST; a failed append keeps the edits.
      (org-glance-graph:delete graph id)
      (when buf (org-glance--discard-buffer buf))
      (message "org-glance: headline deleted (disk reclaimed at next compaction)")
      t)))

;;;###autoload
(cl-defun org-glance-delete ()
  "Choose a headline and delete it (tombstone; referrer-aware confirmation).
Ignores `org-glance-filter-spec', so filtered-out headlines stay deletable."
  (interactive)
  (org-glance-ensure-init)
  (org-glance-material:delete
   org-glance-graph
   (org-glance-headline-metadata:id (org-glance-material:completing-read
                                     org-glance-graph :prompt "Delete: "))))

(cl-defun org-glance-material:duplicate (graph id)
  "Add a copy of headline ID to GRAPH under a fresh id; return the new id.
The blob is copied verbatim minus ORG_GLANCE_HASH; snapshots stay behind."
  (cl-check-type graph org-glance-graph)
  (org-glance-material--require-logical id)
  (let ((headline (org-glance-graph:headline graph id))
        (new-id (org-glance-graph:make-id graph)))
    (unless headline (user-error "No live headline with id %s" id))
    (org-glance-graph:add
     graph
     (org-glance-headline--map-contents headline
       (org-entry-put nil "ORG_GLANCE_ID" new-id)
       (org-entry-delete nil "ORG_GLANCE_HASH")))
    new-id))

(cl-defun org-glance-material:set-title (graph id title)
  "Set headline ID's heading TITLE in GRAPH; state, priority and tags stay."
  (org-glance-material--replace-headline
   graph id
   (lambda (headline)
     (org-glance-headline--map-contents headline
       (org-edit-headline title)))))

(cl-defun org-glance-material:set-priority (graph id priority)
  "Set headline ID's PRIORITY cookie (a character) in GRAPH; nil clears it."
  (org-glance-material--replace-headline
   graph id
   (lambda (headline)
     (org-glance-headline--map-contents headline
       (org-priority (or priority 'remove))))))

(cl-defun org-glance-material:set-property (graph id property value)
  "Set headline ID's drawer PROPERTY to VALUE in GRAPH; blank VALUE deletes it.
Managed keys (`org-glance-material-hidden-properties') refuse."
  (let ((prop (org-glance--property-key property)))
    (when (member prop org-glance-material-hidden-properties)
      (user-error "Property %s is managed by org-glance" prop))
    (org-glance-material--replace-headline
     graph id
     (lambda (headline)
       (org-glance-headline--map-contents headline
         (if (org-glance--present-string? value)
             (org-entry-put nil prop (string-trim value))
           (org-entry-delete nil prop)))))))

(cl-defun org-glance-material:set-planning (graph id kind &optional remove)
  "Set headline ID's KIND (`schedule' or `deadline') planning in GRAPH.
Read the date with `org-read-date' and run org's planner; REMOVE clears it.
Signal a `user-error' on unsaved edits.  Return the new headline."
  (let ((setter (if (eq kind 'schedule) #'org-schedule #'org-deadline)))
    (org-glance-material--replace-headline
     graph id
     (lambda (headline)
       (let ((time (unless remove
                     (org-read-date nil nil nil
                                    (format "%s: " (capitalize (symbol-name kind))))))
             ;; A temp parse cannot host org's deferred note buffer.
             (org-log-reschedule nil)
             (org-log-redeadline nil))
         (org-glance-headline--map-contents headline
           (funcall setter (when remove '(4)) time)))))))

(cl-defun org-glance-material:crypt-set (graph id encrypt password)
  "Encrypt (ENCRYPT non-nil) or decrypt headline ID in GRAPH under PASSWORD.
Encrypt seals the blocks (the whole body when none) and purges snapshots;
decrypt strips all markers.  Signal a `user-error' when already so or dirty;
a wrong PASSWORD errors before any write.  Return t."
  (cl-check-type password string)
  (org-glance-material--replace-headline
   graph id
   (lambda (headline)
     (when (eq (and (org-glance-headline:encrypted? headline) t) (and encrypt t))
       (user-error "Headline is already %s" (if encrypt "encrypted" "decrypted")))
     (if encrypt
         (org-glance-headline:encrypt headline password)
       (org-glance-headline:decrypt headline password t)))
   :sealed t)
  (when encrypt (org-glance-material--purge-occurrences graph id))
  t)

(cl-defun org-glance-material:crypt-rekey (graph id old new)
  "Re-encrypt headline ID in GRAPH from the OLD password to NEW; return t.
Errors before any write on an unencrypted ID, unsaved edits or a wrong OLD."
  (cl-check-type old string)
  (cl-check-type new string)
  (org-glance-material--replace-headline
   graph id
   (lambda (headline)
     (unless (org-glance-headline:encrypted? headline)
       (user-error "Headline is not encrypted"))
     (org-glance-headline:encrypt (org-glance-headline:decrypt headline old) new))
   :sealed t)
  t)

;;; Commands

(cl-defun org-glance-material--filter-spec ()
  "Return `org-glance-filter-spec' relaxed by the transient's -a and -c.
They stop hiding archived and completed headlines, respectively."
  (let ((args (and (eq transient-current-command 'org-glance-transient)
                   (transient-args 'org-glance-transient)))
        (spec (org-glance-filter:normalize-spec org-glance-filter-spec)))
    (when (and (member "--archived" args)
               (plist-member spec :archived) (null (plist-get spec :archived)))
      (cl-remf spec :archived))
    (when (and (member "--completed" args)
               (plist-member spec :done) (null (plist-get spec :done)))
      (cl-remf spec :done))
    spec))

(cl-defun org-glance-material:pick-metadata (graph)
  "Choose a live GRAPH headline gated by the ambient `org-glance-filter-spec'."
  (org-glance-material:completing-read
   graph :filter (org-glance-filter:predicate (org-glance-material--filter-spec))))

;;;###autoload
(cl-defun org-glance-materialize ()
  "Choose a headline from the graph and materialize it.
The transient's `-d' switch decrypts it on open; otherwise it opens sealed."
  (interactive)
  (org-glance-ensure-init)
  (let* ((graph org-glance-graph)
         (metadata (org-glance-material:pick-metadata graph))
         (id (org-glance-headline-metadata:id metadata))
         (decrypt (and (eq transient-current-command 'org-glance-transient)
                       (member "--decrypt" (transient-args 'org-glance-transient)))))
    (switch-to-buffer (org-glance-material:open graph id :decrypt decrypt))))

;;; Read commands

(cl-defun org-glance-material--choose-link-and-open ()
  "Pick one of the buffer's non-org-glance links, level by level; open it."
  (goto-char (org-glance--pick-link-pos
              (cl-remove-if (lambda (entry)
                              (s-starts-with-p "org-glance-" (or (nth 2 entry) "")))
                            (org-glance--link-paths))))
  (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
    (org-open-at-point)))

(cl-defun org-glance-material:open-link (headline)
  "Open a non-org-glance link from HEADLINE's contents, prompting if several."
  (cl-check-type headline org-glance-headline)
  (org-glance-headline:with-contents headline
    (org-glance-material--choose-link-and-open)))

(cl-defun org-glance-material:open-link-here ()
  "Open a link from this live material buffer (`C-c j', the transient's `j')."
  (interactive)
  (org-glance-material--ensure)
  (save-excursion (org-glance-material--choose-link-and-open)))

(cl-defun org-glance-material--pick-headline (prompt extra-pred)
  "Read a graph headline matching the ambient filter AND EXTRA-PRED under PROMPT."
  (org-glance-ensure-init)
  (let* ((graph org-glance-graph)
         (keep? (org-glance-filter:predicate (org-glance-material--filter-spec)))
         (metadata (org-glance-material:completing-read
                    graph :prompt prompt
                    :filter (lambda (m) (and (funcall keep? m)
                                        (funcall extra-pred m))))))
    (let* ((id (org-glance-headline-metadata:id metadata))
           (contents (org-glance-graph:get-content graph id)))
      (and contents (org-glance-graph--reparse-blob graph metadata contents)))))

;;;###autoload
(cl-defun org-glance-open ()
  "Choose a headline from the graph and open a link inside it."
  (interactive)
  (org-glance-material:open-link
   (org-glance-material--pick-headline "Open: " #'org-glance-headline-metadata:linked?)))

(cl-defun org-glance-material:extract-pairs (pairs &optional key)
  "Copy the value of KEY in alist PAIRS confidentially; return it.
Nil KEY prompts for one.  Signal a `user-error' on empty PAIRS."
  (unless pairs (user-error "No key-value pairs in headline"))
  (let* ((key (or key (completing-read "Extract: " pairs nil t)))
         (pair (assoc-string key pairs))
         (value (cdr pair)))
    (unless pair (user-error "Unknown property"))
    (org-glance-clipboard:copy value)
    (message "Property copied; clipboard expires in %s seconds"
             org-glance-clipboard-ttl)
    value))

(cl-defun org-glance-material:extract (headline &optional key)
  "Copy a property of HEADLINE confidentially; return its value.
Decrypt sealed content before selecting KEY; nil KEY prompts for one."
  (cl-check-type headline org-glance-headline)
  (org-glance-headline:with-contents headline
    (org-glance-material--extract-buffer key)))

(defun org-glance-material--extract-buffer (&optional key)
  "Decrypt scratch contents and copy property KEY, prompting when nil."
  (when (org-glance-headline--buffer-encrypted?)
    (let ((password (read-passwd "Headline password: ")))
      (unwind-protect
          (progn
            (org-glance-headline--crypt-upgrade-legacy)
            (org-glance--crypt-unseal-blocks password))
        (clear-string password))))
  (org-glance-material:extract-pairs
   (org-glance-headline--buffer-properties) key))

(cl-defun org-glance-material:set-interval (&optional remove)
  "Set this headline's date interval (`C-c i'); with REMOVE (`C-u'), drop it.
Replaces the buffer's first active range outside crypt blocks, or inserts one."
  (interactive "P")
  (org-glance-material--ensure)
  (org-with-wide-buffer
    (org-glance-material--goto-first-heading)
      (org-end-of-meta-data t)
      ;; Never inside a crypt block: index reads SEALED bytes (invariant 14).
      (cl-flet ((goto-body-range ()
                  (cl-loop while (re-search-forward org-tr-regexp nil t)
                           unless (org-glance--crypt-block-at (match-beginning 0))
                           return t
                           finally return nil)))
        (let ((body (point)))
          (cond
           (remove
            (if (goto-body-range)
                (progn
                  (replace-match "" t t)
                  (when (string-blank-p (buffer-substring (line-beginning-position)
                                                          (line-end-position)))
                    (delete-region (line-beginning-position)
                                   (min (point-max) (1+ (line-end-position)))))
                  (message "Interval removed"))
              (user-error "No interval to remove")))
           (t
            (let* ((from (org-read-date nil t nil "Interval from: "))
                   (to (org-read-date nil t nil "Interval to: "))
                   (range (concat (format-time-string (org-time-stamp-format) from)
                                  "--"
                                  (format-time-string (org-time-stamp-format) to))))
              (if (goto-body-range)
                  (replace-match range t t)
                (goto-char body)
                (unless (bolp) (insert "\n"))
                (insert range "\n"))
              (message "Interval set: %s" range))))))))

(cl-defun org-glance-material:extract-here ()
  "Copy a body `KEY: value' pair from this materialized headline (`C-c e').
Read live edits; decrypt sealed blocks in a scratch buffer before selection."
  (interactive)
  (org-glance-material--ensure)
  (org-glance-headline:with-contents
      (buffer-substring-no-properties (point-min) (point-max))
    (org-glance-material--extract-buffer)))

;;;###autoload
(cl-defun org-glance-extract ()
  "Choose a headline from the graph and extract a key-value pair from it."
  (interactive)
  (org-glance-material:extract
   (org-glance-material--pick-headline
    "Extract from: "
    (lambda (m) (or (org-glance-headline-metadata:propertized? m)
                    (org-glance-headline-metadata:encrypted? m))))))

;;; References

(cl-defun org-glance-material--read-kind (graph)
  "Read a reference kind, GRAPH's own offered; return its slug or nil."
  (let* ((known (mapcar #'org-glance--kind-pretty (org-glance-graph:edge-kinds graph)))
         (kind (s-trim (completing-read "Reference kind (empty for none): " known))))
    (unless (string-empty-p kind)
      (org-glance--kind-slug kind))))

(cl-defun org-glance-material--read-reference (graph self &key with-kind)
  "Choose a reference target in GRAPH other than SELF; return (ID TITLE KIND).
When WITH-KIND is non-nil, read KIND before choosing the target."
  (let* ((kind (when with-kind (org-glance-material--read-kind graph)))
         (meta (org-glance-material:completing-read
                graph :prompt "Refer to: "
                :filter (lambda (m) (not (equal self (org-glance-headline-metadata:id m)))))))
    (list (org-glance-headline-metadata:id meta)
          (org-glance--title-clean (org-glance-headline-metadata:title meta))
          kind)))

(cl-defun org-glance-material:insert-reference (graph self &key with-kind)
  "Insert a reference edge at point, or self-insert `@'.
At a word boundary in body or title, pick a GRAPH headline other than SELF
and insert its `org-glance-material:' link.  An active region becomes the link
title.  WITH-KIND reads the kind first.  At a heading's column 0 (speed keys)
or mid-word, self-insert as remapped."
  (let* ((region? (use-region-p))
         (beg (and region? (region-beginning)))
         (end (and region? (region-end)))
         (region-title (and region?
                            (buffer-substring-no-properties beg end))))
    (if (and (not region?)
             (or (and (org-at-heading-p) (bolp))
                 (not (or (bolp) (memq (char-before) '(?\s ?\t ?\n))))))
      (call-interactively (or (command-remapping 'self-insert-command)
                              #'self-insert-command))
      (pcase-let ((`(,id ,title ,kind)
                   (org-glance-material--read-reference graph self
                                                        :with-kind with-kind)))
        (when region?
          (delete-region beg end))
        (insert (org-glance--edge->string id kind (or region-title title)))))))

(cl-defun org-glance-material:refer (&optional arg)
  "Insert a reference to another headline at point, or self-insert `@'.
Delegates to `org-glance-material:insert-reference'.  ARG (`C-u @') reads the
kind before the target.  `C-q @' inserts a literal `@'."
  (interactive "P")
  (org-glance-material:insert-reference org-glance-material--graph
                                        org-glance-material--id
                                        :with-kind arg))

(provide 'org-glance-material)
;;; org-glance-material.el ends here
