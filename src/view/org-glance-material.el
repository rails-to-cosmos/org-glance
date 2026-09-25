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
(require 'org-glance-filter)
(require 'org-glance-tag)
(require 'org-glance-tag-config)
(require 'org-glance-datetime-mode)
(require 'org-glance-view)

(require 'org-glance-core)

;;; Selection

(cl-defun org-glance-material:label (metadata)
  "Return the `completing-read' label \"[tags] title\" of headline METADATA."
  (cl-check-type metadata org-glance-headline-metadata)
  (let ((tags (org-glance-headline-metadata:tag-strings metadata)))
    (concat (if tags (format "[%s] " (s-join "," tags)) "")
            (org-glance--title-clean (org-glance-headline-metadata:title metadata)))))

(cl-defun org-glance-material:completing-read (graph &key (prompt "Headline: ") filter)
  "Choose a live headline from GRAPH under PROMPT; return its metadata.
FILTER, if non-nil, is a predicate on the metadata."
  (cl-check-type graph org-glance-graph)
  (let* ((org-done-keywords (org-glance--done-keywords))
         (metas (cl-loop for meta in (org-glance-graph:headlines graph)
                         when (or (null filter) (funcall filter meta))
                         collect meta))
         (labels (mapcar #'org-glance-material:label metas))
         (counts (-frequencies labels))
         (candidates (cl-mapcar (lambda (meta label)
                                  (cons (if (> (alist-get label counts 0 nil #'equal) 1)
                                            (format "%s ·%s" label
                                                    (s-left 8 (org-glance-headline-metadata:id meta)))
                                          label)
                                        meta))
                                metas labels)))
    (unless candidates
      (let ((total (length (org-glance-graph:headlines graph)))
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
(define-key org-glance-material-mode-map (kbd "C-c j") #'org-glance-material:open-link-here)
(define-key org-glance-material-mode-map (kbd "C-c i") #'org-glance-material:set-interval)
(define-key org-glance-material-mode-map (kbd "@") #'org-glance-material:refer)

(defconst org-glance-project-dir-property "ORG_GLANCE_PROJECT_DIR"
  "Drawer property naming the project directory `org-glance-llm' opens in.
Set it with `org-glance-material:set-project-dir' (`C-c d').")

(defvar-local org-glance-material--graph nil
  "Graph backing the current materialized buffer.")

(cl-defun org-glance-material--ensure ()
  "Signal a `user-error' unless the current buffer is a materialized headline."
  (unless (and org-glance-material--graph org-glance-material--id)
    (user-error "Not in a materialized headline buffer")))

(cl-defun org-glance-material:current ()
  "Return this materialized buffer's (GRAPH . ID), or signal a `user-error'.
Plugins use it instead of the buffer-local internals."
  (org-glance-material--ensure)
  (cons org-glance-material--graph org-glance-material--id))

(defvar-local org-glance-material--id nil
  "ORG_GLANCE_ID of the headline materialized in the current buffer.")

(defvar-local org-glance-material--cycle nil
  "Per-tag `#+TODO:'-style cycle string for this buffer's headline, or nil.")

(cl-defun org-glance-material:sync ()
  "Append the just-saved file's metadata to the graph's WAL; flag views stale.
Buffer-local `after-save-hook'.  Parses the FILE: only it holds the managed
drawer lines (invariant 21), and its crypt blocks are sealed (invariant 27).
A file whose id differs from the buffer's is skipped with a message."
  (when (and org-glance-material--graph org-glance-material--id)
    (let* ((graph org-glance-material--graph)
           (id org-glance-material--id)
           (headline (let ((org-todo-keywords
                            (org-glance-tag-config:cycle->keywords-or
                             org-glance-material--cycle org-todo-keywords)))
                       (org-glance-headline--from-string
                        (f-read-text buffer-file-name 'utf-8)))))
      (if (equal (org-glance-headline:id headline) id)
          (progn
            (org-glance-graph:insert graph (list (org-glance-headline:metadata headline)))
            (org-glance-view:mark-graph-stale graph))
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
           (buffer-substring-no-properties (point-min) (point-max)))
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
      (let ((header (s-trim (buffer-substring-no-properties
                             (point)
                             (save-excursion (org-end-of-meta-data) (point)))))
            (pinned (cl-loop while (search-forward "#+begin_pin" nil t)
                             collect (save-excursion
                                       (beginning-of-line)
                                       (buffer-substring-no-properties
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

(defvar-local org-glance-material--encrypted nil
  "Non-nil when this materialized buffer's stored blob is encrypted.")
(defvar-local org-glance-material--password nil
  "Cached password of an encrypted materialized buffer, or nil when forgotten.")
(defvar-local org-glance-material--password-timer nil
  "Timer that forgets `org-glance-material--password' after the TTL.")

(cl-defun org-glance-material--clear-password ()
  "Forget the cached password and cancel its expiry timer."
  (setq-local org-glance-material--password nil)
  (when (timerp org-glance-material--password-timer)
    (cancel-timer org-glance-material--password-timer))
  (setq-local org-glance-material--password-timer nil))

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
  "Return the cached password, prompting (and re-arming the TTL) when forgotten."
  (or org-glance-material--password
      (progn (org-glance-material--set-password (read-passwd "Headline password: "))
             org-glance-material--password)))

(cl-defun org-glance-material--harden-buffer ()
  "Keep an encrypted buffer's plaintext off disk: no auto-save/backup/lockfile."
  (let ((asf buffer-auto-save-file-name))
    (auto-save-mode -1)
    (when (and asf (file-exists-p asf)) (ignore-errors (delete-file asf))))
  (setq-local buffer-auto-save-file-name nil
              backup-inhibited t
              create-lockfiles nil))

(cl-defun org-glance-material--encrypt-buffer ()
  "Seal the buffer's crypt blocks in place before the file is written to disk.
Buffer-local `before-save-hook'; prompts for the password if the TTL expired."
  (when org-glance-material--encrypted
    (let ((inhibit-read-only t))
      (org-glance--crypt-seal-blocks (org-glance-material--require-password)))))

(cl-defun org-glance-material--decrypt-buffer ()
  "Unseal the buffer's crypt blocks in place and clear the modified flag.
A legacy whole-body cipher is first wrapped in one block.  Buffer-local
`after-save-hook', run after `org-glance-material:sync' (invariant 27)."
  (when org-glance-material--encrypted
    (let ((inhibit-read-only t))
      (when (org-glance-headline--crypt-upgrade-legacy)
        (message "org-glance: upgraded to the crypt-block format"))
      (org-glance--crypt-unseal-blocks (org-glance-material--require-password))
      (set-buffer-modified-p nil))))

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
  "Mark the buffer encrypted, harden it and wire its crypt hooks; idempotent."
  (setq-local org-glance-material--encrypted t)
  (org-glance-material--harden-buffer)
  (add-hook 'before-save-hook #'org-glance-material--encrypt-buffer nil t)
  (add-hook 'after-save-hook #'org-glance-material--decrypt-buffer t t)
  (add-hook 'kill-buffer-hook #'org-glance-material--clear-password nil t))

(cl-defun org-glance-material--maybe-decrypt (meta buffer)
  "Prompt and decrypt BUFFER when META is encrypted and BUFFER still sealed.
Return non-nil when it decrypted; wire and harden the buffer first.  A wrong
password forgets it, kills BUFFER and re-signals."
  (with-current-buffer buffer
    (when (and (org-glance-headline-metadata:encrypted? meta)
               (org-glance-headline--buffer-encrypted?))
      (org-glance-material--wire-crypto)
      (org-glance-material--set-password (read-passwd "Headline password: "))
      (condition-case err
          (org-glance-material--decrypt-buffer)
        (error (org-glance-material--clear-password)
               (org-glance--discard-buffer buffer)
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
  (unless (org-glance-material--maybe-decrypt
           (org-glance-graph:live-meta org-glance-material--graph
                                       org-glance-material--id)
           (current-buffer))
    (message "org-glance: nothing sealed here")))

(cl-defun org-glance-material:seal ()
  "Seal this buffer's crypt blocks in place and forget the cached password.
Signal a `user-error' on an unencrypted or modified buffer (invariant 11)."
  (interactive)
  (org-glance-material--ensure)
  (unless org-glance-material--encrypted
    (user-error "Not an encrypted materialized buffer"))
  (when (buffer-modified-p)
    (user-error "Save this buffer before sealing it"))
  (org-glance-material--encrypt-buffer)
  (set-buffer-modified-p nil)               ; sealed == the bytes on disk
  (org-glance-material--clear-password)
  (message "org-glance: sealed -- `C-c #' unseals"))

(cl-defun org-glance-material:crypt-unwrap ()
  "Remove the crypt block around point; its body becomes public on save.
A sealed body decrypts first; the last unwrap drops encryption entirely."
  (interactive)
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
      (org-glance-material--clear-password)
      (message "org-glance: last crypt block unwrapped -- headline public on save"))))

(cl-defun org-glance-material:crypt ()
  "Run the crypt action for the context at point (`C-c #'); first match wins:
region -> wrap it; sealed buffer -> unseal; block at point -> unwrap;
decrypted buffer -> seal; plaintext -> wrap the whole body.  Sealed comes
before block: a sealed buffer's point sits inside its ciphertext block."
  (interactive)
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
Stored in `org-glance-project-dir-property' sans trailing slash; `C-u' clears."
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
  (save-excursion
    (org-glance-material--goto-first-heading)
    (if (org-glance--present-string? dir)
        (org-entry-put nil org-glance-project-dir-property
                       (directory-file-name dir))
      (org-entry-delete nil org-glance-project-dir-property)))
  (let ((inhibit-message t)) (save-buffer))
  (message "Project dir %s" (if (org-glance--present-string? dir) dir "cleared")))

(defcustom org-glance-material-hidden-properties org-glance-headline:hash-ignore-properties
  "Uppercase drawer property keys org-glance manages in material buffers.
Kept in the file only (invariant 21): removed at open, spliced into writes."
  :group 'org-glance
  :type '(repeat string))

(defvar-local org-glance-material--managed-keys nil
  "`org-glance-material-hidden-properties' as it stood when this buffer opened.
Fixed for the buffer's life; an option change applies to later buffers.")

(defvar-local org-glance-material--reserved-lines nil
  "Managed drawer lines removed from this buffer at open, verbatim.
`org-glance-material--reserved-annotations' writes them back on every save.")

(cl-defun org-glance-material--line-key (line)
  "Return the uppercase property key of drawer LINE, or nil."
  (when (string-match "^[ \t]*:\\([A-Za-z0-9_-]+\\):" line)
    (upcase (match-string 1 line))))

(cl-defun org-glance-material--reserved-line? ()
  "Non-nil when the line at point is a managed drawer property."
  (member (org-glance-material--line-key (thing-at-point 'line t))
          org-glance-material--managed-keys))

(cl-defun org-glance-material--remove-reserved-lines ()
  "Delete the managed lines of the heading's property drawer; return them.
Each line comes back verbatim, newline included, in drawer order.  A drawer
left with no property goes whole."
  (save-excursion
    (org-glance-material--goto-first-heading)
    (when-let* ((body (and org-glance-material--managed-keys
                           (org-at-heading-p)
                           (org-get-property-block))))
      (let ((end (copy-marker (cdr body)))
            lines)
        (goto-char (car body))
        (while (< (point) end)
          (if (org-glance-material--reserved-line?)
              (let ((beg (point)))
                (forward-line 1)
                (push (buffer-substring-no-properties beg (point)) lines)
                (delete-region beg (point)))
            (forward-line 1)))
        (when (and lines (= (car body) end))    ; emptied: drop the drawer
          (goto-char end)
          (delete-region (line-beginning-position 0)
                         (line-beginning-position 2)))
        (set-marker end nil)
        (nreverse lines)))))

(cl-defun org-glance-material--strip-reserved ()
  "Move the managed drawer lines out of the buffer into the stash.
Outside undo, and the modified flag is kept: the buffer is the file minus what
org-glance manages, which `--reserved-annotations' restores on write.
Buffer-local `after-revert-hook' too, since a revert reads them back in."
  (let ((buffer-undo-list t)
        (modified (buffer-modified-p)))
    (setq-local org-glance-material--reserved-lines
                (org-glance-material--remove-reserved-lines))
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
          (concat (unless (bolp) "\n") ":PROPERTIES:\n" lines ":END:\n"))))

(cl-defun org-glance-material--reserved-annotations (start _end)
  "Splice the stashed managed lines back into the bytes being written.
A `write-region-annotate-functions' member, so the buffer never changes.  START
is a string when the caller writes one; nothing is added then."
  (when (and org-glance-material--reserved-lines (not (stringp start)))
    (save-excursion
      (org-glance-material--goto-first-heading)
      (when (org-at-heading-p)
        (list (org-glance-material--drawer-splice
               (apply #'concat org-glance-material--reserved-lines)))))))

(cl-defun org-glance-material--dedupe-tags ()
  "Collapse case-twin heading tags to the downcased one, with a warning.
Buffer-local `before-save-hook'; a tag without a case-twin keeps its case."
  (when org-glance-material--id
    (save-excursion
      (org-glance-material--goto-first-heading)
      (let ((seen (make-hash-table :test 'equal))
            dups new)
        (dolist (tag (org-get-tags nil t))
          (let ((canon (downcase tag)))
            (cond ((not (gethash canon seen))
                   (puthash canon tag seen)
                   (push tag new))
                  (t (push canon dups)
                     (setq new (cl-substitute canon (gethash canon seen) new
                                              :test #'equal))
                     (puthash canon canon seen)))))
        (when dups
          (org-set-tags (nreverse new))
          (display-warning 'org-glance
                           (format "case-duplicate tag%s collapsed: %s"
                                   (if (cdr dups) "s" "")
                                   (s-join ", " (delete-dups dups)))))))))

(defvar revert-buffer-preserve-modes)  ; files.el: special only in its own file

(cl-defun org-glance-material--revert (ignore-auto noconfirm)
  "Revert this buffer without re-running its modes (`revert-buffer-function').
A mode re-run kills every buffer-local: the stash the next write restores
from, and the wiring that makes this a material buffer.  IGNORE-AUTO and
NOCONFIRM pass through to `revert-buffer--default'."
  (let ((revert-buffer-preserve-modes t))
    (revert-buffer--default ignore-auto noconfirm)))

(cl-defun org-glance-material--drop-hand-typed-reserved ()
  "Delete managed drawer lines the user typed, warning per line.
Buffer-local `before-save-hook': the stash is what `--reserved-annotations'
writes, so a typed line would land as a second copy or a fabricated value."
  (dolist (line (org-glance-material--remove-reserved-lines))
    (display-warning
     'org-glance
     (format "%s is managed by org-glance; your line was dropped"
             (org-glance-material--line-key line)))))

(cl-defun org-glance-material:open (graph id &key decrypt)
  "Open headline ID from GRAPH for editing as its content-blob file; return it.
Crypt blocks stay SEALED unless DECRYPT; an open material buffer for ID is
reused.  Signal a `user-error' when ID is dead or has no stored blob."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (let ((meta (org-glance-graph:live-meta graph id)))
    (unless meta
      (user-error "No live headline with id %s" id))
    (let ((path (org-glance-graph:content-path graph id)))
      (unless (f-exists? path)
        (user-error "No stored content for id %s" id))
      (when-let* ((existing (find-buffer-visiting path)))
        (when (equal id (buffer-local-value 'org-glance-material--id existing))
          (when decrypt (org-glance-material--maybe-decrypt meta existing))
          (cl-return-from org-glance-material:open existing)))
      (let* ((cycle (org-glance-tag-config:cycle-for-filter
                     graph (list :tags (append (org-glance-headline-metadata:tags meta) nil))))
             (buffer
             ;; Bound GLOBALLY: `find-file-noselect' reads the DEFAULT value.
             (let ((org-todo-keywords
                    (org-glance-tag-config:cycle->keywords-or cycle org-todo-keywords)))
               (find-file-noselect path))))
        (with-current-buffer buffer
          (rename-buffer (format "*org-glance: %s*" (org-glance-headline-metadata:title meta)) t)
          (setq-local org-glance-material--graph graph
                      org-glance-material--id id)
          ;; invariant 2: temp-then-rename; a crash never truncates the blob.
          (setq-local file-precious-flag t)
          ;; NEVER `setq-local' `org-todo-keywords': `sync' binds it globally.
          (setq-local org-glance-material--cycle cycle)
          (add-hook 'after-save-hook #'org-glance-material:sync nil t)
          (org-glance-material-mode 1)
          ;; invariant 21: managed keys live in the file, never in the buffer.
          (setq-local org-glance-material--managed-keys
                      org-glance-material-hidden-properties)
          (org-glance-material--strip-reserved)
          (add-hook 'write-region-annotate-functions
                    #'org-glance-material--reserved-annotations nil t)
          (setq-local revert-buffer-function #'org-glance-material--revert)
          (add-hook 'after-revert-hook #'org-glance-material--strip-reserved nil t)
          (add-hook 'before-save-hook #'org-glance-material--drop-hand-typed-reserved nil t)
          (add-hook 'before-save-hook #'org-glance-material--dedupe-tags nil t)
          (when decrypt (org-glance-material--maybe-decrypt meta buffer)))
        buffer))))

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

(cl-defun org-glance-material:change-todo-live (graph id arg finalize)
  "Advance ID's TODO state in GRAPH via `org-todo' with prefix ARG; persist it.
A buffer this opens is saved once the change commits (after any note), then
killed, and FINALIZE runs on the new state in the origin buffer.  An open
material buffer is edited in place; the user saves it."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (let* ((path (org-glance-graph:content-path graph id))
         (fresh (null (get-file-buffer path)))
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
               (when (buffer-live-p buf) (kill-buffer buf))
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
                     ((buffer-modified-p)
                      (setq owned t commit-now t)))))
                (when commit-now (finish (persist))))
            (unless owned (org-glance--discard-buffer buf))))))))

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
           (unless existing (when (buffer-live-p buf) (kill-buffer buf))))
         (resume (buf existing id)          ; run after an interactive note settles
           (with-current-buffer buf (save-buffer))
           (push id changed)
           ;; Kill + advance OFF `org-store-log-note's extent (its window restore).
           (run-at-time 0 nil (lambda () (kill-fresh buf existing) (drive))))
         (change-row (id)                   ; -> t when it SUSPENDS on a note, else nil
           (let* ((path (org-glance-graph:content-path graph id))
                  (existing (get-file-buffer path)))
             (cond
              ((and existing (buffer-modified-p existing))
               (push (cons id "unsaved changes") skipped) nil)
              (t
               (let ((buf (ignore-errors (org-glance-material:open graph id)))
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
                            (t (when (buffer-modified-p) (save-buffer))
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
  (let* ((path (org-glance-graph:content-path graph id))
         (existing (org-glance-material--assert-blob-clean path)))
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
        (unless existing (org-glance--discard-buffer buffer)))
      changed)))

(cl-defun org-glance-material--assert-blob-clean (path)
  "Signal a `user-error' when PATH's visiting buffer has unsaved edits.
Return the visiting buffer, or nil."
  (let ((existing (find-buffer-visiting path)))
    (when (and existing (buffer-modified-p existing))
      (user-error "org-glance: %s has unsaved edits"
                  (file-name-nondirectory path)))
    existing))

(cl-defun org-glance-material--replace-headline (graph id transform)
  "Replace headline ID in GRAPH with (TRANSFORM headline); return the new one.
Signal a `user-error' when the blob has unsaved edits or ID is dead; an erring
TRANSFORM aborts before any write.  Discard the now-stale open buffer."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (let* ((path (org-glance-graph:content-path graph id))
         (existing (org-glance-material--assert-blob-clean path))
         (headline (org-glance-graph:headline graph id)))
    (unless headline (user-error "No live headline with id %s" id))
    (let ((new (funcall transform headline)))
      (org-glance-graph:add graph new)
      (when existing (org-glance--discard-buffer existing))
      new)))

(cl-defun org-glance-material:delete (graph id)
  "Tombstone headline ID in GRAPH after confirmation; return t when deleted.
The prompt names referrers and unsaved edits; ID's open buffer is discarded
and views flagged stale."
  (let* ((title (org-glance-graph:title-or-id graph id))
         (buf (find-buffer-visiting (org-glance-graph:content-path graph id)))
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
           (if (and buf (buffer-modified-p buf))
               " (its open buffer has UNSAVED edits, which will be discarded)"
             "")
           " ")))
    (when (yes-or-no-p prompt)
      ;; invariant 11: tombstone FIRST; a failed append keeps the edits.
      (org-glance-graph:delete graph id)
      (when buf (org-glance--discard-buffer buf))
      (org-glance-view:mark-graph-stale graph)
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
       (org-glance-headline:decrypt headline password t))))
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
     (org-glance-headline:encrypt (org-glance-headline:decrypt headline old) new)))
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
    (org-glance-graph:headline graph (org-glance-headline-metadata:id metadata))))

;;;###autoload
(cl-defun org-glance-open ()
  "Choose a headline from the graph and open a link inside it."
  (interactive)
  (org-glance-material:open-link
   (org-glance-material--pick-headline "Open: " #'org-glance-headline-metadata:linked?)))

(cl-defun org-glance-material:extract-pairs (pairs &optional key)
  "Copy the value of KEY in alist PAIRS to the kill ring; return it.
Nil KEY prompts for one.  Signal a `user-error' on empty PAIRS."
  (unless pairs (user-error "No key-value pairs in headline"))
  (let* ((key (or key (completing-read "Extract: " pairs nil t)))
         (value (alist-get key pairs nil nil #'string=)))
    (kill-new value)
    (message "Copied: %s" value)
    value))

(cl-defun org-glance-material:extract (headline &optional key)
  "Copy the body `KEY: value' of HEADLINE to the kill ring; return the value.
Nil KEY prompts for one."
  (cl-check-type headline org-glance-headline)
  (org-glance-material:extract-pairs (org-glance-headline:properties headline) key))

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
Reads the live buffer, decrypted crypt blocks included."
  (interactive)
  (org-glance-material--ensure)
  (org-glance-material:extract-pairs
   (org-glance-headline:with-contents
       (buffer-substring-no-properties (point-min) (point-max))
     (org-glance-headline--buffer-properties))))

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
