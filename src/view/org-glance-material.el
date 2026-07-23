;; -*- lexical-binding: t -*-

;;; org-glance-material.el --- graph-backed selection + materialize/sync

;;; Commentary:
;; Command layer over the graph.  Selection lists live headlines from the
;; graph; materialize opens a headline's content blob in an editable buffer;
;; saving writes it back as a NEW version (the store is append-only,
;; last-write-wins), guarded by a hash check against concurrent changes.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'ol)
(require 's)

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
  "Completing-read label for headline METADATA: \"[tags] title\"."
  (cl-check-type metadata org-glance-headline-metadata)
  (let ((tags (org-glance-headline-metadata:tag-strings metadata)))
    (concat (if tags (format "[%s] " (s-join "," tags)) "")
            (org-glance--title-clean (org-glance-headline-metadata:title metadata)))))

(cl-defun org-glance-material:completing-read (graph &key (prompt "Headline: ") filter)
  "Choose a live headline from GRAPH and return its metadata.
FILTER, if non-nil, is a predicate on the metadata."
  (cl-check-type graph org-glance-graph)
  ;; FILTER often calls `active?'/`done?', which read the buffer-local
  ;; `org-done-keywords' -- nil in this command/minibuffer context.  Bind the
  ;; user's done set so the filter is correct regardless of the current buffer.
  (let* ((org-done-keywords (org-glance--done-keywords))
         (metas (cl-loop for meta in (org-glance-graph:headlines graph)
                         when (or (null filter) (funcall filter meta))
                         collect meta))
         ;; Duplicate labels get a short-id suffix so the pick is injective --
         ;; resolving a collision to the FIRST metadata would silently act on
         ;; the wrong headline.
         (counts (-frequencies (mapcar #'org-glance-material:label metas)))
         (candidates (mapcar (lambda (meta)
                               (let ((label (org-glance-material:label meta)))
                                 (cons (if (> (alist-get label counts 0 nil #'equal) 1)
                                           (format "%s ·%s" label
                                                   (s-left 8 (org-glance-headline-metadata:id meta)))
                                         label)
                                       meta)))
                             metas)))
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
;;
;; A materialized headline is its content blob (`…/data/<id>/data.org`) opened as
;; a REAL file.  Saving is therefore an ordinary file save (works under any
;; config -- evil, super-save, whatever binds the save command), and a
;; buffer-local `after-save-hook' refreshes the graph metadata index from the
;; saved content.  Emacs's own "file changed on disk" handling covers concurrent
;; edits, so there is no custom conflict prompt.

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Keymap for `org-glance-material-mode'.")

(define-minor-mode org-glance-material-mode
  "Minor mode for a materialized headline buffer."
  :lighter " glance"
  :global nil
  :group 'org-glance
  :keymap org-glance-material-mode-map
  (when org-glance-material-mode
    ;; org requires tab-width 8; no tabs.
    (setq tab-width 8 indent-tabs-mode nil)
    ;; Advance only the earliest of multiple repeatable timestamps on repeat.
    (org-glance-datetime-mode 1)))

;; `C-c #' by context: region -> wrap it in a crypt block; `C-u' -> unwrap the
;; block at point; else encrypt the whole body (plaintext buffer) or forget
;; the cached password (already encrypted).
(define-key org-glance-material-mode-map (kbd "C-c #") #'org-glance-material:crypt)
;; `C-c d': set/clear the headline's project directory (`org-glance-llm' uses it).
(define-key org-glance-material-mode-map (kbd "C-c d") #'org-glance-material:set-project-dir)
;; `C-c e': copy a body `KEY: value' from this headline (the views' `e').
(define-key org-glance-material-mode-map (kbd "C-c e") #'org-glance-material:extract-here)
;; `C-c i': set the date interval (<from>--<to> body range); `C-u' removes it.
(define-key org-glance-material-mode-map (kbd "C-c i") #'org-glance-material:set-interval)
;; `@' at a word boundary (body or heading title) references another headline
;; (C-u adds a kind); at column 0 of a heading and mid-word it self-inserts.  `C-c @' views references, `C-u C-c @' back-references.
(define-key org-glance-material-mode-map (kbd "@") #'org-glance-material:refer)
(define-key org-glance-material-mode-map (kbd "C-c @") #'org-glance-material:references)

(defconst org-glance-project-dir-property "ORG_GLANCE_PROJECT_DIR"
  "Drawer property naming a headline's project directory.
`org-glance-llm' opens its session there instead of the content-addressable
data dir; set it with `org-glance-material:set-project-dir' (`C-c d').")

(defvar-local org-glance-material--graph nil
  "Graph backing the current materialized buffer.")

(cl-defun org-glance-material--ensure ()
  "Signal a `user-error' unless the current buffer is a materialized headline."
  (unless (and org-glance-material--graph org-glance-material--id)
    (user-error "Not in a materialized headline buffer")))

(defvar-local org-glance-material--id nil
  "ORG_GLANCE_ID of the headline materialized in the current buffer.")

(defvar-local org-glance-material--cycle nil
  "Per-tag `#+TODO:'-style cycle string for this buffer's headline, or nil.
Used by `org-glance-material:sync' to re-parse the saved buffer with the tag's
own keywords in scope (so a state like READING is not folded into the title).")

(cl-defun org-glance-material:sync ()
  "Refresh the graph metadata index from the just-saved materialized blob.
Buffer-local `after-save-hook': the file save already persisted the content
(the durable commit), so this only re-parses the headline and appends a fresh
metadata record to the append-only WAL (`headlines.jsonl').  That is the whole
hot path -- open views are NOT rewritten here; the WAL append makes them stale,
and they re-render lazily when next displayed (see `org-glance-view').  Each
open view of the graph is merely FLAGGED stale (a cheap boolean) so its
`glance:stale' lighter shows until it refreshes.  No-op if the buffer's
ORG_GLANCE_ID was changed."
  (when (and org-glance-material--graph org-glance-material--id)
    (let* ((graph org-glance-material--graph)
           (id org-glance-material--id)
           ;; Re-parse with the tag's todo cycle GLOBALLY bound, so it reaches
           ;; `--from-string's internal temp buffer and a state like READING is
           ;; recognised instead of folding into the title.  `org-todo-keywords' is
           ;; not buffer-local here (see `material:open'), so this `let' binds the
           ;; global value the temp buffer's `org-mode' reads.
           (headline (let ((org-todo-keywords
                            (org-glance-tag-config:cycle->keywords-or
                             org-glance-material--cycle org-todo-keywords)))
                       (org-glance-headline--from-string
                        (buffer-substring-no-properties (point-min) (point-max))))))
      (if (equal (org-glance-headline:id headline) id)
          (progn
            (org-glance-graph:insert graph (list (org-glance-headline:metadata headline)))
            (org-glance-view:mark-graph-stale graph))
        (message "org-glance: ORG_GLANCE_ID changed (expected %s); metadata not updated" id)))))

;;; Repeated headlines: occurrence history
;;
;; A repeating headline stays ONE graph entity.  Completing a repetition
;; snapshots the done state VERBATIM into `data/<id>/occurrences/<STAMP>.org'
;; (immutable files, newest `org-glance-repeat-history-depth' kept), then the
;; live headline is trimmed to its header + pinned blocks.  Pick a snapshot
;; with `l' (table/overview) or `C-c l' (here).  See
;; docs/proposals/2026-07-18-repeat-occurrences.done.org.

(defcustom org-glance-repeat-history-depth 0
  "How many completed occurrences to keep per repeating headline.
0 disables occurrence history (and the after-repeat trim); N > 0 keeps the
newest N snapshots under the headline's `occurrences/' dir, pruning older
ones on each completion (syncthing-style); t keeps them ALL."
  :group 'org-glance
  :type '(choice (natnum :tag "Keep newest N (0 disables)")
                 (const :tag "Unlimited" t)))

(defconst org-glance-repeat-history-depth-property "ORG_GLANCE_REPEAT_HISTORY_DEPTH"
  "Drawer property overriding `org-glance-repeat-history-depth' per headline.
An integer (0 disables) or t/inf/unlimited (all aliases for unlimited);
unparseable values read as 0, floats truncate.")

(cl-defun org-glance-material--property (key)
  "Drawer property KEY at this materialized buffer's heading, or nil."
  (save-excursion
    (org-glance-material--goto-first-heading)
    (org-entry-get (point) key)))

(cl-defun org-glance-material--history-depth ()
  "Effective repeat-history depth for this buffer's headline.
The `ORG_GLANCE_REPEAT_HISTORY_DEPTH' drawer property wins over the global
`org-glance-repeat-history-depth'; t/inf mean unlimited."
  (if-let ((v (org-glance-material--property org-glance-repeat-history-depth-property)))
      (pcase (downcase (s-trim v))
        ((or "t" "inf" "unlimited") t)
        (n (truncate (string-to-number n))))   ; junk -> 0 -> disabled; 3.5 -> 3
    org-glance-repeat-history-depth))

(defvar-local org-glance-material--snapshotted nil
  "Non-nil when `snapshot-on-repeat' actually wrote an occurrence file.
The SINGLE owner of the \"this repeat preserved history\" decision:
`cleanup-after-repeat' trims iff it consumes this flag, so the two advice
gates can never drift (a failed/skipped snapshot never loses the body).")

(cl-defun org-glance-material--occurrence-stamp (ts)
  "Filename stamp for occurrence timestamp TS (an org-element), or now if nil.
Lexically sortable; a same-occurrence re-completion maps to the same stamp and
overwrites (idempotent)."
  (format-time-string
   "%Y-%m-%dT%H%M"
   (if ts (org-time-string-to-time (org-element-property :raw-value ts))
     (current-time))))

(cl-defun org-glance-material:snapshot-on-repeat (&rest _)
  "Preserve the completed repetition as an occurrence snapshot.
Runs `:before' `org-auto-repeat-maybe' (the headline is still in its done
state, timestamps not yet advanced); gated by the effective depth
(`org-glance-material--history-depth' -- the headline's own property wins).
Sets `--snapshotted' only on a successful write -- the trim consumes that
flag."
  (setq org-glance-material--snapshotted nil)
  (when-let* ((depth (and org-glance-material-mode
                          org-glance-material--graph
                          (org-glance-material--history-depth)))
              (ts (and (or (eq depth t) (> depth 0))
                       (member (org-get-todo-state) org-done-keywords)
                       ;; ONE subtree parse: the earliest repeated timestamp both
                       ;; gates the snapshot and names it.
                       (car (org-glance-datetime-active-repeated-timestamps
                             'include-schedules 'include-deadlines)))))
    ;; This buffer is DECRYPTED plaintext; snapshotting it would write
    ;; plaintext history for an encrypted headline (invariant 14).  Skip.
    (if org-glance-material--encrypted
        (message "org-glance: encrypted headline keeps no occurrence history")
      (with-demoted-errors "org-glance: occurrence snapshot failed: %S"   ; inv 9
        (let* ((graph org-glance-material--graph)
               (id org-glance-material--id)
               (dir (org-glance-graph:occurrences-path graph id)))
          (f-mkdir-full-path dir)
          ;; temp-then-rename like every store write (invariant 2)
          (org-glance-graph--atomic-write
           graph (f-join dir (concat (org-glance-material--occurrence-stamp ts) ".org"))
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

(define-key org-glance-material-mode-map (kbd "C-c l") #'org-glance-material:history)

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

;; Idempotent on reload: `advice-add' is a no-op for an already-added function.
(advice-add 'org-auto-repeat-maybe :before #'org-glance-material:snapshot-on-repeat '((depth . -90)))
(advice-add 'org-auto-repeat-maybe :after #'org-glance-material:cleanup-after-repeat)

;;; Encrypted headlines: decrypt on open, re-encrypt on save
;;
;; A materialized blob IS `data.org' (the store's canonical file), so plaintext
;; must never touch disk.  The password prompted on open is cached buffer-local
;; (with a TTL, `org-glance-material-password-ttl'); `before-save-hook' encrypts
;; the body so the file is written as ciphertext, then `after-save-hook' (after
;; `sync' has re-indexed the ciphertext) decrypts the body back for editing.  The
;; buffer holds plaintext only in memory, and auto-save/backup/lockfiles are
;; disabled so that plaintext cannot leak to `#data.org#' / `data.org~' / lock
;; files.  SECURITY NOTE: the decrypted body and the cached password still live in
;; process memory (and the undo list) for the buffer's lifetime; moving to
;; gpg-agent removes that -- see the GPG-migration proposal.

(defcustom org-glance-material-password-ttl 300
  "Seconds an encrypted buffer caches its password before forgetting it.
After the TTL the next save re-prompts.  0 keeps it for the buffer's lifetime.
Forget it early with `org-glance-material:lock'."
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
  "Open the buffer's ciphertext in place for editing.
A legacy whole-body cipher is first wrapped in one crypt block
(`org-glance-headline--crypt-upgrade-legacy'; the disk upgrades on the next
save), then every sealed block is unsealed.  Clears the modified flag: the
plaintext buffer matches the ciphertext already on disk.  Buffer-local
`after-save-hook' (runs after `org-glance-material:sync')."
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
  "Delete ID's occurrence snapshots: PLAINTEXT copies of now-secret content.
Called on BOTH encrypt paths -- `crypt-set' and the first in-buffer crypt
block (invariant 14).  Idempotent."
  (let ((dir (org-glance-graph:occurrences-path graph id)))
    (when (f-exists? dir)
      (f-delete dir t)
      (message "org-glance: plaintext occurrence history removed (headline is now encrypted)"))))

(cl-defun org-glance-material--wire-crypto ()
  "Mark the buffer encrypted: harden it and wire the seal/unseal round-trip.
Idempotent -- `add-hook' deduplicates and hardening re-runs harmlessly."
  (setq-local org-glance-material--encrypted t)
  (org-glance-material--harden-buffer)
  (add-hook 'before-save-hook #'org-glance-material--encrypt-buffer nil t)
  (add-hook 'after-save-hook #'org-glance-material--decrypt-buffer t t)
  (add-hook 'kill-buffer-hook #'org-glance-material--clear-password nil t))

(cl-defun org-glance-material--maybe-decrypt (meta buffer)
  "When META is encrypted, harden BUFFER, prompt for the password, and decrypt it.
Caches the password (with TTL) and wires the save-time re-encrypt round-trip.
A wrong password forgets it, kills BUFFER and re-signals, so `open' fails clean."
  (when (org-glance-headline-metadata:encrypted? meta)
    (org-glance-material--wire-crypto)
    (org-glance-material--set-password (read-passwd "Headline password: "))
    (condition-case err
        (org-glance-material--decrypt-buffer)
      (error (org-glance-material--clear-password)
             (org-glance--discard-buffer buffer)
             (signal (car err) (cdr err))))))

(cl-defun org-glance-material:crypt-region (beg end)
  "Wrap BEG..END in a `#+begin_crypt' block; it seals on the next save.
The first block in a plaintext buffer prompts for a password (confirmed) and
wires the seal/unseal round-trip.  The region must lie inside the body."
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
    ;; the headline just became secret: purge its plaintext occurrence history
    (org-glance-material--purge-occurrences org-glance-material--graph
                                            org-glance-material--id))
  (deactivate-mark)
  (message "org-glance: region wrapped -- seals on save"))

(cl-defun org-glance-material:crypt-unwrap ()
  "Remove the crypt block around point; its body becomes public on save.
A still-sealed body is decrypted first.  Unwrapping the last block makes the
whole headline public: the buffer stops sealing and forgets its password."
  (interactive)
  (let ((block (org-glance--crypt-block-at (point))))
    (unless block (user-error "Point is not inside a crypt block"))
    (when (org-glance--crypt-sealed? block)
      (org-glance--crypt-unseal-blocks (org-glance-material--require-password))
      (setq block (org-glance--crypt-block-at (point))))
    (org-glance--crypt-unwrap-block block)
    (when (and org-glance-material--encrypted
               (null (org-glance--crypt-block-regions)))
      (setq-local org-glance-material--encrypted nil)
      (org-glance-material--clear-password)
      (message "org-glance: last crypt block unwrapped -- headline public on save"))))

(cl-defun org-glance-material:crypt ()
  "Crypt action at point, by context.
Active region -> wrap it in a crypt block (`org-glance-material:crypt-region').
Prefix arg -> unwrap the block at point (`org-glance-material:crypt-unwrap').
No region, plaintext buffer -> encrypt the WHOLE body (one block).
No region, already encrypted -> forget the cached password
\(`org-glance-material:lock')."
  (interactive)
  (cond
   ((use-region-p)
    (org-glance-material:crypt-region (region-beginning) (region-end)))
   (current-prefix-arg (org-glance-material:crypt-unwrap))
   (org-glance-material--encrypted (org-glance-material:lock))
   (t (pcase-let ((`(,beg . ,end) (org-glance-headline--body-region)))
        (org-glance-material:crypt-region beg end)))))

(cl-defun org-glance-material:set-project-dir (dir)
  "Set the materialized headline's project directory (`C-c d') to DIR and save.
DIR is stored in the `ORG_GLANCE_PROJECT_DIR' drawer property, where
`org-glance-llm' opens its session.  With a prefix arg, clear the property."
  (interactive
   (list (unless current-prefix-arg
           ;; Directory-valued throughout: the stored default AND the stored
           ;; result keep a trailing "/", so the minibuffer opens INSIDE the
           ;; directory (completing its contents) instead of offering the
           ;; directory itself as the pending selection.
           (file-name-as-directory
            (expand-file-name
             (read-directory-name
              "Project dir: "
              (if-let ((cur (org-glance-material--property
                             org-glance-project-dir-property)))
                  (file-name-as-directory cur)
                "./")))))))
  (org-glance-material--ensure)
  (save-excursion
    (org-glance-material--goto-first-heading)
    (if (org-glance--present-string? dir)
        (org-entry-put nil org-glance-project-dir-property dir)
      (org-entry-delete nil org-glance-project-dir-property)))
  (let ((inhibit-message t)) (save-buffer))
  (message "Project dir %s" (if (org-glance--present-string? dir) dir "cleared")))

(defcustom org-glance-material-hidden-properties org-glance-headline:hash-ignore-properties
  "Drawer property KEYS (uppercase) that org-glance MANAGES in material buffers.
Two enforcements share this list: the lines are CONCEALED (overlays only --
the file and store keep them; a drawer whose every property is listed hides
entirely), and hand edits to them are REVERTED on save with a warning
\(`org-glance-material--restore-reserved').  nil disables both."
  :group 'org-glance
  :type '(repeat string))

(defvar-local org-glance-material--reserved-values nil
  "Snapshot ((KEY . VALUE-or-nil)…) of the reserved properties taken at open.
`--restore-reserved' reverts any hand edit to these on save -- the keys are
managed by org-glance, concealed in the buffer, and not the user's to change.")

(cl-defun org-glance-material--reserved-snapshot ()
  "Current values of the reserved properties at the buffer's heading."
  (mapcar (lambda (key) (cons key (org-glance-material--property key)))
          org-glance-material-hidden-properties))

(cl-defun org-glance-material--dedupe-tags ()
  "Collapse case-duplicate heading tags to the canonical downcased one.
Buffer-local `before-save-hook': \":Food:food:\" becomes \":food:\" (with
a warning); a tag without a case-twin keeps its case."
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

(cl-defun org-glance-material--restore-reserved ()
  "Revert hand edits to reserved properties, warning per reverted KEY.
Buffer-local `before-save-hook', so the restored value is what lands on disk
and what `org-glance-material:sync' then reads."
  (when org-glance-material--reserved-values
    (save-excursion
      (org-glance-material--goto-first-heading)
      (pcase-dolist (`(,key . ,original) org-glance-material--reserved-values)
        (unless (equal original (org-entry-get (point) key))
          (if original
              (org-entry-put (point) key original)
            (org-delete-property key))
          (display-warning
           'org-glance
           (format "%s is managed by org-glance; your edit was reverted" key)))))))

(cl-defun org-glance-material--hide-reserved-properties (&rest _)
  "Conceal `org-glance-material-hidden-properties' lines via overlays.
Idempotent (old overlays dropped first); re-run from `after-save-hook' because
a crypt reseal/unseal rewrites the drawer region and kills its overlays."
  (remove-overlays (point-min) (point-max) 'org-glance-reserved t)
  (when org-glance-material-hidden-properties
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-property-start-re nil t)
        (let ((drawer-beg (line-beginning-position))
              (hidden nil)                ; (BEG . END) per concealed line
              (total 0)
              (done nil)
              drawer-end)
          (while (and (not done) (zerop (forward-line 1)) (not (eobp)))
            (cond
             ((looking-at org-property-end-re)
              (setq drawer-end (min (point-max) (1+ (line-end-position)))
                    done t))
             ((looking-at "^[ \t]*:\\([A-Za-z0-9_-]+\\):")
              (cl-incf total)
              (when (member (upcase (match-string 1))
                            org-glance-material-hidden-properties)
                (push (cons (line-beginning-position)
                            (min (point-max) (1+ (line-end-position))))
                      hidden)))
             (t (setq done t))))               ; malformed drawer: no :END: seen
          (dolist (region (if (and drawer-end (= total (length hidden)) hidden)
                              (list (cons drawer-beg drawer-end)) ; all concealed
                            hidden))
            (let ((ov (make-overlay (car region) (cdr region))))
              (overlay-put ov 'invisible t)
              (overlay-put ov 'evaporate t)
              (overlay-put ov 'org-glance-reserved t))))))))

(cl-defun org-glance-material:open (graph id)
  "Open headline ID from GRAPH for editing, as its content-blob file.
Return the buffer.  A live, already-wired material buffer for ID is returned
as-is -- no re-setup, and (for an encrypted headline) no password re-prompt:
its plaintext is already on screen.  Errors if ID is unknown, tombstoned, or
has no stored blob."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (let ((meta (org-glance-graph:get-headline graph id)))
    (unless (org-glance-headline-metadata? meta)
      (user-error "No live headline with id %s" id))
    (let ((path (org-glance-graph:content-path graph id)))
      (unless (f-exists? path)
        (user-error "No stored content for id %s" id))
      (when-let ((existing (find-buffer-visiting path)))
        (when (equal id (buffer-local-value 'org-glance-material--id existing))
          (cl-return-from org-glance-material:open existing)))
      ;; Fresh open only: the tag's todo cycle (a stat + possible tag-config
      ;; parse) is pure waste on the reuse path above.
      (let* ((cycle (org-glance-tag-config:cycle-for-filter
                     graph (list :tags (append (org-glance-headline-metadata:tags meta) nil))))
             (buffer
             ;; Initialize the buffer's `org-mode' with the tag's TODO cycle bound
             ;; GLOBALLY: `org-set-regexps-and-options' (run inside `find-file-noselect')
             ;; reads the DEFAULT value of `org-todo-keywords', not a buffer-local one
             ;; (the W1 finding), so this is what makes the buffer's derived keyword vars
             ;; -- and hence native rendering, cycling and `org-todo' -- know the tag's
             ;; states (e.g. READING), WITHOUT a `#+TODO:' in the kept-clean blob.
             (let ((org-todo-keywords
                    (org-glance-tag-config:cycle->keywords-or cycle org-todo-keywords)))
               (find-file-noselect path))))
        (with-current-buffer buffer
          (rename-buffer (format "*org-glance: %s*" (org-glance-headline-metadata:title meta)) t)
          (setq-local org-glance-material--graph graph
                      org-glance-material--id id)
          ;; Save atomically: write a temp file then rename it over data.org, so a
          ;; crash mid-save can never truncate the blob (the canonical materialized
          ;; file).  Mirrors the temp-then-rename in `org-glance-graph:put-content'.
          (setq-local file-precious-flag t)
          ;; A configured tag's todo cycle is NOT stored in the blob (kept clean);
          ;; stash it so `org-glance-material:sync' can re-parse the save with the
          ;; tag's own keywords in scope (else a state like READING folds into the
          ;; title).  NB do NOT `setq-local' `org-todo-keywords' here -- that would
          ;; make it buffer-local, and sync's dynamic `let' must bind it GLOBALLY to
          ;; reach `--from-string's internal temp buffer.
          (setq-local org-glance-material--cycle cycle)
          (add-hook 'after-save-hook #'org-glance-material:sync nil t)
          (org-glance-material-mode 1)
          (org-glance-material--maybe-decrypt meta buffer)
          ;; Conceal bookkeeping drawer lines; re-conceal after every save
          ;; (depth 100: after the crypt unseal hook has restored plaintext).
          (org-glance-material--hide-reserved-properties)
          (add-hook 'after-save-hook #'org-glance-material--hide-reserved-properties 100 t)
          ;; Reserved properties are managed: snapshot the originals and revert
          ;; hand edits on save (with a warning), before the file is written.
          ;; Order vs the crypt seal is immaterial: restore edits only the
          ;; heading drawer, the seal only body crypt blocks -- disjoint regions.
          (setq-local org-glance-material--reserved-values
                      (org-glance-material--reserved-snapshot))
          (add-hook 'before-save-hook #'org-glance-material--restore-reserved nil t)
          ;; Case-duplicate tags (:Food:food:) collapse to the canonical
          ;; downcased one before the file is written (invariant 13).
          (add-hook 'before-save-hook #'org-glance-material--dedupe-tags nil t))
        buffer))))

;;; TODO state change: exactly `C-c C-t' on the headline (materialize -> sync)
;;
;; A read-only view (overview / table) advances a headline's state with byte-exact
;; org fidelity by MATERIALIZING it and running `org-todo' in the real blob buffer:
;; per-tag keywords are live (see `org-glance-material:open'), so state cycling, the
;; CLOSED timestamp, `org-after-todo-state-change-hook', dependency blocking,
;; repeaters (snapshot-on-repeat) and the interactive LOGBOOK note all happen natively.
;; Persistence reuses the ordinary save -> `after-save-hook' -> `:sync' path (atomic
;; blob + WAL + mark-stale); there is no separate write.
;;
;; The wrinkle is the note.  `org-todo' applies the state + CLOSED synchronously but
;; DEFERS an interactive note (`org-log-done' `note', repeat-notes, `C-u' force-log)
;; to `*Org Note*'.  So we save immediately when no note is queued (signalled by
;; `org-log-setup', read in the same stack frame), else save from a one-shot `:after'
;; advice on `org-store-log-note' -- which fires on BOTH commit (`C-c C-c') and abort
;; (`C-c C-k'); on abort the state + CLOSED are kept, matching native `C-c C-t'.  A
;; FRESH background buffer is killed and the origin view refreshed once committed; a
;; PRE-EXISTING materialized buffer is edited in place and left for the user to save
;; (we never flush their unsaved edits).

(defvar org-log-setup)         ; org.el: non-nil while an interactive note is queued
(defvar org-log-note-how)      ; org.el: `note' (prompt) vs `time'/`state' (timestamp)
(defvar org-log-note-this-command) ; org.el: command that queued the note
(declare-function org-add-log-note "org" (&optional purpose))

(cl-defun org-glance-material--goto-first-heading ()
  "Move point to the first heading of the current buffer."
  (goto-char (point-min))
  (unless (org-at-heading-p) (outline-next-heading)))

(cl-defun org-glance-material:change-todo-live (graph id arg finalize)
  "Advance ID's TODO state exactly like `C-c C-t' on the headline, then persist.
Materialize ID (its real blob buffer, per-tag keywords live), run `org-todo'
with ARG as the prefix, and let org apply the state + CLOSED + repeater + any
note natively.  When the change commits (immediately, or after an interactive
note settles), save the buffer -- its `after-save-hook' (`:sync') persists
(atomic blob + WAL) and flags views -- then, for a buffer we opened,
kill it and run FINALIZE (a one-arg thunk of the new state) in the ORIGIN view.
A pre-existing materialized buffer is edited in place; the user saves it."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (let* ((path (org-glance-graph:content-path graph id))
         (fresh (null (get-file-buffer path)))
         (origin (current-buffer))
         (buf (org-glance-material:open graph id)))  ; user-errors if not live
    (if (not fresh)
        ;; Already materialized: act in place, the user drives the save.
        (progn
          (switch-to-buffer buf)
          (org-glance-material--goto-first-heading)
          (let ((current-prefix-arg arg)) (call-interactively #'org-todo)))
      ;; Fresh background buffer: run org-todo, then commit (now or after the note).
      (let ((owned nil) (commit-now nil))
        (cl-labels
            ((finish (state)
               (when (buffer-live-p buf) (kill-buffer buf))
               (when (buffer-live-p origin)
                 (with-current-buffer origin (funcall finalize state))))
             ;; Save persists via the buffer's `after-save-hook' (`:sync'); return
             ;; the new state string.
             (persist ()
               (with-current-buffer buf
                 (save-buffer)
                 (substring-no-properties (or (org-get-todo-state) "")))))
          (unwind-protect
              (progn
                (with-current-buffer buf
                  (org-glance-material--goto-first-heading)
                  (let ((current-prefix-arg arg) (org-log-setup nil))
                    ;; State + CLOSED + hooks + repeater are applied here; only an
                    ;; interactive note is deferred (signalled by `org-log-setup').
                    (call-interactively #'org-todo)
                    (cond
                     (org-log-setup
                      ;; Note pending -> persist + finish after it settles;
                      ;; DEFER the kill/refresh off org's extent.
                      (org-glance-material--on-next-log-note
                       (lambda ()
                         (let ((state (persist)))
                           (run-at-time 0 nil (lambda () (finish state))))))
                      (setq owned t))
                     ((buffer-modified-p)
                      (setq owned t commit-now t)))))
                ;; No note: commit synchronously, now OUTSIDE the buffer edit (so
                ;; killing it is safe).
                (when commit-now (finish (persist))))
            ;; org-todo threw (e.g. a dependency block) before ownership passed to
            ;; the note advice / commit -> don't leak the background buffer.
            (unless owned (org-glance--discard-buffer buf))))))))

(cl-defun org-glance-material--on-next-log-note (continuation)
  "Run CONTINUATION once the pending log note settles (commit OR abort).
One-shot self-removing `:after' advice on `org-store-log-note'; return the
advice function so a caller that ends up not suspending can remove it."
  (letrec ((adv (lambda (&rest _)
                  (advice-remove 'org-store-log-note adv)
                  (funcall continuation))))
    (advice-add 'org-store-log-note :after adv)
    adv))

(cl-defun org-glance-material:set-todo-bulk (graph ids state finalize)
  "Set every id in IDS to TODO STATE with full `C-c C-t' logging, then FINALIZE.
Rows are processed ONE AT A TIME because org's note machinery uses a single
global marker per command: batching would record only the last row's log.  For
each id: materialize it (per-tag keywords live) and `org-todo' to STATE -- org
applies the state, CLOSED, any repeater and the state log natively -- then
  * a timestamp log is flushed synchronously (org-agenda's own pattern), so the
    LOGBOOK entry is recorded inline before the buffer is saved and killed;
  * an interactive note pops up `*Org Note*'; the next row waits until it
    settles (commit OR abort), so notes are taken one prompt per row.
Nothing is discarded.  The buffer we opened is saved (its `after-save-hook',
`:sync', persists + flags views) and killed; a PRE-EXISTING buffer with unsaved
edits is left untouched (skipped) so bulk never clobbers live work; a row whose
cycle rejects STATE, or is no longer live, is skipped.  FINALIZE runs in the
ORIGIN buffer once every row settles, with (CHANGED SKIPPED): the ids set and
the (id . reason) pairs skipped."
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
                            ;; Interactive note: drive the prompt now; the advice
                            ;; persists + advances the chain once it settles.
                            ((and org-log-setup (eq org-log-note-how 'note))
                             (let ((adv (org-glance-material--on-next-log-note
                                         (lambda () (resume buf existing id)))))
                               (unwind-protect
                                   (let ((this-command org-log-note-this-command))
                                     (org-add-log-note)  ; pops `*Org Note*'
                                     (setq suspended t))
                                 (unless suspended
                                   (advice-remove 'org-store-log-note adv)))))
                            ;; Timestamp log: flush it inline, no window churn.
                            (org-log-setup
                             (save-window-excursion
                               (let ((this-command org-log-note-this-command))
                                 (org-add-log-note)))
                             (with-current-buffer buf (save-buffer))
                             (push id changed))
                            ;; Plain state change (or already in STATE).
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
Materialize ID's blob, rewrite its `org-set-tags', and save so the after-save
`:sync' persists the change and refreshes open views.  Signal a `user-error'
when a pre-existing blob buffer holds unsaved edits (never clobber live work).
Return non-nil when the tag set actually changed."
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
                   (canon (lambda (x) (downcase (format "%s" x))))
                   (new (if remove
                            (cl-remove (funcall canon tag) tags
                                       :key canon :test #'string=)
                          (if (cl-member (funcall canon tag) tags
                                         :key canon :test #'string=)
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
  "Replace headline ID in GRAPH with (TRANSFORM headline), re-indexing it.
Signal a `user-error' when the blob is open with unsaved edits or ID is dead; a
TRANSFORM that errors (e.g. a wrong password) aborts before any write.
Discard a stale open buffer.  Return the new headline."
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
  "Tombstone headline ID after a referrer-aware confirmation; t when deleted.
The prompt lists the titles of headlines whose relations point at ID -- their
body links stay put and dangle harmlessly (follow reports, filters skip, edge
columns fall back to the id).  The blob and its occurrence snapshots are
reclaimed at the next compaction.  Discards ID's open material buffer and
flags views stale."
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
           ;; never clobber silently (invariant 11): a dirty open buffer is
           ;; called out in the SAME consent prompt
           (if (and buf (buffer-modified-p buf))
               " (its open buffer has UNSAVED edits, which will be discarded)"
             "")
           " ")))
    (when (yes-or-no-p prompt)
      ;; tombstone FIRST: if the append fails (ENOSPC), the open buffer -- the
      ;; user's only unsaved copy -- must survive (write-ordering, inv 5 spirit)
      (org-glance-graph:delete graph id)
      (when buf (org-glance--discard-buffer buf))
      (org-glance-view:mark-graph-stale graph)
      (message "org-glance: headline deleted (disk reclaimed at next compaction)")
      t)))

;;;###autoload
(cl-defun org-glance-delete ()
  "Choose a headline and delete it (tombstone; referrer-aware confirmation).
Deliberately ignores the ambient `org-glance-filter-spec' -- filtered-out
\(e.g. DONE) headlines must stay deletable."
  (interactive)
  (org-glance-ensure-init)
  (org-glance-material:delete
   org-glance-graph
   (org-glance-headline-metadata:id (org-glance-material:completing-read
                                     org-glance-graph :prompt "Delete: "))))

(cl-defun org-glance-material:duplicate (graph id)
  "Add a copy of headline ID to GRAPH under a fresh id; return the new id.
The copy is the blob verbatim -- body, planning, drawer, crypt blocks --
with only its ORG_GLANCE_ID replaced (a stale ORG_GLANCE_HASH line is
dropped; the hash recomputes).  Occurrence snapshots are not copied."
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
  "Set headline ID's heading TITLE; todo state, priority and tags kept."
  (org-glance-material--replace-headline
   graph id
   (lambda (headline)
     (org-glance-headline--map-contents headline
       (org-edit-headline title)))))

(cl-defun org-glance-material:set-priority (graph id priority)
  "Set headline ID's PRIORITY cookie (a character); nil clears it."
  (org-glance-material--replace-headline
   graph id
   (lambda (headline)
     (org-glance-headline--map-contents headline
       (org-priority (or priority 'remove))))))

(cl-defun org-glance-material:set-property (graph id property value)
  "Set headline ID's drawer PROPERTY to VALUE; blank VALUE deletes the key.
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
Prompts via `org-read-date'; REMOVE clears the planning instead.  Runs org's
own planner in a temp parse of the blob, so repeaters and habit cookies typed
at the prompt land natively.  Errors on unsaved edits
\(`org-glance-material--replace-headline').  Return the new headline."
  (let ((setter (if (eq kind 'schedule) #'org-schedule #'org-deadline)))
    (org-glance-material--replace-headline
     graph id
     (lambda (headline)
       ;; Prompt only after --replace-headline's guards (unsaved edits, dead
       ;; id): C-g here still aborts before any write.
       (let ((time (unless remove
                     (org-read-date nil nil nil
                                    (format "%s: " (capitalize (symbol-name kind))))))
             ;; A temp parse cannot host org's deferred reschedule/redeadline
             ;; note buffer -- never let the planner queue one.
             (org-log-reschedule nil)
             (org-log-redeadline nil))
         (org-glance-headline--map-contents headline
           (funcall setter (when remove '(4)) time)))))))

(cl-defun org-glance-material:crypt-set (graph id encrypt password)
  "Encrypt (ENCRYPT non-nil) or decrypt headline ID in GRAPH under PASSWORD.
Encrypt seals the body's crypt blocks (wrapping the whole body in one when none
exist); decrypt opens every block AND removes the markers -- fully public.
Re-index so the `encrypted?' projection flips.  Signal a `user-error' when it is
already in the requested state (or open with unsaved edits); a wrong PASSWORD on
decrypt errors before any write.  Return t."
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
  "Re-encrypt headline ID in GRAPH from the OLD password to NEW.
Signal a `user-error' when ID is not encrypted (or open with unsaved edits), or
when OLD is wrong -- the decrypt fails before any write.  Return t."
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
;;
;; The picker commands -- materialize / open / extract -- gate their candidate
;; list by the ambient `org-glance-filter-spec' (default: active headlines),
;; composed with each command's own intrinsic constraint (open needs a link,
;; extract needs a property/encryption).  The same filter is overlaid onto the
;; overview and agenda (see `org-glance-overview'); capture creates a new
;; headline, so a state filter does not apply there.

(cl-defun org-glance-material:pick-metadata (graph)
  "Choose a live GRAPH headline gated by the ambient `org-glance-filter-spec'."
  (org-glance-material:completing-read
   graph :filter (org-glance-filter:predicate org-glance-filter-spec)))

;;;###autoload
(cl-defun org-glance-materialize ()
  "Choose a headline from the graph and materialize it."
  (interactive)
  (org-glance-ensure-init)
  (let* ((graph org-glance-graph)
         (metadata (org-glance-material:pick-metadata graph))
         (id (org-glance-headline-metadata:id metadata)))
    (switch-to-buffer (org-glance-material:open graph id))))

;;; Read commands: open / extract (operate on the stored blob, read-only)

(cl-defun org-glance-material:open-link (headline)
  "Open a non-org-glance link from HEADLINE's contents, prompting if several.
Reconstructs the content in a temp buffer and runs `org-open-at-point' at the
chosen link, mirroring the v1 behaviour."
  (cl-check-type headline org-glance-headline)
  (org-glance-headline:with-contents headline
    (cl-loop for (_link title pos type) in (org-glance--parse-links)
             unless (s-starts-with-p "org-glance-" type)
             collect (list title pos) into links
             finally
             (goto-char (cond ((> (length links) 1)
                               (cadr (assoc (completing-read "Open link: " links nil t) links #'string=)))
                              ((= (length links) 1) (cadar links))
                              (t (user-error "No links in headline"))))
             ;; Mirror v1: open `file:' links in the same window.
             (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
               (org-open-at-point)))))

(cl-defun org-glance-material--pick-headline (prompt extra-pred)
  "Read a graph headline matching the ambient filter AND EXTRA-PRED under PROMPT."
  (org-glance-ensure-init)
  (let* ((graph org-glance-graph)
         (keep? (org-glance-filter:predicate org-glance-filter-spec))
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
  "Copy a value from PAIRS (an alist KEY -> VALUE) to the kill ring; return it.
With KEY, take it non-interactively; else completing-read the key.  Signal a
`user-error' on empty PAIRS.  The core shared by `org-glance-material:extract'
(from a headline) and the table's `e' action (from the property index)."
  (unless pairs (user-error "No key-value pairs in headline"))
  (let* ((key (or key (completing-read "Extract: " pairs nil t)))
         (value (alist-get key pairs nil nil #'string=)))
    (kill-new value)
    (when (called-interactively-p 'any) (message "Copied: %s" value))
    value))

(cl-defun org-glance-material:extract (headline &optional key)
  "Copy a body `KEY: value' pair from HEADLINE to the kill ring; return the value.
With KEY, extract it non-interactively; otherwise prompt."
  (cl-check-type headline org-glance-headline)
  (org-glance-material:extract-pairs (org-glance-headline:properties headline) key))

(cl-defun org-glance-material:set-interval (&optional remove)
  "Set this headline's date interval (`C-c i'); with REMOVE (`C-u'), drop it.
Prompts `org-read-date' twice and replaces the buffer's first active range
\(`org-tr-regexp'), or inserts a fresh `<from>--<to>' line after the
heading's meta-data.  A LIVE buffer edit -- nothing persists until the
ordinary save."
  (interactive "P")
  (org-glance-material--ensure)
  (org-with-wide-buffer
    (org-glance-material--goto-first-heading)
      (org-end-of-meta-data t)
      ;; Search the BODY only (title/planning/drawers can carry ranges that
      ;; are not the interval), and never a match inside a decrypted crypt
      ;; block -- editing there would silently mutate secret content the
      ;; index (which reads SEALED bytes, invariant 14) can never reflect.
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
Prompts for the key; the value lands on the kill ring.  Reads the LIVE
buffer (decrypted crypt blocks included) via the same pair scanner the
stored-headline path uses -- no temp-buffer reparse."
  (interactive)
  (org-glance-material--ensure)
  (message "Copied: %s"
           (org-glance-material:extract-pairs
            (org-glance--buffer-key-value-pairs))))

;;;###autoload
(cl-defun org-glance-extract ()
  "Choose a headline from the graph and extract a key-value pair from it."
  (interactive)
  (org-glance-material:extract
   (org-glance-material--pick-headline
    "Extract from: "
    (lambda (m) (or (org-glance-headline-metadata:propertized? m)
                    (org-glance-headline-metadata:encrypted? m))))))

;;; References (`@'): edges to other headlines
;;
;; A reference IS a link in the body; `org-glance--link-edge' owns the wire
;; format.  See docs/proposals/2026-07-18-relations.done.org.

(cl-defun org-glance-material--read-reference (graph self &key with-kind)
  "Choose a reference target in GRAPH: (ID TITLE KIND-or-nil).
Required match, SELF's id excluded (the shared picker disambiguates duplicate
labels).  WITH-KIND prompts for a kind: completion over the kinds already in
GRAPH (`org-glance-graph:edge-kinds'), free input allowed, empty = none."
  (let* ((meta (org-glance-material:completing-read
                graph :prompt "Refer to: "
                :filter (lambda (m) (not (equal self (org-glance-headline-metadata:id m))))))
         (kind (when with-kind
                 (let ((k (s-trim (completing-read
                                   "Reference kind (empty for none): "
                                   (mapcar #'org-glance--kind-pretty
                                           (org-glance-graph:edge-kinds graph))))))
                   (unless (string-empty-p k) (org-glance--kind-slug k))))))
    (list (org-glance-headline-metadata:id meta)
          (org-glance--title-clean (org-glance-headline-metadata:title meta))
          kind)))

(cl-defun org-glance-material:refer (&optional arg)
  "Insert a reference to another headline at point, or self-insert `@'.
At a word boundary -- in the body OR inside the heading title: completing-read
a headline (required match, self excluded) and insert an
`org-glance-material:' link; with ARG (`C-u @') also prompt for a reference
kind.  At a heading's column 0 (org speed keys live there) or mid-word
\(emails), delegate to org's own `self-insert' remapping.  `C-q @' inserts a
literal `@' anywhere."
  (interactive "P")
  (if (or (and (org-at-heading-p) (bolp))   ; the speed-command position
          (not (or (bolp) (memq (char-before) '(?\s ?\t ?\n)))))
      (call-interactively (or (command-remapping 'self-insert-command)
                              #'self-insert-command))
    (pcase-let ((`(,id ,title ,kind)
                 (org-glance-material--read-reference
                  org-glance-material--graph org-glance-material--id
                  :with-kind arg)))
      ;; Prose edge: "roasted by [[...][Manhattan]]" -- only the name is a
      ;; link; the canonical kind lives in the link path.
      (insert (org-glance--edge->string id kind title)))))

(cl-defun org-glance-material:references (&optional arg)
  "Table of the headlines this one refers to; with ARG, its back-references.
Passes the bare relation filter (no ambient `:done' merge), so DONE headlines
stay visible.  References read this headline's LAST-SAVED metadata -- save
first to see edges added in this session."
  (interactive "P")
  (require 'org-glance-table)      ; not at top level: table requires material
  (let ((graph org-glance-material--graph)
        (id org-glance-material--id))
    (unless (and graph id) (user-error "Not in a materialized headline buffer"))
    (if arg
        (org-glance-table:visit graph `(:refers-to ,id)
                                :context (list :anchor id :dir 'backlinks))
      (let* ((meta (org-glance-graph:get-headline graph id))
             (targets (and (org-glance-headline-metadata? meta)
                           (delete-dups (mapcar #'car (org-glance-headline-metadata:relations meta))))))
        (unless targets (user-error "Headline has no references (save after adding some)"))
        (org-glance-table:visit graph `(:id-any ,targets)
                                :context (list :anchor id :dir 'refs))))))

(provide 'org-glance-material)
;;; org-glance-material.el ends here
