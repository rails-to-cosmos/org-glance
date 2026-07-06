;; -*- lexical-binding: t -*-

;;; org-glance-material.el --- graph-backed selection + materialize/sync

;;; Commentary:
;; Command layer over the graph.  Selection lists live headlines from the
;; graph; materialize opens a headline's content blob in an editable buffer;
;; apply writes it back as a NEW version (the store is append-only,
;; last-write-wins), guarded by a hash check against concurrent changes.

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'org)
(require 'ol)
(require 's)

(require 'org-glance-utils)
(require 'org-glance-headline)
(require 'org-glance-graph)
(require 'org-glance-filter)
(require 'org-glance-tag-config)
(require 'org-glance-datetime-mode)
(require 'org-glance-view)

;; Defined in org-glance.el (which requires this file); referenced only at runtime.
(defvar org-glance-graph)
(declare-function org-glance-initialized? "org-glance")

;;; Selection

(cl-defun org-glance-material:label (metadata)
  "Completing-read label for headline METADATA: \"[tags] title\"."
  (cl-check-type metadata org-glance-headline-metadata)
  (let ((tags (append (org-glance-headline-metadata:tags metadata) nil)))
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
         (candidates (cl-loop for meta in (org-glance-graph:headlines graph)
                              when (or (null filter) (funcall filter meta))
                              collect (cons (org-glance-material:label meta) meta))))
    (unless candidates
      (user-error "No matching headlines (run `M-x org-glance-reindex' if you upgraded)"))
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

(define-key org-glance-material-mode-map (kbd "C-c C-q") #'kill-current-buffer)
(define-key org-glance-material-mode-map (kbd "C-c C-l") #'org-glance-material:lock)

(defvar-local org-glance-material--graph nil
  "Graph backing the current materialized buffer.")
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
                            (if org-glance-material--cycle
                                (org-glance-tag-config:cycle->keywords org-glance-material--cycle)
                              org-todo-keywords)))
                       (org-glance-headline--from-string
                        (buffer-substring-no-properties (point-min) (point-max))))))
      (if (equal (org-glance-headline:id headline) id)
          (progn
            (org-glance-graph:insert graph (list (org-glance-headline:metadata headline)))
            (org-glance-view:mark-graph-stale graph))
        (message "org-glance: ORG_GLANCE_ID changed (expected %s); metadata not updated" id)))))

;;; Repeated headlines: clone-on-repeat
;;
;; When a repeated headline is completed, optionally preserve the finished
;; repetition as a NEW graph headline (fresh id, repeater disarmed) before org
;; bumps the timestamps, then trim the live headline back to its header and
;; pinned blocks -- the accumulated history lives on in the clone.

(defcustom org-glance-clone-on-repeat-p nil
  "Create a new headline copy when repeating rather than modifying in place."
  :group 'org-glance
  :type 'boolean)

(cl-defun org-glance-material:clone-on-repeat (&rest _)
  "Preserve the completed repetition of the materialized headline.
Runs `:before' `org-auto-repeat-maybe' (the headline is still in its done
state): snapshot the subtree, disarm its repeaters, strip its ORG_GLANCE_ID and
capture it into the graph as a new headline."
  (when (and org-glance-clone-on-repeat-p
             org-glance-material-mode
             org-glance-material--graph
             (member (org-get-todo-state) org-done-keywords)
             (org-glance-datetime-headline-repeated-p))
    (let ((graph org-glance-material--graph)
          (contents (buffer-substring-no-properties (point-min) (point-max))))
      (with-temp-buffer
        (insert contents)
        (org-glance--org-mode)
        (goto-char (point-min))
        (org-glance-datetime-reset-buffer-timestamps-except-earliest)
        ;; `org-entry-delete' resolves the sole heading from any point in this
        ;; single-entry buffer, so no reset to point-min is needed here.
        (org-delete-property "ORG_GLANCE_ID") ;; the clone gets a fresh id
        (org-glance-graph:capture graph (current-buffer))))))

(cl-defun org-glance-material:cleanup-after-repeat (&rest _)
  "Trim the repeated materialized headline to its header and pinned blocks.
Runs `:after' `org-auto-repeat-maybe'; only meaningful when
`org-glance-material:clone-on-repeat' preserved the previous repetition."
  (when (and org-glance-clone-on-repeat-p
             org-glance-material-mode
             (org-glance-datetime-headline-repeated-p))
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
(advice-add 'org-auto-repeat-maybe :before #'org-glance-material:clone-on-repeat '((depth . -90)))
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

(cl-defun org-glance-material--body-region ()
  "Cons (BEG . END) of the current headline's body: meta-data end to subtree end.
The same region `org-glance-headline:encrypt' rewrites, so crypto round-trips."
  (cons (save-excursion (goto-char (point-min)) (org-end-of-meta-data t) (point))
        (save-excursion (goto-char (point-min)) (org-end-of-subtree t) (point))))

(cl-defun org-glance-material--encrypt-buffer ()
  "Encrypt the materialized body in place before the file is written to disk.
Buffer-local `before-save-hook'; prompts for the password if the TTL expired."
  (when org-glance-material--encrypted
    (let ((region (org-glance-material--body-region))
          (inhibit-read-only t))
      (org-glance--encrypt-region (car region) (cdr region)
                                  (org-glance-material--require-password)))))

(cl-defun org-glance-material--decrypt-buffer ()
  "Decrypt the materialized body in place for editing.
Clears the modified flag: the plaintext buffer matches the ciphertext already on
disk.  Buffer-local `after-save-hook' (runs after `org-glance-material:sync')."
  (when org-glance-material--encrypted
    (let ((region (org-glance-material--body-region))
          (inhibit-read-only t))
      (org-glance--decrypt-region (car region) (cdr region)
                                  (org-glance-material--require-password))
      (set-buffer-modified-p nil))))

(cl-defun org-glance-material:lock ()
  "Forget this encrypted buffer's cached password now; the next save re-prompts.
The decrypted body stays in the buffer until then."
  (interactive)
  (if org-glance-material--encrypted
      (progn (org-glance-material--clear-password)
             (message "org-glance: password forgotten"))
    (user-error "Not an encrypted materialized buffer")))

(cl-defun org-glance-material--maybe-decrypt (meta buffer)
  "When META is encrypted, harden BUFFER, prompt for the password, and decrypt it.
Caches the password (with TTL) and wires the save-time re-encrypt round-trip.
A wrong password forgets it, kills BUFFER and re-signals, so `open' fails clean."
  (when (org-glance-headline-metadata:encrypted? meta)
    (setq-local org-glance-material--encrypted t)
    (org-glance-material--harden-buffer)
    (org-glance-material--set-password (read-passwd "Headline password: "))
    (condition-case err
        (org-glance-material--decrypt-buffer)
      (error (org-glance-material--clear-password)
             (kill-buffer buffer)
             (signal (car err) (cdr err))))
    (add-hook 'before-save-hook #'org-glance-material--encrypt-buffer nil t)
    (add-hook 'after-save-hook #'org-glance-material--decrypt-buffer t t)
    (add-hook 'kill-buffer-hook #'org-glance-material--clear-password nil t)))

(cl-defun org-glance-material:open (graph id)
  "Open headline ID from GRAPH for editing, as its content-blob file.
Return the buffer.  Errors if ID is unknown, tombstoned, or has no stored blob."
  (cl-check-type graph org-glance-graph)
  (cl-check-type id string)
  (let ((meta (org-glance-graph:get-headline graph id)))
    (unless (org-glance-headline-metadata? meta)
      (user-error "No live headline with id %s" id))
    (let ((path (org-glance-graph:content-path graph id))
          (cycle (org-glance-tag-config:cycle-for-filter
                  graph (list :tags (append (org-glance-headline-metadata:tags meta) nil)))))
      (unless (f-exists? path)
        (user-error "No stored content for id %s" id))
      (let ((buffer
             ;; Initialize the buffer's `org-mode' with the tag's TODO cycle bound
             ;; GLOBALLY: `org-set-regexps-and-options' (run inside `find-file-noselect')
             ;; reads the DEFAULT value of `org-todo-keywords', not a buffer-local one
             ;; (the W1 finding), so this is what makes the buffer's derived keyword vars
             ;; -- and hence native rendering, cycling and `org-todo' -- know the tag's
             ;; states (e.g. READING), WITHOUT a `#+TODO:' in the kept-clean blob.
             (let ((org-todo-keywords
                    (if cycle (org-glance-tag-config:cycle->keywords cycle)
                      org-todo-keywords)))
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
          (org-glance-material--maybe-decrypt meta buffer))
        buffer))))

(cl-defun org-glance-material:apply ()
  "Save the materialized buffer: write the blob and sync its metadata."
  (interactive)
  (unless org-glance-material--graph
    (user-error "Not in an org-glance materialized buffer"))
  (save-buffer))

;;; TODO state change: exactly `C-c C-t' on the headline (materialize -> sync)
;;
;; A read-only view (overview / table) advances a headline's state with byte-exact
;; org fidelity by MATERIALIZING it and running `org-todo' in the real blob buffer:
;; per-tag keywords are live (see `org-glance-material:open'), so state cycling, the
;; CLOSED timestamp, `org-after-todo-state-change-hook', dependency blocking,
;; repeaters (clone-on-repeat) and the interactive LOGBOOK note all happen natively.
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
(defvar org-log-note-marker)   ; org.el: marker `org-add-log-note' will return to
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
                      ;; Note pending -> persist + finish after it settles (commit OR
                      ;; abort), via a one-shot self-removing advice on
                      ;; `org-store-log-note'; DEFER the kill/refresh off org's extent.
                      (letrec ((adv (lambda (&rest _)
                                      (advice-remove 'org-store-log-note adv)
                                      (let ((state (persist)))
                                        (run-at-time 0 nil (lambda () (finish state)))))))
                        (advice-add 'org-store-log-note :after adv))
                      (setq owned t))
                     ((buffer-modified-p)
                      (setq owned t commit-now t)))))
                ;; No note: commit synchronously, now OUTSIDE the buffer edit (so
                ;; killing it is safe).
                (when commit-now (finish (persist))))
            ;; org-todo threw (e.g. a dependency block) before ownership passed to
            ;; the note advice / commit -> don't leak the background buffer.
            (when (and (not owned) (buffer-live-p buf)) (kill-buffer buf))))))))

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
                             (letrec ((adv (lambda (&rest _)
                                             (advice-remove 'org-store-log-note adv)
                                             (resume buf existing id))))
                               (advice-add 'org-store-log-note :after adv)
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

;;; Commands
;;
;; The picker commands -- materialize / open / extract -- gate their candidate
;; list by the ambient `org-glance-filter-spec' (default: active headlines),
;; composed with each command's own intrinsic constraint (open needs a link,
;; extract needs a property/encryption).  The same filter is overlaid onto the
;; overview and agenda (see `org-glance-overview'); capture creates a new
;; headline, so a state filter does not apply there.

(cl-defun org-glance-materialize ()
  "Choose a headline from the graph and materialize it."
  (interactive)
  (cl-assert (org-glance-initialized?))
  (let* ((graph org-glance-graph)
         (metadata (org-glance-material:completing-read
                    graph :filter (org-glance-filter:predicate org-glance-filter-spec)))
         (id (org-glance-headline-metadata:id metadata)))
    (switch-to-buffer (org-glance-material:open graph id))))

;;; Read commands: open / extract (operate on the stored blob, read-only)

(cl-defun org-glance-material:open-link (headline)
  "Open a non-org-glance link from HEADLINE's contents, prompting if several.
Reconstructs the content in a temp buffer and runs `org-open-at-point' at the
chosen link, mirroring the v1 behaviour."
  (cl-check-type headline org-glance-headline)
  (org-glance-headline:with-contents headline
    (cl-loop for (link title pos) in (org-glance--parse-links)
             unless (s-starts-with-p "[[org-glance-" link)
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
  (cl-assert (org-glance-initialized?))
  (let* ((graph org-glance-graph)
         (keep? (org-glance-filter:predicate org-glance-filter-spec))
         (metadata (org-glance-material:completing-read
                    graph :prompt prompt
                    :filter (lambda (m) (and (funcall keep? m)
                                        (funcall extra-pred m))))))
    (org-glance-graph:headline graph (org-glance-headline-metadata:id metadata))))

(cl-defun org-glance-open ()
  "Choose a headline from the graph and open a link inside it."
  (interactive)
  (org-glance-material:open-link
   (org-glance-material--pick-headline "Open: " #'org-glance-headline-metadata:linked?)))

(cl-defun org-glance-material:extract (headline &optional key)
  "Copy a key-value pair from HEADLINE's contents to the kill ring; return value.
With KEY, extract it non-interactively; otherwise prompt."
  (cl-check-type headline org-glance-headline)
  (let ((pairs (org-glance-headline:properties headline)))
    (unless pairs (user-error "No key-value pairs in headline"))
    (let* ((key (or key (completing-read "Extract: " pairs nil t)))
           (value (alist-get key pairs nil nil #'string=)))
      (kill-new value)
      (when (called-interactively-p 'any) (message "Copied: %s" value))
      value)))

(cl-defun org-glance-extract ()
  "Choose a headline from the graph and extract a key-value pair from it."
  (interactive)
  (org-glance-material:extract
   (org-glance-material--pick-headline
    "Extract from: "
    (lambda (m) (or (org-glance-headline-metadata:propertized? m)
                    (org-glance-headline-metadata:encrypted? m))))))

(provide 'org-glance-material)
;;; org-glance-material.el ends here
