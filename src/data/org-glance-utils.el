;; -*- lexical-binding: t -*-

(require 'f)
(require 'aes)
(require 'dash)
(require 'org-element)

(defconst org-glance:key-value-pair-re "^-?\\([[:word:],[:blank:],_,/,-]+\\)\\:[[:blank:]]*\\(.*\\)$")

(cl-defun org-glance--present-string? (v)
  "Non-nil when V is a non-empty string.
Whitespace-only strings count as present (unlike `s-present?')."
  (and (stringp v) (not (string-empty-p v))))

(cl-defun org-glance--file-mtime (path)
  "Filesystem modification time of PATH, or nil when it does not exist."
  (and (f-exists? path)
       (file-attribute-modification-time (file-attributes path))))

(defconst org-glance--conflict-marker-re
  "^\\(<<<<<<<\\|=======\\|>>>>>>>\\)"
  "Regexp matching the start of a git conflict-marker line.
Anchored to a line start (Emacs `^' matches after every newline in a string).
Covers git's default two-way merge markers.  A `*.jsonl' union driver prevents
these in the WAL, but a `config/*.eld' sidecar it never touches can still carry
them after a git sync.")

(define-obsolete-variable-alias 'org-glance-graph-conflict-resolution
  'org-glance-conflict-resolution "org-glance 0.2")

(defcustom org-glance-conflict-resolution 'ask
  "How to auto-resolve a git conflict found in the store or its sidecars.
A store dir synced across machines (git/Syncthing) can arrive with conflict
markers.  This governs every resolver alike -- the `meta/*.jsonl' WAL and the
`config/*.eld' sidecars:
`ask'   -- prompt to approve a union merge (keep data from both sides), default;
`union' -- resolve automatically, without prompting;
nil     -- never resolve; signal an error so the conflict stays visible."
  :group 'org-glance
  :type '(choice (const :tag "Prompt to approve" ask)
                 (const :tag "Auto-resolve (union)" union)
                 (const :tag "Never (error)" nil)))

(cl-defun org-glance--conflict-marked? (text)
  "Non-nil when TEXT carries git conflict markers."
  (and text (string-match-p org-glance--conflict-marker-re text)))

(cl-defun org-glance--strip-conflict-markers (text)
  "TEXT with its git conflict-marker lines removed -- a union of both sides.
Deletes only the `<<<<<<<'/`======='/`>>>>>>>' lines, keeping every content line
from either side: exactly what git's built-in `union' driver produces."
  (replace-regexp-in-string
   (concat org-glance--conflict-marker-re ".*\n?") "" text))

(cl-defun org-glance--resolve-conflict (subject resolve-fn)
  "Heal a git conflict in SUBJECT by calling RESOLVE-FN, honouring the policy.
SUBJECT is a short name for the prompt/message.  Per the
`org-glance-conflict-resolution' custom: `union' runs RESOLVE-FN silently, `ask'
prompts for approval first, nil signals an error so the conflict stays visible;
a declined prompt also errors.  Returns RESOLVE-FN's value.  RESOLVE-FN owns the
actual merge/rewrite -- this is only the gate, so every store and sidecar shares
one policy and one prompt."
  (pcase org-glance-conflict-resolution
    ('nil (error "org-glance: unresolved git conflict in %s" subject))
    (mode
     (unless (or (eq mode 'union)
                 (y-or-n-p (format "org-glance: git conflict in %s -- \
resolve by union merge (keep data from both sides)? " subject)))
       (error "org-glance: git conflict in %s left unresolved" subject))
     (funcall resolve-fn))))

(cl-defun org-glance--read-eld-forms (text)
  "Every readable top-level form in TEXT, git conflict markers stripped first.
`--write-eld' serialises an .eld as one `prin1' line, so a conflict is a whole
line hunk: `--strip-conflict-markers' leaves both sides' complete forms back to
back.  Read them all (stopping at the first unreadable tail) so a caller can
pick a side or merge them."
  (let ((clean (org-glance--strip-conflict-markers text))
        (pos 0) forms)
    (ignore-errors
      (let ((len (length clean)))
        (while (< pos len)
          (pcase-let ((`(,form . ,next) (read-from-string clean pos)))
            (push form forms)
            (setq pos next)))))
    (nreverse forms)))

(cl-defun org-glance--read-eld (path)
  "Read the single Lisp form in the .eld PATH, or nil when absent/unreadable.
A git-conflicted sidecar would otherwise read back as a stray `<<<<<<<' marker
symbol and crash the caller; in that case keep the first NON-EMPTY side instead
-- `consp', not `listp', so a side written as literal `nil' (an emptied config)
never shadows a populated side that follows it.  Callers that can union the
sides (see `org-glance-tag-metrics') do so; this floor just refuses to hand
back garbage, returning nil only when every side is empty or unreadable."
  (when (f-exists? path)
    (let ((text (f-read-text path 'utf-8)))
      (if (org-glance--conflict-marked? text)
          (cl-find-if #'consp (org-glance--read-eld-forms text))
        (ignore-errors (car (read-from-string text)))))))

(cl-defun org-glance--write-eld (path form)
  "Serialize FORM to the .eld PATH, creating parent dirs."
  (f-mkdir-full-path (f-dirname path))
  (f-write-text (prin1-to-string form) 'utf-8 path))

(cl-defun org-glance--heal-eld (path merge-fn &optional subject)
  "Read the .eld at PATH, union-resolving a git conflict through MERGE-FN.
A clean PATH returns its single form (nil if absent/unreadable).  A conflicted
one combines every readable side-form with MERGE-FN (a function of the LIST of
side-forms) under `org-glance--resolve-conflict' (SUBJECT names it, default
PATH's basename), writes the result back so the markers are gone for good, and
returns it.  The reusable counterpart to the `--read-eld' floor: a caller brings
only its domain merge, the conflict machinery is shared."
  (when (f-exists? path)
    (let ((text (f-read-text path 'utf-8)))
      (if (not (org-glance--conflict-marked? text))
          (ignore-errors (car (read-from-string text)))
        (let ((subject (or subject (file-name-nondirectory path))))
          (org-glance--resolve-conflict
           subject
           (lambda ()
             (let ((merged (funcall merge-fn (org-glance--read-eld-forms text))))
               (org-glance--write-eld path merged)
               (message "org-glance: union-resolved git conflict in %s" subject)
               merged))))))))

(cl-defun org-glance--buffer-links ()
  (cl-loop for link-element in (org-element-map (org-element-parse-buffer) 'link #'identity)
           for beg = (org-element-property :begin link-element)
           for end = (org-element-property :end link-element)
           for title = (substring-no-properties
                        (or (-some->> link-element
                              (org-element-contents)
                              (org-element-interpret-data))
                            (org-element-property :raw-link link-element)))
           for link = (s-trim (buffer-substring-no-properties beg end))
           collect title into titles
           collect link into links
           collect beg into positions
           finally return (-zip links titles positions)))

(cl-defun org-glance--buffer-key-value-pairs ()
  "Extract key-value pairs from buffer.
Run completing read on keys and copy selected values to kill ring.

Assume string is a key-value pair if it matches `org-glance:key-value-pair-re'."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward org-glance:key-value-pair-re nil t)
             for key = (s-trim (substring-no-properties (match-string 1)))
             for value = (s-trim (substring-no-properties (match-string 2)))
             collect (cons key value))))

(cl-defun org-glance--parse-links ()
  "Simple org-link parser, return list of cons cells (link . contents)."
  (cl-loop with descriptions = (cl-loop for (key . val) in (org-glance--buffer-key-value-pairs) collect (cons val key))
           with links = (org-glance--buffer-links)
           for (link title pos) in links
           for description = (alist-get link descriptions nil nil #'string=)
           when description
           collect (list link description pos)
           else
           collect (list link title pos)))

(defun org-glance--encrypt-region (beg end &optional password)
  "Encrypt region from BEG to END using PASSWORD."
  (interactive "r")
  (let* ((original-text (buffer-substring-no-properties beg end))
         (encrypted-text (aes-encrypt-buffer-or-string original-text password)))
    (save-excursion
      (delete-region beg end)
      (goto-char beg)
      (insert encrypted-text))))

(defun org-glance--decrypt-region (beg end &optional password)
  "Decrypt region from BEG to END using PASSWORD."
  (interactive "r")
  (if-let (decrypted-text (let ((encrypted (buffer-substring-no-properties beg end)))
                            (if (with-temp-buffer
                                  (insert encrypted)
                                  (aes-is-encrypted))
                                (aes-decrypt-buffer-or-string encrypted password)
                              (user-error "Headline is not encrypted"))))
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert decrypted-text))
    (user-error "Wrong password")))

(defun org-glance--discard-buffer (buffer)
  "Kill BUFFER without the `Buffer modified; kill anyway?' confirmation.
For buffers org-glance owns and means to discard -- a capture temp file whose
content is already in the graph, a materialization abandoned on error -- the
modified flag is only noise, so clear it before killing.  No-op if BUFFER is
already dead."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (set-buffer-modified-p nil))
    (kill-buffer buffer)))

(defun org-glance--kill-buffer-noconfirm ()
  "Clear the current buffer's modified flag and return t.
Install this buffer-locally on `kill-buffer-query-functions' for a buffer
org-glance owns and means to discard, so any code path that kills it -- the
interactive `C-c C-c' finalize, a programmatic finalize, or org-capture's own
teardown -- proceeds silently, with no `Buffer modified; kill anyway?'
confirmation.  Query functions run before that confirmation, so clearing the
flag here makes it a no-op."
  (set-buffer-modified-p nil)
  t)

(provide 'org-glance-utils)
