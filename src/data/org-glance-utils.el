;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'f)
(require 'aes)
(require 'dash)
(require 'org-element)

(defconst org-glance:key-value-pair-re "^-?\\([[:word:],[:blank:],_,/,-]+\\)\\:[[:blank:]]*\\(.*\\)$")

(cl-defun org-glance--present-string? (v)
  "Non-nil when V is a non-empty string.
Whitespace-only strings count as present (unlike `s-present?')."
  (and (stringp v) (not (string-empty-p v))))

(cl-defun org-glance--sorted-distinct (strings)
  "Return STRINGS de-duplicated and sorted with `string<'."
  (sort (-distinct strings)               ; hash-backed O(N) dedup (was O(N^2))
        #'string<))

(cl-defun org-glance--file-mtime (path)
  "Filesystem modification time of PATH, or nil when it does not exist."
  (and (f-exists? path)
       (file-attribute-modification-time (file-attributes path))))

(defconst org-glance--conflict-marker-re
  "^\\(<<<<<<<\\|=======\\|>>>>>>>\\)"
  "Regexp matching a git conflict-marker line start.
`^' matches after every newline in a string.  A `*.jsonl' union driver stops
these in the WAL; a `config/*.eld' sidecar can still carry them after a sync.")

(define-obsolete-variable-alias 'org-glance-graph-conflict-resolution
  'org-glance-conflict-resolution "org-glance 0.2")

(defcustom org-glance-conflict-resolution 'ask
  "How to auto-resolve a git conflict in the store or its sidecars.
A synced store dir (git/Syncthing) can arrive with conflict markers.  Governs
every resolver -- the `meta/*.jsonl' WAL and the `config/*.eld' sidecars:
`ask'   -- prompt to approve a union merge (keep both sides), the default;
`union' -- resolve silently;
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
Drops only the `<<<<<<<'/`======='/`>>>>>>>' lines, what git's `union' produces."
  (replace-regexp-in-string
   (concat org-glance--conflict-marker-re ".*\n?") "" text))

(cl-defun org-glance--resolve-conflict (subject resolve-fn)
  "Heal a git conflict in SUBJECT via RESOLVE-FN, per the policy custom.
SUBJECT names the conflict in the prompt.  Per `org-glance-conflict-resolution':
`union' runs RESOLVE-FN silently, `ask' prompts first, nil (or a declined
prompt) errors.  RESOLVE-FN owns the merge; return its value."
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
`--write-eld' writes one `prin1' line, so a conflict leaves both sides' whole
forms back to back once stripped.  Read all, stopping at the first bad tail."
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
On a git-conflicted PATH keep the first NON-EMPTY side (`consp', so a side
written as literal `nil' never shadows a populated one) rather than crash on the
`<<<<<<<' marker symbol.  Callers that union the sides use `--heal-eld' instead."
  (when (f-exists? path)
    (let ((text (f-read-text path 'utf-8)))
      (if (org-glance--conflict-marked? text)
          (cl-find-if #'consp (org-glance--read-eld-forms text))
        (ignore-errors (car (read-from-string text)))))))

(cl-defun org-glance--write-eld (path form)
  "Serialize FORM to the .eld PATH, creating parent dirs."
  (f-mkdir-full-path (f-dirname path))
  (f-write-text (prin1-to-string form) 'utf-8 path))

(cl-defun org-glance--eld-alist-ref (path key)
  "Value for KEY in the keyed alist stored at .eld PATH (`equal' keys), or nil."
  (alist-get key (org-glance--read-eld path) nil nil #'equal))

(cl-defun org-glance--eld-alist-set (path key value)
  "Upsert KEY -> VALUE in the keyed alist at .eld PATH; a nil VALUE drops KEY.
Rewrites the whole alist atomically via `--write-eld'."
  (let ((all (cl-remove key (org-glance--read-eld path) :key #'car :test #'equal)))
    (when value (setq all (cons (cons key value) all)))
    (org-glance--write-eld path all)))

(cl-defun org-glance--heal-eld (path merge-fn &optional subject)
  "Read the .eld at PATH, union-resolving a git conflict through MERGE-FN.
A clean PATH returns its single form (nil if absent/unreadable).  A conflicted
one folds its readable side-forms with MERGE-FN (over that LIST) under
`--resolve-conflict' (SUBJECT names it, default PATH's basename), writes the
result back, and returns it.  Gated counterpart to the `--read-eld' floor."
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
  "Buffer links as (LINK TITLE POS TYPE PATH) tuples, in buffer order.
LINK is the raw bracket text, TITLE the description (or raw link), POS the
start; TYPE and PATH the parsed `org-element' link type and unescaped path."
  (cl-loop for link-element in (org-element-map (org-element-parse-buffer) 'link #'identity)
           for beg = (org-element-property :begin link-element)
           for end = (org-element-property :end link-element)
           for title = (substring-no-properties
                        (or (-some->> link-element
                              (org-element-contents)
                              (org-element-interpret-data))
                            (org-element-property :raw-link link-element)))
           for link = (s-trim (buffer-substring-no-properties beg end))
           collect (list link title beg
                         (org-element-property :type link-element)
                         (org-element-property :path link-element))))

;;; Relation edges
;;
;; An edge IS an `org-glance-material:' link in the source headline's body
;; (blobs canonical, indexes derived): `[[org-glance-material:ID][Title]]',
;; optionally `?kind=KIND' for a typed reference (a book's author vs editor).
;; Legacy `org-glance-visit:' links count as kindless edges, so old notes gain
;; relations for free.

(defconst org-glance-link-material-type "org-glance-material"
  "Org link type materializing a headline by id; the canonical edge form.")

(defconst org-glance--link-edge-types
  (list org-glance-link-material-type "org-glance-visit")
  "Link types that denote a relation edge to another headline.")

(cl-defun org-glance--kind-slug (kind)
  "Canonical wire form of a reference KIND: downcased, spaces to dashes.
\"Roasted By\" -> \"roasted-by\".  Applied on BOTH encode and decode, so a
hand-typed or legacy spaced kind normalizes on the next parse."
  (replace-regexp-in-string "[ \t]+" "-" (downcase (s-trim kind))))

(cl-defun org-glance--kind-pretty (kind)
  "Human form of a KIND slug: dashes back to spaces (\"roasted-by\" ->
\"roasted by\")."
  (replace-regexp-in-string "-" " " kind))

(cl-defun org-glance--link-edge (type path)
  "Edge (TARGET-ID . KIND-SLUG-or-nil) denoted by a TYPE/PATH link, or nil.
The decode half of the edge wire format; `--edge->link-path' encodes."
  (when (and (member type org-glance--link-edge-types)
             (stringp path)
             (string-match "\\`\\([^?]+\\)\\(?:\\?kind=\\(.+\\)\\)?\\'" path))
    (cons (match-string 1 path)
          (when-let ((kind (match-string 2 path)))
            (org-glance--kind-slug kind)))))

(cl-defun org-glance--edge->link-path (id &optional kind)
  "Link path (TYPE:ID[?kind=SLUG]) for an edge to ID; the encode half."
  (concat org-glance-link-material-type ":" id
          (and kind (concat "?kind=" (org-glance--kind-slug kind)))))

(cl-defun org-glance--links->edges (links)
  "Distinct relation edges among LINKS (the `--buffer-links' tuple shape)."
  (-distinct (cl-loop for (_link _title _pos type path) in links
                      for edge = (org-glance--link-edge type path)
                      when edge collect edge)))

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
  "Buffer links as (LINK DESCRIPTION POS TYPE PATH), preferring a body
`KEY: value' description over the link's own title when one matches."
  (cl-loop with descriptions = (cl-loop for (key . val) in (org-glance--buffer-key-value-pairs) collect (cons val key))
           for (link title pos type path) in (org-glance--buffer-links)
           collect (list link (or (alist-get link descriptions nil nil #'string=) title)
                         pos type path)))

;;; Crypt blocks
;;
;; A `#+begin_crypt' … `#+end_crypt' special block marks a body region that is
;; ciphertext AT REST and plaintext only in the material buffer.  The markers
;; are the persistent secrecy annotation; sealing/unsealing rewrites only the
;; body between them.  Plaintext outside the blocks stays indexable (links,
;; properties), which is the whole point.  Base64 ciphertext cannot collide
;; with the marker lines.

(defconst org-glance--crypt-begin-re "^[ \t]*#\\+begin_crypt[ \t]*$"
  "Regexp matching a crypt block's opening marker line (case-insensitive use).")

(defconst org-glance--crypt-end-re "^[ \t]*#\\+end_crypt[ \t]*$"
  "Regexp matching a crypt block's closing marker line (case-insensitive use).")

(defconst org-glance--aes-header-re "aes-encrypted V [0-9]+\\.[0-9]+-.+"
  "Regexp matching the first line of `aes.el' ciphertext.")

(defun org-glance--crypt-block-regions ()
  "Crypt blocks of the current buffer, in buffer order.
Each is a plist (:beg B :body-beg BB :body-end BE :end E): B/E delimit the
marker lines (E excludes the trailing newline), BB/BE the body between them.
A begin marker with no closing marker is ignored."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t) blocks)
      (while (re-search-forward org-glance--crypt-begin-re nil t)
        (let ((beg (match-beginning 0))
              (body-beg (min (1+ (match-end 0)) (point-max))))
          (when (re-search-forward org-glance--crypt-end-re nil t)
            (push (list :beg beg :body-beg body-beg
                        :body-end (match-beginning 0) :end (match-end 0))
                  blocks))))
      (nreverse blocks))))

(defun org-glance--crypt-block-at (pos)
  "The crypt block plist containing POS (markers inclusive), or nil."
  (cl-find-if (lambda (b) (<= (plist-get b :beg) pos (plist-get b :end)))
              (org-glance--crypt-block-regions)))

(defun org-glance--crypt-sealed? (block)
  "Non-nil when BLOCK's body is ciphertext."
  (save-excursion
    (goto-char (plist-get block :body-beg))
    (looking-at org-glance--aes-header-re)))

(defun org-glance--crypt-sealed-blocks-p ()
  "Non-nil when the buffer has at least one sealed crypt block."
  (cl-some #'org-glance--crypt-sealed? (org-glance--crypt-block-regions)))

(defun org-glance--crypt--replace-body (block text)
  "Replace BLOCK's body with TEXT, keeping the end marker on its own line."
  (let ((beg (plist-get block :body-beg)))
    (delete-region beg (plist-get block :body-end))
    (save-excursion
      (goto-char beg)
      (insert text)
      (unless (bolp) (insert "\n")))))

(defun org-glance--crypt--transform-blocks (sealed transform)
  "Replace each block body whose sealed state equals SEALED with (TRANSFORM BODY).
Processes blocks last-to-first so earlier positions stay valid while later
bodies are rewritten."
  (dolist (block (reverse (org-glance--crypt-block-regions)))
    (when (eq sealed (and (org-glance--crypt-sealed? block) t))
      (org-glance--crypt--replace-body
       block (funcall transform
                      (buffer-substring-no-properties (plist-get block :body-beg)
                                                      (plist-get block :body-end)))))))

(defun org-glance--crypt-seal-blocks (password)
  "Encrypt every unsealed crypt-block body in the buffer with PASSWORD."
  (org-glance--crypt--transform-blocks
   nil (lambda (body) (aes-encrypt-buffer-or-string body password))))

(defun org-glance--crypt-unseal-blocks (password)
  "Decrypt every sealed crypt-block body in the buffer with PASSWORD.
Signal a `user-error' on a wrong PASSWORD."
  (org-glance--crypt--transform-blocks
   t (lambda (body) (or (aes-decrypt-buffer-or-string body password)
                        (user-error "Wrong password")))))

(defun org-glance--crypt-wrap-region (beg end)
  "Wrap BEG..END in crypt block markers, on whole lines.
BEG is moved to its line start; END extends to the following line start (a final
line without a newline gets one), so the markers sit on their own lines."
  (save-excursion
    (goto-char end)
    (unless (bolp) (insert "\n"))
    (insert "#+end_crypt\n")
    (goto-char beg)
    (forward-line 0)
    (insert "#+begin_crypt\n")))

(defun org-glance--crypt-unwrap-block (block)
  "Remove BLOCK's marker lines, keeping its body."
  (delete-region (plist-get block :body-end)
                 (min (1+ (plist-get block :end)) (point-max)))
  (delete-region (plist-get block :beg) (plist-get block :body-beg)))

(defun org-glance--crypt-unwrap-blocks ()
  "Remove every crypt block's marker lines, keeping the bodies.  Return count."
  (let ((count 0))
    (dolist (block (reverse (org-glance--crypt-block-regions)) count)
      (org-glance--crypt-unwrap-block block)
      (cl-incf count))))

(defun org-glance--discard-buffer (buffer)
  "Kill BUFFER without the `Buffer modified; kill anyway?' confirmation.
For a buffer org-glance owns and means to discard (a captured temp file, a
materialization abandoned on error): clear the modified flag, then kill.  No-op
if BUFFER is already dead."
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
