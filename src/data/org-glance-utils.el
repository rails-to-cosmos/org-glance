;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'f)
(require 'aes)
(require 'dash)
(require 'org-element)
(require 'org-list)

(defconst org-glance:key-value-pair-re "^-?\\([[:word:],[:blank:],_,/,-]+\\)\\:[[:blank:]]*\\(.*\\)$")

(cl-defun org-glance--present-string? (v)
  "Non-nil when V is a non-empty string.
Whitespace-only strings count as present (unlike `s-present?')."
  (and (stringp v) (not (string-empty-p v))))

(cl-defun org-glance--property-key (key)
  "Return user-supplied drawer property KEY canonicalized: trimmed, upcased."
  (upcase (string-trim key)))

(cl-defun org-glance--sorted-distinct (strings)
  "Return STRINGS de-duplicated and sorted with `string<'."
  (sort (-distinct strings)               ; hash-backed O(N) dedup (was O(N^2))
        #'string<))

(cl-defun org-glance--check-struct-field-order (&key slots fields subject)
  "Signal unless FIELDS lists struct SLOTS in order; else return t.
SUBJECT names the field table in the error."
  (let ((struct-slots (mapcar #'car slots))
        (table-slots (mapcar #'car fields)))
    (unless (equal struct-slots table-slots)
      (error "org-glance: %s field table out of sync with the struct: %S vs %S"
             subject table-slots struct-slots)))
  t)

(cl-defun org-glance--file-mtime (path)
  "Return PATH's modification time, or nil when it does not exist."
  (and (f-exists? path)
       (file-attribute-modification-time (file-attributes path))))

(cl-defun org-glance--file-size (path)
  "Return PATH's size in bytes, or 0 when it does not exist."
  (or (file-attribute-size (file-attributes path)) 0))

(cl-defun org-glance--insert-bytes (path &optional beg end)
  "Insert PATH's bytes [BEG, END) into the current buffer, made unibyte.
Return t, or nil on a file error; unibyte keeps caller offsets in BYTES."
  (set-buffer-multibyte nil)
  (condition-case nil
      (progn (insert-file-contents-literally path nil beg end) t)
    (file-error nil)))

(defconst org-glance--conflict-marker-re
  "^\\(<<<<<<<\\|=======\\|>>>>>>>\\)"
  "Regexp matching a git conflict-marker line start, on any line of a string.")

(define-obsolete-variable-alias 'org-glance-graph-conflict-resolution
  'org-glance-conflict-resolution "org-glance 0.2")

(defcustom org-glance-conflict-resolution 'ask
  "How to resolve a git conflict in the store's WAL or `config/*.eld' sidecars.
`ask' prompts before a union merge keeping both sides (the default), `union'
merges silently, nil signals an error."
  :group 'org-glance
  :type '(choice (const :tag "Prompt to approve" ask)
                 (const :tag "Auto-resolve (union)" union)
                 (const :tag "Never (error)" nil)))

(cl-defun org-glance--conflict-marked? (text)
  "Non-nil when TEXT carries git conflict markers."
  (and text (string-match-p org-glance--conflict-marker-re text)))

(cl-defun org-glance--strip-conflict-markers (text)
  "Return TEXT minus its git conflict-marker lines: the union of both sides."
  (replace-regexp-in-string
   (concat org-glance--conflict-marker-re ".*\n?") "" text))

(cl-defun org-glance--resolve-conflict (subject resolve-fn)
  "Heal a git conflict in SUBJECT by calling RESOLVE-FN; return its value.
Per `org-glance-conflict-resolution', `union' calls it silently, `ask' prompts
first, naming SUBJECT, and nil or a declined prompt signals an error."
  (pcase org-glance-conflict-resolution
    ('nil (error "org-glance: unresolved git conflict in %s" subject))
    (mode
     (unless (or (eq mode 'union)
                 (y-or-n-p (format "org-glance: git conflict in %s -- \
resolve by union merge (keep data from both sides)? " subject)))
       (error "org-glance: git conflict in %s left unresolved" subject))
     (funcall resolve-fn))))

(cl-defun org-glance--read-eld-forms (text)
  "Return the forms read from TEXT, conflict markers stripped, until one fails."
  (let ((clean (org-glance--strip-conflict-markers text))
        (pos 0) forms)
    (ignore-errors
      (let ((len (length clean)))
        (while (< pos len)
          (pcase-let ((`(,form . ,next) (read-from-string clean pos)))
            (push form forms)
            (setq pos next)))))
    (nreverse forms)))

(cl-defun org-glance--read-eld-with (path conflict-fn)
  "Read the single form of the .eld PATH, or nil when absent or unreadable.
For a conflict-marked PATH, return CONFLICT-FN called on its raw text instead."
  (when (f-exists? path)
    (let ((text (f-read-text path 'utf-8)))
      (if (org-glance--conflict-marked? text)
          (funcall conflict-fn text)
        (ignore-errors (car (read-from-string text)))))))

(cl-defun org-glance--read-eld (path)
  "Read the single Lisp form in the .eld PATH, or nil when absent or unreadable.
On a git conflict return the first `consp' side; `--heal-eld' unions the sides."
  (org-glance--read-eld-with
   path
   (lambda (text) (cl-find-if #'consp (org-glance--read-eld-forms text)))))

(cl-defun org-glance--atomic-write (path content &optional (overwrite t))
  "Write CONTENT to a temp file beside PATH, then rename it over PATH.
OVERWRITE nil makes the rename refuse an existing PATH (invariant 2).  A write
or rename that fails removes the temp file."
  (let ((tmp (make-temp-name (concat path ".tmp.")))
        (done nil))
    (unwind-protect
        (progn (f-write-text content 'utf-8 tmp)
               (rename-file tmp path overwrite)
               (setq done t))
      (unless done (ignore-errors (delete-file tmp))))))

(cl-defun org-glance--write-eld (path form)
  "Serialize FORM to the .eld PATH atomically, creating parent dirs."
  (f-mkdir-full-path (f-dirname path))
  (org-glance--atomic-write path (prin1-to-string form)))

(cl-defun org-glance--eld-alist-ref (path key)
  "Return KEY's value in the alist at .eld PATH (`equal' keys), or nil."
  (alist-get key (org-glance--read-eld path) nil nil #'equal))

(cl-defun org-glance--eld-alist-set (path key value)
  "Upsert KEY -> VALUE in the alist at .eld PATH; a nil VALUE drops KEY."
  (let ((all (cl-remove key (org-glance--read-eld path) :key #'car :test #'equal)))
    (when value (setq all (cons (cons key value) all)))
    (org-glance--write-eld path all)))

(cl-defun org-glance--heal-eld (path merge-fn &optional subject)
  "Read the .eld at PATH, union-resolving a git conflict through MERGE-FN.
On conflict, gated by `--resolve-conflict' naming SUBJECT (default the file
name), write back and return MERGE-FN applied to the list of side forms."
  (org-glance--read-eld-with
   path
   (lambda (text)
     (let ((subject (or subject (file-name-nondirectory path))))
       (org-glance--resolve-conflict
        subject
        (lambda ()
          (let ((merged (funcall merge-fn (org-glance--read-eld-forms text))))
            (org-glance--write-eld path merged)
            (message "org-glance: union-resolved git conflict in %s" subject)
            merged)))))))

(cl-defun org-glance--buffer-links ()
  "Return the buffer's links, in order, as (RAW-TEXT TYPE UNESCAPED-PATH)."
  (cl-loop for link-element in (org-element-map (org-element-parse-buffer) 'link #'identity)
           for beg = (org-element-property :begin link-element)
           for end = (org-element-property :end link-element)
           collect (list (s-trim (buffer-substring-no-properties beg end))
                         (org-element-property :type link-element)
                         (org-element-property :path link-element))))

;;; Relation edges: the `org-glance-material:' body link is canonical (inv 5).

(defconst org-glance-link-material-type "org-glance-material"
  "Org link type materializing a headline by id; the canonical edge form.")

(defconst org-glance--link-edge-types
  (list org-glance-link-material-type "org-glance-visit")
  "Link types that denote a relation edge to another headline.")

(cl-defun org-glance--kind-slug (kind)
  "Return relation KIND as a slug: trimmed, downcased, blank spans to dashes.
Invariant 13 applies it at every boundary: \"Roasted By\" -> \"roasted-by\"."
  (replace-regexp-in-string "[ \t]+" "-" (downcase (s-trim kind))))

(cl-defun org-glance--kind-pretty (kind)
  "Return the display form of KIND slug: dashes back to spaces."
  (replace-regexp-in-string "-" " " kind))

(cl-defun org-glance--link-edge (type path)
  "Return the edge (TARGET-ID . KIND-or-nil) a TYPE/PATH link denotes, or nil."
  (when (and (member type org-glance--link-edge-types)
             (stringp path)
             (string-match "\\`\\([^?]+\\)\\(?:\\?kind=\\(.+\\)\\)?\\'" path))
    (cons (match-string 1 path)
          (when-let* ((kind (match-string 2 path)))
            (org-glance--kind-slug kind)))))

(cl-defun org-glance--edge->link-path (id &optional kind)
  "Return the link path TYPE:ID[?kind=SLUG] of an edge to ID of KIND."
  (concat org-glance-link-material-type ":" id
          (and kind (concat "?kind=" (org-glance--kind-slug kind)))))

(cl-defun org-glance--edge->string (id kind title)
  "Return an edge to ID as prose: pretty KIND, if any, and a link titled TITLE."
  (concat (and kind (concat (org-glance--kind-pretty kind) " "))
          (org-link-make-string (org-glance--edge->link-path id kind) title)))

(cl-defun org-glance--links-partition (links)
  "Partition `--buffer-links' LINKS into (EDGES . PLAIN).
EDGES are the distinct (TARGET . KIND) edges, PLAIN the raw text of the rest."
  (cl-loop for (text type path) in links
           if (member type org-glance--link-edge-types)
           collect (org-glance--link-edge type path) into edges
           else collect text into plain
           finally return (cons (-distinct (delq nil edges)) plain)))

(defsubst org-glance--downcased-string (x)
  "Coerce X to a downcased string, the canonical tag/title form (invariant 13)."
  (downcase (format "%s" x)))

(cl-defun org-glance--strings (values)
  "Coerce VALUES to strings, case kept; comparisons use `--downcased-string'."
  (mapcar (lambda (value) (format "%s" value)) values))

(cl-defun org-glance--buffer-key-value-pairs ()
  "Return (KEY . VALUE) of every line matching `org-glance:key-value-pair-re'."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward org-glance:key-value-pair-re nil t)
             for key = (s-trim (substring-no-properties (match-string 1)))
             for value = (s-trim (substring-no-properties (match-string 2)))
             collect (cons key value))))

(cl-defun org-glance--item-body-start (item)
  "Return where ITEM's text begins, past its bullet, counter and checkbox."
  (save-excursion
    (goto-char (org-element-property :begin item))
    (when (looking-at org-list-full-item-re)
      (goto-char (match-end 0)))
    (point)))

(cl-defun org-glance--item-label (item)
  "Return plain-list ITEM's first-line text, bullet and checkbox stripped."
  (save-excursion
    (goto-char (org-glance--item-body-start item))
    (s-trim (buffer-substring-no-properties (point) (line-end-position)))))

(cl-defun org-glance--link-ancestry (element)
  "Return the labels of ELEMENT's enclosing list items, outermost first.
The innermost, the link's own item, is dropped (invariant 25)."
  (mapcar #'org-glance--item-label
          (nreverse (cdr (cl-loop for parent = (org-element-property :parent element)
                                  then (org-element-property :parent parent)
                                  while parent
                                  when (org-element-type-p parent 'item)
                                  collect parent)))))

(cl-defun org-glance--link-item-prefix (element)
  "Return the text introducing ELEMENT inside its own list item, or nil.
That is the item text before it, other links and a trailing `:' or `-' removed."
  (when-let* ((item (org-element-lineage element '(item))))
    (let ((start (org-glance--item-body-start item))
          (end (org-element-property :begin element)))
      (when (< start end)
        (let ((text (s-trim (replace-regexp-in-string
                             org-link-bracket-re ""
                             (buffer-substring-no-properties start end)))))
          (unless (string-empty-p text)
            (let ((label (s-trim (replace-regexp-in-string "[:-]+\\'" "" text))))
              (unless (string-empty-p label) label))))))))

(cl-defun org-glance--link-label (element)
  "Return the label naming ELEMENT in the link picker (invariant 25).
Its description, else its `org-glance--link-item-prefix', else the raw link."
  (let ((description (-some->> element
                       (org-element-contents)
                       (org-element-interpret-data)
                       (substring-no-properties)
                       (s-trim))))
    (or (and (org-glance--present-string? description) description)
        (org-glance--link-item-prefix element)
        (substring-no-properties (org-element-property :raw-link element)))))

(cl-defun org-glance--link-paths ()
  "Return the buffer's links as (PATH POS TYPE TARGET) tuples (invariant 25).
PATH is the enclosing item labels plus `--link-label'; TARGET is the raw link."
  (cl-loop for element in (org-element-map (org-element-parse-buffer) 'link #'identity)
           collect (list (append (org-glance--link-ancestry element)
                                 (list (s-trim (org-glance--link-label element))))
                         (org-element-property :begin element)
                         (org-element-property :type element)
                         (org-element-property :raw-link element))))

(cl-defun org-glance--pick-link-pos (entries)
  "Prompt through ENTRIES ((PATH POS TYPE TARGET)...); return the chosen POS.
One PATH component per prompt, a lone candidate taken unasked, exhausted ties
offered by TARGET (invariant 25).  Empty ENTRIES signal a `user-error'."
  (cond
   ((null entries) (user-error "No links in headline"))
   ((null (cdr entries)) (cadr (car entries)))
   ((null (car (car entries)))
    (let ((by-target (cl-loop for entry in entries
                              collect (cons (format "%s" (or (nth 3 entry) (cadr entry)))
                                            entry))))
      (cadr (cdr (assoc (completing-read "Open link: " (mapcar #'car by-target) nil t)
                        by-target)))))
   (t
    (let* ((groups (--group-by (or (car (car it)) "") entries))
           (chosen (if (null (cdr groups))
                       (cdr (car groups))   ; one branch: descend, do not ask
                     (cdr (assoc (completing-read "Open link: " (mapcar #'car groups) nil t)
                                 groups)))))
      (org-glance--pick-link-pos
       (mapcar (lambda (entry) (cons (cdr (car entry)) (cdr entry))) chosen))))))

;;; Crypt blocks: per-block secrecy; plaintext outside stays indexed (inv 14).

(defconst org-glance--crypt-begin-re "^[ \t]*#\\+begin_crypt[ \t]*$"
  "Regexp matching a crypt block's opening marker line (case-insensitive use).")

(defconst org-glance--crypt-end-re "^[ \t]*#\\+end_crypt[ \t]*$"
  "Regexp matching a crypt block's closing marker line (case-insensitive use).")

(defconst org-glance--aes-header-re "aes-encrypted V [0-9]+\\.[0-9]+-.+"
  "Regexp matching the first line of `aes.el' ciphertext.")

(defun org-glance--crypt-block-regions ()
  "Return the buffer's crypt blocks, in order, as plists.
Each is (:beg B :body-beg BB :body-end BE :end E): B/E bound the marker lines,
E excluding its newline, BB/BE the body.  An unclosed block is ignored."
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
  "Return the crypt block containing POS, markers included, or nil."
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
  "Replace each body whose sealed state equals SEALED with (TRANSFORM BODY).
Iterates last-to-first (invariant 28)."
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
  "Wrap BEG..END in crypt block markers, each on a line of its own.
The begin marker opens BEG's line; the end marker splits END's line if needed."
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
  "Remove every crypt block's marker lines, keeping the bodies.
Iterates last-to-first (invariant 28)."
  (dolist (block (reverse (org-glance--crypt-block-regions)))
    (org-glance--crypt-unwrap-block block)))

(defun org-glance--discard-buffer (buffer)
  "Kill BUFFER without the `Buffer modified; kill anyway?' confirmation.
Only for a buffer org-glance owns; no-op when BUFFER is already dead."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (set-buffer-modified-p nil))
    (kill-buffer buffer)))

(defun org-glance--kill-buffer-noconfirm ()
  "Clear the current buffer's modified flag and return t.
Installed buffer-locally on `kill-buffer-query-functions' of a buffer
org-glance owns, it silences the modified-buffer prompt on every kill path."
  (set-buffer-modified-p nil)
  t)

(provide 'org-glance-utils)
