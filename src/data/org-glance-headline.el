;; -*- lexical-binding: t -*-

(require 's)
(require 'dash)
(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'thunk)
(require 'org-clock)

(require 'org-glance-utils)
(require 'org-glance-tag)

(unless (fboundp 'org-element-type-p)
  (cl-defun org-element-type-p (node types)
    (cl-typecase node
      (list (member (car node) (cl-typecase types
                                 (list types)
                                 (symbol (list types)))))
      (otherwise nil))))

(defconst org-glance-headline:hash-ignore-properties (list "ORG_GLANCE_ID" "ORG_GLANCE_HASH" "ORG_GLANCE_CREATION_TIME"))

(defconst org-glance-headline:hash-ignore-drawers (list "LOGBOOK")
  "Drawer names whose contents never affect the content hash (invariant 5).")

(cl-defstruct (org-glance-headline (:predicate org-glance-headline?)
                                      (:conc-name org-glance-headline:))
  (contents nil :read-only t :type string)
  (id nil :read-only t :type string)
  (state nil :read-only t :type string)
  (tags nil :read-only t :type list)
  (title nil :read-only t :type string)
  (priority nil :read-only t :type number)
  (-schedule nil :read-only t :type (or null list))
  (-deadline nil :read-only t :type (or null list))

  (archived? nil :read-only t :type bool)
  (commented? nil :read-only t :type bool)

  ;; (CONTENTS . PLIST), keyed by the exact string it was computed from.
  (-facts nil :read-only t :type list)

  (-hash nil :read-only t :type (or string function))
  (-encrypted? nil :read-only t :type (or bool function))
  (-properties nil :read-only t :type (or list function))
  (-node-properties nil :read-only t :type (or list function)))

(cl-defun org-glance--org-mode ()
  "Enter `org-mode' for parsing org-glance content, skipping mode hooks.
Then set `tab-width' to 8, which org's parser requires and `org-mode' resets,
and disable `indent-tabs-mode' (invariant 12)."
  (delay-mode-hooks (org-mode))
  (setq tab-width 8 indent-tabs-mode nil))

(cl-defmacro org-glance-headline:with-contents (contents &rest forms)
  "Run FORMS in a scratch org buffer holding CONTENTS, a headline or string.
CONTENTS is evaluated once, in the caller's buffer."
  (declare (indent 1))
  (let ((value (gensym "contents")))
    `(let ((,value ,contents))
       (with-temp-buffer
         (insert (cl-typecase ,value
                   (org-glance-headline (org-glance-headline:contents ,value))
                   (string ,value)
                   (otherwise (error "Expected `org-glance-headline' or string, but got %s"
                                     (type-of ,value)))))
         (org-glance--org-mode)
         (goto-char (point-min))
         ,@forms))))

(cl-defun org-glance-headline:at-point ()
  (save-excursion
    (cl-loop initially (or (org-at-heading-p) (org-back-to-heading-or-point-min))
             while (org-at-heading-p)
             for element = (org-element-at-point)
             if (org-element-type-p element 'headline)
             return (org-glance-headline--from-element element)
             else if (or (org-before-first-heading-p) (bobp))
             do (error "Unable to find `org-glance-headline' at point")
             else
             do (org-up-heading-or-point-min))))

(cl-defun org-glance-headline:encrypted? (headline)
  (let ((encrypted? (org-glance-headline:-encrypted? headline)))
    (cl-typecase encrypted?
      (boolean encrypted?)
      (function (thunk-force encrypted?))
      (otherwise (error "Lazy evaluation failed: `org-glance-headline:encrypted?'")))))

(cl-defun org-glance-headline:hash (headline)
  (thunk-force (org-glance-headline:-hash headline)))

(cl-defun org-glance-headline:properties (headline)
  (thunk-force (org-glance-headline:-properties headline)))

(cl-defun org-glance-headline:get-user-property (property headline)
  (alist-get property (org-glance-headline:properties headline) nil nil #'string=))

(cl-defun org-glance-headline:node-properties (headline)
  "Return HEADLINE's `:PROPERTIES:' drawer as an alist, keys uppercased.
Body `KEY: value' lines are `org-glance-headline:properties'."
  (thunk-force (org-glance-headline:-node-properties headline)))

(cl-defun org-glance-headline:node-property (property headline)
  "Return HEADLINE's drawer PROPERTY, matched case-insensitively, or nil."
  (alist-get (upcase property) (org-glance-headline:node-properties headline)
             nil nil #'string=))

(cl-defun org-glance-headline:schedule (headline)
  "Return HEADLINE's scheduled timestamp as a raw org string, or nil."
  (cl-check-type headline org-glance-headline)
  (-some->> (org-glance-headline:-schedule headline)
    (org-element-property :raw-value)))

(cl-defun org-glance-headline:deadline (headline)
  "Return HEADLINE's deadline timestamp as a raw org string, or nil."
  (cl-check-type headline org-glance-headline)
  (-some->> (org-glance-headline:-deadline headline)
    (org-element-property :raw-value)))


(defun org-glance-headline--buffer-propertized? ()
  "Non-nil if the current buffer's headline body has a `KEY: value' pair."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward org-glance:key-value-pair-re nil t) t)))

(defun org-glance-headline--buffer-encrypted? ()
  "Non-nil when a crypt block is sealed or the whole body is legacy ciphertext."
  (or (org-glance-headline--crypt-legacy-cipher-p)
      (org-glance--crypt-sealed-blocks-p)))

(defun org-glance-headline--hash-log-drawers ()
  "Return the drawer names stripped before hashing.
`org-glance-headline:hash-ignore-drawers' plus those `org-log-into-drawer' and
`org-clock-into-drawer' name; t means LOGBOOK, an integer none."
  (delete-dups
   (delq nil (append org-glance-headline:hash-ignore-drawers
                     (mapcar (lambda (v) (cond ((stringp v) v) ((eq v t) "LOGBOOK")))
                             (list (bound-and-true-p org-log-into-drawer)
                                   (bound-and-true-p org-clock-into-drawer)))))))

(defun org-glance-headline--delete-log-drawers ()
  "Delete the current buffer's log drawers, contents and all."
  (let ((case-fold-search t)
        (re (concat "^[ \t]*:" (regexp-opt (org-glance-headline--hash-log-drawers) t)
                    ":[ \t]*$")))
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (let ((beg (match-beginning 0)))
        (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
          (delete-region beg (min (point-max) (1+ (line-end-position)))))))))

(defun org-glance-headline--buffer-hash ()
  "Return the buffer's content hash, sans ignored properties and log drawers.
Stripping them MUTATES the buffer: call it LAST on a shared one."
  (goto-char (point-min))
  (dolist (property org-glance-headline:hash-ignore-properties)
    (org-entry-delete nil property))
  (org-glance-headline--delete-log-drawers)
  (let ((data (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (with-temp-buffer (insert data) (buffer-hash))))

(cl-defmacro org-glance-headline--lazy-fact (contents &rest body)
  "Return a thunk evaluating BODY in an org buffer holding string CONTENTS."
  (declare (indent 1))
  `(progn
     (cl-check-type ,contents string)
     (thunk-delay (org-glance-headline:with-contents ,contents ,@body))))

(cl-defun org-glance-headline--hash (contents)
  (org-glance-headline--lazy-fact contents (org-glance-headline--buffer-hash)))

(defun org-glance-headline--buffer-properties ()
  "Return the body `KEY: value' pairs of the headline filling a scratch buffer.
Deletes its log drawers first: no fact sees what the hash ignores (invariant 5)."
  (org-glance-headline--delete-log-drawers)
  (org-glance--buffer-key-value-pairs))

(cl-defun org-glance-headline--properties (contents)
  (org-glance-headline--lazy-fact contents (org-glance-headline--buffer-properties)))

(cl-defun org-glance-headline--node-properties (contents)
  (org-glance-headline--lazy-fact contents (org-entry-properties nil 'standard)))

(cl-defun org-glance-headline--encrypted (contents)
  (org-glance-headline--lazy-fact contents (org-glance-headline--buffer-encrypted?)))

(defconst org-glance-headline--contents-derived-slots
  `((-facts           . nil)
    (-hash            . ,#'org-glance-headline--hash)
    (-properties      . ,#'org-glance-headline--properties)
    (-node-properties . ,#'org-glance-headline--node-properties)
    (-encrypted?      . ,#'org-glance-headline--encrypted))
  "Slots derived from `contents', as (SLOT . BUILDER); nil BUILDER resets SLOT.
`org-glance-headline--copy' rebuilds them when it replaces `:contents'.")

(defconst org-glance-headline--content-fact-keys
  '(:relations :links :linked :propertized :encrypted :range :hash)
  "Keys `org-glance-headline--content-facts' returns.
The FROM vocabulary of `org-glance-headline-metadata:fields', checked at load.")

(cl-defun org-glance-headline--content-facts (headline)
  "Return HEADLINE's content-derived facts plist.
Reuse its `-facts' memo when keyed by its own (`eq') contents, else compute."
  (let ((memo (org-glance-headline:-facts headline)))
    (if (and memo (eq (car memo) (org-glance-headline:contents headline)))
        (cdr memo)
      (org-glance-headline:with-contents headline
        (org-glance-headline--buffer-content-facts)))))

(cl-defun org-glance-headline--buffer-content-facts ()
  "Return the `--content-fact-keys' plist of the headline filling the buffer.
One pass over the text the hash reads: log drawers go first (invariant 5), the
hash runs LAST.  Both mutate the buffer, so it must be a scratch one."
  (org-glance-headline--delete-log-drawers)
  (goto-char (point-min))
  (pcase-let* ((links (org-glance--buffer-links))
               (`(,edges . ,plain) (org-glance--links-partition links)))
    (list :relations   edges
          :links       plain
          :linked      (and links t)
          :propertized (org-glance-headline--buffer-propertized?)
          :encrypted   (org-glance-headline--buffer-encrypted?)
          :range       (org-glance-headline--buffer-range)
          :hash        (org-glance-headline--buffer-hash))))

(cl-defun org-glance-headline--buffer-range ()
  "Return the body's first active range as bracketed (FROM TO), or nil.
The search starts past the heading's meta-data, so title, planning and drawers
never project; it spans descendants, so a child's range can.  Sealed crypt
blocks hide theirs (invariant 14)."
  (save-excursion
    (goto-char (point-min))
    (org-end-of-meta-data t)
    (when (re-search-forward org-tr-regexp nil t)
      (list (concat "<" (match-string-no-properties 1) ">")
            (concat "<" (match-string-no-properties 2) ">")))))

(cl-defun org-glance-headline--from-string (contents)
  (cl-check-type contents string)
  (org-glance-headline:with-contents contents      ; with-contents already entered org-mode
    (unless (or (org-at-heading-p) (re-search-forward org-heading-regexp nil t))
      (error "Unable to find `org-element' of type `headline' in the provided contents"))
    (let* ((headline (org-glance-headline:at-point))
           (parsed (org-glance-headline:contents headline)))
      ;; Runs AFTER `at-point' captured CONTENTS -- the hash pass mutates.
      ;; Exact equality: computing here equals a fresh CONTENTS-only buffer.
      (if (equal parsed (buffer-substring-no-properties (point-min) (point-max)))
          (org-glance-headline--copy headline
            :-facts (cons parsed (org-glance-headline--buffer-content-facts)))
        headline))))

(cl-defun org-glance-headline--from-lines (&rest lines)
  (declare (indent 0))
  (org-glance-headline--from-string (s-join "\n" lines)))

(cl-defun org-glance-headline--from-element (element)
  "Create `org-glance-headline' from `org-element' ELEMENT."
  (let ((id (org-element-property :ORG_GLANCE_ID element))
        ;; Invariant 29: org's ARCHIVE marker is bookkeeping, never a tag.
        (tags (delete-dups
               (cl-remove (org-glance-tag:from-string org-archive-tag)
                          (mapcar #'org-glance-tag:from-string
                                  (org-element-property :tags element)))))
        (archived? (not (null (org-element-property :archivedp element))))
        (commented? (not (null (org-element-property :commentedp element))))
        (state (substring-no-properties (or (org-element-property :todo-keyword element) "")))
        (priority (org-element-property :priority element))
        (schedule (org-element-property :scheduled element))
        (deadline (org-element-property :deadline element))
        (title (or (org-element-property :ORG_GLANCE_TITLE element)
                   (org-element-property :TITLE element)
                   (org-element-property :raw-value element)
                   ""))
        (contents (let ((buffer (or (org-element-property :buffer element) (current-buffer)))
                        (begin (org-element-property :begin element))
                        (end (org-element-property :end element)))
                    (with-current-buffer buffer
                      (buffer-substring-no-properties begin end)))))
    (apply #'make-org-glance-headline
           :id id
           :title title
           :tags tags
           :state state
           :priority priority
           :-schedule schedule
           :-deadline deadline
           :contents contents
           :archived? archived?
           :commented? commented?
           (cl-loop for (slot . builder) in org-glance-headline--contents-derived-slots
                    when builder
                    append (list (intern (format ":%s" slot))
                                 (funcall builder contents))))))

(cl-defun org-glance-headline--copy (headline &rest update-plist)
  "Copy HEADLINE, replacing the slots UPDATE-PLIST names.
A new `:contents' rebuilds each `--contents-derived-slots' slot left out."
  (declare (indent 1))
  (cl-check-type headline org-glance-headline)
  (when (plist-member update-plist :contents)
    (let ((contents (plist-get update-plist :contents)))
      (pcase-dolist (`(,slot . ,builder) org-glance-headline--contents-derived-slots)
        (let ((key (intern (format ":%s" slot))))
          (unless (plist-member update-plist key)
            (setq update-plist
                  (plist-put update-plist key
                             (and builder (funcall builder contents)))))))))
  (cl-loop for slot-info in (cdr (cl-struct-slot-info 'org-glance-headline))
           for slot-name = (car slot-info)
           for slot-property = (intern (format ":%s" slot-name))
           for slot-value = (if (plist-member update-plist slot-property)
                                (plist-get update-plist slot-property)
                              (cl-struct-slot-value 'org-glance-headline slot-name headline))
           collect slot-property into params
           collect slot-value into params
           finally (return (apply #'make-org-glance-headline params))))

(cl-defmacro org-glance-headline--rewrite-contents (headline &rest body)
  "Return HEADLINE's contents, trimmed, after BODY edits them in an org buffer."
  (declare (indent 1))
  `(org-glance-headline:with-contents ,headline
     ,@body
     (s-trim (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defmacro org-glance-headline--map-contents (headline &rest body)
  "Return HEADLINE re-parsed after BODY edits its contents in an org buffer."
  (declare (indent 1))
  `(org-glance-headline--from-string
    (org-glance-headline--rewrite-contents ,headline ,@body)))

(cl-defun org-glance-headline--body-region ()
  "Return the headline body as (BEG . END): end of meta-data to end of subtree."
  (cons (save-excursion (goto-char (point-min)) (org-end-of-meta-data t) (point))
        (save-excursion (goto-char (point-min)) (org-end-of-subtree t) (point))))

(defun org-glance-headline--crypt-legacy-cipher-p ()
  "Non-nil for the legacy layout: no crypt block, ciphertext at body start."
  (and (null (org-glance--crypt-block-regions))
       (save-excursion
         (goto-char (point-min))
         (org-end-of-meta-data t)
         (looking-at org-glance--aes-header-re))))

(defun org-glance-headline--crypt-upgrade-legacy ()
  "Wrap a legacy whole-body cipher in one crypt block; return non-nil if done.
The result is a sealed block.  A plaintext body never matches, so never wraps."
  (when (org-glance-headline--crypt-legacy-cipher-p)
    (let ((body (org-glance-headline--body-region)))
      (org-glance--crypt-wrap-region (car body) (cdr body)))
    t))

(cl-defun org-glance-headline:encrypt (headline password)
  "Return HEADLINE with its crypt blocks sealed under PASSWORD (invariant 14).
With no block, first wrap the whole body in one.  An already-encrypted HEADLINE
returns unchanged."
  (cl-check-type headline org-glance-headline)
  (cl-check-type password string)
  (if (org-glance-headline:encrypted? headline)
      headline
    (org-glance-headline--copy headline
      :contents (org-glance-headline--rewrite-contents headline
                  (unless (org-glance--crypt-block-regions)
                    (let ((body (org-glance-headline--body-region)))
                      (org-glance--crypt-wrap-region (car body) (cdr body))))
                  (org-glance--crypt-seal-blocks password))
      :-encrypted? t)))

(cl-defun org-glance-headline:decrypt (headline password &optional unwrap)
  "Return HEADLINE with every sealed block opened under PASSWORD.
Upgrade a legacy whole-body cipher to one block first; UNWRAP drops the markers.
A wrong PASSWORD signals a `user-error'; unencrypted HEADLINE returns as is."
  (cl-check-type headline org-glance-headline)
  (cl-check-type password string)
  (if (not (org-glance-headline:encrypted? headline))
      headline
    (org-glance-headline--copy headline
      :contents (org-glance-headline--rewrite-contents headline
                  (org-glance-headline--crypt-upgrade-legacy)
                  (org-glance--crypt-unseal-blocks password)
                  (when unwrap (org-glance--crypt-unwrap-blocks)))
      :-encrypted? nil)))

(cl-defun org-glance-headline:search-forward (id)
  (cl-check-type id string)
  (save-excursion
    (cl-loop while (re-search-forward (concat ":ORG_GLANCE_ID:[ \t]+" id) nil 'no-error)
             for headline = (org-glance-headline:at-point)
             when (and (org-glance-headline? headline)
                       (string= id (org-glance-headline:id headline)))
             return headline)))

(cl-defun org-glance--title-clean (title)
  "Return TITLE with each link rendered as its description, else its target."
  (cl-check-type title string)
  (replace-regexp-in-string
   org-link-bracket-re
   (lambda (match) (or (match-string 2 match) (match-string 1 match) ""))
   title))

(provide 'org-glance-headline)
