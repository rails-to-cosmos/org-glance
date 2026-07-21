;; -*- lexical-binding: t -*-

(require 's)
(require 'dash)
(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'cl-macs)
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

(defconst org-glance-headline:hash-ignore-properties (list "ORG_GLANCE_ID" "ORG_GLANCE_HASH"))

(cl-defstruct (org-glance-headline (:predicate org-glance-headline?)
                                      (:conc-name org-glance-headline:))
  (contents nil :read-only t :type string)
  (id nil :read-only t :type string)
  (state nil :read-only t :type string)
  (tags nil :read-only t :type list)
  (title nil :read-only t :type string)
  (priority nil :read-only t :type number)
  (indent nil :read-only t :type number)
  ;; Hold the parsed org-element timestamp object (or nil); the public
  ;; `org-glance-headline:schedule' / `:deadline' methods expose raw strings.
  (-schedule nil :read-only t :type (or null list))
  (-deadline nil :read-only t :type (or null list))

  ;; Metadata
  (archived? nil :read-only t :type bool)
  (closed nil :read-only t :type (or null string))
  (commented? nil :read-only t :type bool)

  ;; Lazy attributes start with "-". Each has a builder: `org-glance-headline--<slot-name>'
  (-hash nil :read-only t :type (or string function))
  (-encrypted? nil :read-only t :type (or bool function))
  (-properties nil :read-only t :type (or list function))
  (-node-properties nil :read-only t :type (or list function)))

(cl-defun org-glance--org-mode ()
  "Enter `org-mode' for parsing org-glance content in the current buffer.
Skips mode hooks (faster; avoids globalized minor modes like undo-tree) and
forces `tab-width' to 8 -- which org's parser REQUIRES, and which a fresh buffer
(inheriting the user's default, often 4) lacks; `org-mode' itself resets it via
`kill-all-local-variables', so it is set here, after.  Tabs are disabled."
  (delay-mode-hooks (org-mode))
  (setq tab-width 8 indent-tabs-mode nil))

(cl-defmacro org-glance-headline:with-contents (contents &rest forms)
  (declare (indent 1))
  `(with-temp-buffer
     (insert (cl-typecase ,contents
               (org-glance-headline (org-glance-headline:contents ,contents))
               (string ,contents)
               (otherwise (error "Expected `org-glance-headline' or string, but got %s" (type-of ,contents)))))
     ;; org's parser requires tab-width 8; ensure it before any form parses.
     (org-glance--org-mode)
     (goto-char (point-min))
     ,@forms))

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
  "Alist of HEADLINE's org `:PROPERTIES:' drawer properties (keys uppercased).
Distinct from `org-glance-headline:properties', which parses body `KEY: value'
lines for the extract feature; this reads the property drawer."
  (thunk-force (org-glance-headline:-node-properties headline)))

(cl-defun org-glance-headline:node-property (property headline)
  "Value of HEADLINE's drawer PROPERTY (an org `:PROPERTIES:' key), or nil.
PROPERTY is matched case-insensitively (e.g. \"TAG\", \"ORG_GLANCE_ID\")."
  (alist-get (upcase property) (org-glance-headline:node-properties headline)
             nil nil #'string=))

(cl-defun org-glance-headline:schedule (headline)
  "Scheduled timestamp of HEADLINE as a raw org string, or nil."
  (cl-check-type headline org-glance-headline)
  (-some->> (org-glance-headline:-schedule headline)
    (org-element-property :raw-value)))

(cl-defun org-glance-headline:deadline (headline)
  "Deadline timestamp of HEADLINE as a raw org string, or nil."
  (cl-check-type headline org-glance-headline)
  (-some->> (org-glance-headline:-deadline headline)
    (org-element-property :raw-value)))

;; The four content-derived facts, each computed in the CURRENT `org-mode' buffer
;; (holding one headline).  `--content-facts' composes all four in one shared
;; buffer for the metadata build; the -hash/-encrypted thunks reuse the same
;; helpers, so each computation has a single definition.

(defun org-glance-headline--propertized-here ()
  "Non-nil if the current buffer's headline body has a `KEY: value' pair."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward org-glance:key-value-pair-re nil t) t)))

(defun org-glance-headline--encrypted-here ()
  "Non-nil if the current buffer's headline carries ciphertext.
Either the whole body is `aes-encrypted' (the legacy layout) or at least one
`#+begin_crypt' block is sealed (see `org-glance--crypt-block-regions')."
  (or (org-glance-headline--crypt-legacy-cipher-p)
      (org-glance--crypt-sealed-blocks-p)))

(defun org-glance-headline--hash-here ()
  "Content hash of the current buffer with the id/hash drawer properties removed.
MUTATES the buffer (deletes those properties), so call it LAST when sharing one."
  (goto-char (point-min))
  (dolist (property org-glance-headline:hash-ignore-properties)
    (org-entry-delete nil property))
  (let ((data (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (with-temp-buffer (insert data) (buffer-hash))))

(cl-defun org-glance-headline--hash (contents)
  (cl-check-type contents string)
  (thunk-delay (org-glance-headline:with-contents contents
                 (org-glance-headline--hash-here))))

(cl-defun org-glance-headline--properties (contents)
  (cl-check-type contents string)
  (thunk-delay (org-glance-headline:with-contents contents
                 (org-glance--buffer-key-value-pairs))))

(cl-defun org-glance-headline--node-properties (contents)
  (cl-check-type contents string)
  (thunk-delay (org-glance-headline:with-contents contents
                 (org-entry-properties nil 'standard))))

(cl-defun org-glance-headline--encrypted (contents)
  (cl-check-type contents string)
  (thunk-delay (org-glance-headline:with-contents contents
                 (org-glance-headline--encrypted-here))))

(cl-defun org-glance-headline--content-facts (headline)
  "HEADLINE's content-derived metadata facts, in ONE org-mode pass.
Returns (:relations RS :links LS :linked L :propertized P :encrypted E
:range R :hash H), sharing one `with-contents' buffer + `org-mode' init
across all six (the
store's metadata build reparses the same blob otherwise).  The links parse
once, feeding both `linked?' and the relation edges.  Hash is LAST: it deletes
the id/hash drawer properties in place, after the read-only facts."
  (org-glance-headline:with-contents headline
    (pcase-let* ((links (org-glance--buffer-links))   ; with-contents is at point-min
                 (`(,edges . ,plain) (org-glance--links-partition links)))
      (list :relations   edges
            :links       plain
            :linked      (and links t)
            :propertized (org-glance-headline--propertized-here)
            :encrypted   (org-glance-headline--encrypted-here)
            :range       (org-glance-headline--range-here)
            :hash        (org-glance-headline--hash-here)))))

(cl-defun org-glance-headline--range-here ()
  "The BODY's first active date range as (FROM TO) with brackets, or nil.
The headline's interval (`org-tr-regexp').  The search starts after the
heading's meta-data (`org-end-of-meta-data'), so a range in the title, a
planning line, the property drawer or the LOGBOOK never projects; ranges
inside sealed crypt blocks are invisible by construction (invariant 14).
The scan spans the whole record -- a parent's contents include its
descendants (like every content fact), so a child's range can project."
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
    (org-glance-headline:at-point)))

(cl-defun org-glance-headline--from-lines (&rest lines)
  (declare (indent 0))
  (cl-check-type lines list)
  (org-glance-headline--from-string (apply (lambda (&rest tokens) (s-join "\n" tokens)) lines)))

(cl-defun org-glance-headline--from-element (element)
  "Create `org-glance-headline' from `org-element' ELEMENT."
  (let ((id (org-element-property :ORG_GLANCE_ID element))
        ;; org's ARCHIVE marker tag is bookkeeping (the `archived?' flag below
        ;; carries it) -- never a collection tag, so no phantom pickable
        ;; \"archive\" view of headlines the ambient filter hides.
        (tags (delete-dups
               (cl-remove (org-glance-tag:from-string org-archive-tag)
                          (mapcar #'org-glance-tag:from-string
                                  (org-element-property :tags element)))))
        (archived? (not (null (org-element-property :archivedp element))))
        (commented? (not (null (org-element-property :commentedp element))))
        (closed (org-element-property :closed element))
        (state (substring-no-properties (or (org-element-property :todo-keyword element) "")))
        (priority (org-element-property :priority element))
        (indent (or (org-element-property :level element) 1))
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
    (make-org-glance-headline :id id
                                 :title title
                                 :tags tags
                                 :state state
                                 :priority priority
                                 :indent indent
                                 :-schedule schedule
                                 :-deadline deadline
                                 :contents contents
                                 :archived? archived?
                                 :commented? commented?
                                 :closed closed
                                 :-hash (org-glance-headline--hash contents)
                                 :-properties (org-glance-headline--properties contents)
                                 :-node-properties (org-glance-headline--node-properties contents)
                                 :-encrypted? (org-glance-headline--encrypted contents))))

(cl-defun org-glance-headline--copy (headline &rest update-plist)
  "Copy HEADLINE but replace slot values described in UPDATE-PLIST."
  (declare (indent 1))
  (cl-check-type headline org-glance-headline)
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
  "HEADLINE's contents after evaluating BODY in a buffer holding them."
  (declare (indent 1))
  `(org-glance-headline:with-contents ,headline
     ,@body
     (s-trim (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defmacro org-glance-headline--map-contents (headline &rest body)
  "A fresh `org-glance-headline' re-parsed from HEADLINE after BODY edits it.
BODY runs in a temp org buffer holding HEADLINE's contents
\(`org-glance-headline--rewrite-contents')."
  (declare (indent 1))
  `(org-glance-headline--from-string
    (org-glance-headline--rewrite-contents ,headline ,@body)))

(cl-defun org-glance-headline--body-region ()
  "Cons (BEG . END) of the current buffer's headline body:
end of meta-data (planning + drawers) to end of subtree."
  (cons (save-excursion (goto-char (point-min)) (org-end-of-meta-data t) (point))
        (save-excursion (goto-char (point-min)) (org-end-of-subtree t) (point))))

(defun org-glance-headline--crypt-legacy-cipher-p ()
  "Non-nil when the buffer holds the pre-block layout:
no crypt blocks, and the whole body is ciphertext (aes header at body start)."
  (and (null (org-glance--crypt-block-regions))
       (save-excursion
         (goto-char (point-min))
         (org-end-of-meta-data t)
         (looking-at org-glance--aes-header-re))))

(defun org-glance-headline--crypt-upgrade-legacy ()
  "Wrap a legacy whole-body cipher in one crypt block; return non-nil if done.
A wrapped bare cipher IS a sealed block (`org-glance--crypt-sealed?'), so after
this the block path handles everything -- legacy blobs become block blobs at
their first decrypt, with no downstream branching.  Structurally safe: a public
body has no aes header, so the upgrade can never re-wrap unwrapped plaintext."
  (when (org-glance-headline--crypt-legacy-cipher-p)
    (let ((body (org-glance-headline--body-region)))
      (org-glance--crypt-wrap-region (car body) (cdr body)))
    t))

(cl-defun org-glance-headline:encrypt (headline password)
  "HEADLINE with its secret regions sealed under PASSWORD.
When the body carries `#+begin_crypt' blocks, seal exactly those (plaintext
between them stays public and indexable); with no blocks, wrap the whole body
in one crypt block and seal it.  Already-encrypted HEADLINE returns unchanged."
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
  "HEADLINE with its ciphertext opened under PASSWORD.
A legacy whole-body cipher is first upgraded to one crypt block
(`org-glance-headline--crypt-upgrade-legacy'); then every sealed block is
unsealed.  Markers stay unless UNWRAP is non-nil -- rekeying keeps them, making
a headline public removes them.  Signal on a wrong PASSWORD.  Not-encrypted
HEADLINE returns unchanged."
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
  "Render org link markup in TITLE as plain text.
\"[[target][desc]]\" becomes \"desc\"; a bare \"[[target]]\" becomes \"target\"."
  (cl-check-type title string)
  (replace-regexp-in-string
   org-link-bracket-re
   (lambda (match) (or (match-string 2 match) (match-string 1 match) ""))
   title))

(provide 'org-glance-headline)
