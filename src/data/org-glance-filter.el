;; -*- lexical-binding: t -*-

;;; org-glance-filter.el --- the org-glance headline filter language

;;; Commentary:
;; The single, shared filter language over headline *metadata*.  A FILTER is
;; either nil (all headlines), a bare tag symbol/string (shorthand for
;; `(:tags (TAG))'), or a normalized plist spec.  Three pure helpers interpret
;; it: `:normalize-spec' (the single coercion point), `:predicate' (spec ->
;; closure on metadata) and `:identity' (spec -> an unambiguous printed form,
;; the basis of the overview cache key).
;;
;; This used to live inside `org-glance-overview'; it was lifted here so the
;; SAME language drives every consumer -- the picker commands
;; (materialize/open/extract), the overview + agenda, the in-overview `/'
;; refinement, and the `org-glance-transient' transient.  `org-glance-overview'
;; keeps only the cache/render machinery layered on top (`spec-key',
;; `cached-file', the SPEC sidecar, ...).
;;
;; `plist-member' (not `plist-get') gates each clause, so `(:state nil)'
;; (stateless headlines) is distinct from omitting `:state' (no constraint).

;;; Code:

(require 'cl-lib)
(require 's)
(require 'dash)

(require 'org-glance-tag)
(require 'org-glance-graph)

;;; The ambient filter

(defcustom org-glance-filter-spec '(:done nil :archived nil :commented nil)
  "Ambient filter applied to headline actions, as a normalized filter spec.
Default: active (not-done), unarchived, uncommented headlines.  Set via the
`org-glance-transient' transient (clear with `c' to see everything);
consumed by the picker commands (materialize / open / extract) and overlaid
onto the overview and agenda.  See `org-glance-filter:predicate' for the
spec language."
  :group 'org-glance
  :type 'sexp)

;;; The language

(defconst org-glance-filter:table
  ;; KEY -> plist:
  ;;   :match    clause kind built by `--match-clause' (nil = the clause is
  ;;             structural and hand-built in `predicate': :done is
  ;;             parameterised by :done-keywords; :where is a raw predicate;
  ;;             :done-keywords contributes no clause of its own)
  ;;   :accessor metadata accessor the clause tests
  ;;   :canon    canonicalisation kind shared by the spec identity and the
  ;;             cache key (see `--canon-value'; nil = as-is)
  `((:tags           :match member-all     :canon tags
                     :accessor ,#'org-glance-headline-metadata:tags)
    (:state          :match state-equal
                     :accessor ,#'org-glance-headline-metadata:state)
    (:done)
    (:done-keywords  :canon string-list)
    (:id             :match equal          :accessor ,#'org-glance-headline-metadata:id)
    (:title          :match equal          :accessor ,#'org-glance-headline-metadata:title)
    (:title-contains :match substring      :canon downcase
                     :accessor ,#'org-glance-headline-metadata:title)
    (:hash           :match equal          :accessor ,#'org-glance-headline-metadata:hash)
    (:priority       :match eql            :accessor ,#'org-glance-headline-metadata:priority)
    (:linked         :match bool           :accessor ,#'org-glance-headline-metadata:linked?)
    (:propertized    :match bool           :accessor ,#'org-glance-headline-metadata:propertized?)
    (:encrypted      :match bool           :accessor ,#'org-glance-headline-metadata:encrypted?)
    (:archived       :match bool           :accessor ,#'org-glance-headline-metadata:archived?)
    (:commented      :match bool           :accessor ,#'org-glance-headline-metadata:commented?)
    (:schedule       :match present-absent :accessor ,#'org-glance-headline-metadata:schedule)
    (:deadline       :match present-absent :accessor ,#'org-glance-headline-metadata:deadline)
    ;; Relation views.  :transient marks keys whose views are one-off (a raw
    ;; predicate, or an identity embedding another headline's id/link set):
    ;; the overview never caches them and the table never persists their
    ;; per-filter config -- both would accrete one entry per visited headline.
    (:refers-to      :match edge-target    :transient t
                     :accessor ,#'org-glance-headline-metadata:relations)
    (:id-any         :match member         :canon string-list :transient t
                     :accessor ,#'org-glance-headline-metadata:id)
    (:where          :transient t))
  "The single source of truth for the headline filter language.
Drives `org-glance-filter:keys', `:predicate' and `--canonical-pairs'
together -- previously each new key meant four lockstep edits, two of them
silently forgettable (a missing predicate clause made the filter match
EVERYTHING; a missing canonicalisation case fragmented the cache).  Adding a
plain value-key = one row here.  `:title' is exact; `:title-contains' is a
case-insensitive substring.")

(defconst org-glance-filter:keys
  (mapcar #'car org-glance-filter:table)
  "Recognised keys in a normalised filter spec (`:tag' folds into `:tags').
Derived from `org-glance-filter:table'.")

(cl-defun org-glance-filter:normalize-spec (filter)
  "Coerce FILTER into a canonical plist spec (or nil for \"all\").
nil/() stays nil; a bare tag symbol/string becomes `(:tags (TAG))'; a plist is
returned with any `:tag' folded into `:tags' so downstream code only ever sees
`:tags'.  Signals an error on an unrecognised key."
  (cond
   ((null filter) nil)
   ((symbolp filter) (list :tags (list (format "%s" filter))))
   ((stringp filter) (list :tags (list filter)))
   ((listp filter)
    (let (out tag tags tags-seen)
      (cl-loop for (k v) on filter by #'cddr
               do (cond ((eq k :tag) (setq tag v))
                        ((eq k :tags) (setq tags v tags-seen t))
                        (t (setq out (plist-put out k v)))))
      (when (or tags-seen tag)
        ;; An empty tag list is no constraint -- drop it so `(:tags nil)' folds
        ;; to the "all" key and shares the unfiltered cache (like `(:tag nil)').
        (when-let ((all (append (cond ((null tags) nil)
                                      ((listp tags) tags)
                                      (t (list tags)))
                                (when tag (list tag)))))
          (setq out (plist-put out :tags all))))
      ;; :done-keywords only parameterises :done; without :done it changes
      ;; nothing, so drop it (avoids a no-op filter and a fragmented cache).
      (when (and (plist-member out :done-keywords) (not (plist-member out :done)))
        (cl-remf out :done-keywords))
      (cl-loop for (k _v) on out by #'cddr
               unless (memq k org-glance-filter:keys)
               do (error "Unrecognised filter key: %S" k))
      out))
   (t (error "Invalid filter: %S" filter))))

(defsubst org-glance-filter--downcased-string (x)
  "Coerce X to a downcased string (the filter language's tag/title canon form)."
  (downcase (format "%s" x)))

(cl-defun org-glance-filter--match-clause (kind accessor want)
  "Build one predicate clause testing ACCESSOR's value against WANT, per KIND."
  (pcase kind
    ;; Tags are stored interned + downcased, so compare case-insensitively
    ;; (mirrors v1's `org-glance:tag-filter').
    ('member-all (let ((want (mapcar #'org-glance-filter--downcased-string want)))
                   (lambda (m)
                     (let ((have (mapcar #'org-glance-filter--downcased-string
                                         (append (funcall accessor m) nil))))
                       (cl-every (lambda (tag) (member tag have)) want)))))
    ;; A headline with no todo keyword carries state "" (not nil); treat a nil
    ;; or "" filter value alike, so `(:state nil)' means "stateless".
    ('state-equal (let ((want (or want "")))
                    (lambda (m) (equal want (or (funcall accessor m) "")))))
    ('equal (lambda (m) (equal want (funcall accessor m))))
    ('eql (lambda (m) (eql want (funcall accessor m))))
    ;; Relation kinds: `edge-target' tests the (TARGET-ID . KIND) edge list for
    ;; WANT as a target (any kind); `member' tests the scalar accessor value
    ;; against the WANT list (`:id-any').
    ('edge-target (lambda (m) (assoc want (funcall accessor m))))
    ('member (lambda (m) (member (funcall accessor m) want)))
    ('bool (let ((want (and want t)))
             (lambda (m) (eq want (and (funcall accessor m) t)))))
    ;; Case-insensitive substring (the interactive `/' refinement).
    ('substring (let ((needle (org-glance-filter--downcased-string want)))
                  (lambda (m) (s-contains? needle (downcase (or (funcall accessor m) ""))))))
    ('present-absent (lambda (m)
                       (let ((present (org-glance--present-string? (funcall accessor m))))
                         (pcase want
                           (:present present)
                           (:absent (not present))
                           (_ (error "Planning filter expects :present or :absent, got %S" want))))))
    (_ (error "org-glance: filter key has no :match kind: %S" kind))))

(cl-defun org-glance-filter:predicate (filter)
  "Return a predicate (lambda (METADATA) -> generalized-boolean) for FILTER.
A nil/empty filter yields a predicate that accepts every headline.  Each present
clause must hold (logical AND).  See `org-glance-filter:table'."
  (let ((spec (org-glance-filter:normalize-spec filter))
        (clauses nil))
    ;; Structural specials the table cannot express:
    (when (plist-member spec :done)
      ;; `done?' reads the buffer-local `org-done-keywords' (nil outside an Org
      ;; buffer), so resolve a concrete done-set ONCE here -- a per-overview
      ;; `:done-keywords' wins over the global default chain -- and test membership
      ;; directly against it.  This mirrors `org-glance-headline-metadata:done?' (a
      ;; plain `member') but avoids a per-candidate dynamic `let' + function call on
      ;; the picker's hot path: `(and (member ...) t)' == `(not (null (member ...)))'.
      (let ((want (and (plist-get spec :done) t))
            (done-keywords (or (plist-get spec :done-keywords)
                               (org-glance--done-keywords))))
        (push (lambda (m)
                (eq want (and (member (org-glance-headline-metadata:state m) done-keywords) t)))
              clauses)))
    (when (plist-member spec :where)
      (let ((fn (plist-get spec :where)))
        (cl-check-type fn function)
        (push fn clauses)))
    ;; Everything else is table-driven: one row per key.
    (cl-loop for (key . props) in org-glance-filter:table
             for kind = (plist-get props :match)
             when (and kind (plist-member spec key))
             do (push (org-glance-filter--match-clause
                       kind (plist-get props :accessor) (plist-get spec key))
                      clauses))
    (if clauses
        (lambda (m) (cl-every (lambda (clause) (funcall clause m)) clauses))
      (lambda (_m) t))))

(cl-defun org-glance-filter--canon-value (key value)
  "Canonical form of VALUE under KEY, per the table's :canon kind.
Shared by the spec identity and the cache key (a hash of that identity)."
  (pcase (plist-get (alist-get key org-glance-filter:table) :canon)
    ('tags (sort (mapcar #'org-glance-filter--downcased-string value) #'string<))
    ('string-list (sort (mapcar (lambda (x) (format "%s" x)) value) #'string<))
    ('downcase (org-glance-filter--downcased-string value))
    (_ value)))

(cl-defun org-glance-filter--canonical-pairs (spec)
  "Order-independent (KEY . VALUE) alist for normalised SPEC.
Keys sorted; values canonicalised per the table so that `prin1' renders them
unambiguously."
  (sort (cl-loop for (k v) on spec by #'cddr
                 collect (cons k (org-glance-filter--canon-value k v)))
        (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))

;;; Overview links: "TAG[?KEY=VALUE&...]" -> filter spec
;;
;; `org-glance-overview:' link paths encode a filter.  Value syntax derives
;; from the table's :match/:canon kinds -- no second key list to maintain.

(cl-defun org-glance-filter--link-value (key v)
  "Link value string V coerced for filter KEY, per the table's kinds.
Lists are comma-separated; booleans read t/nil; planning keys read
present/absent (the clause's own vocabulary); `:priority' reads a letter;
`:where' (a function) is not linkable."
  (let* ((props (alist-get key org-glance-filter:table))
         (kind (plist-get props :match)))
    (cond
     ((eq key :where) (error "org-glance: `:where' is not linkable"))
     ((or (eq key :done) (eq kind 'bool))
      (pcase v ("t" t) ("nil" nil)
             (_ (error "org-glance: link filter boolean must be t/nil: %S" v))))
     ;; :schedule/:deadline speak the predicate's own vocabulary -- a t/nil
     ;; here would error per headline inside the clause, not at parse time.
     ((eq kind 'present-absent)
      (pcase v ("present" :present) ("absent" :absent)
             (_ (error "org-glance: planning link value must be present/absent: %S" v))))
     ((or (memq kind '(member-all member))
          (eq (plist-get props :canon) 'string-list))
      (split-string v "," t))
     ((eq kind 'eql) (string-to-char v))           ; :priority, as a letter
     (t v))))

(cl-defun org-glance-filter:from-link-path (path)
  "Filter spec encoded in an overview link PATH: \"TAG[?KEY=VALUE&...]\".
TAG empty or `all' adds no tag constraint.  KEYs are the filter-table keys;
unknown keys error via `normalize-spec'.  Returns a normalized spec."
  (pcase-let* ((`(,head ,query) (split-string path "[?]"))
               (spec (unless (member (downcase head) '("" "all"))
                       (list :tags (list (downcase head))))))
    (dolist (kv (and query (split-string query "&" t)))
      (unless (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" kv)
        (error "org-glance: malformed link filter clause: %S" kv))
      (let* ((key (intern (concat ":" (match-string 1 kv))))
             (value (org-glance-filter--link-value key (match-string 2 kv))))
        (setq spec (plist-put spec key
                              (if (eq key :tags)   ; join with the path TAG
                                  (append (plist-get spec :tags) value)
                                value)))))
    (org-glance-filter:normalize-spec spec)))

(cl-defun org-glance-filter:transient? (filter)
  "Non-nil when FILTER carries a `:transient' key (see the table).
Transient views are one-off: never overview-cached, no persisted table config."
  (cl-loop for (k _v) on (org-glance-filter:normalize-spec filter) by #'cddr
           thereis (plist-get (alist-get k org-glance-filter:table) :transient)))

(cl-defun org-glance-filter:identity (filter)
  "Unambiguous printed identity of FILTER's canonical form.
Two filters are \"the same\" when these identities match (the directory
name is a lossy hash prefix)."
  (->> filter
       org-glance-filter:normalize-spec
       org-glance-filter--canonical-pairs
       prin1-to-string))

;;; Generalized refinement: build a new spec from a dimension choice
;;
;; Shared by the in-overview `/' filter and the `org-glance-transient'
;; transient so the two stay consistent.  Each returns a fresh normalized spec.

(cl-defun org-glance-filter:read-state (&optional graph)
  "Completing-read a todo-state choice for the filter.
Offers `active' / `done' / `all' plus GRAPH's concrete todo states.  Returns the
symbol `active', `done' or `all', or a concrete state string.  Specials win on a
\(rare) name clash with a lowercase concrete state.  Signals `user-error' on
empty input."
  (let* ((states (and graph (org-glance-graph:states graph)))
         (concrete (cl-remove-if (lambda (s) (member s '("active" "done" "all"))) states))
         (choice (completing-read "Todo state: "
                                  (append '("active" "done" "all") concrete) nil t)))
    (when (string-empty-p choice) (user-error "No state given"))
    (pcase choice ("active" 'active) ("done" 'done) ("all" 'all) (_ choice))))

(cl-defun org-glance-filter:set-state (spec choice)
  "Return SPEC with its todo-state dimension replaced by CHOICE.
CHOICE is `active'/`done'/`all' or a concrete state string (see `:read-state');
it covers both the `:state' and `:done' keys.  `all' clears the dimension;
`active'/`done' set `:done'; a concrete state string sets `:state'."
  (let ((s (org-glance-filter:normalize-spec spec)))
    (cl-remf s :state)
    (cl-remf s :done)
    (pcase choice
      ('all s)
      ('active (plist-put s :done nil))
      ('done   (plist-put s :done t))
      ((pred stringp) (plist-put s :state choice))
      (_ (error "org-glance: bad state choice %S" choice)))))

(cl-defun org-glance-filter:set-substring (spec needle)
  "Return SPEC with `:title-contains' set to NEEDLE, or cleared if NEEDLE empty."
  (let ((s (org-glance-filter:normalize-spec spec)))
    (cl-remf s :title-contains)
    (if (or (null needle) (string-empty-p needle))
        s
      (plist-put s :title-contains needle))))

(cl-defun org-glance-filter:merge (base extra)
  "Merge normalized EXTRA's clauses onto BASE; EXTRA wins on a key conflict.
Both are coerced via `:normalize-spec'.  Used to overlay a one-off choice (a
prompted tag) onto the ambient `org-glance-filter-spec'."
  (let ((s (org-glance-filter:normalize-spec base)))
    (cl-loop for (k v) on (org-glance-filter:normalize-spec extra) by #'cddr
             do (setq s (plist-put s k v)))
    s))

(cl-defun org-glance-filter:describe (filter)
  "Short human label for FILTER, for transient/menu display."
  (cl-loop with spec = (org-glance-filter:normalize-spec filter)
           for (k v) on spec by #'cddr
           collect (pcase k
                     (:done (if v "done" "active"))
                     (:state (format "state=%s" v))
                     (:tags (format "tags=%s" (mapconcat (-partial #'format "%s") v "+")))
                     (:title-contains (format "title~%s" v))
                     ;; Compact: an :id-any value embeds a whole target list and
                     ;; ids are long -- keep buffer names/titles bounded.
                     (:refers-to (format "refs->%s" (s-left 8 v)))
                     (:id-any (format "id-any(%d)" (length v)))
                     (:where "where")
                     (:archived (if v "archived" "-archived"))
                     (:commented (if v "commented" "-commented"))
                     (_ (format "%s=%s" (substring (symbol-name k) 1) v)))
           into parts
           finally return (if parts (s-join ", " parts) "all")))

(cl-defun org-glance-filter:tags (filter)
  "Return the :tags from FILTER as a list of downcased symbols, or nil.
Tags are canonically downcased (`org-glance-tag?'); downcasing here means a
mixed-case filter value yields a valid tag for capture / config lookup rather
than a spurious symbol."
  (let ((tags (plist-get (org-glance-filter:normalize-spec filter) :tags)))
    (mapcar #'org-glance-tag:from-string tags)))

(provide 'org-glance-filter)
;;; org-glance-filter.el ends here
