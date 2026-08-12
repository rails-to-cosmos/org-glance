;; -*- lexical-binding: t -*-

;;; org-glance-filter.el --- the org-glance headline filter language


;;; Code:

(require 'cl-lib)
(require 's)
(require 'dash)

(require 'org-glance-tag)
(require 'org-glance-graph)


(defcustom org-glance-filter-spec '(:done nil :archived nil :commented nil)
  "Ambient filter applied to headline actions, as a normalized filter spec.
Default: active (not-done), unarchived, uncommented headlines.  Set via the
`org-glance-transient' transient (clear with `c' to see everything);
consumed by the picker commands (materialize / open / extract) and overlaid
onto the overview and agenda.  See `org-glance-filter:predicate' for the
spec language."
  :group 'org-glance
  :type 'sexp)


(defconst org-glance-filter:table
  ;; Row: KEY :match KIND :accessor FN :canon KIND (nil :canon = as-is).
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
    ;; Invariant 17: transient views are never cached, never persisted.
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

(defconst org-glance-filter--structural-keys '(:done :done-keywords :where)
  "Keys `org-glance-filter:predicate' handles outside the `:match' dispatch.
`:done' is parameterised by `:done-keywords'; `:where' is a raw predicate.
Every other row MUST declare a `:match' kind: a nil one builds no clause, so
the key constrains nothing and the filter matches EVERYTHING.")

(cl-defun org-glance-filter--check-table (table)
  "Signal unless TABLE is a valid filter table; else return t.
Runs at load over the real table; tests call it with broken ones."
  (cl-loop for (key . props) in table
           for kind = (plist-get props :match)
           do (cond ((memq key org-glance-filter--structural-keys)
                     (when kind
                       (error "org-glance: structural filter key %S must not declare :match" key)))
                    ((null kind)
                     (error "org-glance: filter key %S declares no :match kind" key))
                    ((null (plist-get props :accessor))
                     (error "org-glance: filter key %S declares :match but no :accessor" key))))
  t)

(org-glance-filter--check-table org-glance-filter:table)

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
        ;; An empty tag list folds to the "all" key and shares its cache.
        (when-let ((all (append (org-glance-tag:as-list tags)
                                (when tag (list tag)))))
          (setq out (plist-put out :tags all))))
      (when (and (plist-member out :done-keywords) (not (plist-member out :done)))
        (cl-remf out :done-keywords))
      (cl-loop for (k _v) on out by #'cddr
               unless (memq k org-glance-filter:keys)
               do (error "Unrecognised filter key: %S" k))
      out))
   (t (error "Invalid filter: %S" filter))))

(cl-defun org-glance-filter--match-clause (kind accessor want)
  "Build one predicate clause testing ACCESSOR's value against WANT, per KIND."
  (pcase kind
    ('member-all (let ((want (mapcar #'org-glance--downcased-string want)))
                   (lambda (m)
                     (let ((have (mapcar #'org-glance--downcased-string
                                         (append (funcall accessor m) nil))))
                       (cl-every (lambda (tag) (member tag have)) want)))))
    ;; A stateless headline carries state "": `(:state nil)' must match it.
    ('state-equal (let ((want (or want "")))
                    (lambda (m) (equal want (or (funcall accessor m) "")))))
    ('equal (lambda (m) (equal want (funcall accessor m))))
    ('eql (lambda (m) (eql want (funcall accessor m))))
    ('edge-target (lambda (m) (assoc want (funcall accessor m))))
    ('member (lambda (m) (member (funcall accessor m) want)))
    ('bool (let ((want (and want t)))
             (lambda (m) (eq want (and (funcall accessor m) t)))))
    ('substring (let ((needle (org-glance--downcased-string want)))
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
    (when (plist-member spec :done)
      ;; Resolve the done-set once -- `org-done-keywords' is nil outside Org.
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
    ('tags (sort (mapcar #'org-glance--downcased-string value) #'string<))
    ('string-list (sort (org-glance--strings value) #'string<))
    ('downcase (org-glance--downcased-string value))
    (_ value)))

(cl-defun org-glance-filter--canonical-pairs (spec)
  "Order-independent (KEY . VALUE) alist for normalised SPEC.
Keys sorted; values canonicalised per the table so that `prin1' renders them
unambiguously."
  (sort (cl-loop for (k v) on spec by #'cddr
                 collect (cons k (org-glance-filter--canon-value k v)))
        (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))


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

(cl-defun org-glance-filter:sole-tag (filter)
  "Return FILTER's sole tag as a string, or nil unless it names exactly one.
Backs the prompt-free `C' in overview / table: a single-tag view names its tag,
while the unfiltered or multi-tag view has none, so the caller re-prompts."
  (let ((tags (org-glance-filter:tags filter)))
    (when (and tags (null (cdr tags)))
      (symbol-name (car tags)))))

(provide 'org-glance-filter)
;;; org-glance-filter.el ends here
