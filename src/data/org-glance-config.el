;;; org-glance-config.el --- Typed shared settings -*- lexical-binding: t -*-

(require 'org-glance-config-file)
(require 'org-glance-tag-config)

(cl-defstruct (org-glance-settings (:constructor org-glance-settings--make)
                                   (:conc-name org-glance-settings:))
  tree cycles tags storage views colors diagnostics)

(defun org-glance-config--error (message)
  "Signal an invalid configuration with MESSAGE."
  (signal 'org-glance-config-error (list message)))

(defun org-glance-config--object (value)
  "Require a JSON object VALUE."
  (unless (hash-table-p value) (org-glance-config--error "Expected a settings object"))
  value)

(defun org-glance-config--field (object key predicate &optional default)
  "Validate OBJECT's KEY with PREDICATE, using DEFAULT only for absence."
  (let ((value (gethash key object :missing)))
    (if (eq value :missing) default
      (unless (funcall predicate value)
        (org-glance-config--error (concat "Invalid setting: " key)))
      value)))

(defun org-glance-config--known (object names)
  "Refuse OBJECT members outside NAMES."
  (maphash (lambda (key _) (unless (member key names)
                             (org-glance-config--error (concat "Unknown setting: " key)))) object))

(defun org-glance-config--tag (text)
  "Return non-nil when TEXT is an exact supported tag."
  (and (stringp text) (string-match-p "\\`[[:alnum:]_@#%_-]+\\'" text)))

(defun org-glance-config--trim (text)
  "Trim TEXT's Unicode whitespace using the shared settings convention."
  (string-trim text "[[:space:]]+" "[[:space:]]+"))

(defun org-glance-config--token (text)
  "Return TEXT's keyword name, refusing malformed tokens and metadata."
  (unless (and (stringp text)
               (string-match "\\`\\([[:alpha:]_]+\\)\\(?:([^()[:space:]]*)\\)?\\'" text))
    (org-glance-config--error "Invalid TODO keyword or metadata"))
  (match-string 1 text))

(defun org-glance-config--cycles (value)
  "Validate VALUE's ordered TODO cycles and return their token pairs."
  (unless (vectorp value) (org-glance-config--error "todoCycles must be an array"))
  (mapcar
   (lambda (cycle)
     (org-glance-config--object cycle)
     (org-glance-config--known cycle '("active" "inactive"))
     (let* ((a (append (org-glance-config--field cycle "active" #'vectorp []) nil))
            (i (append (org-glance-config--field cycle "inactive" #'vectorp []) nil))
            (names (mapcar #'org-glance-config--token (append a i))))
       (unless (and names (= (length names) (length (delete-dups (copy-sequence names)))))
         (org-glance-config--error "Empty cycle or duplicate TODO keyword"))
       (cons a i))) (append value nil)))

(defun org-glance-config--names (cycles)
  "Merge CYCLES' keyword names with active meaning winning within one scope."
  (let* ((active (delete-dups (mapcan (lambda (c) (mapcar #'org-glance-config--token (car c))) cycles)))
         (inactive (delete-dups (mapcan (lambda (c) (mapcar #'org-glance-config--token (cdr c))) cycles))))
    (cons active (cl-remove-if (lambda (word) (member word active)) inactive))))

(defun org-glance-config--pairs (object)
  "Return OBJECT's members in key order."
  (let (pairs)
    (maphash (lambda (key value) (push (cons key value) pairs)) object)
    (sort pairs (lambda (a b) (string< (car a) (car b))))))

(defun org-glance-config:decode (snapshot)
  "Decode SNAPSHOT into typed shared settings without observing the filesystem."
  (let* ((object (org-glance-config-file--envelope (org-glance-config-file:object snapshot)))
         (cycles (org-glance-config--cycles
                  (org-glance-config--field object "todoCycles" #'vectorp [])))
         (tree (org-glance-config--object
                (org-glance-config--field object "tree" #'hash-table-p (make-hash-table :test 'equal))))
         (raw-tags (org-glance-config--field object "tags" #'hash-table-p (make-hash-table :test 'equal)))
         (seen (make-hash-table :test 'equal)) tags storage diagnostics)
    (org-glance-config--known tree '("inboxTag" "headlineHistoryDepth" "repositoryAutosync"
                                     "repositoryRemote" "googleOAuthClientId" "defaultAgent"))
    (let ((values (make-hash-table :test 'equal)))
      (puthash "inboxTag" (downcase (org-glance-config--field tree "inboxTag" #'org-glance-config--tag "inbox")) values)
      (puthash "headlineHistoryDepth" (org-glance-config--field tree "headlineHistoryDepth"
                                                                (lambda (v) (and (integerp v) (<= 0 v) (<= v #x7fffffffffffffff))) 10) values)
      (puthash "repositoryAutosync" (org-glance-config--field tree "repositoryAutosync"
                                                              (lambda (v) (memq v '(t :false))) :false) values)
      (puthash "repositoryRemote" (org-glance-config--trim (org-glance-config--field tree "repositoryRemote"
                                                                         (lambda (v) (and (stringp v) (string-match-p
                                                                                "\\`[a-zA-Z0-9_./][a-zA-Z0-9_./-]*\\'" (org-glance-config--trim v)))) "origin")) values)
      (let ((client (org-glance-config--field tree "googleOAuthClientId"
                                              (lambda (v) (and (stringp v) (string-match-p
                                                                       "\\`[^[:space:]]+\\'" (org-glance-config--trim v)))))))
        (puthash "googleOAuthClientId" (and client (org-glance-config--trim client)) values))
      (puthash "defaultAgent" (org-glance-config--field tree "defaultAgent" #'stringp) values)
      (setq tree values))
    (dolist (pair (org-glance-config--pairs raw-tags))
      (let* ((name (downcase (car pair))) (raw (org-glance-config--object (cdr pair))))
        (unless (and (org-glance-config--tag name) (not (gethash name seen)))
          (org-glance-config--error "Invalid or ambiguous case-folded tag name"))
        (puthash name t seen)
        (org-glance-config--known raw '("title" "template" "todoCycles" "storage"))
        (let ((tag-cycles (org-glance-config--cycles (org-glance-config--field raw "todoCycles" #'vectorp [])))
              (policy (org-glance-config--field raw "storage"
                                                (lambda (v) (member v '("local" "portable"))))))
          (when (and (equal name "notification") (equal policy "portable"))
            (org-glance-config--error "notification requires local family storage"))
          (when (equal policy "local") (push name storage))
          (push (list name
                      (org-glance-tag-config :tag (intern name)
                                             :title (org-glance-config--field raw "title" #'stringp)
                                             :template (org-glance-config--field raw "template" #'stringp)
                                             :todo (mapconcat (lambda (cycle) (string-join
                                                                               (append (car cycle) '("|") (cdr cycle)) " ")) tag-cycles "\n"))
                      tag-cycles) tags))))
    (maphash (lambda (key _) (unless (member key '("format" "tree" "todoCycles" "tags" "views"
                                                   "stateColors" "application" "actions" "datasets" "clients"))
                               (push (concat "unknown setting: " key) diagnostics))) object)
    (let ((views (org-glance-config--field object "views" #'hash-table-p (make-hash-table :test 'equal)))
          (colors (org-glance-config--field object "stateColors" #'hash-table-p (make-hash-table :test 'equal))))
      (org-glance-config--known views '("default" "agenda" "archive" "notifications"))
      (maphash (lambda (_ value) (unless (stringp value) (org-glance-config--error "Invalid view query"))) views)
      (maphash (lambda (_ pairs)
                 (org-glance-config--object pairs)
                 (maphash (lambda (_ value) (unless (stringp value)
                                              (org-glance-config--error "Invalid state color"))) pairs)) colors)
      (org-glance-settings--make :tree tree :cycles cycles
                                 :tags (sort tags (lambda (a b) (string< (car a) (car b))))
                                 :storage (sort (delete-dups (cons "notification" storage)) #'string<)
                                 :views views :colors colors :diagnostics (sort diagnostics #'string<)))))

(defun org-glance-config:keywords (settings file tags)
  "Resolve SETTINGS with FILE's declared keyword pair and ordered TAGS."
  (let* ((system (org-glance-config--names (org-glance-settings:cycles settings)))
         (tag-settings (org-glance-settings:tags settings))
         (all (cons system (mapcar (lambda (tag) (org-glance-config--names (nth 2 tag))) tag-settings)))
         (recognition (delete-dups (append (mapcan (lambda (kw) (copy-sequence (car kw))) all)
                                           (mapcan (lambda (kw) (copy-sequence (cdr kw))) all)))))
    (org-glance-tag-config:resolve-keywords
     (append (list system)
             (mapcar (lambda (tag) (org-glance-config--names (nth 2 (assoc (downcase tag) tag-settings)))) tags)
             (list file)) recognition)))

(defun org-glance-config:header (settings file tags &optional source)
  "Render SETTINGS' effective FILE and TAGS declaration with SOURCE metadata."
  (let* ((keywords (org-glance-config:keywords settings file tags))
         (cycles (append (org-glance-settings:cycles settings)
                         (mapcan (lambda (tag) (copy-sequence (nth 2
                                                                   (assoc (downcase tag) (org-glance-settings:tags settings))))) tags)))
         (tokens (append (apply #'append (mapcar (lambda (c) (append (car c) (cdr c))) cycles))
                         (when source (with-temp-buffer
                                        (insert source)
                                        (mapcan (lambda (entry) (mapcan #'split-string (cdr entry)))
                                                (org-collect-keywords '("TODO" "SEQ_TODO" "TYP_TODO")))))))
         (decorate (lambda (name) (or (cl-find-if
                                       (lambda (token) (and (not (equal token "|"))
                                             (equal name (org-glance-config--token token))
                                             (not (equal name token)))) tokens) name))))
    (concat "#+TODO: " (string-join (append (mapcar decorate (car keywords)) '("|")
                                            (mapcar decorate (cdr keywords))) " ") "\n")))

(defun org-glance-config:fingerprints (settings)
  "Return SETTINGS' semantic dependency fingerprints separately from revisions."
  (let* ((system (org-glance-config--names (org-glance-settings:cycles settings)))
         (defaults (org-glance-tag-config:resolve-keywords nil nil))
         (tags (org-glance-settings:tags settings))
         (names (mapcar (lambda (tag) (cons (car tag) (org-glance-config--names (nth 2 tag)))) tags))
         (tree (org-glance-settings:tree settings)))
    (cl-labels ((pair (p) (vector (vconcat (car p)) (vconcat (cdr p))))
                (cycles (cs) (vconcat (mapcar #'pair cs)))
                (hash (version value) (secure-hash 'sha256
                                                   (substring (org-glance-config-file--serialize (vector version value)) 0 -1))))
      (list
       (hash "recognition/1" (vconcat (sort (delete-dups (append
                 (car defaults) (cdr defaults) (car system) (cdr system)
                 (mapcan (lambda (p) (append (cadr p) (copy-sequence (cddr p)))) names))) #'string<)))
       (hash "classification/1" (vector (pair defaults) (pair system)
                                        (vconcat (mapcar (lambda (p) (vector (car p) (pair (cdr p))))
                                                         (cl-remove-if-not (lambda (p) (or (cadr p) (cddr p))) names)))))
       (hash "policy/1" (vconcat (cons (vconcat (org-glance-settings:storage settings))
                                       (mapcar (lambda (k) (or (gethash k tree) :null))
                                               '("inboxTag" "headlineHistoryDepth" "repositoryAutosync" "repositoryRemote"
                                                 "googleOAuthClientId" "defaultAgent")))))
       (hash "presentation/1" (vector (cycles (org-glance-settings:cycles settings))
                                      (vconcat (mapcar (lambda (tag) (vector (car tag)
                                                                             (or (org-glance-tag-config:title (cadr tag)) :null)
                                                                             (or (org-glance-tag-config:template (cadr tag)) :null) (cycles (nth 2 tag)))) tags))
                                      (vconcat (mapcar (lambda (p) (vector (car p) (cdr p)))
                                                       (org-glance-config--pairs (org-glance-settings:views settings))))
                                      (vconcat (mapcar (lambda (p) (vector (car p)
                                                                           (vconcat (mapcar (lambda (q) (vector (car q) (cdr q))) (org-glance-config--pairs (cdr p))))))
                                                       (org-glance-config--pairs (org-glance-settings:colors settings))))))))))

(cl-defstruct (org-glance-org-presentation
               (:constructor org-glance-org-presentation--make)
               (:conc-name org-glance-org-presentation:))
  prefix body origin header)

(cl-defun org-glance-config:present (settings tags source &key (start 0) (end (length source)))
  "Project SETTINGS and TAGS onto SOURCE's half-open canonical START/END Span."
  (unless (and (stringp source) (integerp start) (integerp end)
               (<= 0 start end (length source)))
    (org-glance-config--error "Invalid canonical body Span"))
  (let* ((body (substring source start end))
         (file (org-glance-tag-config:declared-keywords source))
         (header (org-glance-config:header settings file tags source))
         (source (with-temp-buffer
                   (insert body)
                   (org-collect-keywords '("TODO" "SEQ_TODO" "TYP_TODO"))))
         (prefix (if source "" (if (string-match-p "\r\n" body)
                                   (replace-regexp-in-string "\n" "\r\n" header t t) header))))
    (org-glance-org-presentation--make :prefix prefix :body body :origin start :header header)))

(defun org-glance-config:insert-presentation (presentation)
  "Insert PRESENTATION into an empty buffer with read-only generated context."
  (unless (= (point-min) (point-max)) (org-glance-config--error "Presentation needs an empty buffer"))
  (let* ((header (org-glance-org-presentation:header presentation))
         (tokens (split-string (substring header (length "#+TODO: "))))
         (context (lambda (_) (cons 'sequence tokens))))
    (insert (org-glance-org-presentation:prefix presentation)
            (org-glance-org-presentation:body presentation))
    (let ((org-todo-setup-filter-hook (list context)))
      (delay-mode-hooks (org-mode)))
    (setq-local org-todo-setup-filter-hook (list context))
    (let ((end (+ (point-min) (length (org-glance-org-presentation:prefix presentation)))))
      (add-text-properties (point-min) end
                           '(read-only t front-sticky (read-only) rear-nonsticky (read-only)))
      (set-buffer-modified-p nil))))

(defun org-glance-config:compatible-header (scopes)
  "Return SCOPES' common Org header, refusing incompatible named contexts.
SCOPES contains (NAME . PRESENTATION) pairs."
  (if (null scopes) ""
    (let ((first (org-glance-org-presentation:header (cdar scopes))))
      (dolist (scope (cdr scopes))
        (unless (equal first (org-glance-org-presentation:header (cdr scope)))
          (org-glance-config--error
           (format "Incompatible Org context: %s and %s; active/done keywords, order or metadata differ; export compatible scopes separately"
                   (caar scopes) (car scope)))))
      first)))

(defun org-glance-config:canonical-body (presentation text)
  "Extract PRESENTATION's canonical body from TEXT, refusing a changed prefix."
  (let ((prefix (org-glance-org-presentation:prefix presentation)))
    (unless (string-prefix-p prefix text)
      (org-glance-config--error "Generated Org context is read-only"))
    (substring-no-properties text (length prefix))))

(cl-defun org-glance-config:map-edit (presentation &key start end text)
  "Map PRESENTATION's zero-based START/END edit of TEXT to its source Span."
  (let ((offset (length (org-glance-org-presentation:prefix presentation)))
        (body (org-glance-org-presentation:body presentation))
        (origin (org-glance-org-presentation:origin presentation)))
    (unless (and (integerp start) (integerp end) (<= offset start end)
                 (<= end (+ offset (length body))))
      (org-glance-config--error "Edit crosses the canonical body boundary"))
    (list (+ origin (- start offset)) (+ origin (- end offset)) text)))

(provide 'org-glance-config)
;;; org-glance-config.el ends here
