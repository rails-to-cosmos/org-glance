;;; org-glance-cache.el --- Shared local projection  -*- lexical-binding: t; -*-

;;; Commentary:
;; The rebuildable SQLite projection shared with the Glance daemon.

;;; Code:

(declare-function org-glance--batch-worker-command "org-glance-core")

(require 'cl-lib)
(require 'json)
(require 'sqlite)

(require 'org-glance-graph)
(require 'org-glance-headline)
(require 'org-glance-filter)
(require 'org-glance-version)

(defconst org-glance-cache:version 6
  "Shared Glance cache schema version.")

(defcustom org-glance-cache-progress-step 25
  "Family interval between cache rebuild progress messages."
  :type 'natnum
  :group 'org-glance)

(defconst org-glance-cache--schema
  '("CREATE TABLE IF NOT EXISTS source (path TEXT PRIMARY KEY, digest TEXT NOT NULL, producer TEXT NOT NULL)"
    "CREATE TABLE IF NOT EXISTS glance_payload (path TEXT PRIMARY KEY REFERENCES source(path) ON DELETE CASCADE, config TEXT NOT NULL, parser_version INTEGER NOT NULL, producer TEXT NOT NULL, payload TEXT NOT NULL)"
    "CREATE TABLE IF NOT EXISTS headline (id TEXT PRIMARY KEY, path TEXT NOT NULL REFERENCES source(path) ON DELETE CASCADE, digest TEXT NOT NULL, org_id TEXT, org_id_property TEXT, title TEXT NOT NULL, state TEXT, priority TEXT, tags TEXT NOT NULL, scheduled TEXT, deadline TEXT, closed TEXT, category TEXT NOT NULL, created TEXT, producer TEXT NOT NULL)"
    "CREATE INDEX IF NOT EXISTS headline_path ON headline(path)"
    "CREATE INDEX IF NOT EXISTS headline_org_id ON headline(org_id)"
    "CREATE TABLE IF NOT EXISTS org_headline (id TEXT PRIMARY KEY REFERENCES headline(id) ON DELETE CASCADE, path TEXT NOT NULL, digest TEXT NOT NULL, content_hash TEXT NOT NULL, record TEXT NOT NULL, producer TEXT NOT NULL)"
    "CREATE TABLE IF NOT EXISTS edge (src TEXT NOT NULL REFERENCES headline(id) ON DELETE CASCADE, dst TEXT NOT NULL REFERENCES headline(id) ON DELETE CASCADE, kind TEXT, via TEXT NOT NULL, PRIMARY KEY(src, dst, kind, via))"
    "CREATE INDEX IF NOT EXISTS edge_dst ON edge(dst)"
    "CREATE TABLE IF NOT EXISTS headline_family (logical_id TEXT PRIMARY KEY, state TEXT NOT NULL, current_version_id TEXT, family_fingerprint TEXT NOT NULL, reconciled_epoch TEXT, verified INTEGER NOT NULL, revived INTEGER NOT NULL)"
    "CREATE TABLE IF NOT EXISTS headline_event (logical_id TEXT NOT NULL REFERENCES headline_family(logical_id) ON DELETE CASCADE, event_id TEXT NOT NULL, kind TEXT NOT NULL, parents_json TEXT NOT NULL, targets_json TEXT NOT NULL, observed_leaves_json TEXT NOT NULL, payload_digest TEXT, producer TEXT NOT NULL, created TEXT NOT NULL, envelope TEXT NOT NULL, PRIMARY KEY(logical_id,event_id))"
    "CREATE TABLE IF NOT EXISTS headline_payload_observation (logical_id TEXT NOT NULL, event_id TEXT NOT NULL, path TEXT NOT NULL, observed_digest TEXT, payload_state TEXT NOT NULL, PRIMARY KEY(logical_id,event_id), FOREIGN KEY(logical_id,event_id) REFERENCES headline_event(logical_id,event_id) ON DELETE CASCADE)"
    "CREATE TABLE IF NOT EXISTS headline_projection (logical_id TEXT NOT NULL REFERENCES headline_family(logical_id) ON DELETE CASCADE, event_id TEXT, role TEXT NOT NULL, source_digest TEXT, parser_contract TEXT NOT NULL, record_payload TEXT, PRIMARY KEY(logical_id,event_id))"
    "CREATE UNIQUE INDEX IF NOT EXISTS headline_projection_metadata ON headline_projection(logical_id) WHERE event_id IS NULL"
    "CREATE TABLE IF NOT EXISTS producer_projection (logical_id TEXT NOT NULL REFERENCES headline_family(logical_id) ON DELETE CASCADE, event_id TEXT, producer TEXT NOT NULL, parser_contract TEXT NOT NULL, record_payload TEXT NOT NULL, PRIMARY KEY(logical_id,event_id,producer))"
    "CREATE UNIQUE INDEX IF NOT EXISTS producer_projection_metadata ON producer_projection(logical_id,producer) WHERE event_id IS NULL"
    "CREATE TABLE IF NOT EXISTS inventory_directory (path TEXT PRIMARY KEY, stamp TEXT NOT NULL)")
  "DDL shared with `Glance.Cache'.")

(cl-defun org-glance-cache:path (graph)
  "Return GRAPH's shared SQLite cache path."
  (org-glance-graph:cache-file graph "glance.sqlite3"))

(cl-defun org-glance-cache--transaction (db thunk)
  "Call THUNK in an immediate transaction on DB."
  (sqlite-execute db "BEGIN IMMEDIATE")
  (condition-case err
      (prog1 (funcall thunk) (sqlite-commit db))
    (error (ignore-errors (sqlite-rollback db))
           (signal (car err) (cdr err)))))

(cl-defun org-glance-cache--initialise (db)
  "Build DB's shared schema, replacing an incompatible version."
  (let* ((version (caar (sqlite-select db "PRAGMA user_version")))
         (reset? (/= version org-glance-cache:version)))
    (when reset?
      (dolist (table (sqlite-select
                      db
                      "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%' ORDER BY CASE name WHEN 'producer_projection' THEN 0 WHEN 'headline_projection' THEN 1 WHEN 'headline_payload_observation' THEN 2 WHEN 'headline_event' THEN 3 WHEN 'headline_family' THEN 4 WHEN 'edge' THEN 5 WHEN 'org_headline' THEN 6 WHEN 'glance_payload' THEN 7 WHEN 'headline' THEN 8 WHEN 'source' THEN 9 ELSE 10 END"))
        (sqlite-execute db
                        (format "DROP TABLE IF EXISTS \"%s\""
                                (replace-regexp-in-string "\"" "\"\"" (car table))))))
    (dolist (statement org-glance-cache--schema)
      (sqlite-execute db statement))
    (when reset?
      (sqlite-execute db (format "PRAGMA user_version=%d"
                                 org-glance-cache:version)))))

(cl-defun org-glance-cache--prepare (db)
  "Configure DB for shared local writers."
  (sqlite-execute db "PRAGMA busy_timeout=5000")
  (sqlite-select db "PRAGMA journal_mode=WAL")
  (sqlite-execute db "PRAGMA foreign_keys=ON"))

(cl-defun org-glance-cache--with-db (graph fn)
  "Call FN with GRAPH's prepared shared database, when SQLite is available."
  (when (sqlite-available-p)
    (make-directory (org-glance-graph:cache-path graph) t)
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (progn
            (org-glance-cache--prepare db)
            (org-glance-cache--transaction
             db (lambda () (org-glance-cache--initialise db)))
            (funcall fn db))
        (sqlite-close db)))))

(cl-defun org-glance-cache--read-db (graph fn)
  "Call FN against GRAPH's last committed cache without taking a write lock."
  (let ((path (org-glance-cache:path graph)))
    (when (and (sqlite-available-p) (file-exists-p path))
      (let ((db (sqlite-open path)))
        (unwind-protect
            (progn
              (sqlite-execute db "PRAGMA busy_timeout=50")
              (funcall fn db))
          (sqlite-close db))))))

(cl-defun org-glance-cache--file-digest (path)
  "Return PATH's raw SHA-256 digest."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(cl-defun org-glance-cache--tags (metadata)
  "Return METADATA's tags in Glance's colon-delimited form."
  (let ((tags (org-glance-headline-metadata:tag-strings metadata)))
    (if tags (concat ":" (mapconcat #'identity tags ":") ":") "")))

(cl-defun org-glance-cache--priority (metadata)
  "Return METADATA's priority as Glance text, or nil."
  (when-let* ((priority (org-glance-headline-metadata:priority metadata)))
    (char-to-string priority)))

(cl-defun org-glance-cache--entry-value (headline property &optional inherit)
  "Return HEADLINE's Org entry PROPERTY, optionally with INHERIT semantics."
  (org-glance-headline:with-contents headline
    (org-entry-get nil property inherit)))

(cl-defun org-glance-cache--row-id (id path)
  "Return the shared cache identity for logical ID stored at PATH."
  (if (org-glance-version:file-p path)
      (concat id "/" (file-name-nondirectory
                       (directory-file-name (file-name-directory path))))
    id))

(cl-defun org-glance-cache--project (db graph metadata)
  "Project METADATA from GRAPH into DB."
  (let* ((id (org-glance-headline-metadata:id metadata))
         (path (org-glance-graph:content-path graph id))
         (row-id (org-glance-cache--row-id id path))
         (contents (org-glance-graph:get-content graph id))
         (headline (and contents (org-glance-headline--from-string contents)))
         (digest (and contents (org-glance-cache--file-digest path)))
         (content-hash (and headline (org-glance-headline:hash headline)))
         (record (plist-put
                  (org-glance-headline-metadata:serialize metadata)
                  :hash content-hash))
         (payload (and record
                       (decode-coding-string (json-serialize record) 'utf-8 t))))
    (when (and contents headline digest)
      (sqlite-execute
       db
       "INSERT INTO source(path,digest,producer) VALUES(?,?,?) ON CONFLICT(path) DO UPDATE SET digest=excluded.digest, producer=excluded.producer"
       (vector path digest "org-glance"))
      (sqlite-execute
       db
       "INSERT INTO headline(id,path,digest,org_id,org_id_property,title,state,priority,tags,scheduled,deadline,closed,category,created,producer) VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) ON CONFLICT(id) DO UPDATE SET path=excluded.path,digest=excluded.digest,org_id=excluded.org_id,org_id_property=excluded.org_id_property,title=excluded.title,state=excluded.state,priority=excluded.priority,tags=excluded.tags,scheduled=excluded.scheduled,deadline=excluded.deadline,closed=excluded.closed,category=excluded.category,created=excluded.created,producer=excluded.producer"
       (vector row-id path digest id
               (org-glance-headline:node-property "ID" headline)
               (org-glance-headline-metadata:title metadata)
               (org-glance-headline-metadata:state metadata)
               (org-glance-cache--priority metadata)
               (org-glance-cache--tags metadata)
               (org-glance-headline-metadata:schedule metadata)
               (org-glance-headline-metadata:deadline metadata)
               (org-glance-cache--entry-value headline "CLOSED")
               (or (org-glance-cache--entry-value headline "CATEGORY" t) "")
               (org-glance-headline:node-property
                "ORG_GLANCE_CREATION_TIME" headline)
               "org-glance"))
      (sqlite-execute
       db
       "INSERT INTO org_headline(id,path,digest,content_hash,record,producer) VALUES(?,?,?,?,?,?) ON CONFLICT(id) DO UPDATE SET path=excluded.path,digest=excluded.digest,content_hash=excluded.content_hash,record=excluded.record,producer=excluded.producer"
       (vector row-id path digest content-hash payload "org-glance")))))

(cl-defun org-glance-cache--project-edges (db metadata)
  "Replace METADATA's resolved outgoing edges in DB."
  (let* ((id (org-glance-headline-metadata:id metadata))
         (src (caar (sqlite-select db "SELECT id FROM headline WHERE org_id=?"
                                   (vector id)))))
    (when src (sqlite-execute db "DELETE FROM edge WHERE src=?" (vector src)))
    (dolist (edge (org-glance-headline-metadata:relations metadata))
      (when-let* ((dst (caar (sqlite-select db "SELECT id FROM headline WHERE org_id=?"
                                            (vector (car edge))))))
        (sqlite-execute
         db "INSERT OR REPLACE INTO edge(src,dst,kind,via) VALUES(?,?,?,?)"
         (vector src dst (cdr edge) "row"))))))

(cl-defun org-glance-cache--event-envelope (version)
  "Return VERSION as a canonical shared schema-2 event envelope."
  (let* ((kind (org-glance-version:kind version))
         (event (pcase kind
                  ('snapshot "snapshot-published")
                  ('tombstone "tombstone-published")
                  ('rejection "leaves-rejected")))
         (identity (list :version 2 :event event
                         :headline (org-glance-version:headline version)
                         :id (org-glance-version:id version)))
         (shape (if (eq kind 'rejection)
                    (list :targets
                          (apply #'vector (org-glance-version:targets version))
                          :observedLeaves
                          (apply #'vector
                                 (org-glance-version:observed-leaves version)))
                  (list :parents
                        (apply #'vector (org-glance-version:parents version)))))
         (tail (list :contentSha256
                     (org-glance-version:content-sha256 version)
                     :created (org-glance-version:created version)
                     :producer (org-glance-version:producer version))))
    (concat (json-serialize (append identity shape tail)) "\n")))

(cl-defun org-glance-cache--version-metadata (graph id version contents)
  "Parse VERSION's available Snapshot bytes in DIR as family ID metadata."
  (when (eq 'snapshot (org-glance-version:kind version))
    (condition-case nil
        (let* ((seed (org-glance-graph--reparse-blob
                      graph (make-org-glance-headline-metadata :tags []) contents))
               (basis (org-glance-headline:metadata* seed))
               (metadata (org-glance-headline:metadata*
                          (org-glance-graph--reparse-blob graph basis contents)))
               (record (org-glance-headline-metadata:serialize metadata)))
          (org-glance-headline-metadata:deserialize (plist-put record :id id)))
      (error nil))))

(cl-defun org-glance-cache--payload-observation (graph id version payload)
  "Return VERSION's payload state in GRAPH and any parsed metadata."
  (let ((present (car payload))
        (contents (cdr payload)))
    (if (not present)
        (cons "missing" nil)
      (let ((metadata (org-glance-cache--version-metadata
                       graph id version contents)))
        (cond
         ((not metadata) (cons "unparseable" nil))
         ((org-glance-version:valid version) (cons "available" metadata))
         (t (cons "mismatch" metadata)))))))

(cl-defun org-glance-cache--inventory-event-ids (dir)
  "Return every event-directory name currently observable below DIR."
  (let ((versions (f-join dir "versions")))
    (when (file-directory-p versions)
      (org-glance-cache--observe-inventory-directory
       versions
       (lambda () (org-glance-version:inventory dir))))))

(cl-defun org-glance-cache--inventory-proof (event-ids)
  "Return a stable local inventory proof for EVENT-IDS."
  (secure-hash 'sha256 (mapconcat #'identity event-ids "\n")))

(cl-defun org-glance-cache--revived-p (history current)
  "Return non-nil when CURRENT descends from a Tombstone in HISTORY."
  (let ((by-id (make-hash-table :test #'equal)))
    (dolist (version history)
      (puthash (org-glance-version:id version) version by-id))
    (cl-labels ((visit (id seen)
                  (unless (member id seen)
                    (when-let* ((version (gethash id by-id)))
                      (or (eq 'tombstone (org-glance-version:kind version))
                          (cl-some (lambda (parent)
                                     (visit parent (cons id seen)))
                                   (org-glance-version:parents version)))))))
      (and current (visit current nil)))))

(cl-defun org-glance-cache--project-family
    (db graph id &optional (inventory nil inventory-supplied-p))
  "Replace logical family ID's folded cache projection in DB."
  (let* ((dir (org-glance-graph:headline-data-path graph id))
         (observation (if inventory-supplied-p
                          (org-glance-version:observe dir id inventory)
                        (org-glance-version:observe dir id)))
         (inventory (org-glance-version-observation:inventory observation))
         (history (and (file-directory-p (f-join dir "versions"))
                       (org-glance-version-observation:history observation)))
         (payloads (org-glance-version-observation:payloads observation))
         (candidates (org-glance-version:candidates history))
         (leaf-ids (mapcar #'org-glance-version:id candidates))
         (sole (and (= 1 (length candidates)) (car candidates)))
         (state (cond
                 ((null history) "empty")
                 ((null candidates) "no-current")
                 ((> (length candidates) 1) "conflict")
                 ((eq 'tombstone (org-glance-version:kind sole)) "deleted")
                 ((org-glance-version:valid sole) "live")
                 (t "damaged")))
         (current (and sole (org-glance-version:id sole)))
         (revived (and (member state '("live" "damaged"))
                       (org-glance-cache--revived-p history current)))
         (ordered (sort (copy-sequence history)
                        (lambda (a b) (string< (org-glance-version:id a)
                                               (org-glance-version:id b)))))
         (envelopes (mapcar #'org-glance-cache--event-envelope ordered))
         (fingerprint (secure-hash 'sha256
                                   (mapconcat #'identity envelopes "")))
         (inventory-proof (org-glance-cache--inventory-proof inventory))
         (admitted (mapcar #'org-glance-version:id history))
         (payload-observations (make-hash-table :test #'equal))
         (previous (car (sqlite-select
                         db
                         "SELECT logical_id FROM headline_family WHERE logical_id=?"
                         (vector id))))
         (previous-events
          (mapcar #'car
                  (sqlite-select
                   db
                   "SELECT event_id FROM headline_event WHERE logical_id=?"
                   (vector id))))
         (verified (and (null (cl-set-exclusive-or inventory admitted
                                                   :test #'equal))
                        (null (cl-set-difference previous-events inventory
                                                 :test #'equal))))
         (projected nil))
    (unless verified
      (if previous
          (sqlite-execute
           db
           "UPDATE headline_family SET reconciled_epoch=?,verified=0 WHERE logical_id=?"
           (vector inventory-proof id))
        (sqlite-execute
         db
         "INSERT INTO headline_family(logical_id,state,current_version_id,family_fingerprint,reconciled_epoch,verified,revived) VALUES(?,?,?,?,?,0,0)"
         (vector id "empty" nil fingerprint inventory-proof)))
      (cl-return-from org-glance-cache--project-family nil))
    (sqlite-execute db "DELETE FROM headline_family WHERE logical_id=?" (vector id))
    (when history
      (sqlite-execute
       db
       "INSERT INTO headline_family(logical_id,state,current_version_id,family_fingerprint,reconciled_epoch,verified,revived) VALUES(?,?,?,?,?,?,?)"
       (vector id state current fingerprint inventory-proof 1 (if revived 1 0)))
      (dolist (version history)
        (let* ((event (org-glance-version:id version))
               (version-kind (org-glance-version:kind version))
               (kind (pcase version-kind
                       ('snapshot "snapshot-published")
                       ('tombstone "tombstone-published")
                       ('rejection "leaves-rejected")))
               (parents (json-serialize
                         (apply #'vector (org-glance-version:parents version))))
               (targets (json-serialize
                         (apply #'vector (org-glance-version:targets version))))
               (observed (json-serialize
                          (apply #'vector
                                 (org-glance-version:observed-leaves version))))
               (envelope (org-glance-cache--event-envelope version)))
          (sqlite-execute
           db
           "INSERT INTO headline_event(logical_id,event_id,kind,parents_json,targets_json,observed_leaves_json,payload_digest,producer,created,envelope) VALUES(?,?,?,?,?,?,?,?,?,?)"
           (vector id event kind parents targets observed
                   (org-glance-version:content-sha256 version)
                   (org-glance-version:producer version)
                   (org-glance-version:created version) envelope))
          (when (eq 'snapshot (org-glance-version:kind version))
            (let ((observation
                   (org-glance-cache--payload-observation
                    graph id version (gethash event payloads))))
              (when (member event leaf-ids)
                (puthash event observation payload-observations))
              (sqlite-execute
               db
               "INSERT INTO headline_payload_observation(logical_id,event_id,path,observed_digest,payload_state) VALUES(?,?,?,?,?)"
               (vector id event
                       (org-glance-version:data-file dir event)
                       (and (equal "available" (car observation))
                            (org-glance-version:content-sha256 version))
                       (car observation)))))))
      (dolist (version candidates)
        (when (eq 'snapshot (org-glance-version:kind version))
          (let* ((event (org-glance-version:id version))
                 (observation (gethash event payload-observations))
                 (metadata
                  (and (cdr observation)
                       (if (equal "available" (car observation))
                           (cdr observation)
                         (org-glance-headline-metadata:deserialize
                          (plist-put
                           (org-glance-headline-metadata:serialize
                            (cdr observation))
                           :recovery (car observation))))))
                 (role (cond
                        ((not (org-glance-version:valid version)) "recovery")
                        ((> (length leaf-ids) 1) "conflict")
                        (t "current")))
                 (payload (and metadata
                               (decode-coding-string
                                (json-serialize
                                 (org-glance-headline-metadata:serialize metadata))
                                'utf-8 t))))
            (when payload
              (setq projected t)
              (sqlite-execute
               db
               "INSERT INTO headline_projection(logical_id,event_id,role,source_digest,parser_contract,record_payload) VALUES(?,?,?,?,?,?)"
               (vector id event role
                       (org-glance-version:content-sha256 version)
                       "org-glance:1" payload))
              (sqlite-execute
               db
               "INSERT INTO producer_projection(logical_id,event_id,producer,parser_contract,record_payload) VALUES(?,?,?,?,?)"
               (vector id event "org-glance" "org-glance:1" payload))))))
      (unless (or projected (member state '("deleted" "empty")))
        (let* ((observation (and sole
                                 (eq 'snapshot (org-glance-version:kind sole))
                                 (gethash (org-glance-version:id sole)
                                          payload-observations)))
               (damage (or (car-safe observation) "unavailable"))
               (metadata (make-org-glance-headline-metadata
                          :id id :title (format "%s — %s recovery" id damage)
                          :tags [] :recovery damage))
               (payload (decode-coding-string
                         (json-serialize
                          (org-glance-headline-metadata:serialize metadata))
                         'utf-8 t)))
          (sqlite-execute
           db
           "INSERT INTO headline_projection(logical_id,event_id,role,source_digest,parser_contract,record_payload) VALUES(?,?,?,?,?,?)"
           (vector id nil "recovery" nil "org-glance:1" payload))
          (sqlite-execute
           db
           "INSERT INTO producer_projection(logical_id,event_id,producer,parser_contract,record_payload) VALUES(?,?,?,?,?)"
           (vector id nil "org-glance" "org-glance:1" payload)))))))

(cl-defun org-glance-cache--delete (db graph id)
  "Delete ID's source projection from GRAPH's DB."
  (ignore graph)
  (dolist (row (sqlite-select db "SELECT path FROM headline WHERE org_id=?" (vector id)))
    (sqlite-execute db "DELETE FROM source WHERE path=?" (vector (car row)))))

(cl-defun org-glance-cache--headlines (graph)
  "Return GRAPH's live compatibility-WAL metadata."
  (org-glance-graph--wal-headlines graph))

(cl-defun org-glance-cache--family-ids (graph)
  "Return every immutable family ID in GRAPH's portable and local roots."
  (let ((seen (make-hash-table :test #'equal)) ids)
    (cl-labels
        ((walk (data dir)
          (org-glance-cache--observe-inventory-directory
           dir
           (lambda ()
             (cl-loop
              for path in (directory-files dir t directory-files-no-dot-files-regexp)
              when (and (file-directory-p path) (not (file-symlink-p path)))
              if (file-directory-p (f-join path "versions"))
              collect (mapconcat #'identity
                                 (split-string (file-relative-name path data) "/" t)
                                 "")
              else append (walk data path))))))
      (dolist (data (org-glance-graph:data-paths graph))
        (when (file-directory-p data)
          (dolist (id (walk data data))
            (when (gethash id seen)
              (error "org-glance: family %s exists in portable and local storage" id))
            (puthash id t seen)
            (push id ids))))
      (sort ids #'string<))))

(defvar org-glance-cache--inventory-directory-proofs
  (make-hash-table :test #'eq :weakness 'key)
  "Stable directory stamps for each live graph's last complete inventory.")

(defvar org-glance-cache--recovery-publication-hook nil
  "Function called after recovery validation and before cache publication.")

(defvar org-glance-cache--recovery-phase-hook nil
  "Function called with each recovery generation publication phase.")

(defvar org-glance-cache--collect-inventory-proof nil
  "Non-nil while a complete inventory records stable directory stamps.")

(defvar org-glance-cache--observed-inventory-directories nil
  "Directory stamps accumulated by the current complete inventory.")

(defconst org-glance-cache--git-proof-name "inventory-git-proof.eld"
  "Machine-local clean-Git proof for the persisted family inventory.")

(cl-defun org-glance-cache--git-output (directory &rest args)
  "Return Git stdout for ARGS in DIRECTORY, or nil when the command fails."
  (when (executable-find "git")
    (with-temp-buffer
      (let ((default-directory directory))
        (when (= 0 (apply #'process-file "git" nil (list (current-buffer) nil)
                          nil args))
          (string-trim-right (buffer-string)))))))

(cl-defun org-glance-cache--clean-git-proof (graph)
  "Return GRAPH's clean repository identity and HEAD, or nil when uncertain."
  (when-let* ((directory (org-glance-graph:directory graph))
              (root (org-glance-cache--git-output
                     directory "rev-parse" "--show-toplevel"))
              ((not (string-empty-p root)))
              (head (org-glance-cache--git-output root "rev-parse" "HEAD"))
              ((not (string-empty-p head)))
              (relative (file-relative-name directory root))
              (status (org-glance-cache--git-output
                       root "status" "--porcelain=v2" "--untracked-files=all"
                       "--" relative))
              ((string-empty-p status)))
    (list :root (file-truename root) :directory relative :head head)))

(cl-defun org-glance-cache--git-proof-path (graph)
  "Return GRAPH's machine-local clean-Git proof path."
  (org-glance-graph:cache-file graph org-glance-cache--git-proof-name))

(cl-defun org-glance-cache--persist-git-proof (graph)
  "Persist GRAPH's clean Git state, removing an inapplicable prior proof."
  (if-let* ((proof (org-glance-cache--clean-git-proof graph)))
      (org-glance-graph:cache-write graph org-glance-cache--git-proof-name proof)
    (let ((path (org-glance-cache--git-proof-path graph)))
      (when (f-exists? path) (f-delete path)))))

(cl-defun org-glance-cache--git-diff-paths (root before after relative)
  "Return (t . PATHS) changed from BEFORE to AFTER below RELATIVE, or nil."
  (with-temp-buffer
    (let ((default-directory root))
      (when (= 0 (process-file "git" nil (current-buffer) nil
                               "diff" "--name-only" "-z"
                               before after "--" relative))
        (cons t (split-string (buffer-string) "\0" t))))))

(cl-defun org-glance-cache--family-id-from-git-path (data-relative path)
  "Return the family ID below DATA-RELATIVE named by Git PATH."
  (when (string-prefix-p (file-name-as-directory data-relative) path)
    (let* ((tail (string-remove-prefix
                  (file-name-as-directory data-relative) path))
           (parts (split-string tail "/" t))
           (versions (cl-position "versions" parts :test #'equal)))
      (when (memq versions '(1 2))
        (mapconcat #'identity (cl-subseq parts 0 versions) "")))))

(cl-defun org-glance-cache--clean-git-family-delta (graph)
  "Return (:proof PROOF :ids IDS) for a safe clean Git transition, or nil."
  (unless (file-directory-p (org-glance-graph:local-data-path graph))
    (when-let* ((held (org-glance-graph:cache-read
                     graph org-glance-cache--git-proof-name))
              (current (org-glance-cache--clean-git-proof graph))
              ((equal (plist-get held :root) (plist-get current :root)))
              ((equal (plist-get held :directory)
                      (plist-get current :directory)))
              (root (plist-get current :root))
              (data-relative
               (file-relative-name (org-glance-graph:data-path graph) root))
              (diff (org-glance-cache--git-diff-paths
                     root (plist-get held :head) (plist-get current :head)
                     data-relative)))
    (let ((ids (mapcar (apply-partially
                        #'org-glance-cache--family-id-from-git-path
                        data-relative)
                       (cdr diff))))
      (when (cl-every #'identity ids)
        (list :proof current :ids (sort (delete-dups ids) #'string<)))))))

(cl-defun org-glance-cache--directory-stamp (dir)
  "Return DIR's entry-change stamp, or nil when DIR disappeared."
  (when-let* ((attributes (file-attributes dir 'string)))
    (file-attribute-modification-time attributes)))

(cl-defun org-glance-cache--observe-inventory-directory (dir thunk)
  "Call THUNK while recording a stable entry stamp for DIR."
  (if (not org-glance-cache--collect-inventory-proof)
      (funcall thunk)
    (cl-loop repeat 3
             for before = (org-glance-cache--directory-stamp dir)
             for result = (funcall thunk)
             for after = (org-glance-cache--directory-stamp dir)
             when (equal before after)
             return (progn
                      (push (cons dir after)
                            org-glance-cache--observed-inventory-directories)
                      result)
             finally (error "org-glance: immutable inventory changed during census: %s"
                            dir))))

(cl-defun org-glance-cache--inventory-proof-current-p (graph)
  "Return non-nil when GRAPH's last directory-entry proof remains current."
  (let ((git-path (org-glance-cache--git-proof-path graph)))
    (if (f-exists? git-path)
        (when-let* ((held (org-glance-graph:cache-read
                           graph org-glance-cache--git-proof-name)))
          (equal held (org-glance-cache--clean-git-proof graph)))
      (when-let* ((proof
                   (or (gethash graph org-glance-cache--inventory-directory-proofs)
                       (let ((loaded (org-glance-cache--load-inventory-proof graph)))
                         (when loaded
                           (puthash graph loaded
                                    org-glance-cache--inventory-directory-proofs)
                           loaded)))))
        (org-glance-cache--directory-proof-current-p proof)))))

(cl-defun org-glance-cache--projection-coherent-p (graph)
  "Return non-nil when GRAPH's shared projection is internally coherent."
  (= 1
     (or
      (org-glance-cache--read-db
       graph
       (lambda (db)
         (caar
          (sqlite-select
           db
           "SELECT NOT EXISTS (SELECT 1 FROM headline LEFT JOIN headline_family ON headline_family.logical_id=headline.org_id WHERE headline.org_id IS NOT NULL AND headline_family.logical_id IS NULL UNION ALL SELECT 1 FROM headline_family WHERE state NOT IN ('deleted','empty') AND NOT EXISTS (SELECT 1 FROM headline_projection WHERE headline_projection.logical_id=headline_family.logical_id AND headline_projection.parser_contract='org-glance:1'))"))))
      0)))

(cl-defun org-glance-cache--directory-proof-current-p (proof)
  "Return non-nil when every directory stamp in PROOF remains current."
  (cl-every (lambda (entry)
              (equal (cdr entry)
                     (org-glance-cache--directory-stamp (car entry))))
            proof))

(cl-defun org-glance-cache--load-inventory-proof (graph)
  "Return GRAPH's persisted directory-entry proof."
  (org-glance-cache--with-db
   graph
   (lambda (db)
     (mapcar
      (lambda (row)
        (cons (car row) (json-parse-string (cadr row) :array-type 'list)))
      (sqlite-select db "SELECT path,stamp FROM inventory_directory")))))

(cl-defun org-glance-cache--persist-inventory-proof (graph proof)
  "Replace GRAPH's persisted directory-entry PROOF."
  (org-glance-cache--with-db
   graph
   (lambda (db)
     (org-glance-cache--transaction
      db
      (lambda ()
        (sqlite-execute db "DELETE FROM inventory_directory")
        (dolist (entry proof)
          (sqlite-execute
           db "INSERT INTO inventory_directory(path,stamp) VALUES(?,?)"
           (vector (car entry) (json-serialize (vconcat (cdr entry)))))))))))

(cl-defun org-glance-cache--record-complete-inventory (graph thunk)
  "Call THUNK and retain GRAPH's stable directory-entry proof on success."
  (let ((org-glance-cache--collect-inventory-proof t)
        (org-glance-cache--observed-inventory-directories nil))
    (prog1 (funcall thunk)
      (let ((proof (delete-dups org-glance-cache--observed-inventory-directories)))
        (puthash graph proof org-glance-cache--inventory-directory-proofs)
        (org-glance-cache--persist-inventory-proof graph proof)
        (org-glance-cache--persist-git-proof graph)))))

(cl-defun org-glance-cache:publish-staged (live stage &optional hook)
  "Validate STAGE and atomically replace the database at LIVE.
Call HOOK after validation and before publication when non-nil."
  (let ((db (sqlite-open stage)))
    (unwind-protect
        (progn
          (unless (equal '(("ok")) (sqlite-select db "PRAGMA integrity_check"))
            (error "org-glance: staged SQLite integrity failure"))
          (when (sqlite-select db "PRAGMA foreign_key_check")
            (error "org-glance: staged SQLite foreign-key failure"))
          (unless (= org-glance-cache:version
                     (caar (sqlite-select db "PRAGMA user_version")))
            (error "org-glance: staged SQLite format mismatch"))
          (when (sqlite-select
                 db
                 "SELECT logical_id FROM headline_projection WHERE role IN ('current','conflict','recovery','rejected') AND (record_payload IS NULL OR NOT json_valid(record_payload)) LIMIT 1")
            (error "org-glance: staged SQLite projection failure"))
          (sqlite-select db "PRAGMA wal_checkpoint(TRUNCATE)")
          (sqlite-select db "PRAGMA journal_mode=DELETE"))
      (sqlite-close db)))
  (when hook (funcall hook))
  (dolist (sidecar (list (concat live "-shm") (concat live "-wal")))
    (when (file-exists-p sidecar) (delete-file sidecar)))
  (rename-file stage live t))

(cl-defun org-glance-cache--clone-live-generation (live stage)
  "Copy LIVE's committed database generation to STAGE when it exists."
  (when (file-exists-p live)
    (let ((db (sqlite-open live)))
      (unwind-protect
          (progn
            (sqlite-execute db "PRAGMA busy_timeout=5000")
            (sqlite-execute db "VACUUM INTO ?" (vector stage)))
        (sqlite-close db)))))

(cl-defun org-glance-cache--generation-token (path)
  "Return an O(1) token for the committed SQLite generation at PATH."
  (mapcar
   (lambda (candidate)
     (when-let* ((attributes (file-attributes candidate 'string)))
       (list (file-attribute-file-identifier attributes)
             (file-attribute-size attributes)
             (file-attribute-modification-time attributes)
             (file-attribute-status-change-time attributes))))
   (list path (concat path "-wal"))))

(cl-defun org-glance-cache--clear-owned-generation (graph)
  "Remove org-glance-owned state and return incoming shared edges from GRAPH."
  (org-glance-cache--with-db
   graph
   (lambda (db)
     (org-glance-cache--transaction
      db
      (lambda ()
        (let ((incoming
               (sqlite-select
                db
                "SELECT edge.src,edge.dst,dst_headline.org_id,edge.kind,edge.via FROM edge JOIN headline AS src_headline ON edge.src=src_headline.id JOIN headline AS dst_headline ON edge.dst=dst_headline.id WHERE src_headline.producer!='org-glance' AND dst_headline.producer='org-glance'")))
          (sqlite-execute db "DELETE FROM headline_family")
          (sqlite-execute db "DELETE FROM headline WHERE producer='org-glance'")
          (sqlite-execute db "DELETE FROM source WHERE producer='org-glance'")
          (sqlite-execute db "DELETE FROM inventory_directory")
          incoming))))))

(cl-defun org-glance-cache--restore-shared-edges (graph edges)
  "Restore non-org-glance producer EDGES into GRAPH's selected cache."
  (org-glance-cache--with-db
   graph
   (lambda (db)
     (org-glance-cache--transaction
      db
      (lambda ()
        (dolist (edge edges)
          (when-let* ((source
                       (caar (sqlite-select
                              db "SELECT id FROM headline WHERE id=?"
                              (vector (nth 0 edge)))))
                      (target
                       (or (caar (sqlite-select
                                  db "SELECT id FROM headline WHERE id=?"
                                  (vector (nth 1 edge))))
                           (caar (sqlite-select
                                  db
                                  "SELECT id FROM headline WHERE org_id=? ORDER BY rowid LIMIT 1"
                                  (vector (nth 2 edge)))))))
            (sqlite-execute
             db "INSERT OR IGNORE INTO edge(src,dst,kind,via) VALUES(?,?,?,?)"
             (vector source target (nth 3 edge) (nth 4 edge))))))))))

(cl-defun org-glance-cache:recover (graph)
  "Rebuild GRAPH into a separate generation and publish it atomically."
  (let* ((live (org-glance-cache:path graph))
         (parent (file-name-directory live))
         (_ (make-directory parent t))
         (stage-dir (make-temp-file
                     (expand-file-name ".recovery-stage-" parent) t))
         (stage (expand-file-name "glance.sqlite3" stage-dir))
         (missing (make-symbol "missing"))
         (previous-proof
          (gethash graph org-glance-cache--inventory-directory-proofs missing))
         live-token
         retained-edges
         published)
    (unwind-protect
        (progn
          (org-glance-cache--clone-live-generation live stage)
          (setq live-token (org-glance-cache--generation-token live))
          (cl-letf (((symbol-function 'org-glance-cache:path)
                     (lambda (_graph) stage))
                    ((symbol-function 'org-glance-cache--inventory-proof-current-p)
                     (lambda (_graph) nil))
                    ((symbol-function 'org-glance-cache--persist-git-proof)
                     (lambda (_graph) nil)))
            (setq retained-edges
                  (org-glance-cache--clear-owned-generation graph))
            (org-glance-cache:refresh graph)
            (org-glance-cache--restore-shared-edges graph retained-edges))
          (org-glance-cache:publish-staged
           live stage
           (lambda ()
             (when org-glance-cache--recovery-publication-hook
               (funcall org-glance-cache--recovery-publication-hook))
             (let ((current-token (org-glance-cache--generation-token live)))
               (unless (equal live-token current-token)
                 (error "org-glance: live generation changed before publication")))
             (when org-glance-cache--recovery-phase-hook
               (funcall org-glance-cache--recovery-phase-hook
                        'before-publication))))
          (when org-glance-cache--recovery-phase-hook
            (funcall org-glance-cache--recovery-phase-hook 'after-publication))
          (setq published t)
          (org-glance-cache--persist-git-proof graph)
          (org-glance-graph--invalidate-cache graph))
      (unless published
        (if (eq previous-proof missing)
            (remhash graph org-glance-cache--inventory-directory-proofs)
          (puthash graph previous-proof
                   org-glance-cache--inventory-directory-proofs)))
      (when (file-exists-p stage-dir) (delete-directory stage-dir t)))))

(cl-defun org-glance-cache--inventory-current-p (held family)
  "Return non-nil when HELD proves observed FAMILY is already projected."
  (let ((row (assoc (car family) held)))
    (and row
         (= 1 (or (nth 2 row) 0))
         (equal (org-glance-cache--inventory-proof (cdr family))
                (nth 1 row)))))

(cl-defun org-glance-cache--checkpoint-relative-path (graph path)
  "Return portable PATH relative to GRAPH, refusing unsafe source files."
  (let ((root (file-name-as-directory (org-glance-graph:directory graph)))
        (absolute (expand-file-name path)))
    (unless (and (f-file? absolute)
                 (not (file-symlink-p absolute))
                 (file-in-directory-p absolute root))
      (error "org-glance: checkpoint authority path is unsafe: %s" path))
    (file-relative-name absolute root)))

(cl-defun org-glance-cache:checkpoint-source-paths (graph)
  "Return GRAPH's exact verified authority paths for checkpoint admission.
Paths are sorted, distinct, relative to GRAPH, and exclude local cache state."
  (unless (org-glance-cache--inventory-proof-current-p graph)
    (error "org-glance: checkpoint requires a current source inventory"))
  (org-glance-cache--with-db
   graph
   (lambda (db)
     (when (sqlite-select
            db "SELECT logical_id FROM headline_family WHERE verified!=1 LIMIT 1")
       (error "org-glance: checkpoint requires every family to be verified"))
     (let ((paths
            (append
             (list (org-glance-graph--manifest-path graph))
             (org-glance-graph--live-segments graph)
             (let ((system (org-glance-graph:config-file graph "system.org"))
                   (tags (org-glance-graph:config-file graph "tags")))
               (append
                (when (file-regular-p system) (list system))
                (when (and (not (and (boundp 'org-glance-tag-config-dir)
                                     org-glance-tag-config-dir))
                           (file-directory-p tags))
                  (directory-files tags t "\\.org\\'"))))
             (mapcar #'car (sqlite-select db "SELECT path FROM source"))
             (mapcar #'car
                     (sqlite-select
                      db
                      "SELECT path FROM headline_payload_observation WHERE payload_state!='missing'"))
             (mapcar
              (lambda (row)
                (f-join
                 (org-glance-version:path
                  (org-glance-graph:headline-data-path graph (car row))
                  (cadr row))
                 "meta.json"))
              (sqlite-select
               db "SELECT logical_id,event_id FROM headline_event"))))
           (local (file-name-as-directory
                   (org-glance-graph:local-data-path graph))))
       (sort
        (delete-dups
         (mapcar
          (lambda (path)
            (org-glance-cache--checkpoint-relative-path graph path))
          (cl-remove-if
           (lambda (path)
             (string-prefix-p local (file-name-as-directory
                                     (expand-file-name path))))
           paths)))
        #'string<)))))

(cl-defun org-glance-cache--elapsed (started)
  "Format seconds elapsed since STARTED."
  (format "%.1fs" (- (float-time) started)))

(cl-defun org-glance-cache--report-progress
    (started phase completed total &optional (unit "families"))
  "Report cache rebuild PHASE and counts since STARTED.
COMPLETED and TOTAL count UNIT values."
  (message "org-glance: cache rebuild: %s (%d/%d %s, %s)"
           phase completed total unit (org-glance-cache--elapsed started)))

(cl-defun org-glance-cache--report-complete (started rebuilt total)
  "Report REBUILT of TOTAL families completed since STARTED."
  (message "org-glance: cache rebuild complete (%d/%d families rebuilt, %s)"
           rebuilt total (org-glance-cache--elapsed started)))

(cl-defun org-glance-cache--progress-point-p (completed total)
  "Return non-nil when COMPLETED of TOTAL merits a progress message."
  (or (= completed total)
      (= completed 0)
      (and (> org-glance-cache-progress-step 0)
           (= 0 (% completed org-glance-cache-progress-step)))))

(cl-defun org-glance-cache--after-append (graph specs)
  "Apply GRAPH's appended SPECS to the shared cache."
  (let* ((headlines (org-glance-cache--headlines graph))
         (touched (mapcar (lambda (spec)
                            (if (org-glance-headline-metadata? spec)
                                (org-glance-headline-metadata:id spec)
                              (plist-get spec :id)))
                          specs)))
    (org-glance-cache--with-db
     graph
     (lambda (db)
       (org-glance-cache--transaction
        db
        (lambda ()
          (org-glance-cache--project-compatibility-changes
           db graph touched headlines)
          (dolist (id (delete-dups touched))
            (org-glance-cache--project-family db graph id))))))))

(cl-defun org-glance-cache--project-compatibility-changes
    (db graph ids &optional headlines)
  "Project compatibility IDS and affected edges into DB for GRAPH.
HEADLINES defaults to GRAPH's live compatibility-WAL metadata."
  (let ((live (or headlines (org-glance-cache--headlines graph)))
        (changed (delete-dups (copy-sequence ids))))
    (dolist (id changed)
      (if-let* ((metadata
                 (cl-find id live
                          :key #'org-glance-headline-metadata:id
                          :test #'equal)))
          (org-glance-cache--project db graph metadata)
        (org-glance-cache--delete db graph id)))
    (dolist (metadata live)
      (when (or (member (org-glance-headline-metadata:id metadata) changed)
                (cl-some (lambda (target) (member target changed))
                         (org-glance-headline-metadata:relation-targets metadata)))
        (org-glance-cache--project-edges db metadata)))))

(cl-defun org-glance-cache--decode-metadata (payload)
  "Decode one serialized metadata PAYLOAD, returning nil when malformed."
  (ignore-errors
    (org-glance-headline-metadata:deserialize
     (json-parse-string payload :object-type 'plist))))

(cl-defun org-glance-cache--family-metadata (db id)
  "Return ID's current immutable-family metadata from DB."
  (when-let* ((row (car (sqlite-select
                         db
                         "SELECT record_payload FROM headline_projection WHERE logical_id=? AND role='current' ORDER BY event_id LIMIT 1"
                         (vector id)))))
    (org-glance-cache--decode-metadata (car row))))

(cl-defun org-glance-cache--family-inbound-headlines (db ids)
  "Return current family metadata whose relations target IDS."
  (cl-delete-duplicates
   (delq nil
         (cl-loop
          for id in ids
          append
          (mapcar
           (lambda (row) (org-glance-cache--decode-metadata (car row)))
           (sqlite-select
            db
            "SELECT record_payload FROM headline_projection WHERE role='current' AND EXISTS (SELECT 1 FROM json_each(headline_projection.record_payload,'$.relations') WHERE json_extract(json_each.value,'$[0]')=?)"
            (vector id)))))
   :key #'org-glance-headline-metadata:id :test #'equal))

(cl-defun org-glance-cache--project-family-compatibility-changes (db graph ids)
  "Project changed immutable-family IDS and affected edges into DB."
  (let* ((changed (delete-dups (copy-sequence ids)))
         (metadata (delq nil (mapcar
                              (apply-partially
                               #'org-glance-cache--family-metadata db)
                              changed))))
    (dolist (id changed)
      (if-let* ((current
                 (cl-find id metadata
                          :key #'org-glance-headline-metadata:id
                          :test #'equal)))
          (org-glance-cache--project db graph current)
        (org-glance-cache--delete db graph id)))
    (dolist (current
             (cl-delete-duplicates
              (append metadata
                      (org-glance-cache--family-inbound-headlines db changed))
              :key #'org-glance-headline-metadata:id :test #'equal))
      (org-glance-cache--project-edges db current))))

(cl-defun org-glance-cache:refresh (graph)
  "Reconcile GRAPH's live headlines into the shared cache."
  (let ((started (float-time)))
    (if (org-glance-cache--inventory-proof-current-p graph)
        (message "org-glance: cache proof current (0 families rebuilt, %s)"
                 (org-glance-cache--elapsed started))
      (message "org-glance: cache rebuild: census (0/? families, %s)"
               (org-glance-cache--elapsed started))
      (org-glance-cache--record-complete-inventory
       graph
       (lambda ()
         (let* ((headlines (org-glance-cache--headlines graph))
                (families
                 (delete-dups
                  (append (mapcar #'org-glance-headline-metadata:id headlines)
                          (org-glance-cache--family-ids graph))))
                (total (length families))
                (headline-total (length headlines))
                observed
                (rebuilt 0))
           (org-glance-cache--report-progress started "census" 0 total)
           (cl-loop for id in families
                    for completed from 1
                    do (push
                        (cons id (org-glance-cache--inventory-event-ids
                                  (org-glance-graph:headline-data-path graph id)))
                        observed)
                    when (org-glance-cache--progress-point-p completed total)
                    do (org-glance-cache--report-progress
                        started "census" completed total))
           (unless families
             (org-glance-cache--report-progress started "census" 0 0))
           (setq observed (nreverse observed))
           (org-glance-cache--with-db
            graph
            (lambda (db)
              (let* ((held
                      (sqlite-select
                       db
                       "SELECT logical_id,reconciled_epoch,verified FROM headline_family"))
                     (changed
                      (cl-remove-if
                       (lambda (family)
                         (org-glance-cache--inventory-current-p held family))
                       observed))
                     (changed-total (length changed)))
                (org-glance-cache--report-progress
                 started "families" 0 changed-total)
                (org-glance-cache--transaction
                 db
                 (lambda ()
                   (let ((live (mapcar #'org-glance-headline-metadata:id headlines)))
                     (dolist (row (sqlite-select db "SELECT headline.org_id,org_headline.path FROM org_headline JOIN headline USING(id)"))
                       (unless (member (car row) live)
                         (sqlite-execute db "DELETE FROM source WHERE path=?"
                                         (vector (cadr row))))))
                   (dolist (family changed)
                     (org-glance-cache--project-family
                      db graph (car family) (cdr family))
                     (cl-incf rebuilt)
                     (when (org-glance-cache--progress-point-p
                            rebuilt changed-total)
                       (org-glance-cache--report-progress
                        started "families" rebuilt changed-total)))
                   (org-glance-cache--report-progress
                    started "headlines" 0 headline-total "headlines")
                   (cl-loop for metadata in headlines
                            for completed from 1
                            do
                            (let* ((id (org-glance-headline-metadata:id metadata))
                                   (state
                                    (caar
                                     (sqlite-select
                                      db
                                      "SELECT state FROM headline_family WHERE logical_id=?"
                                      (vector id)))))
                              (when (or (null state)
                                        (member state '("live" "damaged")))
                                (org-glance-cache--project db graph metadata)))
                            when (org-glance-cache--progress-point-p
                                  completed headline-total)
                            do (org-glance-cache--report-progress
                                started "headlines" completed
                                headline-total "headlines"))
                   (org-glance-cache--report-progress
                    started "relations" 0 headline-total "headlines")
                   (cl-loop for metadata in headlines
                            for completed from 1
                            do (org-glance-cache--project-edges db metadata)
                            when (org-glance-cache--progress-point-p
                                  completed headline-total)
                            do (org-glance-cache--report-progress
                                started "relations" completed
                                headline-total "headlines"))
                   (sqlite-execute
                    db
                    "DELETE FROM source WHERE producer='org-glance' AND NOT EXISTS (SELECT 1 FROM headline WHERE headline.path=source.path)"))))))
           (org-glance-cache--report-complete started rebuilt total)))))))

(cl-defun org-glance-cache:reconcile-families (graph ids)
  "Reconcile immutable family IDS into GRAPH's shared projection."
  (org-glance-cache--with-db
   graph
   (lambda (db)
     (org-glance-cache--transaction
      db
      (lambda ()
        (dolist (id (delete-dups (copy-sequence ids)))
          (org-glance-cache--project-family db graph id)))))))

(cl-defun org-glance-cache:reconcile-inventory (graph &optional complete)
  "Reconcile immutable families whose inventory checkpoint changed in GRAPH.
COMPLETE forces a census when the local projection cannot trust a Git delta.
Return the number of families folded."
  (if-let* ((delta (and (not complete)
                        (org-glance-cache--clean-git-family-delta graph))))
      (let ((ids (plist-get delta :ids)))
        (message "org-glance: cache reconcile: clean Git delta (%d families)"
                 (length ids))
        (org-glance-cache:reconcile-families graph ids)
        (org-glance-graph:cache-write
         graph org-glance-cache--git-proof-name (plist-get delta :proof))
        (length ids))
    (message "org-glance: cache reconcile: complete census (no safe clean Git transition)")
    (org-glance-cache--record-complete-inventory
     graph
     (lambda ()
       (let ((current (org-glance-cache--family-ids graph)))
       (org-glance-cache--with-db
        graph
        (lambda (db)
          (let* ((held (sqlite-select
                     db
                     "SELECT logical_id,reconciled_epoch,verified FROM headline_family"))
              (ids (sort (delete-dups
                          (append current (mapcar #'car held))) #'string<))
              (observed
               (mapcar
                (lambda (id)
                  (cons id (org-glance-cache--inventory-event-ids
                            (org-glance-graph:headline-data-path graph id))))
                ids))
              (changed
               (if complete
                   observed
                 (cl-remove-if-not
                  (lambda (family)
                    (not (org-glance-cache--inventory-current-p held family)))
                  observed))))
         (org-glance-cache--transaction
          db
          (lambda ()
            (dolist (family changed)
              (org-glance-cache--project-family
               db graph (car family) (cdr family)))
            (org-glance-cache--project-family-compatibility-changes
             db graph (mapcar #'car changed))))
            (length changed)))))))))

(defvar org-glance-cache--inventory-timer nil
  "Timer that reconciles immutable inventories for open graphs.")

(defvar org-glance-cache--refresh-processes
  (make-hash-table :test #'eq :weakness 'key)
  "Active cache-refresh subprocess for each live graph.")

(defvar org-glance-cache--refresh-requested-at
  (make-hash-table :test #'eq :weakness 'key)
  "Time of the latest queued cache refresh for each live graph.")

(defcustom org-glance-cache-refresh-interval 60
  "Minimum seconds between automatic cache refresh jobs for one graph."
  :type 'number
  :group 'org-glance)

(defcustom org-glance-cache-notification-interval 1
  "Seconds between bounded external-relay wake-up checks."
  :type 'number
  :group 'org-glance)

(defvar org-glance-cache--refresh-synchronously nil
  "Non-nil makes graph-open refresh deterministic for the test harness.")

(cl-defun org-glance-cache--worker-command (graph)
  "Return the argv that reconciles GRAPH in an isolated batch Emacs."
  (org-glance--batch-worker-command
   'org-glance-cache:refresh-batch
   (list (org-glance-graph:directory graph))))

(cl-defun org-glance-cache--reconcile-now (graph)
  "Bring GRAPH's cache generation current from authoritative families."
  (if (file-exists-p (org-glance-cache:path graph))
      (let ((proof-current
             (org-glance-cache--inventory-proof-current-p graph)))
        (unless (and proof-current
                     (org-glance-cache--projection-coherent-p graph))
          (if proof-current
              (org-glance-cache:reconcile-inventory graph t)
            (org-glance-cache:reconcile-inventory graph))))
    (org-glance-cache:recover graph)))

(cl-defun org-glance-cache:refresh-batch (directory)
  "Reconcile DIRECTORY once in the current noninteractive Emacs."
  (let* ((org-glance-graph-after-open-functions nil)
         (graph (org-glance-graph directory)))
    (prog1 (org-glance-cache--reconcile-now graph)
      (org-glance-graph:refresh-external graph))))

(cl-defun org-glance-cache--refresh-finished (graph process event)
  "Retire GRAPH's PROCESS and report its terminal EVENT."
  (when (memq (process-status process) '(exit signal failed))
    (when (eq process (gethash graph org-glance-cache--refresh-processes))
      (remhash graph org-glance-cache--refresh-processes))
    (if (and (eq (process-status process) 'exit)
             (= 0 (process-exit-status process)))
        (message "org-glance: cache reconciliation process complete")
      (message "org-glance: cache reconciliation process failed; see %s: %s"
               (buffer-name (process-buffer process))
               (string-trim event)))))

(cl-defun org-glance-cache:refresh-async (graph &optional initial)
  "Queue GRAPH's cache reconciliation in a batch Emacs when due.
INITIAL queues startup work without consuming the first post-open refresh."
  (let ((active (gethash graph org-glance-cache--refresh-processes))
        (now (float-time)))
    (unless (or (and (processp active) (process-live-p active))
                (and (not initial)
                     (< (- now
                           (gethash graph
                                    org-glance-cache--refresh-requested-at
                                    most-negative-fixnum))
                        org-glance-cache-refresh-interval)))
      (unless initial
        (puthash graph now org-glance-cache--refresh-requested-at))
      (condition-case err
          (let* ((buffer
                  (get-buffer-create
                   (format " *org-glance-cache-refresh:%s*"
                           (sxhash-equal (org-glance-graph:directory graph)))))
                 (_ (with-current-buffer buffer
                      (let ((inhibit-read-only t)) (erase-buffer))))
                 (process
                  (make-process
                   :name "org-glance-cache-refresh"
                   :buffer buffer
                   :command (org-glance-cache--worker-command graph)
                   :connection-type 'pipe
                   :coding 'utf-8-unix
                   :noquery t
                   :sentinel
                   (lambda (process event)
                     (org-glance-cache--refresh-finished
                      graph process event)))))
            (puthash graph process org-glance-cache--refresh-processes))
        (error
         (message "org-glance: cache reconciliation could not start: %S" err))))))

(cl-defun org-glance-cache--refresh-on-open (graph)
  "Refresh GRAPH according to the current startup execution policy."
  (if org-glance-cache--refresh-synchronously
      (org-glance-cache--reconcile-now graph)
    (org-glance-cache:refresh-async graph t)))

(cl-defun org-glance-cache--inventory-tick ()
  "Reconcile immutable inventories for every open graph."
  (maphash
   (lambda (_directory graph)
     (condition-case err
         (org-glance-cache:refresh-async
          graph (org-glance-graph--external-pending-p graph))
       (error (message "org-glance: inventory reconciliation skipped: %S" err))))
   org-glance-graph:list))

(cl-defun org-glance-cache--start-inventory-timer (_graph)
  "Start the session-wide immutable-family inventory timer."
  (unless (timerp org-glance-cache--inventory-timer)
    (setq org-glance-cache--inventory-timer
          (run-with-timer 1 org-glance-cache-notification-interval
                          #'org-glance-cache--inventory-tick))))

(cl-defun org-glance-cache:metadata (graph id)
  "Return ID's sole family-derived metadata from GRAPH's shared cache."
  (org-glance-cache--read-db
   graph
   (lambda (db)
     (let ((rows (sqlite-select
                  db
                  "SELECT record_payload FROM headline_projection WHERE logical_id=? AND role IN ('current','recovery') ORDER BY event_id"
                  (vector id))))
       (when (= 1 (length rows))
         (org-glance-cache--decode-metadata (caar rows)))))))

(cl-defun org-glance-cache--read-projection (db)
  "Return the complete committed live metadata projection from DB."
  (let* ((ordinary
          (delq nil
                (mapcar (lambda (row)
                          (org-glance-cache--decode-metadata (car row)))
                        (sqlite-select
                         db
                         "SELECT org_headline.record FROM org_headline JOIN headline USING(id) ORDER BY headline.rowid"))))
         (families (sqlite-select
                    db
                    "SELECT logical_id,state FROM headline_family"))
         (projected
          (delq nil
                (mapcar (lambda (row)
                          (org-glance-cache--decode-metadata (car row)))
                        (sqlite-select
                         db
                         "SELECT record_payload FROM headline_projection WHERE role IN ('current','conflict','recovery','rejected') ORDER BY logical_id,event_id"))))
         (states (make-hash-table :test #'equal :size (length families)))
         (family-rows (make-hash-table :test #'equal :size (length projected)))
         (ordinary-ids (make-hash-table :test #'equal :size (length ordinary))))
    (dolist (family families)
      (puthash (car family) (cadr family) states))
    (dolist (metadata projected)
      (let ((id (org-glance-headline-metadata:id metadata)))
        (unless (gethash id family-rows)
          (puthash id metadata family-rows))))
    (dolist (metadata ordinary)
      (puthash (org-glance-headline-metadata:id metadata) t ordinary-ids))
    (append
     (delq nil
           (mapcar
            (lambda (metadata)
              (let* ((id (org-glance-headline-metadata:id metadata))
                     (state (gethash id states))
                     (family (gethash id family-rows)))
                (cond
                 ((equal state "deleted") nil)
                 (family
                  (org-glance-graph--prefer-family-metadata family metadata))
                 (t metadata))))
            ordinary))
     (cl-remove-if
      (lambda (metadata)
        (gethash (org-glance-headline-metadata:id metadata) ordinary-ids))
      projected))))

(cl-defun org-glance-cache:headlines (graph)
  "Return GRAPH's complete last committed searchable metadata projection."
  (org-glance-cache--read-db graph #'org-glance-cache--read-projection))

(cl-defun org-glance-cache--tag-candidates (db tags)
  "Return committed metadata carrying every member of TAGS from DB."
  (let* ((tags (mapcar (lambda (tag) (downcase (format "%s" tag))) tags))
         (ordinary-clause
          (mapconcat (lambda (_tag) "lower(headline.tags) LIKE ?") tags " AND "))
         (family-clause
          (mapconcat
           (lambda (_tag)
             "EXISTS (SELECT 1 FROM json_each(headline_projection.record_payload,'$.tags') WHERE lower(json_each.value)=?)")
           tags " AND "))
         (ordinary-params
          (apply #'vector (mapcar (lambda (tag) (format "%%:%s:%%" tag)) tags)))
         (tag-params (apply #'vector tags))
         (ordinary
          (sqlite-select
           db
           (concat
            "SELECT headline.org_id,org_headline.record FROM org_headline JOIN headline USING(id) WHERE "
            ordinary-clause " ORDER BY headline.rowid")
           ordinary-params))
         (projected
          (sqlite-select
           db
           (concat
            "SELECT headline_projection.logical_id,headline_projection.record_payload FROM headline_projection WHERE headline_projection.role IN ('current','conflict','recovery','rejected') AND "
            family-clause
            " ORDER BY headline_projection.logical_id,headline_projection.event_id")
           tag-params))
         (states (make-hash-table :test #'equal))
         (family-rows (make-hash-table :test #'equal))
         (ordinary-ids (make-hash-table :test #'equal))
         projected-metadata)
    (dolist (row (sqlite-select db "SELECT logical_id,state FROM headline_family"))
      (puthash (car row) (cadr row) states))
    (dolist (row projected)
      (unless (gethash (car row) family-rows)
        (when-let* ((metadata (org-glance-cache--decode-metadata (cadr row))))
          (puthash (car row) metadata family-rows)
          (push metadata projected-metadata))))
    (setq projected-metadata (nreverse projected-metadata))
    (let ((ordinary-metadata
           (delq nil
                 (mapcar
                  (lambda (row)
                    (let* ((id (car row))
                           (state (gethash id states))
                           (family (gethash id family-rows)))
                      (puthash id t ordinary-ids)
                      (cond
                       ((equal state "deleted") nil)
                       (family family)
                       (state nil)
                       (t (org-glance-cache--decode-metadata (cadr row))))))
                  ordinary))))
      (append
       ordinary-metadata
       (cl-loop for metadata in projected-metadata
                unless (gethash (org-glance-headline-metadata:id metadata)
                                ordinary-ids)
                collect metadata)))))

(cl-defun org-glance-cache:headlines-for-filter (graph filter)
  "Return committed GRAPH Rows matching FILTER, preselecting tag candidates."
  (let* ((spec (org-glance-filter:normalize-spec filter))
         (tags (org-glance-filter:tags spec))
         (candidates
          (if tags
              (org-glance-cache--read-db
               graph (lambda (db) (org-glance-cache--tag-candidates db tags)))
            (org-glance-cache:headlines graph))))
    (cl-remove-if-not (org-glance-filter:predicate spec) candidates)))

(cl-defun org-glance-cache--ordinary-headline (db id)
  "Return ordinary compatibility-WAL ID from DB, or nil."
  (when-let* ((row (car (sqlite-select
                         db
                         "SELECT org_headline.record FROM org_headline JOIN headline USING(id) WHERE headline.org_id=? ORDER BY headline.rowid LIMIT 1"
                         (vector id)))))
    (org-glance-cache--decode-metadata (car row))))

(cl-defun org-glance-cache:headline (graph id)
  "Return ID from GRAPH's committed projection, `tombstone', or nil."
  (org-glance-cache--read-db
   graph
   (lambda (db)
     (let* ((family (car (sqlite-select
                          db
                          "SELECT state FROM headline_family WHERE logical_id=?"
                          (vector id))))
            (state (car family))
            (ordinary (org-glance-cache--ordinary-headline db id))
            (projected-rows
             (and family
                  (sqlite-select
                   db
                   "SELECT record_payload FROM headline_projection WHERE logical_id=? AND role IN ('current','conflict','recovery','rejected') ORDER BY event_id"
                   (vector id))))
            (projected
             (and (= 1 (length projected-rows))
                  (org-glance-cache--decode-metadata
                   (caar projected-rows)))))
       (cond
        ((equal state "deleted") 'tombstone)
        (projected
         (org-glance-graph--prefer-family-metadata projected ordinary))
        (family nil)
        (t ordinary))))))

(cl-defun org-glance-cache:material-path (graph id)
  "Return ID's digest-verified current Snapshot path from GRAPH's committed cache.
Return nil when the family is absent, conflicted, damaged, or its bytes no
longer match the digest admitted by the pinned projection."
  (when-let* ((row
               (org-glance-cache--read-db
                graph
                (lambda (db)
                  (or
                   (car
                    (sqlite-select
                     db
                     "SELECT observation.path,event.payload_digest FROM headline_family AS family JOIN headline_event AS event ON event.logical_id=family.logical_id AND event.event_id=family.current_version_id JOIN headline_payload_observation AS observation ON observation.logical_id=event.logical_id AND observation.event_id=event.event_id WHERE family.logical_id=? AND family.state='live' AND family.verified=1 AND observation.payload_state='available'"
                     (vector id)))
                   (car
                    (sqlite-select
                     db
                     "SELECT org_headline.path,headline.digest FROM org_headline JOIN headline USING(id) WHERE headline.org_id=? AND NOT EXISTS (SELECT 1 FROM headline_family WHERE logical_id=?) ORDER BY headline.rowid LIMIT 1"
                     (vector id id)))))))
              (path (car row))
              (expected (cadr row))
              ((and (stringp expected) (file-exists-p path)))
              ((equal expected (org-glance-cache--file-digest path))))
    path))

(cl-defun org-glance-cache:incoming-relations (graph id)
  "Return indexed (SOURCE-ID . KIND) relations targeting ID in GRAPH."
  (or
   (org-glance-cache--read-db
    graph
    (lambda (db)
      (mapcar
       (lambda (row) (cons (car row) (cadr row)))
       (sqlite-select
        db
        "SELECT DISTINCT source.org_id,edge.kind FROM headline AS target JOIN edge ON edge.dst=target.id JOIN headline AS source ON edge.src=source.id WHERE target.org_id=? ORDER BY source.org_id,edge.kind"
        (vector id)))))
   nil))

(cl-defun org-glance-cache--persisted-revived-family-ids (db)
  "Return family IDs whose current event descends from a Tombstone in DB."
  (mapcar #'car
          (sqlite-select db
                         "SELECT logical_id FROM headline_family WHERE revived=1")))

(cl-defun org-glance-cache:authority (graph)
  "Return GRAPH's family-derived families and searchable projections."
  (or
   (org-glance-cache--read-db
    graph
    (lambda (db)
      (let ((families (sqlite-select
                       db
                       "SELECT logical_id,state,current_version_id FROM headline_family"))
            (headlines
             (delq nil
                   (mapcar
                    (lambda (row)
                      (org-glance-cache--decode-metadata (car row)))
                    (sqlite-select
                     db
                     "SELECT record_payload FROM headline_projection WHERE role IN ('current','conflict','recovery','rejected') ORDER BY logical_id,event_id")))))
        (list :families families
              :revived (org-glance-cache--persisted-revived-family-ids db)
              :headlines headlines))))
   (list :families nil :revived nil :headlines nil)))

(add-hook 'org-glance-graph-after-family-append-functions
          #'org-glance-cache--after-append)
(add-hook 'org-glance-graph-after-open-functions #'org-glance-cache--refresh-on-open)
(add-hook 'org-glance-graph-after-open-functions
          #'org-glance-cache--start-inventory-timer)

(provide 'org-glance-cache)
;;; org-glance-cache.el ends here
