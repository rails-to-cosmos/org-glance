;;; org-glance-cache.el --- Shared local projection  -*- lexical-binding: t; -*-

;;; Commentary:
;; The rebuildable SQLite projection shared with the Glance daemon.

;;; Code:

(require 'cl-lib)
(require 'sqlite)

(require 'org-glance-graph)
(require 'org-glance-headline)
(require 'org-glance-version)

(defconst org-glance-cache:version 3
  "Shared Glance cache schema version.")

(defconst org-glance-cache--schema
  '("CREATE TABLE IF NOT EXISTS source (path TEXT PRIMARY KEY, digest TEXT NOT NULL, producer TEXT NOT NULL)"
    "CREATE TABLE IF NOT EXISTS glance_payload (path TEXT PRIMARY KEY REFERENCES source(path) ON DELETE CASCADE, config TEXT NOT NULL, parser_version INTEGER NOT NULL, producer TEXT NOT NULL, payload TEXT NOT NULL)"
    "CREATE TABLE IF NOT EXISTS headline (id TEXT PRIMARY KEY, path TEXT NOT NULL REFERENCES source(path) ON DELETE CASCADE, digest TEXT NOT NULL, org_id TEXT, org_id_property TEXT, title TEXT NOT NULL, state TEXT, priority TEXT, tags TEXT NOT NULL, scheduled TEXT, deadline TEXT, closed TEXT, category TEXT NOT NULL, created TEXT, producer TEXT NOT NULL)"
    "CREATE INDEX IF NOT EXISTS headline_path ON headline(path)"
    "CREATE TABLE IF NOT EXISTS org_headline (id TEXT PRIMARY KEY REFERENCES headline(id) ON DELETE CASCADE, path TEXT NOT NULL, digest TEXT NOT NULL, content_hash TEXT NOT NULL, record TEXT NOT NULL, producer TEXT NOT NULL)"
    "CREATE TABLE IF NOT EXISTS edge (src TEXT NOT NULL REFERENCES headline(id) ON DELETE CASCADE, dst TEXT NOT NULL REFERENCES headline(id) ON DELETE CASCADE, kind TEXT, via TEXT NOT NULL, PRIMARY KEY(src, dst, kind, via))"
    "CREATE INDEX IF NOT EXISTS edge_dst ON edge(dst)"
    "CREATE TABLE IF NOT EXISTS headline_family (logical_id TEXT PRIMARY KEY, state TEXT NOT NULL, current_version_id TEXT, family_fingerprint TEXT NOT NULL, reconciled_epoch TEXT, verified INTEGER NOT NULL)"
    "CREATE TABLE IF NOT EXISTS headline_event (logical_id TEXT NOT NULL REFERENCES headline_family(logical_id) ON DELETE CASCADE, event_id TEXT NOT NULL, kind TEXT NOT NULL, parents_json TEXT NOT NULL, targets_json TEXT NOT NULL, observed_leaves_json TEXT NOT NULL, payload_digest TEXT, producer TEXT NOT NULL, created TEXT NOT NULL, envelope TEXT NOT NULL, PRIMARY KEY(logical_id,event_id))"
    "CREATE TABLE IF NOT EXISTS headline_payload_observation (logical_id TEXT NOT NULL, event_id TEXT NOT NULL, path TEXT NOT NULL, observed_digest TEXT, payload_state TEXT NOT NULL, PRIMARY KEY(logical_id,event_id), FOREIGN KEY(logical_id,event_id) REFERENCES headline_event(logical_id,event_id) ON DELETE CASCADE)"
    "CREATE TABLE IF NOT EXISTS headline_projection (logical_id TEXT NOT NULL REFERENCES headline_family(logical_id) ON DELETE CASCADE, event_id TEXT NOT NULL, role TEXT NOT NULL, source_digest TEXT, parser_contract TEXT NOT NULL, record_payload TEXT, PRIMARY KEY(logical_id,event_id))"
    "CREATE TABLE IF NOT EXISTS producer_projection (logical_id TEXT NOT NULL REFERENCES headline_family(logical_id) ON DELETE CASCADE, event_id TEXT NOT NULL, producer TEXT NOT NULL, parser_contract TEXT NOT NULL, record_payload TEXT NOT NULL, PRIMARY KEY(logical_id,event_id,producer))")
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

(cl-defun org-glance-cache--version-metadata (graph id dir version)
  "Parse VERSION's available Snapshot bytes in DIR as family ID metadata."
  (when (eq 'snapshot (org-glance-version:kind version))
    (condition-case nil
        (let* ((contents (f-read-text
                          (org-glance-version:data-file
                           dir (org-glance-version:id version))
                          'utf-8))
               (seed (org-glance-graph--reparse-blob
                      graph (make-org-glance-headline-metadata :tags []) contents))
               (basis (org-glance-headline:metadata* seed))
               (metadata (org-glance-headline:metadata*
                          (org-glance-graph--reparse-blob graph basis contents)))
               (record (org-glance-headline-metadata:serialize metadata)))
          (org-glance-headline-metadata:deserialize (plist-put record :id id)))
      (error nil))))

(cl-defun org-glance-cache--payload-observation (graph id dir version)
  "Return VERSION's payload state in GRAPH and any parsed metadata."
  (let ((path (org-glance-version:data-file dir (org-glance-version:id version))))
    (if (not (file-exists-p path))
        (cons "missing" nil)
      (let ((metadata (org-glance-cache--version-metadata graph id dir version)))
        (cond
         ((not metadata) (cons "unparseable" nil))
         ((org-glance-version:valid version) (cons "available" metadata))
         (t (cons "mismatch" metadata)))))))

(cl-defun org-glance-cache--inventory-event-ids (dir)
  "Return every event-directory name currently observable below DIR."
  (let ((versions (f-join dir "versions")))
    (when (file-directory-p versions)
      (sort
       (mapcar #'file-name-nondirectory
               (cl-remove-if #'file-symlink-p (f-directories versions)))
       #'string<))))

(cl-defun org-glance-cache--inventory-proof (event-ids)
  "Return a stable local inventory proof for EVENT-IDS."
  (secure-hash 'sha256 (mapconcat #'identity event-ids "\n")))

(cl-defun org-glance-cache--project-family (db graph id)
  "Replace logical family ID's folded cache projection in DB."
  (let* ((dir (org-glance-graph:headline-data-path graph id))
         (history (and (file-directory-p (f-join dir "versions"))
                       (org-glance-version:history dir id)))
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
         (ordered (sort (copy-sequence history)
                        (lambda (a b) (string< (org-glance-version:id a)
                                               (org-glance-version:id b)))))
         (envelopes (mapcar #'org-glance-cache--event-envelope ordered))
         (fingerprint (secure-hash 'sha256
                                   (mapconcat #'identity envelopes "")))
         (inventory (org-glance-cache--inventory-event-ids dir))
         (inventory-proof (org-glance-cache--inventory-proof inventory))
         (admitted (mapcar #'org-glance-version:id history))
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
         "INSERT INTO headline_family(logical_id,state,current_version_id,family_fingerprint,reconciled_epoch,verified) VALUES(?,?,?,?,?,0)"
         (vector id "empty" nil fingerprint inventory-proof)))
      (cl-return-from org-glance-cache--project-family nil))
    (sqlite-execute db "DELETE FROM headline_family WHERE logical_id=?" (vector id))
    (when history
      (sqlite-execute
       db
       "INSERT INTO headline_family(logical_id,state,current_version_id,family_fingerprint,reconciled_epoch,verified) VALUES(?,?,?,?,?,?)"
       (vector id state current fingerprint inventory-proof 1))
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
                   (org-glance-cache--payload-observation graph id dir version)))
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
                 (observation
                  (org-glance-cache--payload-observation graph id dir version))
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
                                 (org-glance-cache--payload-observation
                                  graph id dir sole)))
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
           (vector id id "recovery" nil "org-glance:1" payload))
          (sqlite-execute
           db
           "INSERT INTO producer_projection(logical_id,event_id,producer,parser_contract,record_payload) VALUES(?,?,?,?,?)"
           (vector id id "org-glance" "org-glance:1" payload)))))))

(cl-defun org-glance-cache--delete (db graph id)
  "Delete ID's source projection from GRAPH's DB."
  (ignore graph)
  (dolist (row (sqlite-select db "SELECT path FROM headline WHERE org_id=?" (vector id)))
    (sqlite-execute db "DELETE FROM source WHERE path=?" (vector (car row)))))

(cl-defun org-glance-cache--headlines (graph)
  "Return GRAPH's live metadata without starting an external fold."
  (let ((org-glance-graph--folding-external t))
    (org-glance-graph--wal-headlines graph)))

(cl-defun org-glance-cache--family-ids (graph)
  "Return every immutable family ID discovered below GRAPH's data root."
  (let ((data (org-glance-graph:data-path graph)))
    (cl-labels
        ((walk
          (dir)
          (cl-loop
           for path in (directory-files dir t directory-files-no-dot-files-regexp)
           when (and (file-directory-p path) (not (file-symlink-p path)))
           if (file-directory-p (f-join path "versions"))
           collect (mapconcat #'identity
                              (split-string (file-relative-name path data) "/" t)
                              "")
           else append (walk path))))
      (if (file-directory-p data) (sort (delete-dups (walk data)) #'string<) nil))))

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
          (dolist (spec specs)
            (let ((id (if (org-glance-headline-metadata? spec)
                          (org-glance-headline-metadata:id spec)
                        (plist-get spec :id))))
              (if (and (listp spec) (plist-get spec :tombstone))
                  (org-glance-cache--delete db graph id)
                (when-let* ((metadata
                             (cl-find id headlines
                                      :key #'org-glance-headline-metadata:id
                                      :test #'equal)))
                  (org-glance-cache--project db graph metadata)))))
          (dolist (id (delete-dups touched))
            (org-glance-cache--project-family db graph id))
          (dolist (metadata headlines)
            (when (or (member (org-glance-headline-metadata:id metadata)
                              touched)
                      (cl-some
                       (lambda (target) (member target touched))
                       (org-glance-headline-metadata:relation-targets metadata)))
              (org-glance-cache--project-edges db metadata)))))))))

(cl-defun org-glance-cache:refresh (graph)
  "Reconcile GRAPH's live headlines into the shared cache."
  (let* ((headlines (org-glance-cache--headlines graph))
         (families (delete-dups
                    (append (mapcar #'org-glance-headline-metadata:id headlines)
                            (org-glance-cache--family-ids graph)))))
    (org-glance-cache--with-db
     graph
     (lambda (db)
       (org-glance-cache--transaction
        db
        (lambda ()
          (let ((live (mapcar #'org-glance-headline-metadata:id headlines)))
            (dolist (row (sqlite-select db "SELECT headline.org_id,org_headline.path FROM org_headline JOIN headline USING(id)"))
              (unless (member (car row) live)
                (sqlite-execute db "DELETE FROM source WHERE path=?"
                                (vector (cadr row))))))
          (dolist (id families)
            (org-glance-cache--project-family db graph id))
          (dolist (metadata headlines)
            (let* ((id (org-glance-headline-metadata:id metadata))
                   (state (caar (sqlite-select
                                 db
                                 "SELECT state FROM headline_family WHERE logical_id=?"
                                 (vector id)))))
              (when (or (null state) (member state '("live" "damaged")))
                (org-glance-cache--project db graph metadata))))
          (dolist (metadata headlines)
            (org-glance-cache--project-edges db metadata))
          (sqlite-execute
           db
           "DELETE FROM source WHERE producer='org-glance' AND NOT EXISTS (SELECT 1 FROM headline WHERE headline.path=source.path)")))))))

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

(cl-defun org-glance-cache:metadata (graph id)
  "Return ID's sole family-derived metadata from GRAPH's shared cache."
  (org-glance-cache--with-db
   graph
   (lambda (db)
     (let ((rows (sqlite-select
                  db
                  "SELECT record_payload FROM headline_projection WHERE logical_id=? AND role IN ('current','recovery') ORDER BY event_id"
                  (vector id))))
       (when (= 1 (length rows))
         (ignore-errors
           (org-glance-headline-metadata:deserialize
            (json-parse-string (caar rows) :object-type 'plist))))))))

(cl-defun org-glance-cache--revived-family-ids (db families)
  "Return family IDs whose current event descends from a Tombstone in DB."
  (cl-loop
   for (logical-id state current) in families
   when (and current (member state '("live" "damaged")))
   for events = (sqlite-select
                 db
                 "SELECT event_id,kind,parents_json FROM headline_event WHERE logical_id=?"
                 (vector logical-id))
   when (cl-labels
            ((tombstone-ancestor-p
              (event-id seen)
              (unless (member event-id seen)
                (when-let* ((event (assoc event-id events)))
                  (or (equal "tombstone-published" (cadr event))
                      (cl-some
                       (lambda (parent)
                         (tombstone-ancestor-p parent (cons event-id seen)))
                       (json-parse-string (nth 2 event) :array-type 'list)))))))
          (tombstone-ancestor-p current nil))
   collect logical-id))

(cl-defun org-glance-cache:authority (graph)
  "Return GRAPH's family-derived families and searchable projections."
  (or
   (org-glance-cache--with-db
    graph
    (lambda (db)
      (let ((families (sqlite-select
                       db
                       "SELECT logical_id,state,current_version_id FROM headline_family"))
            (headlines
             (delq nil
                   (mapcar
                    (lambda (row)
                      (ignore-errors
                        (org-glance-headline-metadata:deserialize
                         (json-parse-string (car row) :object-type 'plist))))
                    (sqlite-select
                     db
                     "SELECT record_payload FROM headline_projection WHERE role IN ('current','conflict','recovery','rejected') ORDER BY logical_id,event_id")))))
        (list :families families
              :revived (org-glance-cache--revived-family-ids db families)
              :headlines headlines))))
   (list :families nil :revived nil :headlines nil)))

(add-hook 'org-glance-graph-after-append-functions
          #'org-glance-cache--after-append)
(add-hook 'org-glance-graph-after-open-functions #'org-glance-cache:refresh)

(provide 'org-glance-cache)
;;; org-glance-cache.el ends here
