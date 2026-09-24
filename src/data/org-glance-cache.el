;;; org-glance-cache.el --- Shared local projection  -*- lexical-binding: t; -*-

;;; Commentary:
;; The rebuildable SQLite projection shared with the Glance daemon.

;;; Code:

(require 'cl-lib)
(require 'sqlite)

(require 'org-glance-graph)
(require 'org-glance-headline)

(defconst org-glance-cache:version 2
  "Shared Glance cache schema version.")

(defconst org-glance-cache--portable-format 1)
(defconst org-glance-cache--parser-version 1)
(defconst org-glance-cache--producer "org-glance")
(defconst org-glance-cache--config-key
  (secure-hash 'sha256 "org-glance:no-semantic-parser-config")
  "Fingerprint for the empty semantic parser configuration.")

(defconst org-glance-cache--schema
  '("CREATE TABLE IF NOT EXISTS source (path TEXT PRIMARY KEY, digest TEXT NOT NULL, producer TEXT NOT NULL)"
    "CREATE TABLE IF NOT EXISTS glance_payload (path TEXT PRIMARY KEY REFERENCES source(path) ON DELETE CASCADE, config TEXT NOT NULL, parser_version INTEGER NOT NULL, producer TEXT NOT NULL, payload TEXT NOT NULL)"
    "CREATE TABLE IF NOT EXISTS headline (id TEXT PRIMARY KEY, path TEXT NOT NULL REFERENCES source(path) ON DELETE CASCADE, digest TEXT NOT NULL, org_id TEXT, org_id_property TEXT, title TEXT NOT NULL, state TEXT, priority TEXT, tags TEXT NOT NULL, scheduled TEXT, deadline TEXT, closed TEXT, category TEXT NOT NULL, created TEXT, producer TEXT NOT NULL)"
    "CREATE INDEX IF NOT EXISTS headline_path ON headline(path)"
    "CREATE TABLE IF NOT EXISTS org_headline (id TEXT PRIMARY KEY REFERENCES headline(id) ON DELETE CASCADE, path TEXT NOT NULL, digest TEXT NOT NULL, content_hash TEXT NOT NULL, record TEXT NOT NULL, producer TEXT NOT NULL)"
    "CREATE TABLE IF NOT EXISTS edge (src TEXT NOT NULL REFERENCES headline(id) ON DELETE CASCADE, dst TEXT NOT NULL REFERENCES headline(id) ON DELETE CASCADE, kind TEXT, via TEXT NOT NULL, PRIMARY KEY(src, dst, kind, via))"
    "CREATE INDEX IF NOT EXISTS edge_dst ON edge(dst)")
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
  (let ((version (caar (sqlite-select db "PRAGMA user_version"))))
    (unless (= version org-glance-cache:version)
      (dolist (table (sqlite-select
                      db
                      "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%' ORDER BY CASE name WHEN 'edge' THEN 0 WHEN 'org_headline' THEN 1 WHEN 'headline' THEN 2 WHEN 'source' THEN 3 ELSE 4 END"))
        (sqlite-execute db
                        (format "DROP TABLE IF EXISTS \"%s\""
                                (replace-regexp-in-string "\"" "\"\"" (car table)))))
      (dolist (statement org-glance-cache--schema)
        (sqlite-execute db statement))
      (sqlite-execute db (format "PRAGMA user_version=%d"
                                 org-glance-cache:version)))
    (dolist (statement org-glance-cache--schema)
      (sqlite-execute db statement))))

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

(cl-defun org-glance-cache--portable-path (graph)
  "Return GRAPH's tracked org-glance projection directory."
  (f-join (org-glance-graph:meta-path graph) "projections" "org-glance"))

(cl-defun org-glance-cache--relative-path (graph path)
  "Return PATH relative to GRAPH, or nil when it escapes the graph root."
  (let ((relative (file-relative-name path (org-glance-graph:directory graph))))
    (unless (or (file-name-absolute-p relative)
                (string-prefix-p "../" relative)
                (equal relative ".."))
      relative)))

(cl-defun org-glance-cache--portable-key (record)
  "Return RECORD's exact portable projection key."
  (list (plist-get record :path) (plist-get record :digest)
        (plist-get record :config) (plist-get record :parser)
        (plist-get record :producer)))

(cl-defun org-glance-cache--portable-records (graph)
  "Return valid portable projection envelopes stored under GRAPH."
  (let ((dir (org-glance-cache--portable-path graph)))
    (when (file-directory-p dir)
      (cl-loop for path in (directory-files dir t "\\.jsonl\\'")
               append
               (cl-loop for line in (split-string (f-read-text path 'utf-8) "\n" t)
                        for record = (ignore-errors
                                       (json-parse-string line :object-type 'plist))
                        when (and record
                                  (= org-glance-cache--portable-format
                                     (plist-get record :format)))
                        collect record)))))

(cl-defun org-glance-cache--portable-payload (graph path digest)
  "Return GRAPH's one exact portable payload for PATH and DIGEST.
Equal duplicates collapse. Divergent duplicates invalidate the key."
  (when-let* ((relative (org-glance-cache--relative-path graph path))
              (key (list relative digest org-glance-cache--config-key
                         org-glance-cache--parser-version
                         org-glance-cache--producer)))
    (let ((payloads
           (delete-dups
            (cl-loop for record in (org-glance-cache--portable-records graph)
                     when (equal key (org-glance-cache--portable-key record))
                     collect (plist-get record :payload)))))
      (when (= 1 (length payloads)) (car payloads)))))

(cl-defun org-glance-cache--write-portable (graph path digest payload)
  "Write GRAPH's immutable keyed projection for PATH, DIGEST and PAYLOAD."
  (when-let* ((relative (org-glance-cache--relative-path graph path)))
    (let* ((record (list :format org-glance-cache--portable-format
                         :producer org-glance-cache--producer
                         :parser org-glance-cache--parser-version
                         :config org-glance-cache--config-key
                         :path relative :digest digest :payload payload))
           (text (concat (json-serialize record) "\n"))
           (stem (secure-hash
                  'sha256
                  (format "%S" (org-glance-cache--portable-key record))))
           (dir (org-glance-cache--portable-path graph))
           (path (f-join dir (concat stem ".jsonl"))))
      (make-directory dir t)
      (cond ((not (file-exists-p path))
             (org-glance--atomic-write path text nil))
            ((equal text (f-read-text path 'utf-8)) nil)
            (t (let ((alternate
                      (f-join dir (format "%s-%s.jsonl" stem
                                          (secure-hash 'sha256 text)))))
                 (unless (and (file-exists-p alternate)
                              (equal text (f-read-text alternate 'utf-8)))
                   (org-glance--atomic-write alternate text nil))))))))

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

(cl-defun org-glance-cache--project (db graph metadata)
  "Project METADATA from GRAPH into DB."
  (let* ((id (org-glance-headline-metadata:id metadata))
         (path (org-glance-graph:content-path graph id))
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
       (vector id path digest id
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
       (vector id path digest content-hash
               payload
               "org-glance")))
    (when (and digest payload)
      (org-glance-cache--write-portable graph path digest payload))))

(cl-defun org-glance-cache--project-edges (db metadata)
  "Replace METADATA's resolved outgoing edges in DB."
  (let ((id (org-glance-headline-metadata:id metadata)))
    (sqlite-execute db "DELETE FROM edge WHERE src=?" (vector id))
    (dolist (edge (org-glance-headline-metadata:relations metadata))
      (when (sqlite-select db "SELECT 1 FROM headline WHERE id=?"
                           (vector (car edge)))
        (sqlite-execute
         db "INSERT OR REPLACE INTO edge(src,dst,kind,via) VALUES(?,?,?,?)"
         (vector id (car edge) (cdr edge) "row"))))))

(cl-defun org-glance-cache--delete (db graph id)
  "Delete ID's source projection from GRAPH's DB."
  (sqlite-execute db "DELETE FROM source WHERE path=?"
                  (vector (org-glance-graph:content-path graph id))))

(cl-defun org-glance-cache--headlines (graph)
  "Return GRAPH's live metadata without starting an external fold."
  (let ((org-glance-graph--folding-external t))
    (org-glance-graph:headlines graph)))

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
          (dolist (metadata headlines)
            (when (or (member (org-glance-headline-metadata:id metadata)
                              touched)
                      (cl-some
                       (lambda (target) (member target touched))
                       (org-glance-headline-metadata:relation-targets metadata)))
              (org-glance-cache--project-edges db metadata)))))))))

(cl-defun org-glance-cache:refresh (graph)
  "Reconcile GRAPH's live headlines into the shared cache."
  (let ((headlines (org-glance-cache--headlines graph)))
    (org-glance-cache--with-db
     graph
     (lambda (db)
       (org-glance-cache--transaction
        db
        (lambda ()
          (let ((live (mapcar #'org-glance-headline-metadata:id headlines)))
            (dolist (row (sqlite-select
                          db "SELECT id,path FROM org_headline"))
              (unless (member (car row) live)
                (sqlite-execute db "DELETE FROM source WHERE path=?"
                                (vector (cadr row))))))
          (dolist (metadata headlines)
            (org-glance-cache--project db graph metadata))
          (dolist (metadata headlines)
            (org-glance-cache--project-edges db metadata))
          (sqlite-execute
           db
           "DELETE FROM source WHERE producer='org-glance' AND NOT EXISTS (SELECT 1 FROM headline WHERE headline.path=source.path)")))))))

(cl-defun org-glance-cache:metadata (graph id)
  "Return ID's valid org-glance projection from GRAPH's shared cache."
  (org-glance-cache--with-db
   graph
   (lambda (db)
     (let* ((path (org-glance-graph:content-path graph id))
            (digest (and (file-exists-p path)
                         (org-glance-cache--file-digest path)))
            (row (car (sqlite-select
                       db
                       "SELECT digest,content_hash,record FROM org_headline WHERE id=?"
                       (vector id))))
            (payload (cond
                      ((and row digest (equal (nth 0 row) digest)) (nth 2 row))
                      (digest (org-glance-cache--portable-payload
                               graph path digest))))
            (record (and payload
                         (ignore-errors
                           (json-parse-string payload :object-type 'plist))))
            (metadata (and record
                           (org-glance-headline-metadata:deserialize record))))
       (when (and metadata
                  (equal id (org-glance-headline-metadata:id metadata))
                  (or (null row)
                      (equal (nth 1 row)
                             (org-glance-headline-metadata:hash metadata))))
         metadata)))))

(add-hook 'org-glance-graph-after-append-functions
          #'org-glance-cache--after-append)
(add-hook 'org-glance-graph-after-open-functions #'org-glance-cache:refresh)

(provide 'org-glance-cache)
;;; org-glance-cache.el ends here
