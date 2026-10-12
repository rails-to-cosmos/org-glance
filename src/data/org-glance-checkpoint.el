;;; org-glance-checkpoint.el --- Portable projection checkpoints  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'sqlite)

(require 'org-glance-cache)
(require 'org-glance-graph)
(require 'org-glance-utils)

(defconst org-glance-checkpoint:schema "org-glance.repository-checkpoint/1")

(defconst org-glance-checkpoint:producer "org-glance-logical-projection-1")

(defcustom org-glance-checkpoint-auto-hydrate t
  "Hydrate a missing local projection from an exact checkpoint on graph open."
  :type 'boolean
  :group 'org-glance)

(defvar org-glance-checkpoint--publication-hook nil
  "Function called after staged validation and before cache publication.")

(cl-defun org-glance-checkpoint--quote (identifier)
  "Quote SQLite IDENTIFIER."
  (concat "\"" (replace-regexp-in-string "\"" "\"\"" identifier) "\""))

(cl-defun org-glance-checkpoint--tables (db)
  "Return DB's portable projection tables in canonical order."
  (mapcar
   #'car
   (sqlite-select
    db
    "SELECT name FROM sqlite_master WHERE type='table' AND name NOT LIKE 'sqlite_%' AND name!='inventory_directory' ORDER BY name")))

(cl-defun org-glance-checkpoint--columns (db table)
  "Return TABLE's columns in schema order."
  (mapcar #'cadr
          (sqlite-select
           db (format "PRAGMA table_info(%s)"
                      (org-glance-checkpoint--quote table)))))

(cl-defun org-glance-checkpoint--keys (db table)
  "Return TABLE's primary-key columns in key order."
  (let* ((info (sqlite-select
                db (format "PRAGMA table_info(%s)"
                           (org-glance-checkpoint--quote table))))
         (keys (sort (cl-remove-if-not (lambda (row) (> (nth 5 row) 0)) info)
                     (lambda (left right) (< (nth 5 left) (nth 5 right))))))
    (unless keys (error "org-glance: checkpoint table has no key: %s" table))
    (mapcar #'cadr keys)))

(cl-defun org-glance-checkpoint--portable-row (graph columns row &optional localize)
  "Translate ROW's path in COLUMNS relative to GRAPH.
LOCALIZE translates a portable path back into GRAPH."
  (let ((result
         (mapcar
          (lambda (value)
            (if (and (stringp value) (not (multibyte-string-p value)))
                (decode-coding-string value 'utf-8 t)
              value))
          row)))
    (when-let* ((index (cl-position "path" columns :test #'equal)))
      (let* ((root (file-name-as-directory (org-glance-graph:directory graph)))
             (path (nth index result))
             (translated
              (if localize
                  (expand-file-name path root)
                (unless (file-in-directory-p path root)
                  (error "org-glance: checkpoint path outside graph: %s" path))
                (file-relative-name path root))))
        (setf (nth index result) translated)))
    result))

(cl-defun org-glance-checkpoint--artifact (graph)
  "Return GRAPH's canonical logical projection artifact and row count."
  (let (tables)
    (org-glance-cache--with-db
     graph
     (lambda (db)
       (setq
        tables
        (mapcar
         (lambda (name)
           (let* ((columns (org-glance-checkpoint--columns db name))
                  (keys (org-glance-checkpoint--keys db name))
                  (query
                   (format "SELECT * FROM %s ORDER BY %s"
                           (org-glance-checkpoint--quote name)
                           (mapconcat #'org-glance-checkpoint--quote keys ","))))
             `((name . ,name)
               (columns . ,(vconcat columns))
               (rows . ,(vconcat
                          (mapcar
                           (lambda (row)
                             (vconcat
                              (org-glance-checkpoint--portable-row
                               graph columns row)))
                           (sqlite-select db query)))))))
         (org-glance-checkpoint--tables db)))))
    (let* ((artifact
            `((schema . ,org-glance-checkpoint:schema)
              (cacheSchema . ,org-glance-cache:version)
              (tables . ,(vconcat tables))))
           (bytes (encode-coding-string
                   (concat (json-serialize artifact) "\n") 'utf-8 t))
           (rows (apply #'+
                        (mapcar (lambda (table)
                                  (length (alist-get 'rows table)))
                                tables))))
      (list :bytes bytes :rows rows))))

(cl-defun org-glance-checkpoint--authority-paths (graph)
  "Return GRAPH's independently observed portable authority path set."
  (let* ((root (file-name-as-directory (org-glance-graph:directory graph)))
         (data (org-glance-graph:data-path graph))
         (system (org-glance-graph:config-file graph "system.org"))
         (tags (org-glance-graph:config-file graph "tags"))
         (paths
          (append
           (list (org-glance-graph--manifest-path graph))
           (org-glance-graph--live-segments graph)
           (when (file-regular-p system) (list system))
           (when (and (not (and (boundp 'org-glance-tag-config-dir)
                                org-glance-tag-config-dir))
                      (file-directory-p tags))
             (directory-files tags t "\\.org\\'"))
           (when (file-directory-p data)
             (directory-files-recursively
              data "\\(?:meta\\.json\\|data\\.org\\)\\'" nil)))))
    (sort
     (delete-dups
      (mapcar
       (lambda (path)
         (unless (and (file-regular-p path)
                      (not (file-symlink-p path))
                      (file-in-directory-p path root))
           (error "org-glance: unsafe checkpoint authority path: %s" path))
         (file-relative-name path root))
       paths))
     #'string<)))

(cl-defun org-glance-checkpoint--configuration-files (graph)
  "Return GRAPH's effective configuration as portable labels and paths."
  (let* ((system (org-glance-graph:config-file graph "system.org"))
         (tag-dir (if (and (boundp 'org-glance-tag-config-dir)
                           (stringp org-glance-tag-config-dir))
                      org-glance-tag-config-dir
                    (org-glance-graph:config-file graph "tags")))
         (files
          (append
           (when (file-regular-p system) (list (cons "system.org" system)))
           (when (file-directory-p tag-dir)
             (mapcar
              (lambda (path)
                (cons (concat "tags/" (file-name-nondirectory path)) path))
              (directory-files tag-dir t "\\.org\\'"))))))
    (sort files (lambda (left right) (string< (car left) (car right))))))

(cl-defun org-glance-checkpoint--projection (graph)
  "Return GRAPH's semantic projection contract."
  (let* ((files
          (mapcar
           (lambda (entry)
             `((path . ,(car entry))
               (sha256 . ,(org-glance-cache--file-digest (cdr entry)))))
           (org-glance-checkpoint--configuration-files graph)))
         (configuration
          `((files . ,(vconcat files))
            (todoKeywords . ,(prin1-to-string org-todo-keywords))
            (propertyInheritance . ,(prin1-to-string org-use-property-inheritance))
            (tagInheritanceExclusions
             . ,(prin1-to-string org-tags-exclude-from-inheritance)))))
    `((cacheSchema . ,org-glance-cache:version)
      (parser . ,(format "emacs-%s-org-%s" emacs-major-version (org-version)))
      (producer . ,org-glance-checkpoint:producer)
      (configurationSha256
       . ,(secure-hash 'sha256
                       (org-glance-checkpoint--encode configuration))))))

(cl-defun org-glance-checkpoint--inventory (graph paths)
  "Return GRAPH source inventory entries for portable PATHS."
  (let ((root (org-glance-graph:directory graph)))
    (mapcar
     (lambda (path)
       (let ((absolute (expand-file-name path root)))
         `((path . ,path)
           (sha256 . ,(org-glance-cache--file-digest absolute))
           (bytes . ,(file-attribute-size (file-attributes absolute))))))
     paths)))

(cl-defun org-glance-checkpoint--encode (value)
  "Return canonical UTF-8 JSON bytes for VALUE."
  (encode-coding-string (concat (json-serialize value) "\n") 'utf-8 t))

(cl-defun org-glance-checkpoint--read-bytes (path)
  "Return PATH's bytes without coding conversion."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(cl-defun org-glance-checkpoint--compress (bytes)
  "Return deterministic gzip compression of BYTES."
  (unless (executable-find "gzip")
    (error "org-glance: sealing a checkpoint requires gzip"))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert bytes)
    (unless (= 0 (call-process-region
                  (point-min) (point-max) "gzip" t t nil "-n" "-9" "-c"))
      (error "org-glance: checkpoint gzip compression failed"))
    (buffer-string)))

(cl-defun org-glance-checkpoint--decompress (bytes)
  "Return decompressed checkpoint BYTES."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert bytes)
    (unless (zlib-decompress-region (point-min) (point-max))
      (error "org-glance: checkpoint gzip decompression failed"))
    (buffer-string)))

(cl-defun org-glance-checkpoint--publish (path bytes)
  "Publish immutable BYTES at PATH and return PATH."
  (make-directory (file-name-directory path) t)
  (if (file-exists-p path)
      (unless (equal bytes (org-glance-checkpoint--read-bytes path))
        (error "org-glance: divergent immutable checkpoint object: %s" path))
    (let ((temporary (make-temp-file
                      (expand-file-name ".checkpoint-" (file-name-directory path)))))
      (unwind-protect
          (progn
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert bytes)
              (let ((coding-system-for-write 'no-conversion))
                (write-region nil nil temporary nil 'silent)))
            (rename-file temporary path nil))
        (when (file-exists-p temporary) (delete-file temporary)))))
  path)

(cl-defun org-glance-checkpoint--root (graph)
  "Return GRAPH's tracked immutable checkpoint root."
  (expand-file-name "checkpoints" (org-glance-graph:store-path graph)))

(cl-defun org-glance-checkpoint--object-path (graph digest)
  "Return GRAPH object path for DIGEST."
  (expand-file-name
   (format "objects/%s/%s.pack.gz" (substring digest 0 2) digest)
   (org-glance-checkpoint--root graph)))

(cl-defun org-glance-checkpoint--descriptor-path (graph digest)
  "Return GRAPH descriptor path for DIGEST."
  (expand-file-name
   (format "descriptors/%s/%s.json" (substring digest 0 2) digest)
   (org-glance-checkpoint--root graph)))

;;;###autoload
(cl-defun org-glance-checkpoint:seal (&optional (graph (org-glance-ensure-init))
                                                (writer (system-name)))
  "Seal GRAPH's verified projection as an immutable checkpoint by WRITER."
  (interactive)
  (let* ((verified (org-glance-cache:checkpoint-source-paths graph))
         (observed (org-glance-checkpoint--authority-paths graph)))
    (unless (equal verified observed)
      (error "org-glance: verified and observed checkpoint inventories differ"))
    (let* ((source (org-glance-checkpoint--inventory graph observed))
           (source-bytes (org-glance-checkpoint--encode (vconcat source)))
           (source-sha (secure-hash 'sha256 source-bytes))
           (artifact (org-glance-checkpoint--artifact graph))
           (artifact-bytes (plist-get artifact :bytes))
           (logical-sha (secure-hash 'sha256 artifact-bytes))
           (object-bytes (org-glance-checkpoint--compress artifact-bytes))
           (artifact-sha (secure-hash 'sha256 object-bytes))
           (object (org-glance-checkpoint--object-path graph artifact-sha))
           (body
            `((schema . ,org-glance-checkpoint:schema)
              (writer . ,writer)
              (source . ((sha256 . ,source-sha)
                         (files . ,(vconcat source))))
              (projection . ,(org-glance-checkpoint--projection graph))
              (artifact . ((codec . "gzip")
                           (sha256 . ,artifact-sha)
                           (bytes . ,(length object-bytes))
                           (logicalSha256 . ,logical-sha)
                           (logicalBytes . ,(length artifact-bytes))
                           (rows . ,(plist-get artifact :rows))))
              (resultSha256 . ,logical-sha)))
           (descriptor-bytes (org-glance-checkpoint--encode body))
           (descriptor-sha (secure-hash 'sha256 descriptor-bytes))
           (descriptor
            (org-glance-checkpoint--descriptor-path graph descriptor-sha)))
      (org-glance-checkpoint--publish object object-bytes)
      (org-glance-checkpoint--publish descriptor descriptor-bytes)
      (when (called-interactively-p 'interactive)
        (message "org-glance: sealed checkpoint %s" descriptor-sha))
      (list :descriptor descriptor :object object :id descriptor-sha
            :rows (plist-get artifact :rows)))))

(cl-defun org-glance-checkpoint--decode (bytes)
  "Decode checkpoint JSON BYTES into a plist."
  (json-parse-string (decode-coding-string bytes 'utf-8)
                     :object-type 'plist :array-type 'list :null-object nil))

(cl-defun org-glance-checkpoint--descriptors (graph)
  "Return admitted descriptor plists for GRAPH."
  (let ((root (expand-file-name "descriptors"
                                (org-glance-checkpoint--root graph))))
    (when (file-directory-p root)
      (mapcar
       (lambda (path)
         (let* ((bytes (org-glance-checkpoint--read-bytes path))
                (digest (secure-hash 'sha256 bytes))
                (claimed (file-name-base path)))
           (unless (equal digest claimed)
             (error "org-glance: checkpoint descriptor identity mismatch: %s" path))
           (let ((value (org-glance-checkpoint--decode bytes)))
             (unless (equal org-glance-checkpoint:schema
                            (plist-get value :schema))
               (error "org-glance: incompatible checkpoint descriptor: %s" path))
             (plist-put value :descriptorPath path))))
       (sort (directory-files-recursively root "[0-9a-f]\\{64\\}\\.json\\'")
             #'string<)))))

(cl-defun org-glance-checkpoint--current-source-p (graph descriptor)
  "Return non-nil when DESCRIPTOR exactly matches GRAPH's source inventory."
  (let* ((source (plist-get descriptor :source))
         (files (plist-get source :files))
         (paths (mapcar (lambda (entry) (plist-get entry :path)) files)))
    (and (equal paths (org-glance-checkpoint--authority-paths graph))
         (equal (plist-get source :sha256)
                (secure-hash
                 'sha256
                 (org-glance-checkpoint--encode
                  (vconcat
                   (org-glance-checkpoint--inventory graph paths))))))))

(cl-defun org-glance-checkpoint--prefix-digest (path bytes)
  "Return the SHA-256 of PATH's first BYTES bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 bytes)
    (secure-hash 'sha256 (current-buffer))))

(cl-defun org-glance-checkpoint--tail-source-p (graph descriptor)
  "Return non-nil when GRAPH is an append-only family tail of DESCRIPTOR."
  (let* ((root (org-glance-graph:directory graph))
         (files (plist-get (plist-get descriptor :source) :files))
         (prior-paths (mapcar (lambda (entry) (plist-get entry :path)) files))
         (current-paths (org-glance-checkpoint--authority-paths graph))
         (open ".org-glance/meta/headlines.jsonl")
         (extra (cl-set-difference current-paths prior-paths :test #'equal)))
    (and
     (cl-subsetp prior-paths current-paths :test #'equal)
     (cl-every
      (lambda (entry)
        (let* ((relative (plist-get entry :path))
               (path (expand-file-name relative root))
               (bytes (plist-get entry :bytes))
               (digest (plist-get entry :sha256)))
          (if (equal relative open)
              (and (>= (file-attribute-size (file-attributes path)) bytes)
                   (equal digest
                          (org-glance-checkpoint--prefix-digest path bytes)))
            (equal digest (org-glance-cache--file-digest path)))))
      files)
     (cl-every
      (lambda (path)
        (string-match-p
         "\\`.org-glance/data/.+/versions/[^/]+/\\(?:meta\\.json\\|data\\.org\\)\\'"
         path))
      extra)
     (or extra
         (> (file-attribute-size
             (file-attributes (expand-file-name open root)))
            (plist-get
             (cl-find open files :key (lambda (entry) (plist-get entry :path))
                      :test #'equal)
             :bytes))))))

(cl-defun org-glance-checkpoint--select (graph)
  "Select GRAPH's best compatible checkpoint as (KIND . DESCRIPTOR)."
  (let* ((compatible
          (cl-remove-if-not
           (lambda (descriptor)
             (equal (plist-get descriptor :projection)
                    (org-glance-checkpoint--decode
                     (org-glance-checkpoint--encode
                      (org-glance-checkpoint--projection graph)))))
           (org-glance-checkpoint--descriptors graph)))
         (exact (cl-remove-if-not
                 (lambda (descriptor)
                   (org-glance-checkpoint--current-source-p graph descriptor))
                 compatible))
         (kind (if exact 'exact 'tail))
         (matches (or exact
                      (cl-remove-if-not
                       (lambda (descriptor)
                         (org-glance-checkpoint--tail-source-p graph descriptor))
                       compatible)))
         (results (delete-dups
                   (mapcar (lambda (held) (plist-get held :resultSha256))
                           matches))))
    (when (> (length results) 1)
      (error "org-glance: ambiguous compatible checkpoints have different results"))
    (when matches (cons kind (car matches)))))

(cl-defun org-glance-checkpoint--insert-query (table columns)
  "Return TABLE insertion SQL for COLUMNS."
  (format "INSERT INTO %s(%s) VALUES(%s)"
          (org-glance-checkpoint--quote table)
          (mapconcat #'org-glance-checkpoint--quote columns ",")
          (mapconcat (lambda (_) "?") columns ",")))

(cl-defun org-glance-checkpoint--hydrate-artifact (graph artifact)
  "Insert decoded ARTIFACT into GRAPH's fresh cache and return its row count."
  (unless (and (equal org-glance-checkpoint:schema (plist-get artifact :schema))
               (= org-glance-cache:version (plist-get artifact :cacheSchema)))
    (error "org-glance: incompatible checkpoint artifact"))
  (let ((tables (plist-get artifact :tables))
        (written 0))
    (org-glance-cache--with-db
     graph
     (lambda (db)
       (unless (equal (org-glance-checkpoint--tables db)
                      (mapcar (lambda (table) (plist-get table :name)) tables))
         (error "org-glance: checkpoint table set differs from schema"))
       (org-glance-cache--transaction
        db
        (lambda ()
          (sqlite-execute db "PRAGMA defer_foreign_keys=ON")
          (dolist (table tables)
            (let* ((name (plist-get table :name))
                   (columns (plist-get table :columns))
                   (rows (plist-get table :rows)))
              (unless (equal columns (org-glance-checkpoint--columns db name))
                (error "org-glance: checkpoint columns differ for %s" name))
              (let ((query (org-glance-checkpoint--insert-query name columns)))
                (dolist (row rows)
                  (sqlite-execute
                   db query
                   (vconcat
                    (org-glance-checkpoint--portable-row
                     graph columns row t)))
                  (cl-incf written)))))))))
    written))

(cl-defun org-glance-checkpoint--hydrate-staged (graph artifact)
  "Hydrate ARTIFACT beside GRAPH's live cache, validate, then publish it."
  (let* ((live (org-glance-cache:path graph))
         (parent (file-name-directory live))
         (_ (make-directory parent t))
         (stage-dir (make-temp-file
                     (expand-file-name ".checkpoint-stage-" parent) t))
         (stage (expand-file-name "glance.sqlite3" stage-dir))
         written)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'org-glance-cache:path)
                     (lambda (_graph) stage)))
            (setq written
                  (org-glance-checkpoint--hydrate-artifact graph artifact)))
          (let ((db (sqlite-open stage)))
            (unwind-protect
                (progn
                  (unless (equal '(("ok"))
                                 (sqlite-select db "PRAGMA integrity_check"))
                    (error "org-glance: checkpoint SQLite integrity failure"))
                  (when (sqlite-select db "PRAGMA foreign_key_check")
                    (error "org-glance: checkpoint foreign-key failure"))
                  (sqlite-select db "PRAGMA wal_checkpoint(TRUNCATE)")
                  (sqlite-select db "PRAGMA journal_mode=DELETE"))
              (sqlite-close db)))
          (when org-glance-checkpoint--publication-hook
            (funcall org-glance-checkpoint--publication-hook))
          (dolist (sidecar (list (concat live "-shm") (concat live "-wal")))
            (when (file-exists-p sidecar) (delete-file sidecar)))
          (rename-file stage live t)
          written)
      (when (file-exists-p stage-dir) (delete-directory stage-dir t)))))

;;;###autoload
(cl-defun org-glance-checkpoint:hydrate
    (&optional (graph (org-glance-ensure-init))
     &key ((:reconcile-tail reconcile-tail) t))
  "Hydrate GRAPH from its unique compatible portable checkpoint.
Signal when neither an exact nor an append-only-tail checkpoint exists."
  (interactive)
  (let* ((selection (or (org-glance-checkpoint--select graph)
                        (error "org-glance: no checkpoint matches current sources")))
         (kind (car selection))
         (descriptor (cdr selection))
         (artifact-spec (plist-get descriptor :artifact))
         (digest (plist-get artifact-spec :sha256))
         (path (org-glance-checkpoint--object-path graph digest))
         (object-bytes (org-glance-checkpoint--read-bytes path)))
    (unless (and (equal "gzip" (plist-get artifact-spec :codec))
                 (= (length object-bytes) (plist-get artifact-spec :bytes))
                 (equal digest (secure-hash 'sha256 object-bytes)))
      (error "org-glance: checkpoint artifact digest mismatch: %s" path))
    (let* ((bytes (org-glance-checkpoint--decompress object-bytes))
           (_ (unless (and
                       (= (length bytes)
                          (plist-get artifact-spec :logicalBytes))
                       (equal (secure-hash 'sha256 bytes)
                              (plist-get artifact-spec :logicalSha256)))
                (error "org-glance: checkpoint logical artifact mismatch")))
           (rows (org-glance-checkpoint--hydrate-staged
                  graph (org-glance-checkpoint--decode bytes))))
      (unless (= rows (plist-get artifact-spec :rows))
        (error "org-glance: checkpoint row count mismatch"))
      (org-glance-graph--invalidate-cache graph)
      (when (and (eq kind 'tail) reconcile-tail)
        (org-glance-cache:refresh graph))
      (when (called-interactively-p 'interactive)
        (message "org-glance: hydrated %d checkpoint rows" rows))
      (list :kind kind :rows rows :descriptor
            (plist-get descriptor :descriptorPath)))))

(cl-defun org-glance-checkpoint--hydrate-on-open (graph)
  "Hydrate GRAPH before background reconciliation when its cache is absent."
  (when (and org-glance-checkpoint-auto-hydrate
             (not (file-exists-p (org-glance-cache:path graph)))
             (file-directory-p
              (expand-file-name "descriptors"
                                (org-glance-checkpoint--root graph))))
    (condition-case err
        (let ((result (org-glance-checkpoint:hydrate
                       graph :reconcile-tail nil)))
          (message "org-glance: cold cache hydrated from checkpoint (%d rows)"
                   (plist-get result :rows)))
      (error
       (message "org-glance: checkpoint unavailable; replay queued: %s"
                (error-message-string err))))))

(add-hook 'org-glance-graph-after-open-functions
          #'org-glance-checkpoint--hydrate-on-open -90)

(provide 'org-glance-checkpoint)
;;; org-glance-checkpoint.el ends here
