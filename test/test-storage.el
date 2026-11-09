;;; test-storage.el --- Storage migration contracts -*- lexical-binding: t; -*-

(require 'test-helpers)

(ert-deftest org-glance-test:storage-adoption-is-idempotent-and-preserves-canonical-bytes ()
  "Adopt legacy layout without rewriting its MANIFEST or immutable events."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (let* ((meta (org-glance-graph:meta-path graph))
           (marker (expand-file-name "SCHEMA" meta))
           (manifest (org-glance-graph--manifest-path graph))
           (payload (org-glance-graph:content-path graph "a"))
           (envelope (expand-file-name "meta.json" (file-name-directory payload)))
           (paths (list manifest payload envelope))
           (before (mapcar #'f-read-bytes paths)))
      (delete-file marker)
      (should (= 0 (org-glance-storage:check meta)))
      (let ((org-glance-storage--migrations nil))
        (should-error (org-glance-storage:migrate meta)))
      (should-not (file-exists-p marker))
      (should-not (file-exists-p (concat marker ".lock")))
      (make-directory (concat marker ".lock"))
      (should-error (org-glance-storage:migrate meta) :type 'user-error)
      (should-not (file-exists-p marker))
      (delete-directory (concat marker ".lock"))
      (should (= 1 (org-glance-storage:migrate meta)))
      (let ((stamp (file-attributes marker)))
        (should (= 1 (org-glance-storage:migrate meta)))
        (should (equal stamp (file-attributes marker))))
      (should (equal before (mapcar #'f-read-bytes paths))))))

(ert-deftest org-glance-test:storage-refuses-future-canonical-formats ()
  "Refuse newer store markers, MANIFESTs and event envelopes before writes."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (let* ((meta (org-glance-graph:meta-path graph))
           (marker (expand-file-name "SCHEMA" meta))
           (manifest (org-glance-graph--manifest-path graph))
           (original-marker (f-read-text marker))
           (original-manifest (f-read-text manifest)))
      (dolist (change (list (cons marker (replace-regexp-in-string
                                         "\"version\":1" "\"version\":9" original-marker t t))
                           (cons manifest "{\"version\":99,\"segments\":[]}\n")))
        (unwind-protect
            (progn
              (let ((mtime (file-attribute-modification-time (file-attributes (car change)))))
                (org-glance-test:write (car change) (cdr change))
                (set-file-times (car change) mtime))
              (should-error (org-glance-graph (org-glance-graph:directory graph))
                            :type 'org-glance-storage-unsupported)
              (should-error (org-glance-graph:add graph
                              (org-glance-test:headline "b" "* Future writer"))
                            :type 'org-glance-storage-unsupported)
              (should-not (file-exists-p (org-glance-graph:headline-data-path graph "b")))
              (should (equal (cdr change) (f-read-text (car change)))))
          (org-glance-test:write marker original-marker)
          (org-glance-test:write manifest original-manifest)))
      (let* ((dir (org-glance-graph:headline-data-path graph "a"))
             (path (expand-file-name "meta.json"
                                     (file-name-directory (org-glance-graph:content-path graph "a"))))
             (record (json-parse-string (f-read-text path) :object-type 'plist)))
        (org-glance-test:write path (json-serialize (plist-put record :version 99)))
        (should-error (org-glance-version:history dir "a")
                      :type 'org-glance-storage-unsupported)))))

(ert-deftest org-glance-test:cache-schema-upgrade-preserves-evidence-and-retries-atomically ()
  "Roll back a failed 11-to-12 upgrade and retain admitted event evidence."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (org-glance-cache:close-readers graph)
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (let ((events (sqlite-select db "SELECT envelope FROM headline_event")))
            (sqlite-execute db "DROP TABLE cache_migration")
            (sqlite-execute db "PRAGMA user_version=11")
            (let ((org-glance-cache--schema-migrations
                   '((11 12 (lambda (stage)
                              (org-glance-cache--add-migration-ledger stage)
                              (error "interrupted migration")) "upgrade"))))
              (should-error (org-glance-cache:recover graph)))
            (should (equal '((11)) (sqlite-select db "PRAGMA user_version")))
            (should-not (sqlite-select db "SELECT name FROM sqlite_master WHERE name='cache_migration'"))
            (should (equal events (sqlite-select db "SELECT envelope FROM headline_event")))
            (should (equal '("a") (mapcar #'org-glance-headline-metadata:id
                                          (org-glance-cache:headlines graph))))
            (should (equal '((12)) (sqlite-select db "PRAGMA user_version")))
            (should (equal '((11 12 "upgrade"))
                           (sqlite-select db "SELECT * FROM cache_migration")))
            (should (equal events (sqlite-select db "SELECT envelope FROM headline_event"))))
        (sqlite-close db)))))

(ert-deftest org-glance-test:cache-refuses-future-schema-without-rebuilding ()
  "Keep a newer private database intact during reads and recovery attempts."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (org-glance-cache:close-readers graph)
    (let ((db (sqlite-open (org-glance-cache:path graph)))
          (future (1+ org-glance-cache:version)))
      (unwind-protect
          (progn
            (sqlite-execute db (format "PRAGMA user_version=%d" future))
            (should-error (org-glance-cache:headlines graph)
                          :type 'org-glance-storage-unsupported)
            (should-error (org-glance-cache:recover graph)
                          :type 'org-glance-storage-unsupported)
            (should (equal (list (list future)) (sqlite-select db "PRAGMA user_version")))
            (should (equal '(("Alpha")) (sqlite-select db "SELECT title FROM headline"))))
        (sqlite-close db)))))

(ert-deftest org-glance-test:cache-historical-private-migrations-preserve-evidence ()
  "Upgrade historical private schemas and skip parsing for additive upgrades."
  (dolist (version '(7 10 11))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
      (org-glance-cache:close-readers graph)
      (let ((db (sqlite-open (org-glance-cache:path graph))))
        (unwind-protect
            (let ((events (sqlite-select db "SELECT envelope FROM headline_event")))
              (sqlite-execute db "DROP TABLE cache_migration")
              (when (<= version 10)
                (sqlite-execute db "DROP INDEX headline_projection_logical_role"))
              (when (= version 7)
                (sqlite-execute db "DROP TABLE configuration_dependency")
                (delete-file (expand-file-name
                              "meta.json"
                              (file-name-directory (org-glance-graph:content-path graph "a")))))
              (sqlite-execute db (format "PRAGMA user_version=%d" version))
              (if (= version 7)
                  (org-glance-cache:upgrade graph)
                (cl-letf (((symbol-function 'org-glance-cache--version-metadata)
                           (lambda (&rest _) (ert-fail "additive migration parsed a payload"))))
                  (org-glance-cache:upgrade graph)))
              (should (equal '((12)) (sqlite-select db "PRAGMA user_version")))
              (should (equal events (sqlite-select db "SELECT envelope FROM headline_event")))
              (should (equal '("a") (mapcar #'org-glance-headline-metadata:id
                                            (org-glance-cache:headlines graph)))))
          (sqlite-close db))))))

(ert-deftest org-glance-test:storage-format-checks-are-per-operation ()
  "Check canonical formats once for a batched write and again next operation."
  (org-glance-test:with-graph graph
    (let ((heads (cl-loop for i below 8 collect
                         (org-glance-test:headline (format "a%d" i) "* Alpha")))
          (check (symbol-function 'org-glance-storage:check))
          (checks 0))
      (cl-letf (((symbol-function 'org-glance-storage:check)
                 (lambda (meta) (cl-incf checks) (funcall check meta))))
        (apply #'org-glance-graph:add graph heads)
        (should (= 1 checks))
        (org-glance-graph:add graph (org-glance-test:headline "b" "* Beta"))
        (should (= 2 checks))))))

(ert-deftest org-glance-test:cache-read-migrates-schema-and-configuration-together ()
  "Repair both dependencies before a point read returns committed metadata."
  (org-glance-test:with-graph graph
    (org-glance-test:publish-config graph "{\"format\":1}")
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (org-glance-cache:close-readers graph)
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (progn
            (sqlite-execute db "DROP TABLE cache_migration")
            (sqlite-execute db "PRAGMA user_version=11")
            (sqlite-execute db "UPDATE configuration_dependency SET fingerprint='stale'")
            (should (equal "Alpha" (org-glance-headline-metadata:title
                                    (org-glance-cache:headline graph "a"))))
            (should (org-glance-cache--configuration-current-p graph))
            (should (equal '((12)) (sqlite-select db "PRAGMA user_version"))))
        (sqlite-close db)))))

(ert-deftest org-glance-test:cache-current-schema-skips-creation-work ()
  "Keep healthy writes free of DDL and still restore a missing derived table."
  (org-glance-test:with-graph graph
    (let ((execute (symbol-function 'sqlite-execute)) (creates 0))
      (cl-letf (((symbol-function 'sqlite-execute)
                 (lambda (db sql &rest args)
                   (when (string-prefix-p "CREATE " sql) (cl-incf creates))
                   (apply execute db sql args))))
        (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
        (should (= 0 creates))
        (org-glance-cache--with-db
         graph (lambda (db) (sqlite-execute db "DROP TABLE headline_payload_observation")))
        (org-glance-graph:add graph (org-glance-test:headline "b" "* Beta"))
        (should (> creates 0))
        (should (equal "Beta" (org-glance-headline-metadata:title
                               (org-glance-cache:headline graph "b"))))))))

(provide 'test-storage)
;;; test-storage.el ends here
