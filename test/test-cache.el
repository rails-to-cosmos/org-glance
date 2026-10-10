;;; test-cache.el --- Tests for the shared SQLite projection  -*- lexical-binding: t -*-

(require 'ert)
(require 'org-glance-cache)

(ert-deftest org-glance-test:shared-cache-projects-distinct-digests ()
  "The shared row keeps raw-file and org-glance content hashes distinct."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* TODO Alpha :work:"
                                "[[glance:b?kind=blocks][Beta]]"))
    (org-glance-graph:add graph (org-glance-test:headline "b" "* Beta"))
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (progn
            (should (equal '((4)) (sqlite-select db "PRAGMA user_version")))
            (should
             (equal
              '(("headline_event") ("headline_family")
                ("headline_payload_observation") ("headline_projection")
                ("producer_projection"))
              (sqlite-select
               db
               "SELECT name FROM sqlite_master WHERE (type='table' AND name LIKE 'headline_%') OR name='producer_projection' ORDER BY name")))
            (should
             (equal '(("headline_projection" 0) ("producer_projection" 0))
                    (sqlite-select
                     db
                     "SELECT 'headline_projection',\"notnull\" FROM pragma_table_info('headline_projection') WHERE name='event_id' UNION ALL SELECT 'producer_projection',\"notnull\" FROM pragma_table_info('producer_projection') WHERE name='event_id'")))
            (should
             (equal '(("headline_projection_metadata")
                      ("producer_projection_metadata"))
                    (sqlite-select
                     db
                     "SELECT name FROM sqlite_master WHERE type='index' AND name IN ('headline_projection_metadata','producer_projection_metadata') ORDER BY name")))
            (should (equal '(("glance_payload"))
                           (sqlite-select
                            db
                            "SELECT name FROM sqlite_master WHERE type='table' AND name='glance_payload'")))
            (should
             (equal '(("a" "live") ("b" "live"))
                    (sqlite-select
                     db
                     "SELECT logical_id,state FROM headline_family ORDER BY logical_id")))
            (should (equal '((2))
                           (sqlite-select db "SELECT count(*) FROM headline_event")))
            (pcase-let* ((`((,envelope ,fingerprint))
                          (sqlite-select
                           db
                           "SELECT headline_event.envelope,headline_family.family_fingerprint FROM headline_event JOIN headline_family USING(logical_id) WHERE logical_id='a'"))
                         (event (json-parse-string envelope :object-type 'plist)))
              (should (= 2 (plist-get event :version)))
              (should (equal "snapshot-published" (plist-get event :event)))
              (should-not (plist-member event :kind))
              (should (equal fingerprint (secure-hash 'sha256 envelope))))
            (should
             (equal '(("a" "current") ("b" "current"))
                    (sqlite-select
                     db
                     "SELECT logical_id,role FROM headline_projection ORDER BY logical_id")))
            (pcase-let ((`((,digest ,content-hash ,producer))
                         (sqlite-select
                          db
                          "SELECT org_headline.digest,content_hash,org_headline.producer FROM org_headline JOIN headline USING(id) WHERE headline.org_id='a'")))
              (should (= 64 (length digest)))
              (should (= 40 (length content-hash)))
              (should-not (equal digest content-hash))
              (should (equal "org-glance" producer)))
            (should (equal '(("a" "b" "blocks" "row"))
                           (sqlite-select
                            db
                            "SELECT src_headline.org_id,dst_headline.org_id,kind,via FROM edge JOIN headline AS src_headline ON edge.src=src_headline.id JOIN headline AS dst_headline ON edge.dst=dst_headline.id"))))
        (sqlite-close db)))
    (let ((metadata (org-glance-cache:metadata graph "a")))
      (should (org-glance-headline-metadata? metadata))
      (should (equal "Alpha" (org-glance-headline-metadata:title metadata))))
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (sqlite-execute
           db "UPDATE org_headline SET digest='stale' WHERE id=(SELECT id FROM headline WHERE org_id='a')")
        (sqlite-close db)))
    (should (equal "Alpha"
                   (org-glance-headline-metadata:title
                    (org-glance-cache:metadata graph "a"))))
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (progn
            (sqlite-execute
             db
             "UPDATE org_headline SET digest=(SELECT digest FROM headline WHERE org_id='a') WHERE id=(SELECT id FROM headline WHERE org_id='a')")
            (sqlite-execute
             db "UPDATE org_headline SET content_hash='stale' WHERE id=(SELECT id FROM headline WHERE org_id='a')"))
        (sqlite-close db)))
    (should (equal "Alpha"
                   (org-glance-headline-metadata:title
                    (org-glance-cache:metadata graph "a"))))))

(ert-deftest org-glance-test:shared-cache-loss-rebuilds-without-touching-portable-files ()
  "A lost local cache rebuilds without writing or deleting portable files."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO Alpha :work:"))
    (let ((portable (f-join (org-glance-graph:meta-path graph) "projections"))
          (cache (org-glance-cache:path graph)))
      (should-not (file-exists-p portable))
      (dolist (suffix '("" "-shm" "-wal"))
        (ignore-errors (delete-file (concat cache suffix))))
      (should-not (org-glance-cache:metadata graph "a"))
      (make-directory portable t)
      (org-glance--atomic-write (f-join portable "legacy.jsonl") "legacy\n")
      (org-glance-cache:refresh graph)
      (should (equal "Alpha"
                     (org-glance-headline-metadata:title
                      (org-glance-cache:metadata graph "a"))))
      (should (equal "legacy\n"
                     (f-read-text (f-join portable "legacy.jsonl") 'utf-8))))))

(ert-deftest org-glance-test:family-inventory-is-search-authority-without-wal ()
  "A complete immutable family is searchable without a metadata notification."
  (org-glance-test:with-graph graph
    (let* ((id "family-only")
           (dir (org-glance-graph:headline-data-path graph id))
           (contents (concat "* TODO Family authority :work:\n"
                             ":PROPERTIES:\n"
                             ":ORG_GLANCE_ID: " id "\n"
                             ":END:\n")))
      (org-glance-version:write-snapshot dir id nil "glance" contents 10)
      (let ((metadata (org-glance-graph:live-meta graph id)))
        (should (org-glance-headline-metadata? metadata))
        (should (equal "Family authority"
                       (org-glance-headline-metadata:title metadata))))
      (should (equal (list id)
                     (mapcar #'org-glance-headline-metadata:id
                             (org-glance-graph:headlines graph))))
      (let ((db (sqlite-open (org-glance-cache:path graph))))
        (unwind-protect
            (should-not
             (equal '(("{}"))
                    (sqlite-select
                     db
                     "SELECT record_payload FROM headline_projection WHERE logical_id='family-only'")))
          (sqlite-close db))))))

(ert-deftest org-glance-test:family-damage-keeps-precise-logical-recovery-rows ()
  "Damaged Snapshot payloads remain searchable with precise recovery state."
  (org-glance-test:with-graph graph
    (dolist (case '(("family-mismatch" . "mismatch")
                    ("family-missing" . "missing")
                    ("family-unparseable" . "unparseable")))
      (let* ((id (car case))
             (wanted (cdr case))
             (dir (org-glance-graph:headline-data-path graph id))
             (version (org-glance-version:write-snapshot
                       dir id nil "glance"
                       (concat "* Recovery source\n:PROPERTIES:\n"
                               ":ORG_GLANCE_ID: " id "\n:END:\n") 10))
             (path (org-glance-version:data-file
                    dir (org-glance-version:id version))))
        (cond
         ((equal wanted "missing") (delete-file path))
         ((equal wanted "mismatch")
          (write-region
           (concat "* Changed recovery source\n:PROPERTIES:\n"
                   ":ORG_GLANCE_ID: " id "\n:END:\n")
           nil path nil 'silent))
         (t
          (let ((coding-system-for-write 'no-conversion))
            (write-region (unibyte-string #xff #xfe) nil path nil 'silent))))))

    (org-glance-cache:refresh graph)

    (dolist (case '(("family-mismatch" . "mismatch")
                    ("family-missing" . "missing")
                    ("family-unparseable" . "unparseable")))
      (let* ((id (car case))
             (wanted (cdr case))
             (metadata (org-glance-graph:live-meta graph id)))
        (should (org-glance-headline-metadata? metadata))
        (should (equal wanted
                       (org-glance-headline-metadata:recovery metadata)))))
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (progn
            (should (equal '(("family-mismatch" "mismatch")
                             ("family-missing" "missing")
                             ("family-unparseable" "unparseable"))
                           (sqlite-select
                            db
                            "SELECT logical_id,payload_state FROM headline_payload_observation ORDER BY logical_id")))
            (should (equal '(("family-mismatch" 0 "recovery")
                             ("family-missing" 1 "recovery")
                             ("family-unparseable" 1 "recovery"))
                           (sqlite-select
                            db
                            "SELECT logical_id,event_id IS NULL,role FROM headline_projection ORDER BY logical_id"))))
        (sqlite-close db)))))

(ert-deftest org-glance-test:family-inventory-supersedes-stale-wal ()
  "A newer immutable Snapshot is searchable before any legacy notification."
  (org-glance-test:with-graph graph
    (let* ((id "family-stale")
           (dir (org-glance-graph:headline-data-path graph id)))
      (org-glance-graph:add
       graph (org-glance-test:headline id "* TODO Alpha"))
      (let* ((parent (car (org-glance-version:current dir id)))
             (contents (concat "* DONE Beta\n"
                               ":PROPERTIES:\n"
                               ":ORG_GLANCE_ID: " id "\n"
                               ":END:\n")))
        (org-glance-version:write-snapshot
         dir id (list (org-glance-version:id parent)) "glance" contents 10))

      (should (equal "Alpha"
                     (org-glance-headline-metadata:title
                      (car (org-glance-graph--wal-headlines graph)))))
      (org-glance-cache:refresh graph)
      (let ((metadata (org-glance-graph:live-meta graph id)))
        (should (equal "Beta" (org-glance-headline-metadata:title metadata)))
        (should (equal "DONE" (org-glance-headline-metadata:state metadata))))

      (org-glance-graph:delete graph id)
      (org-glance-cache:refresh graph)
      (should (eq 'tombstone (org-glance-graph:get-headline graph id)))
      (should-not (org-glance-graph:headlines graph))

      (let* ((parent (car (org-glance-version:current dir id)))
             (contents (concat "* TODO Gamma\n"
                               ":PROPERTIES:\n"
                               ":ORG_GLANCE_ID: " id "\n"
                               ":END:\n")))
        (org-glance-version:write-snapshot
         dir id (list (org-glance-version:id parent)) "glance" contents 10))
      (org-glance-cache:refresh graph)
      (should (member id (plist-get (org-glance-cache:authority graph) :revived)))
      (should (equal "Gamma"
                     (org-glance-headline-metadata:title
                      (org-glance-cache:metadata graph id))))
      (should (equal "Gamma"
                     (org-glance-headline-metadata:title
                      (org-glance-graph:get-headline graph id)))))))

(ert-deftest org-glance-test:family-inventory-retains-last-verified-projection ()
  "A partial event delivery cannot replace the last verified projection."
  (org-glance-test:with-graph graph
    (let* ((id "family-partial")
           (dir (org-glance-graph:headline-data-path graph id))
           (created "2026-10-10T00:00:00Z"))
      (org-glance-graph:add
       graph (org-glance-test:headline id "* TODO Root"))
      (let* ((root (car (org-glance-version:current dir id)))
             (root-id (org-glance-version:id root))
             (child "* DONE Child\n"))
        (org-glance-test--write-family-event
         dir "late-child"
         (list :version 2 :event "snapshot-published" :headline id
               :id "late-child" :parents ["missing-parent"]
               :contentSha256 (secure-hash 'sha256 child)
               :created created :producer "glance")
         child)

        (org-glance-cache:refresh graph)
        (should (equal "Root"
                       (org-glance-headline-metadata:title
                        (org-glance-graph:live-meta graph id))))
        (let ((db (sqlite-open (org-glance-cache:path graph))))
          (unwind-protect
              (progn
                (should (equal '((0 1))
                               (sqlite-select
                                db
                                "SELECT verified,reconciled_epoch IS NOT NULL FROM headline_family WHERE logical_id='family-partial'")))
                (should (equal (list (list root-id))
                               (sqlite-select
                                db
                                "SELECT event_id FROM headline_event WHERE logical_id='family-partial'"))))
            (sqlite-close db)))

        (let ((middle "* TODO Middle\n"))
          (org-glance-test--write-family-event
           dir "missing-parent"
           (list :version 2 :event "snapshot-published" :headline id
                 :id "missing-parent" :parents (vector root-id)
                 :contentSha256 (secure-hash 'sha256 middle)
                 :created created :producer "glance")
           middle))
        (org-glance-cache:refresh graph)
        (should (equal "Child"
                       (org-glance-headline-metadata:title
                        (org-glance-graph:live-meta graph id))))
        (let ((db (sqlite-open (org-glance-cache:path graph))))
          (unwind-protect
              (should (equal '((1 3))
                             (sqlite-select
                              db
                              "SELECT verified,(SELECT count(*) FROM headline_event WHERE logical_id='family-partial') FROM headline_family WHERE logical_id='family-partial'")))
            (sqlite-close db)))))))

(ert-deftest org-glance-test:shared-cache-folds-schema-2-rejections ()
  "The shared family projection distinguishes structural and current Leaves."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Root"))
    (let* ((dir (org-glance-graph:headline-data-path graph "a"))
           (root (car (org-glance-version:current dir "a")))
           (root-id (org-glance-version:id root))
           (created "2026-10-10T00:00:00Z"))
      (dolist (branch '(("left" "* Left\n") ("right" "* Right\n")))
        (let ((id (car branch)) (contents (cadr branch)))
          (org-glance-test--write-family-event
           dir id
           (list :version 2 :event "snapshot-published" :headline "a"
                 :id id :parents (vector root-id)
                 :contentSha256 (secure-hash 'sha256 contents)
                 :created created :producer "glance")
           contents)))
      (org-glance-test--write-family-event
       dir "reject-left"
       (list :version 2 :event "leaves-rejected" :headline "a"
             :id "reject-left" :targets ["left"]
             :observedLeaves ["left" "right"] :contentSha256 nil
             :created created :producer "glance"))
      (org-glance-cache:refresh graph)
      (let ((db (sqlite-open (org-glance-cache:path graph))))
        (unwind-protect
            (progn
              (should (equal '(("live" "right"))
                             (sqlite-select
                              db
                              "SELECT state,current_version_id FROM headline_family WHERE logical_id='a'")))
              (should (equal '(("leaves-rejected" "[\"left\"]" "[\"left\",\"right\"]"))
                             (sqlite-select
                              db
                              "SELECT kind,targets_json,observed_leaves_json FROM headline_event WHERE event_id='reject-left'")))
              (should (equal '(("right" "current"))
                             (sqlite-select
                              db
                              "SELECT event_id,role FROM headline_projection WHERE logical_id='a'"))))
          (sqlite-close db))))))

(ert-deftest org-glance-test:shared-cache-follows-delete ()
  "Deleting a graph headline removes its shared source and incoming edges."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (org-glance-graph:add
     graph (org-glance-test:headline
            "b" "* Beta" "[[glance:a][Alpha]]"))
    (org-glance-graph:delete graph "a")
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (progn
            (should-not (sqlite-select db "SELECT id FROM headline WHERE org_id='a'"))
            (should-not (sqlite-select db "SELECT * FROM edge WHERE dst IN (SELECT id FROM headline WHERE org_id='a')")))
        (sqlite-close db)))))

(ert-deftest org-glance-test:shared-cache-open-repairs-a-missed-delete ()
  "Opening a graph reconciles a tombstone missed after its WAL append."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (let ((org-glance-graph-after-family-append-functions nil))
      (org-glance-graph:delete graph "a"))
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (should (equal '(("a"))
                         (sqlite-select db "SELECT org_id FROM headline")))
        (sqlite-close db)))
    (org-glance-test:reopen graph)
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (progn
            (should-not (sqlite-select db "SELECT id FROM headline"))
            (should (equal '(("deleted"))
                           (sqlite-select
                            db
                            "SELECT state FROM headline_family WHERE logical_id='a'")))
            (should (equal '((2))
                           (sqlite-select
                            db
                            "SELECT count(*) FROM headline_event WHERE logical_id='a'")))
            (should-not
             (sqlite-select
              db
              "SELECT event_id FROM headline_projection WHERE logical_id='a'")))
        (sqlite-close db)))))

(ert-deftest org-glance-test:shared-cache-inventory-finds-a-relay-free-snapshot ()
  "A live inventory fold discovers an immutable Snapshot without a relay line."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO Before"))
    (let* ((dir (org-glance-graph:headline-data-path graph "a"))
           (leaf (car (org-glance-version:candidates
                       (org-glance-version:history dir "a"))))
           (content (replace-regexp-in-string
                     "TODO Before" "DONE After"
                     (org-glance-graph:get-content graph "a") t t)))
      (org-glance-version:write-snapshot
       dir "a" (list (org-glance-version:id leaf)) "glance" content)
      (should (equal "Before"
                     (org-glance-headline-metadata:title
                      (org-glance-cache:metadata graph "a"))))
      (should (= 1 (org-glance-cache:reconcile-inventory graph)))
      (let ((metadata (org-glance-cache:metadata graph "a")))
        (should (equal "After" (org-glance-headline-metadata:title metadata)))
        (should (equal "DONE" (org-glance-headline-metadata:state metadata)))))))

(ert-deftest org-glance-test:shared-cache-commit-failure-reaches-the-writer ()
  "A durable event remains retryable when its required projection commit fails."
  (org-glance-test:with-graph graph
    (cl-letf (((symbol-function 'org-glance-cache--after-append)
               (lambda (&rest _) (error "projection failed"))))
      (should-error
       (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO Alpha"))
       :type 'error))
    (should (= 1 (length (org-glance-version:history
                          (org-glance-graph:headline-data-path graph "a") "a"))))))

(provide 'test-cache)
;;; test-cache.el ends here
