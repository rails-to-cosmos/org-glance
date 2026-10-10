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
            (should (equal '((3)) (sqlite-select db "PRAGMA user_version")))
            (should
             (equal
              '(("headline_event") ("headline_family")
                ("headline_payload_observation") ("headline_projection")
                ("producer_projection"))
              (sqlite-select
               db
               "SELECT name FROM sqlite_master WHERE (type='table' AND name LIKE 'headline_%') OR name='producer_projection' ORDER BY name")))
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
    (should-not (org-glance-cache:metadata graph "a"))
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (progn
            (sqlite-execute
             db
             "UPDATE org_headline SET digest=(SELECT digest FROM headline WHERE org_id='a') WHERE id=(SELECT id FROM headline WHERE org_id='a')")
            (sqlite-execute
             db "UPDATE org_headline SET content_hash='stale' WHERE id=(SELECT id FROM headline WHERE org_id='a')"))
        (sqlite-close db)))
    (should-not (org-glance-cache:metadata graph "a"))))

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
    (let ((org-glance-graph-after-append-functions nil))
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

(provide 'test-cache)
;;; test-cache.el ends here
