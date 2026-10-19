;;; test-cache.el --- Tests for the shared SQLite projection  -*- lexical-binding: t -*-

(require 'ert)
(require 'org-glance-cache)

(ert-deftest org-glance-test:graph-live-point-read-uses-one-cache-lookup ()
  "The public ID lookup delegates directly to the committed cache index."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "point-read" "* TODO Point read"))
    (setf (org-glance-graph:-meta-cache graph) nil)
    (let ((lookups 0)
          (original (symbol-function 'org-glance-cache:headline)))
      (cl-letf (((symbol-function 'org-glance-cache:headline)
                 (lambda (&rest args)
                   (cl-incf lookups)
                   (apply original args)))
                ((symbol-function 'org-glance-graph--ensure-cache)
                 (lambda (&rest _) (ert-fail "point read rebuilt the WAL cache")))
                ((symbol-function 'org-glance-cache:authority)
                 (lambda (&rest _) (ert-fail "point read decoded all families"))))
        (should (equal "Point read"
                       (org-glance-headline-metadata:title
                        (org-glance-graph:live-meta graph "point-read"))))
        (should (= 1 lookups))))))

(ert-deftest org-glance-test:interactive-reads-use-committed-cache-only ()
  "List and point reads neither scan the WAL nor enter cache maintenance."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "fast-read" "* TODO Fast read :work:"))
    (setf (org-glance-graph:-meta-cache graph) nil)
    (cl-letf (((symbol-function 'org-glance-graph--wal-headlines)
               (lambda (&rest _) (ert-fail "interactive WAL census")))
              ((symbol-function 'org-glance-graph--ensure-cache)
               (lambda (&rest _) (ert-fail "interactive WAL cache rebuild")))
              ((symbol-function 'org-glance-cache--transaction)
               (lambda (&rest _) (ert-fail "interactive cache maintenance"))))
      (let ((rows (org-glance-cache:headlines graph))
            (one (org-glance-cache:headline graph "fast-read")))
        (should (equal '("fast-read")
                       (mapcar #'org-glance-headline-metadata:id rows)))
        (should (equal "Fast read"
                       (org-glance-headline-metadata:title one)))))))

(ert-deftest org-glance-test:transient-action-models-never-start-a-census ()
  "Materialize, Open link, and Overview consume the committed projection."
  (org-glance-test:with-graph graph
    (org-glance-graph:add
     graph
     (org-glance-test:headline
      "entry" "* TODO Entry :work:" "[[https://example.invalid][Jump]]"))
    (setf (org-glance-graph:-meta-cache graph) nil)
    (cl-letf (((symbol-function 'org-glance-graph--wal-headlines)
               (lambda (&rest _) (ert-fail "transient WAL census")))
              ((symbol-function 'org-glance-graph--ensure-cache)
               (lambda (&rest _) (ert-fail "transient WAL cache rebuild")))
              ((symbol-function 'org-glance-cache--transaction)
               (lambda (&rest _) (ert-fail "transient cache maintenance"))))
      (should (= 1 (length (org-glance-material:candidates graph))))
      (should (= 1 (length
                    (org-glance-material:candidates
                     graph #'org-glance-headline-metadata:linked?))))
      (should (string-match-p "Entry" (org-glance-overview:render graph)))
      (should (= 1 (length (org-glance-table--rows graph #'always))))
      (cl-letf (((symbol-function 'org-glance-cache:headlines)
                 (lambda (&rest _) (ert-fail "materialize full cache scan")))
                ((symbol-function 'org-glance-graph:refresh-external)
                 (lambda (&rest _) (ert-fail "materialize external refresh"))))
        (let ((buffer (org-glance-material:open graph "entry")))
          (unwind-protect
              (should (buffer-live-p buffer))
            (when (buffer-live-p buffer)
              (kill-buffer buffer))))))))

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
          (should (equal (list (list org-glance-cache:version)) (sqlite-select db "PRAGMA user_version")))
            (should
             (equal
              '(("headline_event") ("headline_family")
                ("headline_payload_observation") ("headline_projection"))
              (sqlite-select
               db
               "SELECT name FROM sqlite_master WHERE (type='table' AND name LIKE 'headline_%') OR name='producer_projection' ORDER BY name")))
            (should
             (equal '(("headline_projection" 0))
                    (sqlite-select
                     db
                     "SELECT 'headline_projection',\"notnull\" FROM pragma_table_info('headline_projection') WHERE name='event_id'")))
            (should
             (equal '(("headline_projection_metadata"))
                    (sqlite-select
                     db
                     "SELECT name FROM sqlite_master WHERE type='index' AND name IN ('headline_projection_metadata','producer_projection_metadata') ORDER BY name")))
            (should (equal nil
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

(ert-deftest org-glance-test:material-path-is-the-digest-verified-current-projection ()
  "The committed family pointer resolves exact bytes without a History scan."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "material-path" "* TODO Material path" "body"))
    (let ((path (cl-letf (((symbol-function 'org-glance-version:history)
                           (lambda (&rest _) (ert-fail "cache read inspected History"))))
                  (org-glance-cache:material-path graph "material-path"))))
      (should (file-exists-p path))
      (org-glance-test:write path "* TODO changed behind the cache\n")
      (should-not (org-glance-cache:material-path graph "material-path")))))

(ert-deftest org-glance-test:tag-filter-decodes-only-candidate-projections ()
  "A tag query lets SQLite exclude unrelated committed Rows before JSON decode."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "wanted" "* TODO Wanted :benchmark:")
      (org-glance-test:headline "other" "* TODO Other :unrelated:"))
    (let ((decoded 0)
          (original (symbol-function 'org-glance-cache--decode-metadata)))
      (cl-letf (((symbol-function 'org-glance-cache--decode-metadata)
                 (lambda (payload)
                   (cl-incf decoded)
                   (funcall original payload))))
        (should (equal '("wanted")
                       (mapcar #'org-glance-headline-metadata:id
                               (org-glance-cache:headlines-for-filter
                                graph '(:tags ("benchmark"))))))
        (should (= 1 decoded))))))

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
      (org-glance-cache:reconcile-inventory graph)
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

(ert-deftest org-glance-test:local-family-is-searchable-and-keeps-its-authority ()
  "A local family participates in cache reads and subsequent writes."
  (org-glance-test:with-graph graph
    (let* ((id "local-family")
           (local (org-glance-graph:local-data-path graph))
           (dir (f-join local "lo" "cal-family"))
           (contents (org-glance-test:org-with-id
                      "* Local family :notification:" id)))
      (org-glance-version:write-snapshot dir id nil "glance" contents 10)
      (org-glance-cache:reconcile-inventory graph t)
      (should (equal dir (org-glance-graph:headline-data-path graph id)))
      (should (equal "Local family"
                     (org-glance-headline-metadata:title
                      (org-glance-graph:live-meta graph id))))
      (should-not
       (cl-find-if
        (lambda (path) (string-prefix-p ".org-glance/local/" path))
        (org-glance-cache:checkpoint-source-paths graph)))
      (let ((path (org-glance-graph:put-content
                   graph (org-glance-test:headline
                          id "* Updated local :notification:"))))
        (should (string-prefix-p (file-name-as-directory local) path)))
      (should-not
       (file-directory-p
        (f-join (org-glance-graph:data-path graph) "lo" "cal-family"))))))

(ert-deftest org-glance-test:duplicate-storage-authority-is-refused ()
  "A family present in both storage roots has no implicit winner."
  (org-glance-test:with-graph graph
    (let* ((id "duplicate-family")
           (contents (org-glance-test:org-with-id "* Duplicate" id))
           (portable (f-join (org-glance-graph:data-path graph)
                             "du" "plicate-family"))
           (local (f-join (org-glance-graph:local-data-path graph)
                          "du" "plicate-family")))
      (org-glance-version:write-snapshot portable id nil "glance" contents 10)
      (org-glance-version:write-snapshot local id nil "glance" contents 10)
      (should-error (org-glance-graph:headline-data-path graph id)
                    :type 'error)
      (should-error (org-glance-cache:reconcile-inventory graph t)
                    :type 'error))))

(ert-deftest org-glance-test:family-inventory-overrides-stale-wal-metadata ()
  "A reconciled current Snapshot wins over an older compatibility-WAL Row."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "family" "* Version one"))
    (let* ((dir (org-glance-graph:headline-data-path graph "family"))
           (leaf (car (org-glance-version:current dir "family"))))
      (org-glance-version:write-snapshot
       dir "family" (list (org-glance-version:id leaf)) "glance"
       "* Version two\n:PROPERTIES:\n:ORG_GLANCE_ID: family\n:END:\n"))
    (org-glance-cache:reconcile-inventory graph)
    (should
     (equal "Version two"
            (org-glance-headline-metadata:title
             (org-glance-graph:get-headline graph "family"))))))

(ert-deftest org-glance-test:family-authority-join-scales-linearly ()
  "Joining WAL and family Rows performs bounded metadata identity reads."
  (let* ((count 64)
         (metas
          (cl-loop for index below count
                   for id = (format "linear-%03d" index)
                   collect (make-org-glance-headline-metadata
                            :id id :title id :tags [])))
         (families
          (mapcar (lambda (meta)
                    (list (org-glance-headline-metadata:id meta) "live" "event"))
                  metas))
         (authority (list :families families :headlines metas :revived nil))
         (reads 0)
         (original-id
          (symbol-function 'org-glance-headline-metadata:id))
         (graph (make-org-glance-graph :directory default-directory)))
    (cl-letf (((symbol-function 'org-glance-cache:authority)
               (lambda (_graph) authority))
              ((symbol-function 'org-glance-graph--wal-headlines)
               (lambda (_graph) metas))
              ((symbol-function 'org-glance-headline-metadata:id)
               (lambda (metadata)
                 (cl-incf reads)
                 (funcall original-id metadata))))
      (should (= count (length (org-glance-graph:headlines graph))))
      (should (< reads (* 8 count))))))

(ert-deftest org-glance-test:clean-git-proof-skips-directory-stamps ()
  "A clean accepted Git state validates without walking family directories."
  (skip-unless (executable-find "git"))
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "git-proof" "* TODO Git proof"))
    (let ((default-directory (org-glance-graph:directory graph)))
      (should (= 0 (process-file "git" nil nil nil "init" "-q")))
      (should (= 0 (process-file "git" nil nil nil "add" "-A")))
      (should (= 0 (process-file
                    "git" nil nil nil "-c" "user.name=Benchmark"
                    "-c" "user.email=benchmark@example.invalid"
                    "commit" "-qm" "fixture"))))
    (org-glance-cache:reconcile-inventory graph)
    (let ((stamps 0)
          (original (symbol-function 'org-glance-cache--directory-stamp)))
      (cl-letf (((symbol-function 'org-glance-cache--directory-stamp)
                 (lambda (&rest args)
                   (cl-incf stamps)
                   (apply original args))))
        (org-glance-test:reopen graph)
        (should (= 0 stamps))))))

(ert-deftest org-glance-test:dirty-git-proof-selects-complete-census ()
  "A worktree change invalidates the Git proof before directory fallback."
  (skip-unless (executable-find "git"))
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "git-dirty" "* TODO Git dirty"))
    (let ((default-directory (org-glance-graph:directory graph)))
      (should (= 0 (process-file "git" nil nil nil "init" "-q")))
      (should (= 0 (process-file "git" nil nil nil "add" "-A")))
      (should (= 0 (process-file
                    "git" nil nil nil "-c" "user.name=Benchmark"
                    "-c" "user.email=benchmark@example.invalid"
                    "commit" "-qm" "fixture"))))
    (org-glance-cache:reconcile-inventory graph)
    (with-temp-buffer
      (insert "changed")
      (append-to-file (point-min) (point-max)
                      (org-glance-graph:content-path graph "git-dirty")))
    (let ((stamps 0)
          (original (symbol-function 'org-glance-cache--directory-stamp)))
      (cl-letf (((symbol-function 'org-glance-cache--directory-stamp)
                 (lambda (&rest args)
                   (cl-incf stamps)
                   (apply original args))))
        (org-glance-test:reopen graph)
        (should (> stamps 0))))))

(ert-deftest org-glance-test:clean-git-transition-folds-only-changed-families ()
  "A clean commit delta bypasses the complete family census."
  (skip-unless (executable-find "git"))
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "changed-family" "* TODO Before")
                          (org-glance-test:headline "stable-family" "* TODO Stable"))
    (let ((default-directory (org-glance-graph:directory graph)))
      (should (= 0 (process-file "git" nil nil nil "init" "-q")))
      (should (= 0 (process-file "git" nil nil nil "add" "-A")))
      (should (= 0 (process-file
                    "git" nil nil nil "-c" "user.name=Benchmark"
                    "-c" "user.email=benchmark@example.invalid"
                    "commit" "-qm" "before"))))
    (org-glance-cache:reconcile-inventory graph)
    (let* ((dir (org-glance-graph:headline-data-path graph "changed-family"))
           (leaf (car (org-glance-version:current dir "changed-family"))))
      (org-glance-version:write-snapshot
       dir "changed-family" (list (org-glance-version:id leaf)) "peer"
       (org-glance-test:org-with-id "* DONE After" "changed-family")))
    (let ((default-directory (org-glance-graph:directory graph)))
      (should (= 0 (process-file "git" nil nil nil "add" "-A")))
      (should (= 0 (process-file
                    "git" nil nil nil "-c" "user.name=Benchmark"
                    "-c" "user.email=benchmark@example.invalid"
                    "commit" "-qm" "after"))))
    (let ((folded nil)
          (original (symbol-function 'org-glance-cache--prepare-family)))
      (cl-letf (((symbol-function 'org-glance-cache--family-ids)
                 (lambda (_graph) (ert-fail "clean delta entered full census")))
                ((symbol-function 'org-glance-cache--prepare-family)
                 (lambda (held-graph id &rest inventory)
                   (push id folded)
                   (apply original held-graph id inventory))))
        (should (= 1 (org-glance-cache:reconcile-inventory graph)))
        (should (equal '("changed-family") folded))))
    (should (equal "After"
                   (org-glance-headline-metadata:title
                    (org-glance-cache:metadata graph "changed-family"))))))

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
        (let* ((revived
                (org-glance-version:write-snapshot
                 dir id (list (org-glance-version:id parent)) "glance" contents 10))
               (later
                (org-glance-version:write-snapshot
                 dir id (list (org-glance-version:id revived)) "glance" contents 10)))
          (org-glance-version:write-snapshot
           dir id (list (org-glance-version:id later)) "glance" contents 10)))

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
      (let ((parses 0)
            (original (symbol-function 'json-parse-string)))
        (cl-letf (((symbol-function 'json-parse-string)
                   (lambda (&rest args)
                     (cl-incf parses)
                     (apply original args))))
          (should (member id (plist-get (org-glance-cache:authority graph) :revived)))
          (should (= 1 parses))))
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
      (dolist (branch '(("left" "Left") ("right" "Right")))
        (let ((id (car branch))
              (contents (org-glance-test:org-with-id
                         (concat "* " (cadr branch)) "a")))
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
              (should (equal '(("left" "rejected") ("right" "current"))
                             (sqlite-select
                              db
                              "SELECT event_id,role FROM headline_projection WHERE logical_id='a'"))))
          (sqlite-close db))))))

(ert-deftest org-glance-test:rejected-snapshot-is-visible-and-an-edit-makes-it-current ()
  "A rejected Snapshot stays addressable; saving it publishes the current child."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Root"))
    (let* ((dir (org-glance-graph:headline-data-path graph "a"))
           (root (car (org-glance-version:current dir "a")))
           (root-id (org-glance-version:id root))
           (created "2026-10-10T00:00:00Z"))
      (dolist (branch '(("left" "Left :work:") ("right" "Right :work:")))
        (let ((id (car branch))
              (contents (org-glance-test:org-with-id
                         (concat "* " (cadr branch)) "a")))
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
      (should (= 1 (cl-count "a/left"
                             (mapcar #'org-glance-headline-metadata:id
                                     (org-glance-cache:headlines graph))
                             :test #'equal)))
      (should (= 1 (cl-count "a/left"
                             (mapcar #'org-glance-headline-metadata:id
                                     (org-glance-cache:headlines-for-filter
                                      graph '(:tags ("work"))))
                             :test #'equal)))
      (should (equal "Right"
                     (org-glance-headline-metadata:title
                      (org-glance-cache:headline graph "a"))))
      (org-glance-test:with-open buffer
          (org-glance-material:open graph "a/left")
        (with-current-buffer buffer
          (goto-char (point-min))
          (should (search-forward "Left" nil t))
          (replace-match "Left edited" t t)
          (org-glance-test:save)))
      (let* ((history (org-glance-version:history dir "a"))
             (current (org-glance-version:current dir "a")))
        (should (= 1 (length current)))
        (should (equal '("left" "right")
                       (sort (copy-sequence
                              (org-glance-version:parents (car current)))
                             #'string<)))
        (should (equal "Left edited"
                       (org-glance-headline-metadata:title
                        (org-glance-cache:headline graph "a"))))
        (should (= 5 (length history)))))))

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

(ert-deftest org-glance-test:restart-closes-cache-to-live-publication-boundary ()
  (dolist (kind '(snapshot tombstone))
    (dolist (phase '(after-sqlite-commit before-live-frame-publication))
      (with-temp-directory dir
        (let* ((graph (org-glance-graph dir))
               (frames 0))
          (org-glance-graph:add graph
            (org-glance-test:headline "a" "* Root"))
          (let ((org-glance-graph--publication-hook
                 (lambda (observed)
                   (when (eq observed phase)
                     (let ((db (sqlite-open (org-glance-cache:path graph))))
                       (unwind-protect
                           (should (= 2 (caar (sqlite-select db
                             "SELECT count(*) FROM headline_event WHERE logical_id='a'"))))
                         (sqlite-close db)))
                     (error "simulated crash at %s" phase))))
                (org-glance-graph-after-append-functions
                 (list (lambda (&rest _) (cl-incf frames)))))
            (should-error
             (pcase kind
               ('snapshot
                (org-glance-graph:add graph
                  (org-glance-test:headline "a" "* Child")))
               ('tombstone (org-glance-graph:delete graph "a")))))
          (should (= 0 frames))
          (let* ((cold (org-glance-test:reopen graph))
                 (history (org-glance-version:history
                           (org-glance-graph:headline-data-path cold "a") "a")))
            (should (= 2 (length history)))
            (pcase kind
              ('snapshot
               (should (equal "Child"
                              (org-glance-headline-metadata:title
                               (org-glance-graph:get-headline cold "a")))))
              ('tombstone
               (should (eq 'tombstone
                           (org-glance-graph:get-headline cold "a")))))))))))

(ert-deftest org-glance-test:publication-staging-is-outside-family-inventory ()
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "staging" "* Root"))
    (let* ((dir (org-glance-graph:headline-data-path graph "staging"))
           (staging (f-join dir "versions" ".version-abandoned")))
      (make-directory staging)
      (write-region "* Partial\n" nil (f-join staging "data.org") nil 'silent)
      (make-directory (f-join dir "versions" "foreign-staging.tmp"))
      (should (= 0 (org-glance-cache:reconcile-inventory graph)))
      (let ((db (sqlite-open (org-glance-cache:path graph))))
        (unwind-protect
            (should (equal '((1))
                           (sqlite-select db
                            "SELECT verified FROM headline_family WHERE logical_id='staging'")))
          (sqlite-close db))))))

(ert-deftest org-glance-test:cache-refresh-reports-proof-and-rebuild-progress ()
  "A slow cache boundary names its mode, phase, exact count, and elapsed time."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "progress" "* Progress"))
    (org-glance-cache:refresh graph)
    (let (messages)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (org-glance-cache:refresh graph))
      (should (cl-some
               (lambda (text)
                 (string-match-p
                  "org-glance: cache proof current (0 families rebuilt, [0-9.]+s)"
                  text))
               messages)))
    (make-directory (f-join (org-glance-graph:data-path graph) "unrelated"))
    (let (messages)
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (push (apply #'format format-string args) messages))))
        (org-glance-cache:refresh graph))
      (should (cl-some
               (lambda (text)
                 (string-match-p
                  "org-glance: cache rebuild: census (1/1 families, [0-9.]+s)"
                  text))
               messages))
      (should (cl-some
               (lambda (text)
                 (string-match-p
                  "org-glance: cache rebuild complete (0/1 families rebuilt, [0-9.]+s)"
                  text))
               messages)))))

(ert-deftest org-glance-test:cache-refresh-never-yields-on-the-caller-thread ()
  "An isolated rebuild has no cooperative interactive-Emacs branch."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "one" "* One"))
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (sqlite-execute
           db "UPDATE headline_family SET verified=0 WHERE logical_id='one'")
        (sqlite-close db)))
    (should-not (boundp 'org-glance-cache--cooperative))
    (let ((yields 0))
      (cl-letf (((symbol-function 'org-glance-cache--inventory-proof-current-p)
                 (lambda (_graph) nil))
                ((symbol-function 'thread-yield)
                 (lambda () (cl-incf yields))))
        (org-glance-cache:refresh graph)
        (should (= 0 yields))))))

(ert-deftest org-glance-test:checkpoint-source-paths-come-from-verified-inventory ()
  "Checkpoint admission receives the exact portable authority path set."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "portable" "* Portable"))
    (org-glance-cache:refresh graph)
    (let* ((root (org-glance-graph:directory graph))
           (family (org-glance-graph:headline-data-path graph "portable"))
           (event (file-name-nondirectory
                   (car (f-directories (f-join family "versions")))))
           (relative (lambda (path) (file-relative-name path root))))
      (should
       (equal
        (sort
         (list
          (funcall relative (org-glance-graph--manifest-path graph))
          (funcall relative (org-glance-graph:headline-meta-path graph))
          (funcall relative
                   (f-join family "versions" event "meta.json"))
          (funcall relative
                   (org-glance-version:data-file family event)))
         #'string<)
        (org-glance-cache:checkpoint-source-paths graph)))
      (let ((db (sqlite-open (org-glance-cache:path graph))))
        (unwind-protect
            (sqlite-execute
             db "UPDATE headline_family SET verified=0 WHERE logical_id='portable'")
          (sqlite-close db)))
      (should-error (org-glance-cache:checkpoint-source-paths graph)
                    :type 'error))))

(ert-deftest org-glance-test:family-projection-observes-filesystem-once ()
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "observed" "* Observed"))
    (let* ((dir (org-glance-graph:headline-data-path graph "observed"))
           (versions (f-join dir "versions"))
           (directory-censuses 0)
           (payload-observations 0)
           (payload-reads 0)
           (original-directories (symbol-function 'f-directories))
           (original-read-text (symbol-function 'f-read-text))
           (original-payload
            (symbol-function 'org-glance-cache--payload-observation))
           (leaf (car (org-glance-version:candidates
                       (org-glance-version:history dir "observed"))))
           (payload (org-glance-version:data-file
                     dir (org-glance-version:id leaf)))
           (db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (progn
            (sqlite-execute db "PRAGMA foreign_keys=ON")
            (cl-letf (((symbol-function 'f-directories)
                     (lambda (path &rest args)
                       (when (equal path versions)
                         (cl-incf directory-censuses))
                       (apply original-directories path args)))
                    ((symbol-function 'f-read-text)
                     (lambda (path &rest args)
                       (when (equal path payload)
                         (cl-incf payload-reads))
                       (apply original-read-text path args)))
                    ((symbol-function 'org-glance-cache--payload-observation)
                     (lambda (&rest args)
                       (cl-incf payload-observations)
                       (apply original-payload args))))
              (org-glance-cache--project-family db graph "observed")
              (should (= 1 directory-censuses))
              (should (= 1 payload-observations))
              (should (= 1 payload-reads))
              (setq directory-censuses 0
                    payload-observations 0
                    payload-reads 0)
              (delete-file payload)
              (org-glance-cache--project-family db graph "observed")
              (should (= 1 directory-censuses))
              (should (= 1 payload-observations))
              (should (= 0 payload-reads))))
        (sqlite-close db)))))

(ert-deftest org-glance-test:empty-family-projection-censuses-once ()
  (org-glance-test:with-graph graph
    (let* ((dir (org-glance-graph:headline-data-path graph "empty"))
           (versions (f-join dir "versions"))
           (directory-censuses 0)
           (original-directories (symbol-function 'f-directories))
           (db (sqlite-open (org-glance-cache:path graph))))
      (make-directory versions t)
      (unwind-protect
          (progn
            (sqlite-execute db "PRAGMA foreign_keys=ON")
            (cl-letf (((symbol-function 'f-directories)
                       (lambda (path &rest args)
                         (when (equal path versions)
                           (cl-incf directory-censuses))
                         (apply original-directories path args))))
              (org-glance-cache--project-family db graph "empty")
              (should (= 1 directory-censuses))
              (setq directory-censuses 0)
              (org-glance-cache--project-family db graph "empty" nil)
              (should (= 0 directory-censuses))))
        (sqlite-close db)))))

(ert-deftest org-glance-test:explicit-refresh-detects-after-nonblocking-read ()
  "A read stays nonblocking; an explicit refresh detects a new event."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "observed" "* Observed"))
    (let ((censuses 0)
          (original (symbol-function 'org-glance-cache--inventory-event-ids)))
      (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 100.0))
                ((symbol-function 'org-glance-cache--inventory-event-ids)
                 (lambda (&rest args)
                   (cl-incf censuses)
                   (apply original args))))
        (org-glance-cache:refresh graph)
        (let ((startup-censuses censuses))
          (should (> startup-censuses 0))
          (org-glance-graph:headlines graph)
          (should (= startup-censuses censuses))
          (let* ((dir (org-glance-graph:headline-data-path graph "observed"))
                 (leaf (car (org-glance-version:current dir "observed"))))
            (org-glance-version:write-snapshot
             dir "observed" (list (org-glance-version:id leaf)) "peer"
             "* Changed\n"))
          (org-glance-graph:headlines graph)
          (should (= startup-censuses censuses))
          (org-glance-cache:refresh graph)
          (should (> censuses startup-censuses)))))))

(ert-deftest org-glance-test:warm-refresh-projects-only-changed-families ()
  "A persisted family proof skips warm projection and narrows stale work."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "observed" "* Before"))
    (let ((projects 0)
          (original (symbol-function 'org-glance-cache--project-family)))
      (cl-letf (((symbol-function 'org-glance-cache--project-family)
                 (lambda (&rest args)
                   (cl-incf projects)
                   (apply original args))))
        (org-glance-cache:refresh graph)
        (should (= 0 projects))
        (let* ((dir (org-glance-graph:headline-data-path graph "observed"))
               (leaf (car (org-glance-version:current dir "observed"))))
          (org-glance-version:write-snapshot
           dir "observed" (list (org-glance-version:id leaf)) "peer"
           "* After\n"))
        (org-glance-cache:refresh graph)
        (should (= 1 projects))))))

(ert-deftest org-glance-test:warm-restart-loads-the-persisted-directory-proof ()
  "A new graph validates warm inventory without enumerating event directories."
  (with-temp-directory dir
    (let ((graph (org-glance-graph dir)))
      (org-glance-graph:add graph (org-glance-test:headline "observed" "* Observed"))
      (org-glance-cache:refresh graph)
      (remhash (org-glance-graph:directory graph) org-glance-graph:list))
    (let ((censuses 0)
          (original (symbol-function 'org-glance-cache--inventory-event-ids)))
      (cl-letf (((symbol-function 'org-glance-cache--inventory-event-ids)
                 (lambda (&rest args)
                   (cl-incf censuses)
                   (apply original args))))
        (let ((graph (org-glance-graph dir)))
          (should (= 0 censuses))
          (let* ((family (org-glance-graph:headline-data-path graph "observed"))
                 (leaf (car (org-glance-version:current family "observed"))))
            (org-glance-version:write-snapshot
             family "observed" (list (org-glance-version:id leaf)) "peer"
             "* Changed\n"))
          (remhash (org-glance-graph:directory graph) org-glance-graph:list))
        (org-glance-graph dir)
        (should (> censuses 0))))))

(ert-deftest org-glance-test:warm-proof-requires-a-coherent-cache-generation ()
  "A current directory proof cannot hide a missing foreign-owned family."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "observed" "* Observed"))
    (org-glance-cache:refresh graph)
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (progn
            (sqlite-execute db "PRAGMA foreign_keys=ON")
            (sqlite-execute
             db "UPDATE headline SET producer='glance' WHERE org_id='observed'")
            (sqlite-execute
             db "DELETE FROM headline_family WHERE logical_id='observed'"))
        (sqlite-close db)))
    (should (org-glance-cache--inventory-proof-current-p graph))
    (should-not (org-glance-cache--projection-coherent-p graph))
    (org-glance-cache--reconcile-now graph)
    (should (org-glance-cache--projection-coherent-p graph))))

(ert-deftest org-glance-test:warm-proof-requires-an-org-glance-family-projection ()
  "A live family with another parser's projection is not current locally."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "observed" "* Observed"))
    (org-glance-cache:refresh graph)
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (sqlite-execute
           db
           "UPDATE headline_projection SET parser_contract='glance:1',record_payload='{}' WHERE logical_id='observed'")
        (sqlite-close db)))
    (should (org-glance-cache--inventory-proof-current-p graph))
    (should-not (org-glance-cache--projection-coherent-p graph))
    (org-glance-cache--reconcile-now graph)
    (should (org-glance-cache--projection-coherent-p graph))
    (should (equal "Observed"
                   (org-glance-headline-metadata:title
                    (org-glance-cache:headline graph "observed"))))))

(ert-deftest org-glance-test:stale-proof-repairs-a-missing-family-projection ()
  "Verified event inventory cannot hide an absent local family projection."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "observed" "* Observed"))
    (org-glance-cache:refresh graph)
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (progn
            (sqlite-execute db "DELETE FROM headline_projection")
            (sqlite-execute db "DELETE FROM inventory_directory"))
        (sqlite-close db)))
    (remhash graph org-glance-cache--inventory-directory-proofs)
    (should-not (org-glance-cache--inventory-proof-current-p graph))
    (should-not (org-glance-cache--projection-coherent-p graph))
    (org-glance-cache--reconcile-now graph)
    (should (equal "Observed"
                   (org-glance-headline-metadata:title
                    (org-glance-cache:headline graph "observed"))))))

(ert-deftest org-glance-test:inventory-tick-skips-a-proven-current-store ()
  "The session timer does not census a store whose warm proof is current."
  (let ((org-glance-graph:list (make-hash-table :test #'equal)))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph
        (org-glance-test:headline "observed" "* Observed"))
      (org-glance-cache:refresh graph)
      (let ((censuses 0)
            (original
             (symbol-function 'org-glance-cache--inventory-event-ids)))
        (cl-letf (((symbol-function 'org-glance-cache--inventory-event-ids)
                   (lambda (&rest args)
                     (cl-incf censuses)
                     (apply original args))))
          (org-glance-cache--inventory-tick)
          (should (= 0 censuses)))))))

(ert-deftest org-glance-test:inventory-timer-uses-the-notification-interval ()
  "The coordinator checks bounded relay hints independently of family refresh."
  (let ((org-glance-cache--inventory-timer nil)
        (org-glance-cache-notification-interval 2.5)
        scheduled)
    (cl-letf (((symbol-function 'run-with-timer)
               (lambda (&rest args)
                 (setq scheduled args)
                 'inventory-timer)))
      (org-glance-cache--start-inventory-timer nil)
      (should (equal '(1 2.5 org-glance-cache--inventory-tick) scheduled)))))

(ert-deftest org-glance-test:inventory-tick-prioritizes-an-external-hint ()
  "A pending relay wakes the isolated worker without waiting for its throttle."
  (let ((org-glance-graph:list (make-hash-table :test #'equal))
        requests)
    (with-temp-directory dir
      (let ((graph (make-org-glance-graph :directory dir)))
        (puthash dir graph org-glance-graph:list)
        (cl-letf (((symbol-function 'org-glance-graph--external-pending-p)
                   (lambda (_graph) t))
                  ((symbol-function 'org-glance-cache:refresh-async)
                   (lambda (requested &optional urgent)
                     (push (list requested urgent) requests)))
                  ((symbol-function 'org-glance-graph:refresh-external)
                   (lambda (&rest _) (ert-fail "timer folded on its caller"))))
          (org-glance-cache--inventory-tick)
          (should (equal (list (list graph t)) requests)))))))

(ert-deftest org-glance-test:graph-open-queues-cache-refresh-in-subprocess ()
  "Opening a graph launches reconciliation outside interactive Emacs."
  (with-temp-directory dir
    (let ((graph (org-glance-graph dir)))
      (org-glance-graph:add graph
        (org-glance-test:headline "observed" "* Observed"))
      (remhash (org-glance-graph:directory graph) org-glance-graph:list))
    (let ((org-glance-cache--refresh-synchronously nil)
          (org-glance-cache--refresh-processes (make-hash-table :test #'eq))
          (org-glance-cache--refresh-requested-at (make-hash-table :test #'eq)))
      (let (process-args)
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq process-args args)
                     'queued-process))
                  ((symbol-function 'make-thread)
                   (lambda (&rest _) (ert-fail "reconciliation used an Emacs thread"))))
          (let ((graph (org-glance-graph dir)))
            (should (equal "org-glance-cache-refresh"
                           (plist-get process-args :name)))
            (should (member "--batch" (plist-get process-args :command)))
            (should (member "--quick" (plist-get process-args :command)))
            (should (cl-find-if
                     (lambda (arg) (s-contains? (file-truename dir) arg))
                     (plist-get process-args :command)))
            (should (equal '("observed") (org-glance-test:ids graph)))))))))

(ert-deftest org-glance-test:cache-worker-finds-a-package-upgraded-in-place ()
  "A live session can launch its worker after its loaded package was replaced."
  (org-glance-test:with-graph graph
    (with-temp-directory elpa
      (let* ((old (f-join elpa "org-glance-20261010.1211"))
             (current (f-join elpa "org-glance-20261012.1838"))
             (loaded (f-join old "org-glance-cache.elc"))
             (load-path (list old)))
        (make-directory current)
        (cl-letf (((symbol-function 'symbol-file)
                   (lambda (&rest _) loaded)))
          (should (member current
                          (org-glance--batch-worker-load-paths
                           'org-glance-cache:refresh-async))))))))

(ert-deftest org-glance-test:startup-refresh-leaves-first-read-due ()
  "Startup work does not throttle the first post-open inventory request."
  (org-glance-test:with-graph graph
    (let ((org-glance-cache--refresh-processes (make-hash-table :test #'eq))
          (org-glance-cache--refresh-requested-at (make-hash-table :test #'eq))
          launches)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (push args launches)
                   'queued-process)))
        (org-glance-cache:refresh-async graph t)
        (should-not (gethash graph org-glance-cache--refresh-requested-at))
        (remhash graph org-glance-cache--refresh-processes)
        (org-glance-cache:refresh-async graph)
        (should (= 2 (length launches)))
        (should (numberp
                 (gethash graph org-glance-cache--refresh-requested-at)))))))

(ert-deftest org-glance-test:cache-refresh-process-sentinel-retires-job ()
  "A completed reconciliation process releases the graph's job slot."
  (org-glance-test:with-graph graph
    (let ((org-glance-cache--refresh-processes (make-hash-table :test #'eq))
          (process 'fake-process))
      (puthash graph process org-glance-cache--refresh-processes)
      (cl-letf (((symbol-function 'process-status) (lambda (_process) 'exit))
                ((symbol-function 'process-exit-status) (lambda (_process) 0)))
        (org-glance-cache--refresh-finished graph process "finished\n"))
      (should-not (gethash graph org-glance-cache--refresh-processes)))))

(ert-deftest org-glance-test:cache-refresh-batch-runs-without-recursive-hooks ()
  "The batch worker recovers a missing generation exactly once."
  (with-temp-directory dir
    (let ((recoveries 0))
      (cl-letf (((symbol-function 'org-glance-cache:recover)
                 (lambda (_graph) (cl-incf recoveries))))
        (org-glance-cache:refresh-batch dir)
        (should (= 1 recoveries))))))

(ert-deftest org-glance-test:synchronous-open-reconciles-before-reads ()
  "Deterministic startup reconciles an existing generation before reads."
  (org-glance-test:with-graph graph
    (let ((org-glance-cache--refresh-synchronously t)
          (reconciliations 0))
      (cl-letf (((symbol-function 'org-glance-cache:reconcile-inventory)
                 (lambda (_graph) (cl-incf reconciliations)))
                ((symbol-function 'org-glance-cache--inventory-proof-current-p)
                 (lambda (_graph) nil))
                ((symbol-function 'org-glance-cache:refresh)
                 (lambda (&rest _) (ert-fail "startup used legacy refresh")))
                ((symbol-function 'org-glance-cache:recover)
                 (lambda (&rest _) (ert-fail "existing cache entered recovery"))))
        (org-glance-cache--refresh-on-open graph)
        (should (= 1 reconciliations))))))

(ert-deftest org-glance-test:cache-refresh-batch-keeps-an-existing-generation ()
  "The routine worker incrementally reconciles an existing generation."
  (org-glance-test:with-graph graph
    (let ((relay-folds 0)
          (reconciliations 0))
      (cl-letf (((symbol-function 'org-glance-cache:reconcile-inventory)
                 (lambda (_graph) (cl-incf reconciliations)))
                ((symbol-function 'org-glance-cache--inventory-proof-current-p)
                 (lambda (_graph) nil))
                ((symbol-function 'org-glance-graph:refresh-external)
                 (lambda (_graph) (cl-incf relay-folds)))
                ((symbol-function 'org-glance-cache:refresh)
                 (lambda (_graph) (ert-fail "existing cache entered full refresh")))
                ((symbol-function 'org-glance-cache:recover)
                 (lambda (_graph) (ert-fail "existing cache entered recovery"))))
        (org-glance-cache:refresh-batch (org-glance-graph:directory graph))
        (should (= 1 relay-folds))
        (should (= 1 reconciliations))))))

(ert-deftest org-glance-test:cache-recovery-publishes-one-complete-generation ()
  "Failed recovery preserves the readable generation; retry replaces it."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "generation" "* TODO Authoritative"))
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (let* ((payload
                  (caar (sqlite-select
                         db "SELECT org_headline.record FROM org_headline JOIN headline USING(id) WHERE headline.org_id='generation'")))
                 (record (json-parse-string payload :object-type 'plist)))
            (sqlite-execute db "UPDATE headline SET title='Retained' WHERE org_id='generation'")
            (sqlite-execute
             db "UPDATE org_headline SET record=? WHERE id=(SELECT id FROM headline WHERE org_id='generation')"
             (vector (json-serialize (plist-put record :title "Retained"))))
            (sqlite-execute
             db "UPDATE headline_projection SET record_payload=? WHERE logical_id='generation'"
             (vector (json-serialize record))))
        (sqlite-close db)))
    (should (equal "Retained"
                   (org-glance-headline-metadata:title
                    (org-glance-cache:headline graph "generation"))))
    (let* ((observed (vector nil))
          (org-glance-cache--recovery-publication-hook
           (lambda ()
             (aset observed 0
                   (org-glance-headline-metadata:title
                    (org-glance-cache:headline graph "generation")))
             (error "simulated recovery publication failure"))))
      (should-error (org-glance-cache:recover graph) :type 'error)
      (should (equal "Retained" (aref observed 0))))
    (should (equal "Retained"
                   (org-glance-headline-metadata:title
                    (org-glance-cache:headline graph "generation"))))
    (org-glance-cache:recover graph)
    (should (equal "Authoritative"
                   (org-glance-headline-metadata:title
                    (org-glance-cache:headline graph "generation"))))))

(ert-deftest org-glance-test:private-cache-migration-preserves-the-shared-database ()
  "Import committed org-glance Rows while leaving the old database untouched."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "owned" "* TODO Owned"))
    (let* ((target (org-glance-cache:path graph))
           (legacy (org-glance-graph:cache-file graph "glance.sqlite3"))
           (db (sqlite-open target)))
      (unwind-protect
          (progn
            (sqlite-execute db "PRAGMA user_version=6")
            (sqlite-execute db "UPDATE headline SET producer='glance'")
            (sqlite-execute db "UPDATE source SET producer='glance'")
            (sqlite-execute db "UPDATE headline_projection SET parser_contract='glance:2',record_payload='{}'")
            (sqlite-execute db "CREATE TABLE glance_payload(path TEXT)")
            (sqlite-execute db "INSERT INTO glance_payload VALUES('peer-preserved')"))
        (sqlite-close db))
      (rename-file target legacy)
      (delete-directory (org-glance-graph:headline-data-path graph "owned") t)
      (let ((digest (org-glance-cache--file-digest legacy)))
        (org-glance-cache--migrate graph)
        (should (equal digest (org-glance-cache--file-digest legacy))))
      (should (equal "Owned" (org-glance-headline-metadata:title
                              (org-glance-cache:headline graph "owned"))))
      (org-glance-cache--read-db
       graph
       (lambda (held)
         (should-not (sqlite-select held "SELECT name FROM sqlite_master WHERE name='glance_payload'"))
         (should (equal '(("org-glance"))
                        (sqlite-select held "SELECT producer FROM headline")))
         (should (equal '((0)) (sqlite-select held "SELECT verified FROM headline_family")))))
      (let ((peer (sqlite-open legacy)))
        (unwind-protect
            (progn
              (sqlite-execute peer "BEGIN IMMEDIATE")
              (sqlite-execute peer "DELETE FROM glance_payload")
              (org-glance-cache:recover graph)
              (should (equal "Owned" (org-glance-headline-metadata:title
                                      (org-glance-cache:headline graph "owned"))))
              (should (equal '(("peer-preserved"))
                             (let ((other (sqlite-open legacy)))
                               (unwind-protect
                                   (sqlite-select other "SELECT path FROM glance_payload")
                                 (sqlite-close other))))))
          (sqlite-rollback peer)
          (sqlite-close peer)))
      (with-temp-file legacy (insert "unreadable old cache"))
      (org-glance-cache--migrate graph)
      (should (equal "Owned" (org-glance-headline-metadata:title
                              (org-glance-cache:headline graph "owned")))))))

(ert-deftest org-glance-test:private-cache-migration-opens-the-legacy-file-read-only ()
  "Refuse writes through the migration handle, including URI-special paths."
  (with-temp-directory directory
    (let* ((graph (org-glance-graph (f-join directory "space ? café")))
           (target (org-glance-cache:path graph))
           (legacy (org-glance-graph:cache-file graph "glance.sqlite3"))
           (db (sqlite-open target))
           (execute (symbol-function 'sqlite-execute))
           (probes 0))
      (org-glance-graph:add graph
        (org-glance-test:headline "owned" "* Read-only snapshot"))
      (unwind-protect
          (sqlite-execute db "PRAGMA user_version=6")
        (sqlite-close db))
      (rename-file target legacy)
      (let ((before (org-glance-cache--file-digest legacy)))
        (cl-letf (((symbol-function 'sqlite-execute)
                   (lambda (handle sql &rest args)
                     (prog1 (apply execute handle sql args)
                       (when (equal sql "ATTACH DATABASE ? AS legacy")
                         (cl-incf probes)
                         (should-error
                          (funcall execute handle
                                   "CREATE TABLE legacy.forbidden(value TEXT)")
                          :type 'sqlite-error))))))
          (org-glance-cache--migrate graph))
        (should (= 1 probes))
        (should (equal "Read-only snapshot"
                       (org-glance-headline-metadata:title
                        (org-glance-cache:headline graph "owned"))))
        (should (equal before (org-glance-cache--file-digest legacy)))))))

(ert-deftest org-glance-test:private-cache-migration-preserves-unknown-legacy-bytes ()
  "An unknown legacy format remains intact while canonical sources rebuild."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "owned" "* Canonical"))
    (let ((target (org-glance-cache:path graph))
          (legacy (org-glance-graph:cache-file graph "glance.sqlite3")))
      (delete-file target)
      (with-temp-file legacy (insert "unknown cache format"))
      (org-glance-cache--reconcile-now graph)
      (should (equal "Canonical" (org-glance-headline-metadata:title
                                  (org-glance-cache:headline graph "owned"))))
      (should (equal "unknown cache format" (f-read-text legacy))))))

(ert-deftest org-glance-test:private-cache-migration-rebuilds-peer-only-projections ()
  "Peer metadata becomes a recovery Row until org-glance verifies the source."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "owned" "* TODO Independently parsed"))
    (let* ((target (org-glance-cache:path graph))
           (legacy (org-glance-graph:cache-file graph "glance.sqlite3"))
           (db (sqlite-open target)))
      (unwind-protect
          (progn
            (sqlite-execute db "PRAGMA user_version=6")
            (sqlite-execute db "DELETE FROM org_headline")
            (sqlite-execute db "UPDATE headline_projection SET parser_contract='glance:2',record_payload='{}'"))
        (sqlite-close db))
      (rename-file target legacy)
      (let ((peer (sqlite-open legacy)))
        (unwind-protect
            (progn
              (sqlite-execute peer "BEGIN IMMEDIATE")
              (sqlite-execute peer "DELETE FROM headline_family")
              (org-glance-cache--migrate graph)
              (should (equal "unavailable"
                             (org-glance-headline-metadata:recovery
                              (org-glance-cache:headline graph "owned")))))
          (sqlite-rollback peer)
          (sqlite-close peer)))
      (org-glance-cache--reconcile-now graph)
      (should (equal "Independently parsed"
                     (org-glance-headline-metadata:title
                      (org-glance-cache:headline graph "owned")))))))

(ert-deftest org-glance-test:cache-recovery-refuses-a-concurrent-publication ()
  "Recovery cannot replace a local Row committed after its inventory proof."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "concurrent" "* TODO Before"))
    (let ((proof-scans 0)
          (original (symbol-function
                     'org-glance-cache--directory-proof-current-p))
          (org-glance-cache--recovery-publication-hook
           (lambda ()
             (org-glance-graph:add graph
               (org-glance-test:headline "concurrent" "* TODO Concurrent")))))
      (cl-letf (((symbol-function 'org-glance-cache--directory-proof-current-p)
                 (lambda (&rest args)
                   (cl-incf proof-scans)
                   (apply original args))))
        (should-error (org-glance-cache:recover graph) :type 'error)
        (should (= 0 proof-scans))))
    (should (equal "Concurrent"
                   (org-glance-headline-metadata:title
                    (org-glance-cache:headline graph "concurrent"))))))

(ert-deftest org-glance-test:cache-refresh-worker-rebuilds-in-separate-emacs ()
  "The real batch worker rebuilds a deleted cache and exits successfully."
  (with-temp-directory dir
    (let* ((graph (org-glance-graph dir))
           (id "worker-visible")
           (data-path (org-glance-graph:headline-data-path graph id)))
      (org-glance-version:write-snapshot
       data-path id nil "test"
       (org-glance-test:org-with-id "* TODO Worker visible" id)
       0)
      (let ((cache (org-glance-cache:path graph)))
        (when (file-exists-p cache) (delete-file cache)))
      (remhash (org-glance-graph:directory graph) org-glance-graph:list)
      (let* ((buffer (generate-new-buffer " *org-glance-worker-test*"))
             (process
              (make-process
               :name "org-glance-worker-test"
               :buffer buffer
               :command (org-glance-cache--worker-command graph)
               :connection-type 'pipe
               :coding 'utf-8-unix
               :noquery t)))
        (unwind-protect
            (progn
              (while (process-live-p process)
                (accept-process-output process 0.1))
              (should (= 0 (process-exit-status process)))
              (let ((db (sqlite-open (org-glance-cache:path graph))))
                (unwind-protect
                    (should (equal (list (list id "current"))
                                   (sqlite-select
                                    db
                                    "SELECT logical_id,role FROM headline_projection WHERE logical_id=?"
                                    (vector id))))
                  (sqlite-close db))))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest org-glance-test:headline-read-never-enters-reconciliation ()
  "A graph read neither polls external state nor requests maintenance."
  (with-temp-directory dir
    (cl-letf (((symbol-function 'org-glance-graph--external-survey)
               (lambda (&rest _) (ert-fail "read surveyed external state")))
              ((symbol-function 'org-glance-graph:refresh-external)
               (lambda (&rest _) (ert-fail "read folded external state")))
              ((symbol-function 'org-glance-cache:refresh-async)
               (lambda (&rest _) (ert-fail "read requested reconciliation")))
              ((symbol-function 'org-glance-cache:reconcile-inventory)
               (lambda (&rest _) (ert-fail "read reconciled inventory"))))
      (let ((graph (org-glance-graph dir)))
        (org-glance-graph:headlines graph)))))

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

(ert-deftest org-glance-test:publication-keeps-pinned-readers-and-open-writers ()
  "Pinned reads retain old Rows; existing handles write the published inode."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "pinned" "* Before"))
    (let* ((path (org-glance-cache:path graph))
           (inode (file-attribute-file-identifier (file-attributes path)))
           (reader (sqlite-open path))
           (writer (sqlite-open path)))
      (unwind-protect
          (progn
            (sqlite-execute reader "BEGIN")
            (should (equal '(("Before"))
                           (sqlite-select reader "SELECT title FROM headline")))
            (org-glance-cache--with-staged-generation
             graph
             (lambda ()
               (org-glance-cache--with-db
                graph (lambda (db)
                        (sqlite-execute db "UPDATE headline SET title='Published'")))))
            (should (equal inode
                           (file-attribute-file-identifier (file-attributes path))))
            (should (equal '(("Before"))
                           (sqlite-select reader "SELECT title FROM headline")))
            (sqlite-commit reader)
            (should (equal '(("Published"))
                           (sqlite-select reader "SELECT title FROM headline")))
            (sqlite-execute writer "UPDATE headline SET title='Later writer'")
            (should (equal '(("Later writer"))
                           (sqlite-select reader "SELECT title FROM headline"))))
        (sqlite-close reader)
        (sqlite-close writer)))))

(ert-deftest org-glance-test:readers-wait-for-the-first-complete-generation ()
  "An opened live file remains unavailable until its first generation commits."
  (with-temp-directory dir
    (let* ((org-glance-graph-after-open-functions nil)
           (graph (org-glance-graph dir))
           (path (org-glance-cache:path graph)))
      (make-directory (file-name-directory path) t)
      (let ((db (sqlite-open path)))
        (unwind-protect
            (progn
              (should-not (plist-get (org-glance-cache:authority graph) :families))
              (should-not (org-glance-cache:headlines graph))
              (should-not (org-glance-cache:headline graph "first")))
          (sqlite-close db)))
      (org-glance-graph:add graph
        (org-glance-test:headline "first" "* First"))
      (should (equal "First"
                     (org-glance-headline-metadata:title
                      (org-glance-cache:headline graph "first")))))))

(ert-deftest org-glance-test:publication-refuses-a-commit-during-staging ()
  "A concurrent committed change makes the staged snapshot stale."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "race" "* Before"))
    (let ((writer (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (progn
            (should-error
             (org-glance-cache--with-staged-generation
              graph
              (lambda ()
                (sqlite-execute writer "UPDATE headline SET title='Concurrent'")
                (org-glance-cache--with-db
                 graph (lambda (db)
                         (sqlite-execute db "UPDATE headline SET title='Stale'")))))
             :type 'error)
            (should (equal '(("Concurrent"))
                           (sqlite-select writer "SELECT title FROM headline"))))
        (sqlite-close writer)))))

(ert-deftest org-glance-test:incompatible-schema-migrates-as-a-complete-generation ()
  "Migration preserves pinned old Rows and publishes rebuilt Rows together."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "migrate" "* Authoritative"))
    (let ((reader (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (progn
            (sqlite-execute reader "UPDATE headline SET title='Retained'")
            (sqlite-execute
             reader (format "PRAGMA user_version=%d" (1- org-glance-cache:version)))
            (sqlite-execute reader "BEGIN")
            (should (equal '(("Retained"))
                           (sqlite-select reader "SELECT title FROM headline")))
            (org-glance-cache:reconcile-families graph '("migrate"))
            (should (equal '(("Retained"))
                           (sqlite-select reader "SELECT title FROM headline")))
            (should (= (1- org-glance-cache:version)
                       (caar (sqlite-select reader "PRAGMA user_version"))))
            (sqlite-commit reader)
            (should (equal '(("Authoritative"))
                           (sqlite-select reader "SELECT title FROM headline")))
            (should (= org-glance-cache:version
                       (caar (sqlite-select reader "PRAGMA user_version")))))
        (sqlite-close reader)))))

(ert-deftest org-glance-test:publication-respects-an-active-live-writer ()
  "A staged publication preserves an already active live writer transaction."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "writer" "* Before"))
    (let ((writer (sqlite-open (org-glance-cache:path graph)))
          (prepare (symbol-function 'org-glance-cache--prepare)))
      (unwind-protect
          (let ((org-glance-cache--recovery-publication-hook
                 (lambda ()
                   (sqlite-execute writer "BEGIN IMMEDIATE")
                   (sqlite-execute writer "UPDATE headline SET title='Writer'"))))
            (cl-letf (((symbol-function 'org-glance-cache--prepare)
                       (lambda (db)
                         (funcall prepare db)
                         (sqlite-execute db "PRAGMA busy_timeout=0"))))
              (should-error
               (org-glance-cache--with-staged-generation
                graph (lambda () nil))
               :type 'sqlite-error))
            (sqlite-commit writer)
            (org-glance-cache--read-db
             graph
             (lambda (db)
               (should (equal '(("Writer"))
                              (sqlite-select db "SELECT title FROM headline"))))))
        (sqlite-close writer)))))

(ert-deftest org-glance-test:publication-refuses-a-source-change-after-census ()
  "An unannounced immutable event invalidates the staged directory proof."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "source-race" "* Before"))
    (let* ((dir (org-glance-graph:headline-data-path graph "source-race"))
           (leaf (car (org-glance-version:current dir "source-race")))
           (before (org-glance-cache:authority graph))
           (org-glance-cache--recovery-publication-hook
            (lambda ()
              (org-glance-version:write-snapshot
               dir "source-race" (list (org-glance-version:id leaf))
               "peer" "* Concurrent source\n"))))
      (should-error (org-glance-cache:recover graph) :type 'error)
      (should (equal before (org-glance-cache:authority graph))))))

(ert-deftest org-glance-test:publication-failure-rolls-back-schema-and-rows ()
  "A failure after copying one table restores the complete live generation."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "rollback" "* Before"))
    (let ((before (org-glance-cache:authority graph))
          (original (symbol-function 'sqlite-execute))
          (copied 0))
      (cl-letf (((symbol-function 'sqlite-execute)
                 (lambda (db sql &rest args)
                   (when (string-prefix-p "INSERT INTO main." sql)
                     (cl-incf copied)
                     (when (= copied 2) (error "injected publication failure")))
                   (apply original db sql args))))
        (should-error
         (org-glance-cache--with-staged-generation
          graph
          (lambda ()
            (org-glance-cache--with-db
             graph (lambda (db)
                     (sqlite-execute db "UPDATE headline SET title='Aborted'")))))
         :type 'error))
      (should (= copied 2))
      (should (equal before (org-glance-cache:authority graph))))))

(ert-deftest org-glance-test:bulk-family-work-releases-the-live-writer ()
  "Family parsing runs on the stage while a live writer can acquire its lock."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "outside" "* Before"))
    (let* ((path (org-glance-cache:path graph))
           (dir (org-glance-graph:headline-data-path graph "outside"))
           (leaf (car (org-glance-version:current dir "outside")))
           (original (symbol-function 'org-glance-cache--project-family))
           (observed 0))
      (org-glance-version:write-snapshot
       dir "outside" (list (org-glance-version:id leaf)) "peer" "* After\n")
      (cl-letf (((symbol-function 'org-glance-cache--project-family)
                 (lambda (&rest args)
                   (should-not (equal path (org-glance-cache:path graph)))
                   (let ((writer (sqlite-open path)))
                     (unwind-protect
                         (progn
                           (sqlite-execute writer "PRAGMA busy_timeout=0")
                           (sqlite-execute writer "BEGIN IMMEDIATE")
                           (cl-incf observed)
                           (sqlite-rollback writer))
                       (sqlite-close writer)))
                   (apply original args))))
        (org-glance-cache:reconcile-inventory graph))
      (should (= 1 observed)))))

(ert-deftest org-glance-test:public-stage-publisher-retains-concurrent-commits ()
  "The standalone publication API keeps its commit guard and live handle."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "standalone" "* Original"))
    (with-temp-directory stage-directory
      (let* ((path (org-glance-cache:path graph))
             (stage (expand-file-name "stage.sqlite3" stage-directory))
             (live (sqlite-open path)))
        (unwind-protect
            (progn
              (sqlite-execute live "VACUUM INTO ?" (vector stage))
              (let ((db (sqlite-open stage)))
                (unwind-protect
                    (sqlite-execute db "UPDATE headline SET title='Stage'")
                  (sqlite-close db)))
              (should-error
               (org-glance-cache:publish-staged
                path stage
                (lambda ()
                  (sqlite-execute live "UPDATE headline SET title='Peer'"))))
              (should (equal '(("Peer"))
                             (sqlite-select live "SELECT title FROM headline")))
              (org-glance-cache:publish-staged path stage)
              (should (equal '(("Stage"))
                             (sqlite-select live "SELECT title FROM headline")))
              (should (file-exists-p stage)))
          (sqlite-close live))))))

(ert-deftest org-glance-test:warm-refresh-never-clones-a-generation ()
  "A current warm inventory performs no staging work."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "warm" "* Warm"))
    (org-glance-cache:refresh graph)
    (cl-letf (((symbol-function 'org-glance-cache--with-staged-generation)
               (lambda (&rest _) (ert-fail "warm refresh cloned database"))))
      (org-glance-cache:refresh graph))))

(ert-deftest org-glance-test:local-write-prepares-before-taking-the-live-writer ()
  "Local CRUD parses outside its transaction and avoids database cloning."
  (org-glance-test:with-graph graph
    (let ((path (org-glance-cache:path graph))
          (family (symbol-function 'org-glance-cache--prepare-family))
          (headline (symbol-function 'org-glance-cache--prepare-headline))
          (prepared 0))
      (cl-labels
          ((check-free ()
             (let ((writer (sqlite-open path)))
               (unwind-protect
                   (progn
                     (sqlite-execute writer "PRAGMA busy_timeout=0")
                     (sqlite-execute writer "BEGIN IMMEDIATE")
                     (cl-incf prepared)
                     (sqlite-rollback writer))
                 (sqlite-close writer)))))
        (cl-letf (((symbol-function 'org-glance-cache--prepare-family)
                   (lambda (&rest args) (check-free) (apply family args)))
                  ((symbol-function 'org-glance-cache--prepare-headline)
                   (lambda (&rest args) (check-free) (apply headline args)))
                  ((symbol-function 'org-glance-cache--with-staged-generation)
                   (lambda (&rest _) (ert-fail "CRUD cloned the database"))))
          (org-glance-graph:add graph
                                (org-glance-test:headline "incremental" "* Incremental"))))
      (should (= prepared 2))
      (should (equal "Incremental"
                     (org-glance-headline-metadata:title
                      (org-glance-cache:headline graph "incremental")))))))

(provide 'test-cache)
;;; test-cache.el ends here
