;;; test-cache.el --- Tests for the shared SQLite projection  -*- lexical-binding: t -*-

(require 'ert)
(require 'org-glance-cache)

(ert-deftest org-glance-test:shared-cache-projects-distinct-digests ()
  "The shared row keeps raw-file and org-glance content hashes distinct."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* TODO Alpha :work:"
                                "[[org-glance-material:b?kind=blocks][Beta]]"))
    (org-glance-graph:add graph (org-glance-test:headline "b" "* Beta"))
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (progn
            (should (equal '((2)) (sqlite-select db "PRAGMA user_version")))
            (should (equal '(("glance_payload"))
                           (sqlite-select
                            db
                            "SELECT name FROM sqlite_master WHERE type='table' AND name='glance_payload'")))
            (pcase-let ((`((,digest ,content-hash ,producer))
                         (sqlite-select
                          db
                          "SELECT digest,content_hash,producer FROM org_headline WHERE id='a'")))
              (should (= 64 (length digest)))
              (should (= 40 (length content-hash)))
              (should-not (equal digest content-hash))
              (should (equal "org-glance" producer)))
            (should (equal '(("a" "b" "blocks" "row"))
                           (sqlite-select
                            db
                            "SELECT src,dst,kind,via FROM edge"))))
        (sqlite-close db)))
    (let ((metadata (org-glance-cache:metadata graph "a")))
      (should (org-glance-headline-metadata? metadata))
      (should (equal "Alpha" (org-glance-headline-metadata:title metadata))))
    (delete-directory (org-glance-cache--portable-path graph) t)
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (sqlite-execute
           db "UPDATE org_headline SET digest='stale' WHERE id='a'")
        (sqlite-close db)))
    (should-not (org-glance-cache:metadata graph "a"))
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (progn
            (sqlite-execute
             db
             "UPDATE org_headline SET digest=(SELECT digest FROM headline WHERE id='a') WHERE id='a'")
            (sqlite-execute
             db "UPDATE org_headline SET content_hash='stale' WHERE id='a'"))
        (sqlite-close db)))
    (should-not (org-glance-cache:metadata graph "a"))))

(ert-deftest org-glance-test:portable-cache-cold-clone-and-conflict ()
  "An exact portable row survives a local-cache loss; disagreement poisons it."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO Alpha :work:"))
    (let* ((dir (org-glance-cache--portable-path graph))
           (files (directory-files dir t "\\.jsonl\\'"))
           (record (json-parse-string (f-read-text (car files) 'utf-8)
                                      :object-type 'plist))
           (cache (org-glance-cache:path graph)))
      (should (= 1 (length files)))
      (should (equal "org-glance" (plist-get record :producer)))
      (should (equal 1 (plist-get record :parser)))
      (should (equal ".org-glance/data/a/data.org"
                     (plist-get record :path)))
      (dolist (suffix '("" "-shm" "-wal"))
        (ignore-errors (delete-file (concat cache suffix))))
      (should (equal "Alpha"
                     (org-glance-headline-metadata:title
                      (org-glance-cache:metadata graph "a"))))
      (setf (plist-get record :payload) "{}")
      (org-glance--atomic-write
       (f-join dir "conflict.jsonl")
       (concat (json-serialize record) "\n"))
      (dolist (suffix '("" "-shm" "-wal"))
        (ignore-errors (delete-file (concat cache suffix))))
      (should-not (org-glance-cache:metadata graph "a")))))

(ert-deftest org-glance-test:portable-cache-packs-and-retires-loose-files ()
  "A full refresh replaces loose projections with one packed current segment."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (org-glance-graph:add graph (org-glance-test:headline "b" "* Beta"))
    (let* ((dir (org-glance-cache--portable-path graph))
           (legacy (f-join dir "legacy.jsonl")))
      (org-glance--atomic-write
       legacy
       (f-read-text (car (directory-files dir t "\\.jsonl\\'")) 'utf-8)
       nil)
      (org-glance-cache:refresh graph)
      (let ((files (directory-files dir t "\\.jsonl\\'")))
        (should (= 1 (length files)))
        (should (string-prefix-p "seg-" (file-name-nondirectory (car files))))
        (should (= 2 (length (split-string
                              (f-read-text (car files) 'utf-8) "\n" t))))
        (should-not (file-exists-p legacy))
        (dotimes (index 256)
          (org-glance--atomic-write
           (f-join dir (format "loose-%03d.jsonl" index))
           (f-read-text (car files) 'utf-8)
           nil))
        (org-glance-graph:add graph (org-glance-test:headline "c" "* Gamma"))
        (setq files (directory-files dir t "\\.jsonl\\'"))
        (should (= 1 (length files)))
        (should (string-prefix-p "seg-" (file-name-nondirectory (car files))))
        (should (= 3 (length (split-string
                              (f-read-text (car files) 'utf-8) "\n" t))))))))

(ert-deftest org-glance-test:shared-cache-follows-delete ()
  "Deleting a graph headline removes its shared source and incoming edges."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (org-glance-graph:add
     graph (org-glance-test:headline
            "b" "* Beta" "[[org-glance-material:a][Alpha]]"))
    (org-glance-graph:delete graph "a")
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (progn
            (should-not (sqlite-select db "SELECT id FROM headline WHERE id='a'"))
            (should-not (sqlite-select db "SELECT * FROM edge WHERE dst='a'")))
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
                         (sqlite-select db "SELECT id FROM headline")))
        (sqlite-close db)))
    (org-glance-test:reopen graph)
    (let ((db (sqlite-open (org-glance-cache:path graph))))
      (unwind-protect
          (should-not (sqlite-select db "SELECT id FROM headline"))
        (sqlite-close db)))))

(provide 'test-cache)
;;; test-cache.el ends here
