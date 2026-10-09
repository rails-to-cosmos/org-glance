;;; test-version.el --- Immutable version contract tests  -*- lexical-binding: t -*-

(require 'test-helpers)
(require 'org-glance-version)

(ert-deftest org-glance-test:version-snapshot-roundtrip ()
  (with-temp-directory dir
    (let* ((doc "* TODO Alpha\n")
           (version (org-glance-version:write-snapshot dir "alpha" nil "org-glance" doc))
           (read (car (org-glance-version:read dir "alpha"))))
      (should (equal 'snapshot (org-glance-version:kind read)))
      (should (equal (secure-hash 'sha256 doc)
                     (org-glance-version:content-sha256 read)))
      (should (equal ?7 (aref (org-glance-version:id read) 14)))
      (should (equal doc (f-read-text
                          (org-glance-version:data-file
                           dir (org-glance-version:id version)) 'utf-8))))))

(ert-deftest org-glance-test:version-concurrent-leaves ()
  (with-temp-directory dir
    (let* ((root (org-glance-version:write-snapshot dir "alpha" nil "org-glance" "root"))
           (parent (list (org-glance-version:id root)))
           (left (org-glance-version:write-snapshot dir "alpha" parent "org-glance" "left"))
           (right (org-glance-version:write-snapshot dir "alpha" parent "glance" "right")))
      (should (equal (sort (list (org-glance-version:id left)
                                 (org-glance-version:id right)) #'string<)
                     (sort (mapcar #'org-glance-version:id
                                   (org-glance-version:leaves
                                    (org-glance-version:read dir "alpha")))
                           #'string<))))))

(ert-deftest org-glance-test:version-tombstone-has-no-content ()
  (with-temp-directory dir
    (let* ((root (org-glance-version:write-snapshot dir "alpha" nil "org-glance" "root"))
           (gone (org-glance-version:write-tombstone
                  dir "alpha" (list (org-glance-version:id root)) "org-glance")))
      (should (equal 'tombstone (org-glance-version:kind gone)))
      (should-not (f-exists? (org-glance-version:data-file
                              dir (org-glance-version:id gone)))))))

(ert-deftest org-glance-test:version-rejects-content-digest-mismatch ()
  (with-temp-directory dir
    (let ((version (org-glance-version:write-snapshot
                    dir "alpha" nil "org-glance" "original")))
      (f-write-text "changed" 'utf-8
                    (org-glance-version:data-file dir (org-glance-version:id version)))
      (should-not (org-glance-version:read dir "alpha"))
      (let ((history (org-glance-version:history dir "alpha")))
        (should (= 1 (length history)))
        (should-not (org-glance-version:valid (car history)))
        (should (equal (list (org-glance-version:id version))
                       (mapcar #'org-glance-version:id
                               (org-glance-version:current dir "alpha"))))))))

(ert-deftest org-glance-test:damaged-child-does-not-promote-ancestor ()
  (with-temp-directory dir
    (let* ((root (org-glance-version:write-snapshot
                  dir "alpha" nil "org-glance" "root"))
           (child (org-glance-version:write-snapshot
                   dir "alpha" (list (org-glance-version:id root))
                   "glance" "child")))
      (f-write-text "changed" 'utf-8
                    (org-glance-version:data-file dir
                                                  (org-glance-version:id child)))
      (should (equal (list (org-glance-version:id child))
                     (mapcar #'org-glance-version:id
                             (org-glance-version:current dir "alpha")))))))

(ert-deftest org-glance-test:legacy-version-id-is-deterministic-uuid-v5 ()
  (let ((id (org-glance-version:legacy-id "alpha" "digest-a")))
    (should (equal id (org-glance-version:legacy-id "alpha" "digest-a")))
    (should-not (equal id (org-glance-version:legacy-id "alpha" "digest-b")))
    (should-not (equal id (org-glance-version:legacy-id "beta" "digest-a")))
    (should (equal ?5 (aref id 14)))))

(ert-deftest org-glance-test:legacy-data-is-the-implicit-root ()
  (with-temp-directory dir
    (let* ((doc "* Alpha\n")
           (data (f-join dir "data.org")))
      (f-write-text doc 'utf-8 data)
      (let ((root (car (org-glance-version:read dir "alpha"))))
        (should (equal 'snapshot (org-glance-version:kind root)))
        (should-not (org-glance-version:parents root))
        (should (equal (org-glance-version:legacy-id
                        "alpha" (secure-hash 'sha256 doc))
                       (org-glance-version:id root)))
        (should (equal "3d7dd5f9-53b5-5684-afd2-e38b5d09f093"
                       (org-glance-version:id root)))
        (should (equal dir (org-glance-version:directory root)))))))

(ert-deftest org-glance-test:legacy-root-is-an-ancestor ()
  (with-temp-directory dir
    (let* ((doc "* Alpha\n")
           (data (f-join dir "data.org"))
           (root-id (org-glance-version:legacy-id
                     "alpha" (secure-hash 'sha256 doc))))
      (f-write-text doc 'utf-8 data)
      (let ((child (org-glance-version:write-snapshot
                    dir "alpha" (list root-id) "org-glance" "* Revised\n")))
        (should (= 2 (length (org-glance-version:read dir "alpha"))))
        (should (equal (list (org-glance-version:id child))
                       (mapcar #'org-glance-version:id
                               (org-glance-version:leaves
                                (org-glance-version:read dir "alpha")))))))))

(ert-deftest org-glance-test:legacy-migration-publishes-before-removal ()
  (with-temp-directory dir
    (let* ((doc "* Alpha\n")
           (data (f-join dir "data.org")))
      (f-write-text doc 'utf-8 data)
      (let ((migrated (org-glance-version:migrate-legacy dir "alpha")))
        (should-not (f-exists? data))
        (should (f-file? (f-join (org-glance-version:directory migrated) "data.org")))
        (let ((meta (f-join (org-glance-version:directory migrated) "meta.json")))
          (should (f-file? meta))
          (should (equal
                   (concat
                    "{\"version\":1,\"headline\":\"alpha\","
                    "\"id\":\"3d7dd5f9-53b5-5684-afd2-e38b5d09f093\","
                    "\"kind\":\"snapshot\",\"parents\":[],"
                    "\"contentSha256\":\"fbebdc976740079bdaa52a59f81ee64b936ac629ad20a006d77486a1e0578bb5\","
                    "\"created\":\"1970-01-01T00:00:00Z\","
                    "\"producer\":\"migration\"}\n")
                   (f-read-text meta 'utf-8))))
        (let ((stored (car (org-glance-version:read dir "alpha"))))
          (should (equal (org-glance-version:id migrated)
                         (org-glance-version:id stored)))
          (should (equal "migration" (org-glance-version:producer stored))))))))

(ert-deftest org-glance-test:legacy-migration-resumes-a-published-root ()
  (with-temp-directory dir
    (let* ((doc "* Alpha\n")
           (data (f-join dir "data.org")))
      (f-write-text doc 'utf-8 data)
      (let ((first (org-glance-version:migrate-legacy dir "alpha")))
        (f-write-text doc 'utf-8 data)
        (let ((resumed (org-glance-version:migrate-legacy dir "alpha")))
          (should (equal (org-glance-version:directory first)
                         (org-glance-version:directory resumed)))
          (should-not (f-exists? data)))))))

(ert-deftest org-glance-test:graph-writes-immutable-children ()
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "alpha" "* TODO Before"))
    (let* ((dir (org-glance-graph:headline-data-path graph "alpha"))
           (first (car (org-glance-version:leaves
                        (org-glance-version:read dir "alpha"))))
           (before (f-read-text (f-join (org-glance-version:directory first)
                                        "data.org") 'utf-8)))
      (should-not (f-file? (f-join dir "data.org")))
      (org-glance-graph:add graph (org-glance-test:headline "alpha" "* DONE After"))
      (let ((leaves (org-glance-version:leaves
                     (org-glance-version:read dir "alpha"))))
        (should (= 1 (length leaves)))
        (should (equal (list (org-glance-version:id first))
                       (org-glance-version:parents (car leaves))))
        (should (equal before
                       (f-read-text (f-join (org-glance-version:directory first)
                                            "data.org") 'utf-8)))))))

(ert-deftest org-glance-test:graph-refuses-an-ambiguous-version-family ()
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "alpha" "* TODO Root"))
    (let* ((dir (org-glance-graph:headline-data-path graph "alpha"))
           (root (car (org-glance-version:leaves
                       (org-glance-version:read dir "alpha"))))
           (parent (list (org-glance-version:id root))))
      (org-glance-version:write-snapshot dir "alpha" parent "glance" "* TODO Left\n")
      (org-glance-version:write-tombstone dir "alpha" parent "org-glance")
      (should-error (org-glance-graph:content-path graph "alpha") :type 'user-error)
      (should-error
       (org-glance-graph:add graph (org-glance-test:headline "alpha" "* TODO Merge"))
       :type 'user-error)
      (should-error (org-glance-graph:delete graph "alpha") :type 'user-error))))

(ert-deftest org-glance-test:graph-counts-damaged-structural-leaves ()
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "alpha" "* TODO Root"))
    (let* ((dir (org-glance-graph:headline-data-path graph "alpha"))
           (root (car (org-glance-version:current dir "alpha")))
           (parent (list (org-glance-version:id root)))
           (damaged (org-glance-version:write-snapshot
                     dir "alpha" parent "glance" "* TODO Damaged\n")))
      (org-glance-version:write-snapshot
       dir "alpha" parent "org-glance" "* TODO Readable\n")
      (f-write-text "* TODO Edited\n" 'utf-8
                    (org-glance-version:data-file
                     dir (org-glance-version:id damaged)))
      (should-error (org-glance-graph:content-path graph "alpha")
                    :type 'user-error))))

(ert-deftest org-glance-test:graph-retains-ten-version-generations-by-default ()
  (org-glance-test:with-graph graph
    (dotimes (n 12)
      (org-glance-graph:add
       graph (org-glance-test:headline "alpha" (format "* TODO Version %d" n))))
    (let* ((dir (org-glance-graph:headline-data-path graph "alpha"))
           (history (org-glance-version:history dir "alpha"))
           (ids (mapcar #'org-glance-version:id history)))
      (should (= 10 (length history)))
      (should (= 1 (cl-loop for version in history
                            sum (cl-count-if-not
                                 (lambda (parent) (member parent ids))
                                 (org-glance-version:parents version))))))))

(ert-deftest org-glance-test:graph-reads-version-depth-from-system-config ()
  (org-glance-test:with-graph graph
    (let ((system (org-glance-graph:config-file graph "system.org")))
      (f-mkdir-full-path (f-parent system))
      (f-write-text "#+GLANCE_HEADLINE_HISTORY_DEPTH: 3\n" 'utf-8 system))
    (dotimes (n 5)
      (org-glance-graph:add
       graph (org-glance-test:headline "alpha" (format "* TODO Version %d" n))))
    (should (= 3 (length
                  (org-glance-version:history
                   (org-glance-graph:headline-data-path graph "alpha") "alpha"))))
    (f-write-text "#+GLANCE_HEADLINE_HISTORY_DEPTH: 0\n" 'utf-8
                  (org-glance-graph:config-file graph "system.org"))
    (dotimes (n 3)
      (org-glance-graph:add
       graph (org-glance-test:headline "alpha" (format "* TODO Unlimited %d" n))))
    (should (= 6 (length
                  (org-glance-version:history
                   (org-glance-graph:headline-data-path graph "alpha") "alpha"))))))

(ert-deftest org-glance-test:version-retention-skips-conflicts ()
  (with-temp-directory dir
    (let* ((root (org-glance-version:write-snapshot
                  dir "alpha" nil "org-glance" "root"))
           (parent (list (org-glance-version:id root))))
      (org-glance-version:write-snapshot dir "alpha" parent "glance" "left")
      (org-glance-version:write-snapshot dir "alpha" parent "org-glance" "right")
      (should-not (org-glance-version:prune dir "alpha" 1))
      (should (= 3 (length (org-glance-version:history dir "alpha")))))))

;;; test-version.el ends here
