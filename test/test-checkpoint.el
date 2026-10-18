;;; test-checkpoint.el --- Portable checkpoint tests  -*- lexical-binding: t; -*-

(require 'org-glance-checkpoint)

(cl-defun org-glance-test:delete-shared-cache (graph)
  "Delete GRAPH's machine-local shared projection files."
  (let ((path (org-glance-cache:path graph)))
    (dolist (held (list path (concat path "-shm") (concat path "-wal")))
      (when (file-exists-p held) (delete-file held)))))

(ert-deftest org-glance-test:checkpoint-seal-and-exact-hydrate-roundtrip ()
  "A sealed projection reconstructs the exact searchable result."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "portable" "* TODO Portable :work:"))
    (org-glance-cache:refresh graph)
    (let* ((before (org-glance-cache:authority graph))
           (sealed (org-glance-checkpoint:seal graph "test-writer"))
           (descriptor (plist-get sealed :descriptor))
           (object (plist-get sealed :object)))
      (should (file-exists-p descriptor))
      (should (file-exists-p object))
      (should (string-match-p
               "/checkpoints/descriptors/[0-9a-f][0-9a-f]/[0-9a-f]\\{64\\}\\.json\\'"
               descriptor))
      (should (string-match-p
               "/checkpoints/objects/[0-9a-f][0-9a-f]/[0-9a-f]\\{64\\}\\.pack\\.gz\\'"
               object))
      (let* ((body (org-glance-checkpoint--decode
                    (org-glance-checkpoint--read-bytes descriptor)))
             (artifact (plist-get body :artifact)))
        (should (equal "gzip" (plist-get artifact :codec)))
        (should (< (plist-get artifact :bytes)
                   (plist-get artifact :logicalBytes))))
      (org-glance-test:delete-shared-cache graph)
      (should (eq 'exact (plist-get (org-glance-checkpoint:hydrate graph) :kind)))
      (should (equal before (org-glance-cache:authority graph))))))

(ert-deftest org-glance-test:checkpoint-refusal-preserves-live-cache ()
  "Corrupt checkpoint bytes never replace the current projection."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "preserved" "* Preserved"))
    (org-glance-cache:refresh graph)
    (let* ((before (org-glance-cache:authority graph))
           (sealed (org-glance-checkpoint:seal graph "test-writer"))
           (object (plist-get sealed :object)))
      (with-temp-buffer
        (insert "corrupt")
        (write-region nil nil object nil 'silent))
      (should-error (org-glance-checkpoint:hydrate graph) :type 'error)
      (should (equal before (org-glance-cache:authority graph)))
      (should-not
       (directory-files (org-glance-graph:cache-path graph) nil
                        "\\`.checkpoint-stage-")))))

(ert-deftest org-glance-test:checkpoint-row-count-is-checked-before-publication ()
  "An invalid artifact row count leaves the complete live projection intact."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "count" "* Preserved"))
    (let* ((before (org-glance-cache:authority graph))
           (artifact (org-glance-checkpoint--artifact graph)))
      (should-error
       (org-glance-checkpoint--hydrate-staged
        graph (org-glance-checkpoint--decode (plist-get artifact :bytes))
        (1+ (plist-get artifact :rows)))
       :type 'error)
      (should (equal before (org-glance-cache:authority graph))))))

(ert-deftest org-glance-test:checkpoint-hydration-does-not-touch-glance-cache ()
  "A held Glance writer stays independent of org-glance checkpoint hydration."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "owned" "* Owned"))
    (org-glance-cache:refresh graph)
    (org-glance-checkpoint:seal graph "test-writer")
    (let ((peer (sqlite-open (org-glance-graph:cache-file
                             graph "glance-index.sqlite3"))))
      (unwind-protect
          (progn
            (sqlite-execute peer "CREATE TABLE sentinel(value TEXT)")
            (sqlite-execute peer "INSERT INTO sentinel VALUES('Glance generation')")
            (sqlite-execute peer "BEGIN IMMEDIATE")
            (sqlite-execute peer "UPDATE sentinel SET value='uncommitted Glance generation'")
            (org-glance-checkpoint:hydrate graph)
            (should (equal '(("uncommitted Glance generation"))
                           (sqlite-select peer "SELECT value FROM sentinel")))
            (should (equal "Owned" (org-glance-headline-metadata:title
                                    (org-glance-cache:headline graph "owned")))))
        (sqlite-rollback peer)
        (sqlite-close peer)))))

(ert-deftest org-glance-test:checkpoint-refuses-a-commit-during-artifact-hydration ()
  "A peer commit while decoding the stage survives the rejected publication."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "race" "* Before"))
    (org-glance-cache:refresh graph)
    (org-glance-checkpoint:seal graph "test-writer")
    (let ((writer (sqlite-open (org-glance-cache:path graph)))
          (hydrate (symbol-function 'org-glance-checkpoint--hydrate-artifact)))
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'org-glance-checkpoint--hydrate-artifact)
                       (lambda (&rest args)
                         (sqlite-execute writer "UPDATE headline SET title='Peer commit'")
                         (apply hydrate args))))
              (should-error (org-glance-checkpoint:hydrate graph) :type 'error))
            (should (equal '(("Peer commit"))
                           (sqlite-select writer "SELECT title FROM headline"))))
        (sqlite-close writer)))))

(ert-deftest org-glance-test:checkpoint-refuses-loss-of-admitted-events ()
  "An older self-consistent artifact cannot erase a newer admitted family."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "old" "* Older"))
    (let* ((artifact (org-glance-checkpoint--artifact graph))
           (decoded (org-glance-checkpoint--decode (plist-get artifact :bytes))))
      (org-glance-graph:add graph (org-glance-test:headline "new" "* Newer"))
      (let ((before (org-glance-cache:authority graph)))
        (should-error (org-glance-checkpoint--hydrate-staged
                       graph decoded (plist-get artifact :rows)) :type 'error)
        (should (equal before (org-glance-cache:authority graph)))))))

(ert-deftest org-glance-test:cold-open-hydrates-before-background-refresh ()
  "A missing local cache is usable from an exact checkpoint during open."
  (with-temp-directory dir
    (let* ((graph (org-glance-graph dir))
           (directory (org-glance-graph:directory graph)))
      (org-glance-graph:add graph
        (org-glance-test:headline "startup" "* TODO Startup"))
      (org-glance-cache:refresh graph)
      (let ((before (org-glance-cache:authority graph))
            (queued 0))
        (org-glance-checkpoint:seal graph "test-writer")
        (org-glance-test:delete-shared-cache graph)
        (remhash directory org-glance-graph:list)
        (let ((org-glance-cache--refresh-synchronously nil))
          (cl-letf (((symbol-function 'org-glance-cache:refresh-async)
                     (lambda (&rest _) (cl-incf queued))))
            (setq graph (org-glance-graph dir))))
        (should (= 1 queued))
        (should (equal before (org-glance-cache:authority graph)))))))

(ert-deftest org-glance-test:checkpoint-hydrates-an-append-only-family-tail ()
  "A sealed base plus immutable event tail reaches the current projection."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "tail" "* TODO Before"))
    (org-glance-cache:refresh graph)
    (org-glance-checkpoint:seal graph "test-writer")
    (org-glance-graph:add graph
      (org-glance-test:headline "tail" "* DONE After"))
    (let ((expected (org-glance-cache:authority graph)))
      (org-glance-test:delete-shared-cache graph)
      (should (eq 'tail (plist-get (org-glance-checkpoint:hydrate graph) :kind)))
      (should (equal expected (org-glance-cache:authority graph))))))

(ert-deftest org-glance-test:ordinary-writes-never-publish-checkpoints ()
  "Only the explicit seal command writes the tracked checkpoint set."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "quiet" "* TODO Quiet"))
    (org-glance-cache:refresh graph)
    (let* ((sealed (org-glance-checkpoint:seal graph "test-writer"))
           (descriptor (plist-get sealed :descriptor))
           (object (plist-get sealed :object))
           (descriptor-bytes (org-glance-checkpoint--read-bytes descriptor))
           (object-bytes (org-glance-checkpoint--read-bytes object)))
      (org-glance-graph:add graph
        (org-glance-test:headline "quiet" "* DONE Changed"))
      (should (equal descriptor-bytes
                     (org-glance-checkpoint--read-bytes descriptor)))
      (should (equal object-bytes
                     (org-glance-checkpoint--read-bytes object)))
      (should (= 1 (length (org-glance-checkpoint--descriptors graph)))))))

(ert-deftest org-glance-test:checkpoint-prepublish-failure-preserves-live-cache ()
  "A failure after staged validation leaves the prior cache readable."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "atomic" "* Atomic"))
    (org-glance-cache:refresh graph)
    (org-glance-checkpoint:seal graph "test-writer")
    (let ((before (org-glance-checkpoint--read-bytes
                   (org-glance-cache:path graph)))
          (org-glance-checkpoint--publication-hook
           (lambda () (error "simulated checkpoint publication failure"))))
      (should-error (org-glance-checkpoint:hydrate graph) :type 'error)
      (should (equal before
                     (org-glance-checkpoint--read-bytes
                      (org-glance-cache:path graph))))
      (should-not
       (directory-files (org-glance-graph:cache-path graph) nil
                        "\\`.checkpoint-stage-")))))

(ert-deftest org-glance-test:checkpoint-refuses-changed-effective-configuration ()
  "A configuration change invalidates a sealed projection."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "configured" "* Configured :work:"))
    (org-glance-cache:refresh graph)
    (org-glance-checkpoint:seal graph "test-writer")
    (org-glance-test:write
     (org-glance-graph:config-file graph "system.org")
     "#+TODO: NEXT | DONE\n")
    (should-error (org-glance-checkpoint:hydrate graph) :type 'error)))

;;; test-checkpoint.el ends here
