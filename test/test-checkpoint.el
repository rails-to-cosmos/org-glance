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
