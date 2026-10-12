;;; test-property-index.el --- Tests for the derived property index  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:property-index-keys-and-values ()
  "The index serves drawer and body properties; key union omits ORG_GLANCE_*."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline-props "a" "* TODO A" '(("AUTHOR" . "Tolkien") ("GENRE" . "fantasy")))
      (org-glance-test:headline-props "b" "* TODO B" '(("AUTHOR" . "Le Guin")) "- rating: 5"))
    (should (equal '("AUTHOR" "GENRE") (org-glance-property-index:keys graph '("a" "b"))))
    (should (equal "Tolkien" (org-glance-property-index:property graph "a" "author")))
    (should (equal "Le Guin" (org-glance-property-index:property graph "b" "AUTHOR")))
    (should (null (org-glance-property-index:property graph "b" "GENRE")))
    (should (equal "5" (alist-get "rating" (org-glance-property-index:body graph "b")
                                  nil nil #'string=)))))

(ert-deftest org-glance-test:property-index-hash-invalidation ()
  "A content change refreshes the indexed value (keyed by content hash)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline-props "a" "* TODO A" '(("AUTHOR" . "Old"))))
    (should (equal "Old" (org-glance-property-index:property graph "a" "AUTHOR")))
    (org-glance-graph:add graph            ; new content -> new hash
      (org-glance-test:headline-props "a" "* TODO A" '(("AUTHOR" . "New"))))
    (should (equal "New" (org-glance-property-index:property graph "a" "AUTHOR")))))

(ert-deftest org-glance-test:property-index-persists-and-clears ()
  "`ensure' persists to disk, surviving a dropped memo; `clear' drops both."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline-props "a" "* TODO A" '(("AUTHOR" . "X"))))
    (org-glance-property-index:ensure graph '("a"))
    (should (f-exists? (org-glance-property-index--file graph)))
    (remhash (org-glance-graph:store-path graph) org-glance-property-index--cache)  ; cold memo
    (should (equal "X" (org-glance-property-index:property graph "a" "AUTHOR")))     ; from disk
    (org-glance-property-index:clear graph)
    (should-not (f-exists? (org-glance-property-index--file graph)))))

(ert-deftest org-glance-test:table-add-column-completing ()
  "`C-c +' offers the visible headlines' drawer properties, read via the index."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline-props "a" "* TODO A" '(("AUTHOR" . "Tolkien"))))
    (org-glance-test:with-table (graph)
        (should (eq table-view-add-column-function #'org-glance-table--add-column-prompt))
        (org-glance-test:offering (offered (caar offered))
          (let ((col (org-glance-table--add-column-prompt))
                (names (mapcar #'car offered)))             ; display strings
            (should (member "AUTHOR" names))                ; discovered from the headline
            (should-not (member "ORG_GLANCE_ID" names))
            (should (equal "AUTHOR" (alist-get 'key col)))
            (should (equal "Tolkien" (funcall (alist-get 'value-fn col) "a" nil))))))))

(ert-deftest org-glance-test:table-property-column-uses-the-row-cache-only ()
  "A property cell must not reopen History after its Row has been selected."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline-props
       "a" "* TODO A :book:" '(("AUTHOR" . "Tolkien"))))
    (let* ((metadata (org-glance-cache:headline graph "a"))
           (row (org-glance-table--row metadata))
           (column (org-glance-table--property-column graph "AUTHOR")))
      ;; Prime the derived sidecar as a normal publication/write path would.
      (should (equal "Tolkien"
                     (org-glance-property-index:property graph "a" "AUTHOR")))
      (cl-letf (((symbol-function 'org-glance-graph:live-meta)
                 (lambda (&rest _)
                   (ert-fail "property cell crossed the committed-cache boundary")))
                ((symbol-function 'org-glance-graph:headline)
                 (lambda (&rest _)
                   (ert-fail "property cell opened History"))))
        (should (equal "Tolkien"
                       (funcall (alist-get 'value-fn column) "a" row)))))))

(ert-deftest org-glance-test:table-extract-via-index ()
  "The table `e' action extracts a body KEY:value read from the property index."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* TODO A" "- author: Tolkien"))
    (let (killed)
      (org-glance-test:offering (offered (caar offered))
        (cl-letf (((symbol-function 'kill-new) (lambda (v &rest _) (setq killed v))))
          (org-glance-table--act-extract graph "a")))
      (should (equal "Tolkien" killed)))))

(ert-deftest org-glance-test:cache-dir-split ()
  "Derived sidecars live in git-ignored `cache/', user config in `config/'.
Reopen prunes a legacy `config/' sidecar; reindex drops `cache/'."
  (org-glance-test:with-graph graph
    (should (s-contains? "/cache/" (org-glance-graph:cache-file graph "x.eld")))
    (should (s-contains? "/config/" (org-glance-graph:config-file graph "x.eld")))
    (should (s-contains? "/cache/" (org-glance-property-index--file graph)))
    (should (f-exists? (f-join (org-glance-graph:store-path graph) ".gitignore")))
    (let ((legacy (org-glance-graph:config-file graph "property-index.eld")))
      (org-glance-test:write legacy "()")
      (org-glance-test:reopen graph)
      (should-not (f-exists? legacy)))
    (f-mkdir-full-path (org-glance-graph:cache-path graph))
    (f-write-text "nil" 'utf-8 (org-glance-graph:cache-file graph "llm-sessions.eld"))
    (org-glance--reindex-batch (org-glance-graph:directory graph))
    (should-not (f-exists? (org-glance-graph:cache-path graph)))))

(ert-deftest org-glance-test:reindex-queues-one-batch-process ()
  "Reindex returns immediately and shares one worker per graph."
  (org-glance-test:with-graph graph
    (let ((org-glance--reindex-processes (make-hash-table :test #'equal))
          process-args)
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest args)
                   (setq process-args args)
                   'queued-process))
                ((symbol-function 'processp)
                 (lambda (process) (eq process 'queued-process)))
                ((symbol-function 'process-live-p)
                 (lambda (process) (eq process 'queued-process)))
                ((symbol-function 'org-glance--reindex-now)
                 (lambda (&rest _)
                   (ert-fail "reindex occupied the caller"))))
        (should (eq 'queued-process
                    (org-glance-reindex
                     (org-glance-graph:directory graph))))
        (should (eq 'queued-process
                    (org-glance-reindex
                     (org-glance-graph:directory graph))))
        (should (equal "org-glance-reindex"
                       (plist-get process-args :name)))
        (should (member "--batch" (plist-get process-args :command)))
        (should (member "--quick" (plist-get process-args :command)))
        (should (cl-find-if
                 (lambda (arg)
                   (s-contains? (org-glance-graph:directory graph) arg))
                 (plist-get process-args :command)))))))

(ert-deftest org-glance-test:reindex-batch-disables-open-workers ()
  "The reindex worker opens one graph without launching nested workers."
  (with-temp-directory dir
    (let ((opens 0))
      (cl-letf (((symbol-function 'org-glance--reindex-now)
                 (lambda (directory)
                   (should (equal dir directory))
                   (should-not org-glance-graph-after-open-functions)
                   (cl-incf opens)
                   7)))
        (should (= 7 (org-glance--reindex-batch dir)))
        (should (= 1 opens))))))

(ert-deftest org-glance-test:reindex-worker-runs-in-separate-emacs ()
  "The real worker backfills metadata and exits successfully."
  (org-glance-test:with-graph graph
    (org-glance-graph:add
        graph
      (org-glance-test:headline
       "worker-reindex" "* Worker reindex" "[[https://example.test]]"))
    (org-glance-graph:insert
        graph
      (list (list :id "worker-reindex" :state "" :title "Worker reindex")))
    (should-not (org-glance-test:field graph "worker-reindex" linked?))
    (let* ((buffer (generate-new-buffer " *org-glance-reindex-test*"))
           (process
            (make-process
             :name "org-glance-reindex-test"
             :buffer buffer
             :command
             (org-glance--batch-worker-command
              'org-glance--reindex-batch
              (list (org-glance-graph:directory graph)))
             :connection-type 'pipe
             :coding 'utf-8-unix
             :noquery t)))
      (unwind-protect
          (progn
            (while (process-live-p process)
              (accept-process-output process 0.1))
            (should (= 0 (process-exit-status process)))
            (should (= 1 (org-glance--reindex-count process)))
            (should (org-glance-test:field graph "worker-reindex" linked?)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(provide 'test-property-index)
;;; test-property-index.el ends here
