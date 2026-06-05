;;; test-graph.el --- Tests for the v2 graph store  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:graph-add-get ()
  "A headline added to the graph is retrievable by id with its fields intact."
  (org-glance-test:with-graph graph
    (let ((headline (org-glance-test:headline "id1" "* TODO foo :a:b:")))
      (org-glance-graph-v2:add graph headline)
      (let ((meta (org-glance-graph-v2:get-headline graph "id1")))
        (should (org-glance-headline-metadata-v2? meta))
        (should (string= "id1"  (org-glance-headline-metadata-v2:id meta)))
        (should (string= "TODO" (org-glance-headline-metadata-v2:state meta)))
        (should (string= "foo"  (org-glance-headline-metadata-v2:title meta)))
        (should (string= (org-glance-headline-v2:hash headline)
                         (org-glance-headline-metadata-v2:hash meta)))
        (should (equal ["a" "b"] (org-glance-headline-metadata-v2:tags meta)))))))

(ert-deftest org-glance-test:graph-get-missing ()
  "Unknown ids return nil."
  (org-glance-test:with-graph graph
    (should (null (org-glance-graph-v2:get-headline graph "nope")))))

(ert-deftest org-glance-test:graph-add-returns-graph ()
  "`add' is chainable: it returns the graph."
  (org-glance-test:with-graph graph
    (should (eq graph (org-glance-graph-v2:add graph (org-glance-test:headline "id1" "* foo"))))))

(ert-deftest org-glance-test:graph-multiple-headlines ()
  "Several headlines coexist and are independently retrievable."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "a" "* TODO alpha")
                             (org-glance-test:headline "b" "* DONE beta"))
    (should (string= "alpha" (org-glance-headline-metadata-v2:title (org-glance-graph-v2:get-headline graph "a"))))
    (should (string= "beta"  (org-glance-headline-metadata-v2:title (org-glance-graph-v2:get-headline graph "b"))))))

(ert-deftest org-glance-test:graph-latest-wins ()
  "Re-adding an id appends a record; the most recent one wins (reverse scan)."
  (org-glance-test:with-graph graph
    (let ((headline (org-glance-test:headline "id1" "* TODO foo")))
      (org-glance-graph-v2:add graph headline)
      (org-glance-graph-v2:add graph (org-glance-headline-v2--copy headline :state "DONE"))
      (should (string= "DONE" (org-glance-headline-metadata-v2:state (org-glance-graph-v2:get-headline graph "id1")))))))

(ert-deftest org-glance-test:graph-delete-tombstone ()
  "Deleting an id makes `get-headline' report a tombstone."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (should (org-glance-headline-metadata-v2? (org-glance-graph-v2:get-headline graph "id1")))
    (org-glance-graph-v2:delete graph "id1")
    (should (eq 'tombstone (org-glance-graph-v2:get-headline graph "id1")))))

(ert-deftest org-glance-test:graph-delete-idempotent ()
  "Deleting an absent or already-deleted id is a no-op (no extra record)."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:delete graph "ghost")
    (should (null (org-glance-graph-v2:get-headline graph "ghost")))
    (org-glance-graph-v2:add graph (org-glance-test:headline "id1" "* foo"))
    (org-glance-graph-v2:delete graph "id1")
    (org-glance-graph-v2:delete graph "id1")
    (should (eq 'tombstone (org-glance-graph-v2:get-headline graph "id1")))))

(ert-deftest org-glance-test:graph-utf8-roundtrip ()
  "Non-ASCII titles survive both read paths.
Regression: `org-glance-jsonl:iterate' (used by `get-headline') read the JSONL
with `insert-file-contents-literally' and fed undecoded UTF-8 bytes to
`json-parse-string', which raised `json-utf8-decode-error'."
  (org-glance-test:with-graph graph
    (let ((title "Façade — Facebook’s “data” café"))
      (org-glance-graph-v2:add graph (org-glance-test:headline "u1" (concat "* TODO " title)))
      ;; reverse chunked literal reader (the previously-broken path)
      (should (string= title (org-glance-headline-metadata-v2:title
                              (org-glance-graph-v2:get-headline graph "u1"))))
      ;; forward reader
      (should (string= title (org-glance-headline-metadata-v2:title
                              (car (org-glance-graph-v2:headlines graph)))))
      ;; full content reconstruct
      (should (string= title (org-glance-headline-v2:title
                              (org-glance-graph-v2:headline graph "u1")))))))

(ert-deftest org-glance-test:graph-utf8-chunk-boundary ()
  "Multibyte content split across the 4096-byte read-chunk boundary still reads
back correctly (carry logic + per-line decode in `org-glance-jsonl:iterate')."
  (org-glance-test:with-graph graph
    (let ((title "café—’“”—naïve—Façade"))
      (dotimes (i 50)
        (org-glance-graph-v2:add graph
                                 (org-glance-test:headline (format "k%d" i)
                                                           (format "* TODO %s %d" title i))))
      ;; the jsonl now spans several 4096-byte chunks; every id must resolve and
      ;; carry its multibyte title intact regardless of where boundaries fall
      (dotimes (i 50)
        (let ((meta (org-glance-graph-v2:get-headline graph (format "k%d" i))))
          (should (org-glance-headline-metadata-v2? meta))
          (should (string= (format "%s %d" title i)
                           (org-glance-headline-metadata-v2:title meta))))))))

(ert-deftest org-glance-test:graph-scheduled-roundtrip ()
  "A scheduled headline serializes (regression: schedule/deadline must be coerced
to raw strings, not org-element timestamp objects, or `json-serialize' crashes)."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "sid"
                                                             "* TODO sched"
                                                             "SCHEDULED: <2025-01-10 Fri>"))
    (let ((meta (org-glance-graph-v2:get-headline graph "sid")))
      (should (org-glance-headline-metadata-v2? meta))
      (should (string= "<2025-01-10 Fri>" (org-glance-headline-metadata-v2:schedule meta))))))

(ert-deftest org-glance-test:graph-capture-assigns-unique-ids ()
  "Capturing assigns a fresh, unique-per-namespace id to each id-less headline."
  (org-glance-test:with-graph graph
    (with-temp-buffer
      (org-mode)
      (insert "* TODO foo :a:\n* TODO bar :b:\n")
      (org-glance-graph-v2:capture graph (current-buffer)))
    (let* ((headlines (org-glance-graph-v2:headlines graph))
           (ids (mapcar #'org-glance-headline-metadata-v2:id headlines)))
      (should (= 2 (length headlines)))
      (should (-none? #'null ids))
      (should (= 2 (length (-uniq ids)))))))

(ert-deftest org-glance-test:graph-capture-preserves-existing-id ()
  "Capturing keeps an already-present ORG_GLANCE_ID."
  (org-glance-test:with-graph graph
    (with-temp-buffer
      (org-mode)
      (insert "* TODO foo\n:PROPERTIES:\n:ORG_GLANCE_ID: keep-me\n:END:\n")
      (org-glance-graph-v2:capture graph (current-buffer)))
    (should (org-glance-headline-metadata-v2? (org-glance-graph-v2:get-headline graph "keep-me")))))

(ert-deftest org-glance-test:graph-headlines-skips-tombstones ()
  "`headlines' returns live records only, newest per id."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "a" "* foo")
                             (org-glance-test:headline "b" "* bar"))
    (org-glance-graph-v2:delete graph "a")
    (should (equal '("b") (mapcar #'org-glance-headline-metadata-v2:id
                                  (org-glance-graph-v2:headlines graph))))))

(ert-deftest org-glance-test:graph-content-roundtrip ()
  "Headline contents persist to the data store and reconstruct fully."
  (org-glance-test:with-graph graph
    (let ((headline (org-glance-test:headline "rt1" "* TODO foo" "body line")))
      (org-glance-graph-v2:add graph headline)
      (should (s-contains? "body line" (org-glance-graph-v2:get-content graph "rt1")))
      (let ((restored (org-glance-graph-v2:headline graph "rt1")))
        (should (org-glance-headline-v2? restored))
        (should (string= "rt1" (org-glance-headline-v2:id restored)))
        (should (string= (org-glance-headline-v2:hash headline)
                         (org-glance-headline-v2:hash restored)))))))

(ert-deftest org-glance-test:graph-unsafe-id-rejected ()
  "Path-unsafe ids are rejected before touching the filesystem."
  (org-glance-test:with-graph graph
    (should-error (org-glance-graph-v2:headline-data-path graph "../escape") :type 'error)
    (should-error (org-glance-graph-v2:headline-data-path graph "a/b") :type 'error)
    ;; a normal tag-hash id is fine
    (should (org-glance-graph-v2:headline-data-path graph "whitepaper-d41d8cd98f00b204"))))

(ert-deftest org-glance-test:graph-flags-roundtrip ()
  "linked?/propertized? projection flags are computed at add time and round-trip."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "f1" "* foo" "[[https://x.example][x]]" "- k: v")
                             (org-glance-test:headline "f2" "* bar" "plain text, no link"))
    (let ((m1 (org-glance-graph-v2:get-headline graph "f1"))
          (m2 (org-glance-graph-v2:get-headline graph "f2")))
      (should (org-glance-headline-metadata-v2:linked? m1))
      (should (org-glance-headline-metadata-v2:propertized? m1))
      (should (not (org-glance-headline-metadata-v2:linked? m2)))
      (should (not (org-glance-headline-metadata-v2:propertized? m2))))))

(ert-deftest org-glance-test:graph-reindex-populates-flags ()
  "Re-index backfills projection flags onto records written without them."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "r1" "* foo" "[[https://x.example][x]]"))
    ;; simulate an old record lacking the linked flag (latest wins)
    (org-glance-graph-v2:insert graph (list (list :id "r1" :state "" :title "foo")))
    (should (not (org-glance-headline-metadata-v2:linked? (org-glance-graph-v2:get-headline graph "r1"))))
    (org-glance-graph-v2:reindex graph)
    (should (org-glance-headline-metadata-v2:linked? (org-glance-graph-v2:get-headline graph "r1")))))

(ert-deftest org-glance-test:org-mode-forces-tab-width-8 ()
  "Parsing setup forces tab-width 8 (org requires it) and disables tabs, even
when the user's default tab-width is 4 -- and metadata still computes.
Regression for `org-glance-headline-v2:metadata' failing with \"Tab width in Org
files must be 8\" in Emacsen whose default tab-width is not 8."
  (let ((orig (default-value 'tab-width)))
    (unwind-protect
        (progn
          (setq-default tab-width 4)
          (with-temp-buffer
            (org-glance--org-mode)
            (should (= tab-width 8))
            (should-not indent-tabs-mode))
          (let ((h (org-glance-headline-v2--from-lines
                    "* TODO foo" ":PROPERTIES:" ":ORG_GLANCE_ID: t1" ":END:" "body")))
            (should (org-glance-headline-metadata-v2? (org-glance-headline-v2:metadata h)))))
      (setq-default tab-width orig))))

(ert-deftest org-glance-test:graph-content-missing ()
  "Reading content for an unknown id yields nil, not an error."
  (org-glance-test:with-graph graph
    (should (null (org-glance-graph-v2:get-content graph "nope")))
    (should (null (org-glance-graph-v2:headline graph "nope")))))

(ert-deftest org-glance-test:graph-headline-tombstoned ()
  "`headline' returns nil for a deleted id even though its blob remains."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "id1" "* foo" "body"))
    (should (org-glance-headline-v2? (org-glance-graph-v2:headline graph "id1")))
    (org-glance-graph-v2:delete graph "id1")
    (should (null (org-glance-graph-v2:headline graph "id1")))
    ;; the low-level blob is still present (append-only store)
    (should (s-contains? "body" (org-glance-graph-v2:get-content graph "id1")))))

(provide 'test-graph)
;;; test-graph.el ends here
