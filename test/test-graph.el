;;; test-graph.el --- Tests for the graph store  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:graph-add-get ()
  "A headline added to the graph is retrievable by id with its fields intact."
  (org-glance-test:with-graph graph
    (let ((headline (org-glance-test:headline "id1" "* TODO foo :a:b:")))
      (org-glance-graph:add graph headline)
      (let ((meta (org-glance-graph:get-headline graph "id1")))
        (should (org-glance-headline-metadata? meta))
        (should (string= "id1"  (org-glance-headline-metadata:id meta)))
        (should (string= "TODO" (org-glance-headline-metadata:state meta)))
        (should (string= "foo"  (org-glance-headline-metadata:title meta)))
        (should (string= (org-glance-headline:hash headline)
                         (org-glance-headline-metadata:hash meta)))
        (should (equal '("a" "b") (org-glance-headline-metadata:tags meta)))))))

(ert-deftest org-glance-test:graph-directory-aliases-share-instance ()
  "Directory spellings with the same truename return one graph instance."
  (with-temp-directory dir
    (should (eq (org-glance-graph dir)
                (org-glance-graph (file-name-as-directory dir))))))

(ert-deftest org-glance-test:graph-get-missing ()
  "Unknown ids return nil."
  (org-glance-test:with-graph graph
    (should (null (org-glance-graph:get-headline graph "nope")))))

(ert-deftest org-glance-test:graph-add-returns-graph ()
  "`add' is chainable: it returns the graph."
  (org-glance-test:with-graph graph
    (should (eq graph (org-glance-graph:add graph (org-glance-test:headline "id1" "* foo"))))))

(ert-deftest org-glance-test:graph-multiple-headlines ()
  "Several headlines coexist and are independently retrievable."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "a" "* TODO alpha")
                             (org-glance-test:headline "b" "* DONE beta"))
    (should (string= "alpha" (org-glance-test:field graph "a" title)))
    (should (string= "beta"  (org-glance-test:field graph "b" title)))))

(ert-deftest org-glance-test:graph-latest-wins ()
  "Re-adding an id appends a record; the last one wins."
  (org-glance-test:with-graph graph
    (let ((headline (org-glance-test:headline "id1" "* TODO foo")))
      (org-glance-graph:add graph headline)
      (org-glance-graph:add graph (org-glance-headline--copy headline :state "DONE"))
      (should (string= "DONE" (org-glance-test:field graph "id1" state))))))

(ert-deftest org-glance-test:graph-delete-idempotent ()
  "Deleting an absent or already-deleted id is a no-op (no extra record)."
  (org-glance-test:with-graph graph
    (org-glance-graph:delete graph "ghost")
    (should (null (org-glance-graph:get-headline graph "ghost")))
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* foo"))
    (org-glance-graph:delete graph "id1")
    (org-glance-graph:delete graph "id1")
    (should (eq 'tombstone (org-glance-graph:get-headline graph "id1")))))

(ert-deftest org-glance-test:graph-utf8-roundtrip ()
  "Non-ASCII titles survive every read path without `json-utf8-decode-error'."
  (org-glance-test:with-graph graph
    (let ((title "Façade — Facebook’s “data” café"))
      (org-glance-graph:add graph (org-glance-test:headline "u1" (concat "* TODO " title)))
      (should (string= title (org-glance-test:field graph "u1" title)))
      (should (string= title (org-glance-headline-metadata:title
                              (car (org-glance-graph:headlines graph)))))
      (should (string= title (org-glance-headline:title
                              (org-glance-graph:headline graph "u1")))))))

(ert-deftest org-glance-test:graph-utf8-chunk-boundary ()
  "Multibyte content spanning a 4096-byte read chunk reads back intact."
  (org-glance-test:with-graph graph
    (let ((title "café—’“”—naïve—Façade"))
      (dotimes (i 50)
        (org-glance-graph:add graph
                                 (org-glance-test:headline (format "k%d" i)
                                                           (format "* TODO %s %d" title i))))
      (dotimes (i 50)
        (let ((meta (org-glance-graph:get-headline graph (format "k%d" i))))
          (should (org-glance-headline-metadata? meta))
          (should (string= (format "%s %d" title i)
                           (org-glance-headline-metadata:title meta))))))))

(ert-deftest org-glance-test:graph-scheduled-roundtrip ()
  "A SCHEDULED timestamp round-trips as a raw string `json-serialize' accepts."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "sid"
                                                             "* TODO sched"
                                                             "SCHEDULED: <2025-01-10 Fri>"))
    (let ((meta (org-glance-graph:get-headline graph "sid")))
      (should (org-glance-headline-metadata? meta))
      (should (string= "<2025-01-10 Fri>" (org-glance-headline-metadata:schedule meta))))))

(ert-deftest org-glance-test:graph-capture-assigns-unique-ids ()
  "Capturing assigns a fresh, unique-per-namespace id to each id-less headline."
  (org-glance-test:with-graph graph
    (org-glance-test:capture graph "* TODO foo :a:\n* TODO bar :b:\n")
    (let ((headlines (org-glance-graph:headlines graph))
          (ids (org-glance-test:ids graph)))
      (should (= 2 (length headlines)))
      (should (-none? #'null ids))
      (should (= 2 (length (-uniq ids)))))))

(ert-deftest org-glance-test:graph-capture-preserves-existing-id ()
  "Capturing keeps an already-present ORG_GLANCE_ID."
  (org-glance-test:with-graph graph
    (org-glance-test:capture graph "* TODO foo\n:PROPERTIES:\n:ORG_GLANCE_ID: keep-me\n:END:\n")
    (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "keep-me")))))

(ert-deftest org-glance-test:graph-capture-stamps-creation-time ()
  "Capture stamps an inactive ORG_GLANCE_CREATION_TIME only when it is absent."
  (org-glance-test:with-graph graph
    (org-glance-test:capture graph
      "* TODO fresh :a:\n* TODO kept :b:\n:PROPERTIES:\n:ORG_GLANCE_CREATION_TIME: [2020-01-01 Wed 09:00]\n:END:\n")
    (let ((ids (org-glance-test:ids graph)))
      (should (= 2 (length ids)))
      (should (string-match-p
               ":ORG_GLANCE_CREATION_TIME: \\[[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9] "
               (org-glance-graph:get-content graph (nth 0 ids))))
      (should (s-contains? "[2020-01-01 Wed 09:00]"
                           (org-glance-graph:get-content graph (nth 1 ids)))))))

(ert-deftest org-glance-test:graph-headlines-skips-tombstones ()
  "`headlines' returns live records only, newest per id."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "a" "* foo")
                             (org-glance-test:headline "b" "* bar"))
    (org-glance-graph:delete graph "a")
    (should (equal '("b") (org-glance-test:ids graph)))))

(ert-deftest org-glance-test:graph-content-roundtrip ()
  "Headline contents persist to the data store and reconstruct fully."
  (org-glance-test:with-graph graph
    (let ((headline (org-glance-test:headline "rt1" "* TODO foo" "body line")))
      (org-glance-graph:add graph headline)
      (should (s-contains? "body line" (org-glance-graph:get-content graph "rt1")))
      (let ((restored (org-glance-graph:headline graph "rt1")))
        (should (org-glance-headline? restored))
        (should (string= "rt1" (org-glance-headline:id restored)))
        (should (string= (org-glance-headline:hash headline)
                         (org-glance-headline:hash restored)))))))

(ert-deftest org-glance-test:graph-content-atomic-write ()
  "`put-content' overwrites the blob atomically, leaving no temp file behind."
  (org-glance-test:with-graph graph
    (org-glance-graph:put-content graph (org-glance-test:headline "aw1" "* foo" "first body"))
    (org-glance-graph:put-content graph (org-glance-test:headline "aw1" "* foo" "second body"))
    (should (s-contains? "second body" (org-glance-graph:get-content graph "aw1")))
    (should-not (s-contains? "first body" (org-glance-graph:get-content graph "aw1")))
    (let ((dir (org-glance-graph:headline-data-path graph "aw1")))
      (should-not (cl-find-if (lambda (f) (s-contains? "data.org.tmp." f))
                              (directory-files dir))))))

(ert-deftest org-glance-test:graph-unsafe-id-rejected ()
  "Path-unsafe ids are rejected before touching the filesystem."
  (org-glance-test:with-graph graph
    (should-error (org-glance-graph:headline-data-path graph "../escape") :type 'error)
    (should-error (org-glance-graph:headline-data-path graph "a/b") :type 'error)
    (should (org-glance-graph:headline-data-path graph "whitepaper-d41d8cd98f00b204"))))

(ert-deftest org-glance-test:graph-flags-roundtrip ()
  "The `linked?'/`propertized?' projection flags compute at add and round-trip."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "f1" "* foo" "[[https://x.example][x]]" "- k: v")
                             (org-glance-test:headline "f2" "* bar" "plain text, no link"))
    (let ((m1 (org-glance-graph:get-headline graph "f1"))
          (m2 (org-glance-graph:get-headline graph "f2")))
      (should (org-glance-headline-metadata:linked? m1))
      (should (org-glance-headline-metadata:propertized? m1))
      (should (not (org-glance-headline-metadata:linked? m2)))
      (should (not (org-glance-headline-metadata:propertized? m2))))))

(ert-deftest org-glance-test:graph-reindex-populates-flags ()
  "Re-index backfills projection flags onto records written without them."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "r1" "* foo" "[[https://x.example][x]]"))
    (org-glance-graph:insert graph (list (list :id "r1" :state "" :title "foo")))
    (should (not (org-glance-test:field graph "r1" linked?)))
    (org-glance-graph:reindex graph)
    (should (org-glance-test:field graph "r1" linked?))))

(ert-deftest org-glance-test:org-mode-forces-tab-width-8 ()
  "`org-glance--org-mode' forces `tab-width' 8, no tabs, under a default of 4.
`org-glance-headline:metadata' then computes (invariant 12)."
  (let ((orig (default-value 'tab-width)))
    (unwind-protect
        (progn
          (setq-default tab-width 4)
          (with-temp-buffer
            (org-glance--org-mode)
            (should (= tab-width 8))
            (should-not indent-tabs-mode))
          (let ((h (org-glance-test:headline "t1" "* TODO foo" "body")))
            (should (org-glance-headline-metadata? (org-glance-headline:metadata h)))))
      (setq-default tab-width orig))))

(ert-deftest org-glance-test:graph-content-missing ()
  "Reading content for an unknown id returns nil without signaling."
  (org-glance-test:with-graph graph
    (should (null (org-glance-graph:get-content graph "nope")))
    (should (null (org-glance-graph:headline graph "nope")))))

(ert-deftest org-glance-test:graph-headline-tombstoned ()
  "`headline' returns nil for a deleted id even though its blob remains."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* foo" "body"))
    (should (org-glance-headline? (org-glance-graph:headline graph "id1")))
    (org-glance-graph:delete graph "id1")
    (should (null (org-glance-graph:headline graph "id1")))
    (should (s-contains? "body" (org-glance-graph:get-content graph "id1")))))

(ert-deftest org-glance-test:graph-states ()
  "`states' returns distinct non-empty todo states, sorted; stateless excluded."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "s1" "* TODO Alpha")
                             (org-glance-test:headline "s2" "* DONE Beta")
                             (org-glance-test:headline "s3" "* TODO Gamma")
                             (org-glance-test:headline "s4" "* Delta"))
    (should (equal '("DONE" "TODO") (org-glance-graph:states graph)))))

;;; In-memory read cache coherence.

(ert-deftest org-glance-test:graph-cache-same-second-add ()
  "An add right after a warmed read is visible even if mtime did not advance."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "c1" "* TODO A"))
    (should (= 1 (length (org-glance-graph:headlines graph)))) ; warms the cache
    (org-glance-graph:add graph (org-glance-test:headline "c2" "* TODO B"))
    (should (= 2 (length (org-glance-graph:headlines graph))))
    (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "c2")))))

(ert-deftest org-glance-test:graph-cache-external-write ()
  "A record appended to the open segment by another writer is observed.
No in-process invalidation fires; the store snapshot detects it (invariant 7)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "e1" "* TODO A"))
    (should (= 1 (length (org-glance-graph:headlines graph)))) ; warms the cache
    (let ((open (org-glance-graph:headline-meta-path graph)))
      (f-append-text (concat (json-serialize (list :id "e2" :state "TODO" :title "B" :seq 9999))
                             "\n")
                     'utf-8 open))
    (should (= 2 (length (org-glance-graph:headlines graph))))
    (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "e2")))))

(ert-deftest org-glance-test:graph-cache-delete-then-tombstone ()
  "A delete after a warmed read shows as a tombstone and leaves the live set."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "d1" "* TODO A"))
    (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "d1"))) ; warm
    (org-glance-graph:delete graph "d1")
    (should (eq 'tombstone (org-glance-graph:get-headline graph "d1")))
    (should (= 0 (length (org-glance-graph:headlines graph))))))

(ert-deftest org-glance-test:graph-cache-compaction-visibility ()
  "A warmed cache sees compaction turn `get-headline' from `tombstone' to nil."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "k1" "* TODO A"))
    (org-glance-graph:delete graph "k1")
    (should (eq 'tombstone (org-glance-graph:get-headline graph "k1"))) ; warm: tombstone
    (org-glance-graph:compact graph)
    (should (null (org-glance-graph:get-headline graph "k1")))))

(ert-deftest org-glance-test:graph-cache-insertion-order-after-update ()
  "First-sighting order survives a cache rebuild after an in-place update."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "o1" "* TODO A")
                             (org-glance-test:headline "o2" "* TODO B")
                             (org-glance-test:headline "o3" "* TODO C"))
    (should (equal '("o1" "o2" "o3") (org-glance-test:ids graph)))
    (org-glance-graph:add graph (org-glance-test:headline "o2" "* DONE B2"))
    (let ((metas (org-glance-graph:headlines graph)))
      (should (equal '("o1" "o2" "o3") (org-glance-test:ids graph)))
      (should (equal "DONE" (org-glance-headline-metadata:state (cadr metas)))))))

(ert-deftest org-glance-test:graph-cache-external-compaction-mtime-independent ()
  "An external compaction is detected by segment names alone (invariant 7).
A separate writer struct bypasses the reader's invalidation, and the open and
MANIFEST mtimes are pinned, so only [seg-01] -> [seg-02] differs."
  (org-glance-test:with-graph reader
    (let* ((writer (make-org-glance-graph :directory (org-glance-graph:directory reader)))
           (open (org-glance-graph:headline-meta-path reader))
           (manifest (org-glance-graph--manifest-path reader))
           (pinned 1500000000))
      (org-glance-graph:add writer
                            (org-glance-test:headline "A" "* TODO A")
                            (org-glance-test:headline "B" "* TODO B")
                            (org-glance-test:headline "C" "* TODO C"))
      (org-glance-graph--seal writer)
      (set-file-times open pinned)
      (set-file-times manifest pinned)
      (should (= 3 (length (org-glance-graph:headlines reader))))
      (should (org-glance-headline-metadata? (org-glance-graph:get-headline reader "A")))
      (org-glance-graph:delete writer "A")
      (org-glance-graph:compact writer)
      (set-file-times open pinned)
      (set-file-times manifest pinned)
      (should (= 2 (length (org-glance-graph:headlines reader))))
      (should (null (org-glance-graph:get-headline reader "A"))))))

(ert-deftest org-glance-test:reindex-leaves-blobs-alone ()
  "Reindex appends metadata records only: blobs are read, never rewritten."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO A :x:" "body"))
    (let ((blob (org-glance-graph:content-path graph "a"))
          (past (encode-time 0 0 0 1 1 2020)))
      (set-file-times blob past)
      (should (= 1 (org-glance-graph:reindex graph)))
      (should (time-equal-p past (file-attribute-modification-time
                                  (file-attributes blob))))
      (should (equal "A" (org-glance-test:field graph "a" title))))))

(cl-defun org-glance-test:tri-state (graph id)
  "Return GRAPH's answer for ID as nil, `tombstone', or the todo state."
  (let ((r (org-glance-graph:get-headline graph id)))
    (cond ((null r) nil)
          ((eq r 'tombstone) 'tombstone)
          (t (org-glance-headline-metadata:state r)))))

(ert-deftest org-glance-test:graph-cache-patched-not-rebuilt ()
  "An append PATCHES the read cache; the next read never re-scans the WAL.
Rebuilds are counted as `org-glance-graph--latest-records' calls."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO A"))
    (should (org-glance-graph:headlines graph))          ; warm the cache
    (let ((rebuilds 0))
      (cl-letf* ((orig (symbol-function 'org-glance-graph--latest-records))
                 ((symbol-function 'org-glance-graph--latest-records)
                  (lambda (&rest args) (cl-incf rebuilds) (apply orig args))))
        (org-glance-graph:add graph (org-glance-test:headline "b" "* TODO B"))
        (should (equal '("a" "b") (org-glance-test:ids graph)))
        (should (= 0 rebuilds))
        (org-glance-graph:add graph (org-glance-test:headline "a" "* DONE A"))
        (should (equal '("a" "b") (org-glance-test:ids graph)))
        (should (equal "DONE" (org-glance-test:field graph "a" state)))
        (should (= 0 rebuilds))
        (org-glance-graph:delete graph "a")
        (should (equal '("b") (org-glance-test:ids graph)))
        (should (eq 'tombstone (org-glance-graph:get-headline graph "a")))
        (should (= 0 rebuilds))
        ;; a re-add after a tombstone cannot keep its slot -> deliberate rebuild
        (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO A again"))
        (should (equal '("a" "b") (org-glance-test:ids graph)))
        (should (= 1 rebuilds))))))

(ert-deftest org-glance-test:graph-cache-patch-matches-rebuild ()
  "After each random mutation the patched cache answers like a cold rebuild.
Live set, order, states and per-id tri-state must match; the test compares
the two paths rather than trusting either."
  (org-glance-test:with-graph graph
    (random "org-glance-cache-fuzz")            ; deterministic sequence
    (let ((ids '("a" "b" "c" "d" "e"))
          (n 0))
      (dotimes (_ 50)
        (let ((id (nth (random (length ids)) ids)))
          (pcase (random 3)
            (0 (org-glance-graph:add graph (org-glance-test:headline
                                            id (format "* TODO %s %d" id (cl-incf n)))))
            (1 (org-glance-graph:add graph (org-glance-test:headline
                                            id (format "* DONE %s %d" id (cl-incf n)))))
            (_ (org-glance-graph:delete graph id))))
        (let ((live (org-glance-test:ids graph))
              (states (mapcar #'org-glance-headline-metadata:state
                              (org-glance-graph:headlines graph)))
              (tri (mapcar (lambda (i) (org-glance-test:tri-state graph i)) ids))
              (fresh (org-glance-test:reopen graph)))
          (should (equal live (org-glance-test:ids fresh)))
          (should (equal states (mapcar #'org-glance-headline-metadata:state
                                        (org-glance-graph:headlines fresh))))
          (should (equal tri (mapcar (lambda (i) (org-glance-test:tri-state fresh i)) ids))))))))

(provide 'test-graph)
;;; test-graph.el ends here
