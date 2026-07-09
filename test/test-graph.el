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
    (should (string= "alpha" (org-glance-headline-metadata:title (org-glance-graph:get-headline graph "a"))))
    (should (string= "beta"  (org-glance-headline-metadata:title (org-glance-graph:get-headline graph "b"))))))

(ert-deftest org-glance-test:graph-latest-wins ()
  "Re-adding an id appends a record; the most recent one wins (reverse scan)."
  (org-glance-test:with-graph graph
    (let ((headline (org-glance-test:headline "id1" "* TODO foo")))
      (org-glance-graph:add graph headline)
      (org-glance-graph:add graph (org-glance-headline--copy headline :state "DONE"))
      (should (string= "DONE" (org-glance-headline-metadata:state (org-glance-graph:get-headline graph "id1")))))))

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
  "Non-ASCII titles survive both read paths.
Regression: an earlier reverse JSONL reader fed undecoded UTF-8 bytes to
`json-parse-string', which raised `json-utf8-decode-error'."
  (org-glance-test:with-graph graph
    (let ((title "Façade — Facebook’s “data” café"))
      (org-glance-graph:add graph (org-glance-test:headline "u1" (concat "* TODO " title)))
      ;; point lookup (the previously-broken path)
      (should (string= title (org-glance-headline-metadata:title
                              (org-glance-graph:get-headline graph "u1"))))
      ;; forward reader
      (should (string= title (org-glance-headline-metadata:title
                              (car (org-glance-graph:headlines graph)))))
      ;; full content reconstruct
      (should (string= title (org-glance-headline:title
                              (org-glance-graph:headline graph "u1")))))))

(ert-deftest org-glance-test:graph-utf8-chunk-boundary ()
  "Multibyte content spanning a 4096-byte boundary still reads back correctly
(per-line UTF-8 decode, independent of any read chunking)."
  (org-glance-test:with-graph graph
    (let ((title "café—’“”—naïve—Façade"))
      (dotimes (i 50)
        (org-glance-graph:add graph
                                 (org-glance-test:headline (format "k%d" i)
                                                           (format "* TODO %s %d" title i))))
      ;; the jsonl now spans several 4096-byte chunks; every id must resolve and
      ;; carry its multibyte title intact regardless of where boundaries fall
      (dotimes (i 50)
        (let ((meta (org-glance-graph:get-headline graph (format "k%d" i))))
          (should (org-glance-headline-metadata? meta))
          (should (string= (format "%s %d" title i)
                           (org-glance-headline-metadata:title meta))))))))

(ert-deftest org-glance-test:graph-scheduled-roundtrip ()
  "A scheduled headline serializes (regression: schedule/deadline must be coerced
to raw strings, not org-element timestamp objects, or `json-serialize' crashes)."
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
  "`put-content' overwrites the blob in place and leaves no temp file behind.
Regression guard for the atomic temp-then-rename write (a torn write must never
truncate an existing data.org)."
  (org-glance-test:with-graph graph
    (org-glance-graph:put-content graph (org-glance-test:headline "aw1" "* foo" "first body"))
    (org-glance-graph:put-content graph (org-glance-test:headline "aw1" "* foo" "second body"))
    ;; latest content wins, in place
    (should (s-contains? "second body" (org-glance-graph:get-content graph "aw1")))
    (should-not (s-contains? "first body" (org-glance-graph:get-content graph "aw1")))
    ;; the temp file was renamed away, not left in the data dir
    (let ((dir (org-glance-graph:headline-data-path graph "aw1")))
      (should-not (cl-find-if (lambda (f) (s-contains? "data.org.tmp." f))
                              (directory-files dir))))))

(ert-deftest org-glance-test:graph-unsafe-id-rejected ()
  "Path-unsafe ids are rejected before touching the filesystem."
  (org-glance-test:with-graph graph
    (should-error (org-glance-graph:headline-data-path graph "../escape") :type 'error)
    (should-error (org-glance-graph:headline-data-path graph "a/b") :type 'error)
    ;; a normal tag-hash id is fine
    (should (org-glance-graph:headline-data-path graph "whitepaper-d41d8cd98f00b204"))))

(ert-deftest org-glance-test:graph-flags-roundtrip ()
  "linked?/propertized? projection flags are computed at add time and round-trip."
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
    ;; simulate an old record lacking the linked flag (latest wins)
    (org-glance-graph:insert graph (list (list :id "r1" :state "" :title "foo")))
    (should (not (org-glance-headline-metadata:linked? (org-glance-graph:get-headline graph "r1"))))
    (org-glance-graph:reindex graph)
    (should (org-glance-headline-metadata:linked? (org-glance-graph:get-headline graph "r1")))))

(ert-deftest org-glance-test:org-mode-forces-tab-width-8 ()
  "Parsing setup forces tab-width 8 (org requires it) and disables tabs, even
when the user's default tab-width is 4 -- and metadata still computes.
Regression for `org-glance-headline:metadata' failing with \"Tab width in Org
files must be 8\" in Emacsen whose default tab-width is not 8."
  (let ((orig (default-value 'tab-width)))
    (unwind-protect
        (progn
          (setq-default tab-width 4)
          (with-temp-buffer
            (org-glance--org-mode)
            (should (= tab-width 8))
            (should-not indent-tabs-mode))
          (let ((h (org-glance-headline--from-lines
                    "* TODO foo" ":PROPERTIES:" ":ORG_GLANCE_ID: t1" ":END:" "body")))
            (should (org-glance-headline-metadata? (org-glance-headline:metadata h)))))
      (setq-default tab-width orig))))

(ert-deftest org-glance-test:graph-content-missing ()
  "Reading content for an unknown id yields nil, not an error."
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
    ;; the low-level blob is still present (append-only store)
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

;;; In-memory read cache coherence
;;
;; The hot read APIs (`headlines'/`get-headline'/`tags'/`states') serve from a
;; snapshot-keyed in-memory cache.  These guard the cases a naive memo gets
;; wrong: an in-process write in the same filesystem-mtime second, an external
;; (another-Emacs) write the in-process invalidation never sees, tombstone
;; liveness through the O(1) lookup, compaction changing an observable read, and
;; insertion order surviving a cache rebuild.

(ert-deftest org-glance-test:graph-cache-same-second-add ()
  "An add right after a warmed read is visible even if mtime did not advance.
Guards that an in-process write invalidates the cache directly (never relies on
filesystem mtime granularity)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "c1" "* TODO A"))
    (should (= 1 (length (org-glance-graph:headlines graph)))) ; warms the cache
    (org-glance-graph:add graph (org-glance-test:headline "c2" "* TODO B"))
    (should (= 2 (length (org-glance-graph:headlines graph))))
    (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "c2")))))

(ert-deftest org-glance-test:graph-cache-external-write ()
  "A write to the open segment behind the graph's back is still observed.
Simulates another Emacs: append a record directly (bypassing the mutation API,
so no in-process invalidation fires); the snapshot check (open mtime+size) must
trigger a rebuild on the next read."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "e1" "* TODO A"))
    (should (= 1 (length (org-glance-graph:headlines graph)))) ; warms the cache
    (let ((open (org-glance-graph:headline-meta-path graph)))
      (f-append-text (concat (json-serialize (list :id "e2" :state "TODO" :title "B" :seq 9999))
                             "\n")
                     'utf-8 open))
    ;; cache was warm and no mutation method ran, yet the new record is seen
    (should (= 2 (length (org-glance-graph:headlines graph))))
    (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "e2")))))

(ert-deftest org-glance-test:graph-cache-delete-then-tombstone ()
  "After warming the cache, a delete is reflected as a tombstone, not the live row."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "d1" "* TODO A"))
    (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "d1"))) ; warm
    (org-glance-graph:delete graph "d1")
    (should (eq 'tombstone (org-glance-graph:get-headline graph "d1")))
    (should (= 0 (length (org-glance-graph:headlines graph))))))

(ert-deftest org-glance-test:graph-cache-compaction-visibility ()
  "Compaction drops a tombstone, turning `get-headline' from `tombstone' to nil.
The cache must invalidate on compaction so this observable change is seen."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "k1" "* TODO A"))
    (org-glance-graph:delete graph "k1")
    (should (eq 'tombstone (org-glance-graph:get-headline graph "k1"))) ; warm: tombstone
    (org-glance-graph:compact graph)
    ;; the only record for k1 was a tombstone -> compaction drops the id entirely
    (should (null (org-glance-graph:get-headline graph "k1")))))

(ert-deftest org-glance-test:graph-cache-insertion-order-after-update ()
  "Insertion (first-sighting) order survives a cache rebuild after an in-place update."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "o1" "* TODO A")
                             (org-glance-test:headline "o2" "* TODO B")
                             (org-glance-test:headline "o3" "* TODO C"))
    (should (equal '("o1" "o2" "o3") (org-glance-test:ids graph)))
    ;; re-add o2 with a new title: order is unchanged, content is the latest
    (org-glance-graph:add graph (org-glance-test:headline "o2" "* DONE B2"))
    (let ((metas (org-glance-graph:headlines graph)))
      (should (equal '("o1" "o2" "o3") (org-glance-test:ids graph)))
      (should (equal "DONE" (org-glance-headline-metadata:state (cadr metas)))))))

(ert-deftest org-glance-test:graph-cache-external-compaction-mtime-independent ()
  "An external compaction is detected via the snapshot's segment-NAME dimension,
even when the open segment's mtime and size are unchanged.  This is the coarse-
filesystem resurrection hole the adversarial review found: a compaction rewrites
the open empty (size back to 0) and swaps the live set, so a purely mtime-based
snapshot could repeat on a 1s-granularity clock and serve a deleted headline.
A SEPARATE graph struct is the external writer (so the reader's in-process
invalidation never fires); the open + MANIFEST mtimes are pinned so ONLY the
sealed-segment name list ([seg-01] -> [seg-02]) distinguishes the two states."
  (org-glance-test:with-graph reader
    (let* ((writer (make-org-glance-graph :directory (org-glance-graph:directory reader)))
           (open (org-glance-graph:headline-meta-path reader))
           (manifest (org-glance-graph--manifest-path reader))
           (pinned 1500000000))
      ;; Writer builds a sealed segment of A,B,C with an empty open segment.
      (org-glance-graph:add writer
                            (org-glance-test:headline "A" "* TODO A")
                            (org-glance-test:headline "B" "* TODO B")
                            (org-glance-test:headline "C" "* TODO C"))
      (org-glance-graph--seal writer)
      ;; Pin times, then warm the READER's cache: live={A,B,C}, sealed=[seg-01].
      (set-file-times open pinned)
      (set-file-times manifest pinned)
      (should (= 3 (length (org-glance-graph:headlines reader))))
      (should (org-glance-headline-metadata? (org-glance-graph:get-headline reader "A")))
      ;; External writer deletes A and compacts: A's tombstone is dropped, B,C are
      ;; folded into seg-02, the open is rewritten empty.  The reader's cache is
      ;; untouched (separate struct -> no in-process invalidation).
      (org-glance-graph:delete writer "A")
      (org-glance-graph:compact writer)
      ;; Pin the open + MANIFEST mtimes back so (mtime,size) match the warm
      ;; snapshot; only the sealed-segment NAMES differ.
      (set-file-times open pinned)
      (set-file-times manifest pinned)
      ;; The reader must rebuild and see the post-compaction state (no resurrection).
      (should (= 2 (length (org-glance-graph:headlines reader))))
      (should (null (org-glance-graph:get-headline reader "A"))))))

(provide 'test-graph)
;;; test-graph.el ends here
