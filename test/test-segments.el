;;; test-segments.el --- Tests for the segmented (LSM-lite) metadata store  -*- lexical-binding: t -*-

(require 'test-helpers)

(defun org-glance-test:sealed-segments (graph)
  "Names of the sealed segments listed in GRAPH's MANIFEST."
  (org-glance-graph--sealed-segments graph))

(ert-deftest org-glance-test:segments-manifest-bootstrap ()
  "A fresh store gets a MANIFEST with no sealed segments; reopening is a no-op."
  (org-glance-test:with-graph graph
    (should (f-exists? (org-glance-graph--manifest-path graph)))
    (should (null (org-glance-test:sealed-segments graph)))
    (should (null (org-glance-graph:headlines graph)))
    ;; reopen (bypass the instance cache) -> same manifest, still empty
    (let ((manifest-before (f-read-text (org-glance-graph--manifest-path graph) 'utf-8)))
      (org-glance-test:reopen graph)
      (should (string= manifest-before
                       (f-read-text (org-glance-graph--manifest-path graph) 'utf-8))))))

(ert-deftest org-glance-test:segments-seal-at-threshold ()
  "The open segment seals into seg-* once it crosses the byte cap."
  (org-glance-test:with-graph graph
    (let ((org-glance-graph-segment-max-bytes 256)
          (org-glance-graph-compact-segment-count 1000)) ; no auto-compact here
      (dotimes (i 8)
        (org-glance-graph:add graph (org-glance-test:headline (format "id%d" i)
                                                                 (format "* Headline number %d" i))))
      (let ((sealed (org-glance-test:sealed-segments graph)))
        (should (>= (length sealed) 1))
        ;; every sealed segment exists, parses line-by-line, and the open file is small
        (dolist (name sealed)
          (let ((lines 0))
            (org-glance-graph--scan-file
             graph (f-join (org-glance-graph:meta-path graph) name)
             (lambda (_r) (cl-incf lines)))
            (should (> lines 0))))
        (should (<= (org-glance-test:open-size graph)
                    org-glance-graph-segment-max-bytes))))))

(ert-deftest org-glance-test:segments-never-split-record ()
  "An oversized record/batch lands whole; the cap is soft."
  (org-glance-test:with-graph graph
    (let ((org-glance-graph-segment-max-bytes 64)
          (org-glance-graph-compact-segment-count 1000))
      (org-glance-graph:add graph
                               (org-glance-test:headline "big" (concat "* " (make-string 200 ?x))))
      (let ((meta (org-glance-graph:get-headline graph "big")))
        (should (org-glance-headline-metadata? meta))
        (should (= 200 (length (org-glance-headline-metadata:title meta))))))))

(ert-deftest org-glance-test:segments-multi-segment-read-and-order ()
  "Reads merge all segments: latest-per-id wins, insertion order preserved."
  (org-glance-test:with-graph graph
    (let ((org-glance-graph-segment-max-bytes 1) ; seal after every insert
          (org-glance-graph-compact-segment-count 1000))
      (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO Alpha"))
      (org-glance-graph:add graph (org-glance-test:headline "b" "* Beta"))
      (org-glance-graph:add graph (org-glance-test:headline "c" "* Gamma"))
      (org-glance-graph:add graph (org-glance-test:headline "a" "* DONE Alpha")) ; update across seals
      (org-glance-graph:delete graph "b")
      (should (>= (length (org-glance-test:sealed-segments graph)) 3))
      ;; point lookups across segment boundaries
      (should (string= "DONE" (org-glance-headline-metadata:state
                               (org-glance-graph:get-headline graph "a"))))
      (should (eq 'tombstone (org-glance-graph:get-headline graph "b")))
      (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "c")))
      (should (null (org-glance-graph:get-headline graph "nope")))
      ;; full scan: first-sighting order (a before c), b tombstoned out, a's latest state
      (let ((metas (org-glance-graph:headlines graph)))
        (should (equal '("a" "c") (mapcar #'org-glance-headline-metadata:id metas)))
        (should (string= "DONE" (org-glance-headline-metadata:state (car metas))))))))

(ert-deftest org-glance-test:segments-utf8-across-segments ()
  "Multibyte titles survive seals and cross-segment reads."
  (org-glance-test:with-graph graph
    (let ((org-glance-graph-segment-max-bytes 1)
          (org-glance-graph-compact-segment-count 1000))
      (org-glance-graph:add graph (org-glance-test:headline "ru" "* Заголовок по-русски"))
      (org-glance-graph:add graph (org-glance-test:headline "emoji" "* Headline 🎯 with emoji"))
      (should (string= "Заголовок по-русски"
                       (org-glance-headline-metadata:title
                        (org-glance-graph:get-headline graph "ru"))))
      (should (string= "Headline 🎯 with emoji"
                       (org-glance-headline-metadata:title
                        (org-glance-graph:get-headline graph "emoji")))))))

(ert-deftest org-glance-test:segments-torn-line-recovery ()
  "A torn (newline-less, half-JSON) final line is ignored by reads and healed by
the next append; intact records are unaffected."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "ok" "* Intact"))
    (let ((open (org-glance-graph:headline-meta-path graph)))
      ;; simulate a crash mid-append: half a record, no trailing newline
      (f-append-text "{\"id\":\"torn\",\"sta" 'utf-8 open)
      (should (= 1 (length (org-glance-graph:headlines graph))))
      (should (null (org-glance-graph:get-headline graph "torn")))
      (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "ok")))
      ;; the next insert drops the torn tail and appends cleanly
      (org-glance-graph:add graph (org-glance-test:headline "next" "* After crash"))
      (should (equal '("ok" "next") (org-glance-test:ids graph))))))

(ert-deftest org-glance-test:segments-interrupted-seal-adopted ()
  "A seal that crashed before its MANIFEST commit is adopted on the next open."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "x" "* Sealed data"))
    (let ((open (org-glance-graph:headline-meta-path graph))
          (sealed (org-glance-graph--segment-path graph 1)))
      ;; simulate --seal dying between rename and manifest swap
      (rename-file open sealed)
      (f-touch open)
      (let ((graph (org-glance-test:reopen graph)))
        (should (member (file-name-nondirectory sealed)
                        (org-glance-test:sealed-segments graph)))
        (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "x")))))))

(ert-deftest org-glance-test:segments-crashed-compaction-discarded ()
  "An unreferenced segment next to a NON-empty open (crashed compaction) is
reaped, not adopted; pre-crash data stays intact."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "keep" "* Keep me"))
    (let ((orphan (org-glance-graph--segment-path graph 9)))
      (f-write-text "{\"id\":\"ghost\",\"title\":\"ghost\",\"seq\":999}\n" 'utf-8 orphan)
      (let ((graph (org-glance-test:reopen graph)))
        (should-not (f-exists? orphan))               ; reaped
        (should (null (org-glance-graph:get-headline graph "ghost")))
        (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "keep")))))))

(ert-deftest org-glance-test:segments-legacy-migration ()
  "A pre-segmentation store (bare headlines.jsonl, no MANIFEST, no seq) is adopted
in place: same reads, order preserved, next insert continues cleanly."
  (with-temp-directory dir
    (let ((meta (f-join dir ".org-glance" "meta")))
      (org-glance-test:write
       (f-join meta "headlines.jsonl")
       (concat "{\"id\":\"l1\",\"state\":\"TODO\",\"title\":\"Legacy один\",\"tags\":[\"work\"],\"hash\":\"h1\"}\n"
               "{\"id\":\"l2\",\"state\":\"\",\"title\":\"Legacy two\",\"tags\":[],\"hash\":\"h2\"}\n"
               "{\"id\":\"l1\",\"state\":\"DONE\",\"title\":\"Legacy один\",\"tags\":[\"work\"],\"hash\":\"h3\"}\n"
               "{\"id\":\"l3\",\"tombstone\":true}\n")))
    (let ((graph (org-glance-graph dir)))
      (should (f-exists? (org-glance-graph--manifest-path graph)))
      (should (null (org-glance-test:sealed-segments graph)))
      ;; single-file semantics preserved
      (let ((metas (org-glance-graph:headlines graph)))
        (should (equal '("l1" "l2") (mapcar #'org-glance-headline-metadata:id metas)))
        (should (string= "DONE" (org-glance-headline-metadata:state (car metas))))
        (should (string= "Legacy один" (org-glance-headline-metadata:title (car metas)))))
      (should (eq 'tombstone (org-glance-graph:get-headline graph "l3")))
      ;; new records sort after all legacy ones
      (org-glance-graph:insert graph (list (list :id "new" :state "" :title "New" :hash "h4")))
      (should (equal '("l1" "l2" "new") (org-glance-test:ids graph))))))

(ert-deftest org-glance-test:segments-compaction ()
  "Compaction merges sealed segments to one, drops superseded records and
tombstones, GCs dead blobs, and keeps live data intact."
  (org-glance-test:with-graph graph
    (let ((org-glance-graph-segment-max-bytes 1)
          (org-glance-graph-compact-segment-count 1000))
      (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO Alpha" "alpha body"))
      (org-glance-graph:add graph (org-glance-test:headline "a" "* DONE Alpha" "alpha body v2"))
      (org-glance-graph:add graph (org-glance-test:headline "b" "* Beta" "beta body"))
      (org-glance-graph:delete graph "b")
      (org-glance-graph--seal graph)              ; push the tombstone out of the open segment
      (should (>= (length (org-glance-test:sealed-segments graph)) 3))
      (should (f-exists? (org-glance-graph:headline-data-path graph "b")))
      (org-glance-graph:compact graph)
      ;; one sealed segment; superseded + tombstoned records gone
      (should (= 1 (length (org-glance-test:sealed-segments graph))))
      (let ((records nil))
        (org-glance-graph--scan-forward graph (lambda (r) (push r records)))
        (should (= 1 (length records)))
        (should (string= "a" (plist-get (car records) :id)))
        (should (string= "DONE" (plist-get (car records) :state))))
      ;; b is fully reclaimed: metadata gone (nil, not tombstone), blob deleted
      (should (null (org-glance-graph:get-headline graph "b")))
      (should-not (f-exists? (org-glance-graph:headline-data-path graph "b")))
      ;; a intact, contents readable
      (should (s-contains? "alpha body v2" (org-glance-graph:get-content graph "a"))))))

(ert-deftest org-glance-test:segments-compaction-preserves-order ()
  "Compaction folds the open segment in, so an id updated in the open keeps its
ORIGINAL first-sighting position (regression: leaving the open out re-sighted
such ids after everything else)."
  (org-glance-test:with-graph graph
    (let ((org-glance-graph-segment-max-bytes 1)
          (org-glance-graph-compact-segment-count 1000))
      (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO Alpha")) ; seals
      (org-glance-graph:add graph (org-glance-test:headline "b" "* Beta")))      ; seals
    ;; update a with the default cap: the record stays in the OPEN segment
    (org-glance-graph:add graph (org-glance-test:headline "a" "* DONE Alpha"))
    (should (equal '("a" "b") (org-glance-test:ids graph)))
    (org-glance-graph:compact graph)
    (let ((metas (org-glance-graph:headlines graph)))
      (should (equal '("a" "b") (mapcar #'org-glance-headline-metadata:id metas)))
      (should (string= "DONE" (org-glance-headline-metadata:state (car metas)))))
    ;; everything merged: one sealed segment, empty open
    (should (= 1 (length (org-glance-test:sealed-segments graph))))
    (should (= 0 (org-glance-test:open-size graph)))))

(ert-deftest org-glance-test:segments-compaction-noop ()
  "Compacting an already-compact store changes nothing on disk."
  (org-glance-test:with-graph graph
    (let ((org-glance-graph-segment-max-bytes 1)
          (org-glance-graph-compact-segment-count 1000))
      (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
      (org-glance-graph:compact graph))
    (let* ((manifest-path (org-glance-graph--manifest-path graph))
           (before (f-read-text manifest-path 'utf-8)))
      (org-glance-graph:compact graph)
      (should (string= before (f-read-text manifest-path 'utf-8))))))

(ert-deftest org-glance-test:segments-auto-compaction ()
  "Crossing the segment-count threshold triggers compaction from within insert."
  (org-glance-test:with-graph graph
    (let ((org-glance-graph-segment-max-bytes 1)
          (org-glance-graph-compact-segment-count 3))
      (dotimes (i 6)
        (org-glance-graph:add graph (org-glance-test:headline (format "id%d" i)
                                                                 (format "* Number %d" i))))
      (should (< (length (org-glance-test:sealed-segments graph)) 3))
      (should (= 6 (length (org-glance-graph:headlines graph)))))))

(ert-deftest org-glance-test:segments-crashed-compaction-keeps-tombstone ()
  "A compaction that crashes at its MANIFEST commit must not resurrect a deleted
headline whose tombstone's only copy is in the open segment (regression: the
open was truncated BEFORE the commit, destroying the tombstone while an old
listed segment still held the headline live)."
  (org-glance-test:with-graph graph
    (let ((org-glance-graph-segment-max-bytes 1)
          (org-glance-graph-compact-segment-count 1000))
      (org-glance-graph:add graph (org-glance-test:headline "x" "* TODO Doomed" "body"))  ; seals
      (org-glance-graph:add graph (org-glance-test:headline "y" "* Alive" "body")))       ; seals
    ;; tombstone for x lands in the OPEN segment only (default cap: no seal)
    (org-glance-graph:delete graph "x")
    (should (eq 'tombstone (org-glance-graph:get-headline graph "x")))
    ;; crash compact at its commit point
    (cl-letf (((symbol-function 'org-glance-graph--write-manifest)
               (lambda (&rest _) (error "simulated crash at commit"))))
      (should-error (org-glance-graph:compact graph)))
    ;; recover: x must STILL be dead, y alive, and the orphan merged seg reaped
    (let ((graph (org-glance-test:reopen graph)))
      (should (eq 'tombstone (org-glance-graph:get-headline graph "x")))
      (should (equal '("y") (org-glance-test:ids graph)))
      ;; a subsequent clean compaction converges: x fully reclaimed
      (org-glance-graph:compact graph)
      (should (null (org-glance-graph:get-headline graph "x")))
      (should-not (f-exists? (org-glance-graph:headline-data-path graph "x")))
      (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "y"))))))

(ert-deftest org-glance-test:segments-crashed-compaction-not-adopted ()
  "Compaction debris (same `seq' ordinals as listed segments) is never adopted by
heal, even in the ambiguous empty-open state; the store does not bloat."
  (org-glance-test:with-graph graph
    (let ((org-glance-graph-segment-max-bytes 1)
          (org-glance-graph-compact-segment-count 1000))
      (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))   ; seals
      (org-glance-graph:add graph (org-glance-test:headline "b" "* Beta")))   ; seals -> open empty
    (let ((segments-before (org-glance-test:sealed-segments graph))
          (count-records (lambda (graph)
                           (let ((n 0))
                             (org-glance-graph--scan-forward graph (lambda (_r) (cl-incf n)))
                             n))))
      (should (= 2 (funcall count-records graph)))
      (cl-letf (((symbol-function 'org-glance-graph--write-manifest)
                 (lambda (&rest _) (error "simulated crash at commit"))))
        (should-error (org-glance-graph:compact graph)))
      (let ((graph (org-glance-test:reopen graph)))
        ;; the orphan shares seqs with listed segments -> reaped, not adopted
        (should (equal segments-before (org-glance-test:sealed-segments graph)))
        (should (= 2 (funcall count-records graph)))
        (should (equal '("a" "b") (org-glance-test:ids graph)))))))

(ert-deftest org-glance-test:segments-freshness-signal ()
  "headline-meta-path's mtime advances on insert, delete, seal, and compact, so
the overview cache invalidates on every kind of store write."
  (org-glance-test:with-graph graph
    (let ((org-glance-graph-segment-max-bytes (* 256 1024))
          (org-glance-graph-compact-segment-count 1000)
          (signal (org-glance-graph:headline-meta-path graph)))
      (cl-flet ((backdate () (set-file-times signal (time-subtract (current-time) 100)))
                (mtime () (file-attribute-modification-time (file-attributes signal))))
        ;; insert bumps
        (backdate)
        (let ((before (mtime)))
          (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
          (should (time-less-p before (mtime))))
        ;; delete bumps
        (backdate)
        (let ((before (mtime)))
          (org-glance-graph:delete graph "a")
          (should (time-less-p before (mtime))))
        ;; seal recreates the open segment fresh
        (backdate)
        (let ((before (mtime)))
          (org-glance-graph--seal graph)
          (should (time-less-p before (mtime))))
        ;; compaction changes observable reads (tombstone -> nil), so it must
        ;; bump the signal too (it replaces the open segment)
        (backdate)
        (let ((before (mtime)))
          (org-glance-graph:compact graph)
          (should (time-less-p before (mtime))))))))

(provide 'test-segments)
;;; test-segments.el ends here
