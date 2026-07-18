;;; test-merge.el --- Git-sync merge/self-heal tests for the metadata store  -*- lexical-binding: t -*-

;; Covers the store's behaviour when its meta/ dir is synced across machines
;; via git: the `merge=union' driver on *.jsonl, self-healing a MANIFEST left
;; conflict-marked by git's default merge, positional last-wins reads over a
;; union-merged open segment, and adopting segments sealed on two machines.

(require 'test-helpers)
(require 'org-glance-tag-metrics)

(cl-defun org-glance-test-merge:conflict-eld (ours theirs)
  "Return .eld text git left conflict-marked with the OURS/THEIRS Lisp forms."
  (concat "<<<<<<< HEAD\n" (prin1-to-string ours) "\n"
          "=======\n" (prin1-to-string theirs) "\n"
          ">>>>>>> other-machine\n"))

(cl-defun org-glance-test-merge:write-metrics-conflict (graph ours theirs)
  "Write a conflict-marked tag-metrics sidecar for GRAPH; return its path."
  (let ((file (org-glance-tag-metrics--file graph)))
    (f-mkdir-full-path (f-dirname file))
    (f-write-text (org-glance-test-merge:conflict-eld ours theirs) 'utf-8 file)
    file))

(cl-defun org-glance-test-merge:seg-names (graph)
  "Sorted basenames of the on-disk seg-*.jsonl files in GRAPH's meta dir."
  (let ((meta (org-glance-graph:meta-path graph)))
    (sort (cl-remove-if-not #'org-glance-graph--segment-generation
                            (directory-files meta nil nil))
          #'string<)))

(cl-defun org-glance-test-merge:conflict-manifest (ours theirs)
  "Return MANIFEST text git left conflict-marked with OURS/THEIRS seg lists."
  (cl-flet ((seg (names) (format "{\"version\":2,\"segments\":[%s]}\n"
                                 (mapconcat (lambda (n) (format "\"%s\"" n)) names ","))))
    (concat "<<<<<<< HEAD\n" (seg ours) "=======\n" (seg theirs)
            ">>>>>>> other-machine\n")))

(cl-defun org-glance-test-merge:record (&rest kv)
  "Return a newline-terminated JSONL metadata line from plist KV."
  (concat (json-serialize (apply #'list kv)) "\n"))

(cl-defun org-glance-test-merge:conflict-open (ours theirs)
  "Return open-segment text git left conflict-marked with OURS/THEIRS records.
OURS and THEIRS are each a string of newline-terminated JSONL record lines."
  (concat "<<<<<<< HEAD\n" ours "=======\n" theirs ">>>>>>> other-machine\n"))

(ert-deftest org-glance-test:merge-gitattributes-written ()
  "Store construction writes meta/.gitattributes with exactly the union driver
line, and a second open never clobbers a hand-edited file (write-if-absent)."
  (org-glance-test:with-graph graph
    (let ((path (f-join (org-glance-graph:meta-path graph) ".gitattributes")))
      (should (f-exists? path))
      (should (string= "*.jsonl merge=union\n" (f-read-text path 'utf-8)))
      ;; hand-edit, then reopen: the existing file must survive untouched
      (f-write-text "*.jsonl merge=union\n# local override\n" 'utf-8 path)
      (let ((graph (org-glance-test:reopen graph)))
        (should (string= "*.jsonl merge=union\n# local override\n"
                         (f-read-text (f-join (org-glance-graph:meta-path graph)
                                              ".gitattributes")
                                      'utf-8)))))))

(ert-deftest org-glance-test:merge-manifest-conflict-self-heal ()
  "A MANIFEST left conflict-marked by git is self-healed on the next open: every
on-disk non-empty seg-*.jsonl becomes visible again, live records are restored,
and the rewritten MANIFEST is valid canonical JSON."
  (org-glance-test:with-graph graph
    (let ((org-glance-graph-segment-max-bytes 1)       ; seal every insert
          (org-glance-graph-compact-segment-count 1000)) ; no auto-compaction
      (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
      (org-glance-graph:add graph (org-glance-test:headline "b" "* Beta"))
      (org-glance-graph:add graph (org-glance-test:headline "c" "* Gamma")))
    (let ((segs (org-glance-test-merge:seg-names graph))
          (manifest (org-glance-graph--manifest-path graph)))
      (should (= 3 (length segs)))
      ;; mangle the MANIFEST exactly as git's default (non-union) merge would
      (f-write-text (org-glance-test-merge:conflict-manifest (list (nth 0 segs)) segs)
                    'utf-8 manifest)
      (let ((graph (org-glance-test:reopen graph)))
        ;; every sealed segment adopted back; none reaped
        (should (equal segs (sort (copy-sequence
                                   (org-glance-graph--sealed-segments graph))
                                  #'string<)))
        (should (equal segs (org-glance-test-merge:seg-names graph)))
        ;; live records restored, in first-sighting order
        (should (equal '("a" "b" "c")
                       (mapcar #'org-glance-headline-metadata:id
                               (org-glance-graph:headlines graph))))
        ;; rewritten MANIFEST is valid canonical JSON, no markers left
        (let* ((text (f-read-text manifest 'utf-8))
               (parsed (json-parse-string text :object-type 'plist)))
          (should (= 2 (plist-get parsed :version)))
          (should (vectorp (plist-get parsed :segments)))
          (should (equal segs (sort (append (plist-get parsed :segments) nil)
                                    #'string<)))
          (should (s-suffix? "\n" text))
          (should-not (s-contains? "<<<<<<<" text)))))))

(ert-deftest org-glance-test:merge-union-positional-last-wins ()
  "A union-merged open segment (extra lines from the other machine appended
after ours, including a newer duplicate id placed later) reads by physical
position: the last record per id wins and no data is lost."
  (org-glance-test:with-graph graph
    ;; two ids via the normal API -- these land in the open segment
    (org-glance-graph:insert graph (list (list :id "a" :state "" :title "A-orig" :hash "ha1")))
    (org-glance-graph:insert graph (list (list :id "b" :state "" :title "B-orig" :hash "hb1")))
    (let ((open (org-glance-graph:headline-meta-path graph)))
      ;; what `merge=union' leaves: the other side's lines spliced in after ours,
      ;; with a newer record for "a" positioned last and a brand-new id "c"
      (f-append-text
       (concat (org-glance-test-merge:record :id "c" :state "" :title "C-new" :hash "hc1" :seq 50)
               (org-glance-test-merge:record :id "a" :state "DONE" :title "A-newer" :hash "ha2" :seq 51))
       'utf-8 open))
    (let ((graph (org-glance-test:reopen graph)))
      ;; positional last-wins: the later "a" record supersedes the earlier one
      (let ((a (org-glance-graph:get-headline graph "a")))
        (should (string= "A-newer" (org-glance-headline-metadata:title a)))
        (should (string= "DONE" (org-glance-headline-metadata:state a))))
      ;; nothing dropped
      (should (string= "B-orig" (org-glance-headline-metadata:title
                                 (org-glance-graph:get-headline graph "b"))))
      (should (string= "C-new" (org-glance-headline-metadata:title
                                (org-glance-graph:get-headline graph "c"))))
      (should (equal '("a" "b" "c")
                     (sort (mapcar #'org-glance-headline-metadata:id
                                   (org-glance-graph:headlines graph))
                           #'string<))))))

(ert-deftest org-glance-test:merge-seal-on-both-adopted ()
  "Two non-empty sealed segments present on disk while the MANIFEST lists neither
\(conflict-marked) are both adopted on open -- records visible, none deleted."
  (with-temp-directory dir
    (let ((meta (f-join dir ".org-glance" "meta")))
      (f-mkdir-full-path meta)
      (f-write-text (org-glance-test-merge:record :id "m1" :state "" :title "Machine one" :hash "h1" :seq 1)
                    'utf-8 (f-join meta "seg-0000000001.jsonl"))
      (f-write-text (org-glance-test-merge:record :id "m2" :state "" :title "Machine two" :hash "h2" :seq 2)
                    'utf-8 (f-join meta "seg-0000000002.jsonl"))
      ;; MANIFEST as git left it: neither side lists both segments
      (f-write-text (org-glance-test-merge:conflict-manifest '() '("seg-0000000001.jsonl"))
                    'utf-8 (f-join meta "MANIFEST"))
      (let ((graph (org-glance-graph dir)))
        ;; both segments now live
        (should (member "seg-0000000001.jsonl"
                        (org-glance-graph--sealed-segments graph)))
        (should (member "seg-0000000002.jsonl"
                        (org-glance-graph--sealed-segments graph)))
        ;; records from both readable
        (should (string= "Machine one" (org-glance-headline-metadata:title
                                        (org-glance-graph:get-headline graph "m1"))))
        (should (string= "Machine two" (org-glance-headline-metadata:title
                                        (org-glance-graph:get-headline graph "m2"))))
        (should (equal '("m1" "m2")
                       (sort (mapcar #'org-glance-headline-metadata:id
                                     (org-glance-graph:headlines graph))
                             #'string<)))
        ;; nothing reaped by gc
        (should (f-exists? (f-join meta "seg-0000000001.jsonl")))
        (should (f-exists? (f-join meta "seg-0000000002.jsonl")))))))

(ert-deftest org-glance-test:merge-open-segment-conflict-union-resolved ()
  "Conflict markers already written into the open segment (a store synced before
the union driver existed) are union-resolved on open when
`org-glance-conflict-resolution' is `union': markers gone, every record
from both sides kept, positional last-wins per id."
  (org-glance-test:with-graph graph
    (let ((open (org-glance-graph:headline-meta-path graph))
          (org-glance-conflict-resolution 'union))
      ;; "a" landed normally; then git wrapped the two machines' concurrent
      ;; appends in markers, theirs carrying a newer "a" positioned last
      (f-write-text
       (concat
        (org-glance-test-merge:record :id "a" :state "" :title "A-orig" :hash "ha1" :seq 1)
        (org-glance-test-merge:conflict-open
         (org-glance-test-merge:record :id "b" :state "TODO" :title "B" :hash "hb1" :seq 2)
         (concat (org-glance-test-merge:record :id "c" :state "" :title "C" :hash "hc1" :seq 3)
                 (org-glance-test-merge:record :id "a" :state "DONE" :title "A-newer" :hash "ha2" :seq 4))))
       'utf-8 open)
      (let ((graph (org-glance-test:reopen graph)))
        ;; markers stripped from disk
        (should-not (s-contains? "<<<<<<<" (f-read-text open 'utf-8)))
        (should-not (s-contains? "=======" (f-read-text open 'utf-8)))
        ;; every id from both sides kept
        (should (equal '("a" "b" "c")
                       (sort (mapcar #'org-glance-headline-metadata:id
                                     (org-glance-graph:headlines graph))
                             #'string<)))
        ;; positional last-wins: theirs' later "a" supersedes ours
        (let ((a (org-glance-graph:get-headline graph "a")))
          (should (string= "A-newer" (org-glance-headline-metadata:title a)))
          (should (string= "DONE" (org-glance-headline-metadata:state a))))))))

(ert-deftest org-glance-test:merge-open-segment-conflict-ask-approved ()
  "With `ask' resolution an approved prompt union-resolves the conflicted open
segment; a declined prompt errors and leaves the markers in place."
  (org-glance-test:with-graph graph
    (let ((open (org-glance-graph:headline-meta-path graph))
          (org-glance-conflict-resolution 'ask))
      (f-write-text (org-glance-test-merge:conflict-open
                     (org-glance-test-merge:record :id "a" :state "" :title "A" :hash "ha1" :seq 1)
                     (org-glance-test-merge:record :id "b" :state "" :title "B" :hash "hb1" :seq 2))
                    'utf-8 open)
      ;; declined -> error, markers untouched
      (org-glance-test:answering ((y-or-n-p nil))
        (should-error (org-glance-test:reopen graph))
        (should (s-contains? "<<<<<<<" (f-read-text open 'utf-8))))
      ;; approved -> resolved, both ids readable
      (org-glance-test:answering ((y-or-n-p t))
        (let ((graph (org-glance-test:reopen graph)))
          (should-not (s-contains? "<<<<<<<" (f-read-text open 'utf-8)))
          (should (equal '("a" "b")
                         (sort (mapcar #'org-glance-headline-metadata:id
                                       (org-glance-graph:headlines graph))
                               #'string<))))))))

(ert-deftest org-glance-test:merge-open-segment-conflict-nil-errors ()
  "With nil resolution the store refuses to open a conflict-marked segment:
signals an error and leaves the markers in place for manual handling."
  (org-glance-test:with-graph graph
    (let ((open (org-glance-graph:headline-meta-path graph))
          (org-glance-conflict-resolution nil))
      (f-write-text (org-glance-test-merge:conflict-open
                     (org-glance-test-merge:record :id "a" :state "" :title "A" :hash "ha1" :seq 1)
                     (org-glance-test-merge:record :id "b" :state "" :title "B" :hash "hb1" :seq 2))
                    'utf-8 open)
      (should-error (org-glance-test:reopen graph))
      (should (s-contains? "<<<<<<<" (f-read-text open 'utf-8))))))

;;; tag-metrics.eld conflict resolution (a config/*.eld the union driver misses)

(ert-deftest org-glance-test:merge-tag-metrics-plist-semantics ()
  "Metric plists union by field: earliest :created, latest :modified, and `max'
for the counters -- never a sum, which a shared base would double-count."
  (let ((m (org-glance-tag-metrics--merge-plists
            (list :created (seconds-to-time 100) :modified (seconds-to-time 200) :captures 3 :removals 1)
            (list :created (seconds-to-time 50)  :modified (seconds-to-time 300) :captures 5 :removals 0))))
    (should (equal (seconds-to-time 50)  (plist-get m :created)))
    (should (equal (seconds-to-time 300) (plist-get m :modified)))
    (should (= 5 (plist-get m :captures)))
    (should (= 1 (plist-get m :removals)))))

(ert-deftest org-glance-test:merge-tag-metrics-conflict-union-resolved ()
  "A git-conflicted tag-metrics.eld is union-merged on read under `union':
earliest :created, latest :modified, max counters; the file is rewritten clean."
  (org-glance-test:with-graph graph
    (let* ((org-glance-conflict-resolution 'union)
           (file (org-glance-test-merge:write-metrics-conflict
                  graph
                  (list (list "x" :created (seconds-to-time 100) :modified (seconds-to-time 200) :captures 3)
                        (list "y" :captures 9))
                  (list (list "x" :created (seconds-to-time 50)  :modified (seconds-to-time 300) :captures 5))))
           (m (org-glance-tag-metrics--read graph))
           (x (cdr (assoc "x" m))))
      (should (equal (seconds-to-time 50)  (plist-get x :created)))
      (should (equal (seconds-to-time 300) (plist-get x :modified)))
      (should (= 5 (plist-get x :captures)))
      (should (= 9 (plist-get (cdr (assoc "y" m)) :captures)))  ; tag only one side had
      (let ((text (f-read-text file 'utf-8)))
        (should-not (string-match-p "<<<<<<<\\|=======\\|>>>>>>>" text))
        (should (equal m (car (read-from-string text))))))))

(ert-deftest org-glance-test:merge-tag-metrics-conflict-ask-approved ()
  "Under `ask', approving the prompt heals the conflicted tag-metrics.eld."
  (org-glance-test:with-graph graph
    (let ((org-glance-conflict-resolution 'ask)
          (file (org-glance-test-merge:write-metrics-conflict
                 graph (list (list "x" :captures 1)) (list (list "x" :captures 2)))))
      (org-glance-test:answering ((y-or-n-p t))
        (let ((m (org-glance-tag-metrics--read graph)))
          (should (= 2 (plist-get (cdr (assoc "x" m)) :captures)))))
      (should-not (s-contains? "<<<<<<<" (f-read-text file 'utf-8))))))

(ert-deftest org-glance-test:merge-tag-metrics-conflict-nil-errors ()
  "With resolution nil, a conflicted tag-metrics.eld errors and stays marked."
  (org-glance-test:with-graph graph
    (let ((org-glance-conflict-resolution nil)
          (file (org-glance-test-merge:write-metrics-conflict
                 graph (list (list "x" :captures 1)) (list (list "x" :captures 2)))))
      (should-error (org-glance-tag-metrics--read graph))
      (should (s-contains? "<<<<<<<" (f-read-text file 'utf-8))))))

(ert-deftest org-glance-test:merge-tag-metrics-heal-on-open ()
  "A git-conflicted tag-metrics.eld is resolved proactively when the graph is
reopened (the after-open hook), not only on the next append."
  (org-glance-test:with-graph graph
    (let ((org-glance-conflict-resolution 'union)
          (file (org-glance-test-merge:write-metrics-conflict
                 graph (list (list "x" :captures 1)) (list (list "x" :captures 2)))))
      (should (s-contains? "<<<<<<<" (f-read-text file 'utf-8)))
      (let ((graph (org-glance-test:reopen graph)))   ; after-open hook heals
        (should-not (s-contains? "<<<<<<<" (f-read-text file 'utf-8)))
        (should (= 2 (plist-get (cdr (assoc "x" (org-glance-tag-metrics--read graph)))
                                :captures)))))))

(ert-deftest org-glance-test:merge-eld-read-floor ()
  "`org-glance--read-eld' on any conflicted .eld keeps a readable side, never the
stray `<<<<<<<' marker symbol that would crash the caller."
  (with-temp-directory dir
    (let ((file (f-join dir "c.eld")))
      (f-write-text (org-glance-test-merge:conflict-eld '(("a" . 1)) '(("b" . 2))) 'utf-8 file)
      (let ((r (org-glance--read-eld file)))
        (should (consp r))
        (should (or (assoc "a" r) (assoc "b" r)))))))

(ert-deftest org-glance-test:merge-eld-read-floor-skips-empty-head ()
  "A side written as literal `nil' (an emptied config) must not shadow a
populated side -- `--read-eld' picks the non-empty one regardless of side order."
  (with-temp-directory dir
    (let ((file (f-join dir "c.eld")))
      ;; nil is HEAD, populated side second: the buggy `listp' floor returned nil.
      (f-write-text (org-glance-test-merge:conflict-eld nil '(("b" . 2))) 'utf-8 file)
      (should (equal '(("b" . 2)) (org-glance--read-eld file)))
      ;; and the mirror ordering still works
      (f-write-text (org-glance-test-merge:conflict-eld '(("a" . 1)) nil) 'utf-8 file)
      (should (equal '(("a" . 1)) (org-glance--read-eld file))))))

;;; Reusable conflict toolkit (org-glance-utils) -- format-agnostic

(ert-deftest org-glance-test:conflict-strip-markers ()
  "`--strip-conflict-markers' drops only the marker lines, keeping both sides;
`--conflict-marked?' detects markers in any text."
  (let ((text "<<<<<<< HEAD\nline-a\n=======\nline-b\n>>>>>>> other\n"))
    (should (equal "line-a\nline-b\n" (org-glance--strip-conflict-markers text)))
    (should (org-glance--conflict-marked? text))
    (should-not (org-glance--conflict-marked? "line-a\nline-b\n"))))

(ert-deftest org-glance-test:conflict-resolve-gate ()
  "`--resolve-conflict' gates on the policy custom: `union' runs the fn silently,
`ask' prompts (approve runs / decline errors), nil errors -- returning fn value."
  (let ((run (lambda () 'did-resolve)))
    (let ((org-glance-conflict-resolution 'union))
      (should (eq 'did-resolve (org-glance--resolve-conflict "x" run))))
    (let ((org-glance-conflict-resolution 'ask))
      (org-glance-test:answering ((y-or-n-p t))
        (should (eq 'did-resolve (org-glance--resolve-conflict "x" run))))
      (org-glance-test:answering ((y-or-n-p nil))
        (should-error (org-glance--resolve-conflict "x" (lambda () (error "must not run"))))))
    (let ((org-glance-conflict-resolution nil))
      (should-error (org-glance--resolve-conflict "x" (lambda () (error "must not run")))))))

(ert-deftest org-glance-test:heal-eld-generic-merge ()
  "`--heal-eld' is format-agnostic: a caller brings any merge-fn over the sides.
A clean file returns its single form; a conflicted one heals via the merge-fn,
writes back marker-free, and returns the merged value."
  (with-temp-directory dir
    (let ((file (f-join dir "s.eld"))
          (org-glance-conflict-resolution 'union)
          (merge (lambda (sides) (apply #'append sides))))  ; toy: concat every side
      (org-glance--write-eld file '(1 2))
      (should (equal '(1 2) (org-glance--heal-eld file merge)))          ; clean -> plain read
      (f-write-text (org-glance-test-merge:conflict-eld '(1 2) '(3 4)) 'utf-8 file)
      (should (equal '(1 2 3 4) (org-glance--heal-eld file merge)))      ; conflicted -> merged
      (let ((after (f-read-text file 'utf-8)))
        (should-not (org-glance--conflict-marked? after))               ; rewritten clean
        (should (equal '(1 2 3 4) (car (read-from-string after))))))))

(provide 'test-merge)
;;; test-merge.el ends here
