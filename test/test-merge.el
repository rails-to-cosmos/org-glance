;;; test-merge.el --- Git-sync merge/self-heal tests for the metadata store  -*- lexical-binding: t -*-

(require 'test-helpers)
(require 'org-glance-tag-metrics)

(cl-defun org-glance-test-merge:conflict-eld (ours theirs)
  "Return .eld text git left conflict-marked with the OURS/THEIRS Lisp forms."
  (concat "<<<<<<< HEAD\n" (prin1-to-string ours) "\n"
          "=======\n" (prin1-to-string theirs) "\n"
          ">>>>>>> other-machine\n"))

(cl-defun org-glance-test-merge:write-metrics-conflict (graph ours theirs)
  "Write GRAPH's tag-metrics file conflicted as OURS/THEIRS; return its path."
  (let ((file (org-glance-tag-metrics--file graph)))
    (org-glance-test:write file (org-glance-test-merge:conflict-eld ours theirs))
    file))

(cl-defun org-glance-test-merge:seg-names (graph)
  "Return sorted basenames of GRAPH's on-disk seg-*.jsonl files."
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

(ert-deftest org-glance-test:merge-gitattributes-written ()
  "Store .gitattributes unions only the WAL's files; a reopen keeps hand edits.
A `*.jsonl' glob would also union the notification queue (invariant 8)."
  (org-glance-test:with-graph graph
    (let ((path (f-join (org-glance-graph:meta-path graph) ".gitattributes"))
          (expected "headlines.jsonl merge=union\nseg-*.jsonl merge=union\n"))
      (should (f-exists? path))
      (should (string= expected (f-read-text path 'utf-8)))
      (should-not (member "*.jsonl merge=union"
                          (split-string (f-read-text path 'utf-8) "\n" t)))
      (f-write-text (concat expected "# local override\n") 'utf-8 path)
      (let ((graph (org-glance-test:reopen graph)))
        (should (string= (concat expected "# local override\n")
                         (f-read-text (f-join (org-glance-graph:meta-path graph)
                                              ".gitattributes")
                                      'utf-8)))))))

(ert-deftest org-glance-test:merge-gitattributes-retires-the-broad-rule ()
  "A legacy JSONL glob becomes the WAL allowlist without losing local lines."
  (org-glance-test:with-graph graph
    (let ((path (f-join (org-glance-graph:meta-path graph) ".gitattributes")))
      (f-write-text "*.jsonl merge=union\n# local override\n" 'utf-8 path)
      (org-glance-test:reopen graph)
      (should (string= (concat "# local override\n"
                               "headlines.jsonl merge=union\n"
                               "seg-*.jsonl merge=union\n")
                       (f-read-text path 'utf-8))))))

(ert-deftest org-glance-test:merge-manifest-conflict-self-heal ()
  "A conflict-marked MANIFEST self-heals to canonical JSON on the next open.
Every non-empty on-disk segment and its live records become visible again."
  (org-glance-test:with-graph graph
    (org-glance-test:with-seal-each-insert
      (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
      (org-glance-graph:add graph (org-glance-test:headline "b" "* Beta"))
      (org-glance-graph:add graph (org-glance-test:headline "c" "* Gamma")))
    (let ((segs (org-glance-test-merge:seg-names graph))
          (manifest (org-glance-graph--manifest-path graph)))
      (should (= 3 (length segs)))
      (f-write-text (org-glance-test-merge:conflict-manifest (list (nth 0 segs)) segs)
                    'utf-8 manifest)
      (let ((graph (org-glance-test:reopen graph)))
        (should (equal segs (sort (copy-sequence
                                   (org-glance-graph--sealed-segments graph))
                                  #'string<)))
        (should (equal segs (org-glance-test-merge:seg-names graph)))
        (should (equal '("a" "b" "c")
                       (mapcar #'org-glance-headline-metadata:id
                               (org-glance-graph:headlines graph))))
        (let* ((text (f-read-text manifest 'utf-8))
               (parsed (json-parse-string text :object-type 'plist)))
          (should (= 2 (plist-get parsed :version)))
          (should (vectorp (plist-get parsed :segments)))
          (should (equal segs (sort (append (plist-get parsed :segments) nil)
                                    #'string<)))
          (should (s-suffix? "\n" text))
          (should-not (s-contains? "<<<<<<<" text)))))))

(ert-deftest org-glance-test:merge-union-positional-last-wins ()
  "A union-merged open segment loses no record and resolves ids by position.
The other machine's appended duplicate, being later, wins (invariant 1)."
  (org-glance-test:with-graph graph
    (org-glance-graph:insert graph (list (list :id "a" :state "" :title "A-orig" :hash "ha1")))
    (org-glance-graph:insert graph (list (list :id "b" :state "" :title "B-orig" :hash "hb1")))
    (let ((open (org-glance-graph:headline-meta-path graph)))
      (f-append-text
       (concat (org-glance-test-merge:record :id "c" :state "" :title "C-new" :hash "hc1" :seq 50)
               (org-glance-test-merge:record :id "a" :state "DONE" :title "A-newer" :hash "ha2" :seq 51))
       'utf-8 open))
    (let ((graph (org-glance-test:reopen graph)))
      (let ((a (org-glance-graph:get-headline graph "a")))
        (should (string= "A-newer" (org-glance-headline-metadata:title a)))
        (should (string= "DONE" (org-glance-headline-metadata:state a))))
      (should (string= "B-orig" (org-glance-test:field graph "b" title)))
      (should (string= "C-new" (org-glance-test:field graph "c" title)))
      (should (equal '("a" "b" "c")
                     (sort (mapcar #'org-glance-headline-metadata:id
                                   (org-glance-graph:headlines graph))
                           #'string<))))))

(ert-deftest org-glance-test:merge-seal-on-both-adopted ()
  "Segments a conflicted MANIFEST omits are adopted on open, none deleted."
  (with-temp-directory dir
    (let ((meta (f-join dir ".org-glance" "meta")))
      (org-glance-test:write (f-join meta "seg-0000000001.jsonl")
                             (org-glance-test-merge:record :id "m1" :state "" :title "Machine one" :hash "h1" :seq 1))
      (org-glance-test:write (f-join meta "seg-0000000002.jsonl")
                             (org-glance-test-merge:record :id "m2" :state "" :title "Machine two" :hash "h2" :seq 2))
      (org-glance-test:write (f-join meta "MANIFEST")
                             (org-glance-test-merge:conflict-manifest '() '("seg-0000000001.jsonl")))
      (let ((graph (org-glance-graph dir)))
        (should (member "seg-0000000001.jsonl"
                        (org-glance-graph--sealed-segments graph)))
        (should (member "seg-0000000002.jsonl"
                        (org-glance-graph--sealed-segments graph)))
        (should (string= "Machine one" (org-glance-test:field graph "m1" title)))
        (should (string= "Machine two" (org-glance-test:field graph "m2" title)))
        (should (equal '("m1" "m2")
                       (sort (mapcar #'org-glance-headline-metadata:id
                                     (org-glance-graph:headlines graph))
                             #'string<)))
        (should (f-exists? (f-join meta "seg-0000000001.jsonl")))
        (should (f-exists? (f-join meta "seg-0000000002.jsonl")))))))

(ert-deftest org-glance-test:merge-open-segment-conflict-union-resolved ()
  "A conflicted open segment union-resolves on open under `union' resolution.
Markers go, both sides' records stay, and the last record per id wins."
  (org-glance-test:with-graph graph
    (let ((open (org-glance-graph:headline-meta-path graph))
          (org-glance-conflict-resolution 'union))
      (f-write-text
       (concat
        (org-glance-test-merge:record :id "a" :state "" :title "A-orig" :hash "ha1" :seq 1)
        (org-glance-test:conflict-open
         (org-glance-test-merge:record :id "b" :state "TODO" :title "B" :hash "hb1" :seq 2)
         (concat (org-glance-test-merge:record :id "c" :state "" :title "C" :hash "hc1" :seq 3)
                 (org-glance-test-merge:record :id "a" :state "DONE" :title "A-newer" :hash "ha2" :seq 4))))
       'utf-8 open)
      (let ((graph (org-glance-test:reopen graph)))
        (should-not (s-contains? "<<<<<<<" (f-read-text open 'utf-8)))
        (should-not (s-contains? "=======" (f-read-text open 'utf-8)))
        (should (equal '("a" "b" "c")
                       (sort (mapcar #'org-glance-headline-metadata:id
                                     (org-glance-graph:headlines graph))
                             #'string<)))
        (let ((a (org-glance-graph:get-headline graph "a")))
          (should (string= "A-newer" (org-glance-headline-metadata:title a)))
          (should (string= "DONE" (org-glance-headline-metadata:state a))))))))

(ert-deftest org-glance-test:merge-open-segment-conflict-ask-approved ()
  "Under `ask', approving the prompt union-resolves a conflicted open segment.
Declining errors and leaves the markers in place."
  (org-glance-test:with-graph graph
    (let ((open (org-glance-graph:headline-meta-path graph))
          (org-glance-conflict-resolution 'ask))
      (f-write-text (org-glance-test:conflict-open
                     (org-glance-test-merge:record :id "a" :state "" :title "A" :hash "ha1" :seq 1)
                     (org-glance-test-merge:record :id "b" :state "" :title "B" :hash "hb1" :seq 2))
                    'utf-8 open)
      (org-glance-test:answering ((y-or-n-p nil))
        (should-error (org-glance-test:reopen graph))
        (should (s-contains? "<<<<<<<" (f-read-text open 'utf-8))))
      (org-glance-test:answering ((y-or-n-p t))
        (let ((graph (org-glance-test:reopen graph)))
          (should-not (s-contains? "<<<<<<<" (f-read-text open 'utf-8)))
          (should (equal '("a" "b")
                         (sort (mapcar #'org-glance-headline-metadata:id
                                       (org-glance-graph:headlines graph))
                               #'string<))))))))

(ert-deftest org-glance-test:merge-open-segment-conflict-nil-errors ()
  "With nil resolution a conflicted open segment errors and keeps its markers."
  (org-glance-test:with-graph graph
    (let ((open (org-glance-graph:headline-meta-path graph))
          (org-glance-conflict-resolution nil))
      (f-write-text (org-glance-test:conflict-open
                     (org-glance-test-merge:record :id "a" :state "" :title "A" :hash "ha1" :seq 1)
                     (org-glance-test-merge:record :id "b" :state "" :title "B" :hash "hb1" :seq 2))
                    'utf-8 open)
      (should-error (org-glance-test:reopen graph))
      (should (s-contains? "<<<<<<<" (f-read-text open 'utf-8))))))

;;; Writer-owned tag-metrics segments heal divergent snapshots by extrema.

(ert-deftest org-glance-test:merge-tag-metrics-plist-semantics ()
  "Snapshots of one metric component merge by field and never sum.
They keep the earliest :created, latest :modified and `max' counters (invariant 8)."
  (let ((m (org-glance-tag-metrics--merge-plists
            (list :created (seconds-to-time 100) :modified (seconds-to-time 200) :captures 3 :removals 1)
            (list :created (seconds-to-time 50)  :modified (seconds-to-time 300) :captures 5 :removals 0))))
    (should (equal (seconds-to-time 50)  (plist-get m :created)))
    (should (equal (seconds-to-time 300) (plist-get m :modified)))
    (should (= 5 (plist-get m :captures)))
    (should (= 1 (plist-get m :removals)))))

(ert-deftest org-glance-test:merge-tag-metrics-conflict-union-resolved ()
  "Under `union', reading a conflicted writer segment heals by field merge."
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
  "Under `ask', approving the prompt heals the conflicted writer segment."
  (org-glance-test:with-graph graph
    (let ((org-glance-conflict-resolution 'ask)
          (file (org-glance-test-merge:write-metrics-conflict
                 graph (list (list "x" :captures 1)) (list (list "x" :captures 2)))))
      (org-glance-test:answering ((y-or-n-p t))
        (let ((m (org-glance-tag-metrics--read graph)))
          (should (= 2 (plist-get (cdr (assoc "x" m)) :captures)))))
      (should-not (s-contains? "<<<<<<<" (f-read-text file 'utf-8))))))

(ert-deftest org-glance-test:merge-tag-metrics-conflict-nil-errors ()
  "With resolution nil, a conflicted writer segment errors and stays marked."
  (org-glance-test:with-graph graph
    (let ((org-glance-conflict-resolution nil)
          (file (org-glance-test-merge:write-metrics-conflict
                 graph (list (list "x" :captures 1)) (list (list "x" :captures 2)))))
      (should-error (org-glance-tag-metrics--read graph))
      (should (s-contains? "<<<<<<<" (f-read-text file 'utf-8))))))

(ert-deftest org-glance-test:merge-tag-metrics-heal-on-open ()
  "Reopening heals a conflicted writer segment via the after-open hook."
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
  "`org-glance--read-eld' reads any conflicted .eld as one readable side.
The `<<<<<<<' marker symbol, which would crash the caller, is never returned."
  (with-temp-directory dir
    (let ((file (f-join dir "c.eld")))
      (f-write-text (org-glance-test-merge:conflict-eld '(("a" . 1)) '(("b" . 2))) 'utf-8 file)
      (let ((r (org-glance--read-eld file)))
        (should (consp r))
        (should (or (assoc "a" r) (assoc "b" r)))))))

(ert-deftest org-glance-test:merge-eld-read-floor-skips-empty-head ()
  "`--read-eld' picks the populated side over a literal nil, in either order."
  (with-temp-directory dir
    (let ((file (f-join dir "c.eld")))
      (f-write-text (org-glance-test-merge:conflict-eld nil '(("b" . 2))) 'utf-8 file)
      (should (equal '(("b" . 2)) (org-glance--read-eld file)))
      (f-write-text (org-glance-test-merge:conflict-eld '(("a" . 1)) nil) 'utf-8 file)
      (should (equal '(("a" . 1)) (org-glance--read-eld file))))))

(ert-deftest org-glance-test:conflict-strip-markers ()
  "`--strip-conflict-markers' drops only the marker lines, keeping both sides;
`--conflict-marked?' detects markers in any text."
  (let ((text "<<<<<<< HEAD\nline-a\n=======\nline-b\n>>>>>>> other\n"))
    (should (equal "line-a\nline-b\n" (org-glance--strip-conflict-markers text)))
    (should (org-glance--conflict-marked? text))
    (should-not (org-glance--conflict-marked? "line-a\nline-b\n"))))

(ert-deftest org-glance-test:conflict-resolve-gate ()
  "`--resolve-conflict' returns the resolver's value under `union' silently.
`ask' prompts first; a declined prompt or a nil policy errors without running."
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
  "`--heal-eld' heals a conflicted file with any caller-supplied merge-fn.
It rewrites marker-free and returns the merged value, or a clean file's form."
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

(ert-deftest org-glance-test:eld-write-survives-a-torn-write ()
  "A sidecar write that dies mid-way leaves the previous .eld intact: the bytes
land in a temp file renamed over PATH (invariant 2)."
  (with-temp-directory dir
    (let ((path (f-join dir "config" "x.eld"))
          (real (symbol-function 'write-region)))
      (org-glance--write-eld path '((a . 1)))
      (cl-letf (((symbol-function 'write-region)
                 (lambda (start end filename &rest args)
                   (let ((text (if (stringp start) start (buffer-substring start end))))
                     (apply real (substring text 0 (min 3 (length text))) nil filename args))
                   (error "Simulated crash mid-write"))))
        (should-error (org-glance--write-eld path '((a . 2) (b . 3)))))
      (should (equal '((a . 1)) (org-glance--read-eld path)))
      (should-not (directory-files (f-dirname path) nil "\\.tmp\\.")))))

(provide 'test-merge)
;;; test-merge.el ends here
