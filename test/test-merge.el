;;; test-merge.el --- Git-sync merge/self-heal tests for the metadata store  -*- lexical-binding: t -*-

;; Covers the store's behaviour when its meta/ dir is synced across machines
;; via git: the `merge=union' driver on *.jsonl, self-healing a MANIFEST left
;; conflict-marked by git's default merge, positional last-wins reads over a
;; union-merged open segment, and adopting segments sealed on two machines.

(require 'test-helpers)

(cl-defun org-glance-test-merge:reopen (graph)
  "Drop GRAPH from the instance cache and re-open it, re-running construction."
  (let ((dir (org-glance-graph:directory graph)))
    (remhash dir org-glance-graph:list)
    (org-glance-graph dir)))

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

(ert-deftest org-glance-test:merge-gitattributes-written ()
  "Store construction writes meta/.gitattributes with exactly the union driver
line, and a second open never clobbers a hand-edited file (write-if-absent)."
  (org-glance-test:with-graph graph
    (let ((path (f-join (org-glance-graph:meta-path graph) ".gitattributes")))
      (should (f-exists? path))
      (should (string= "*.jsonl merge=union\n" (f-read-text path 'utf-8)))
      ;; hand-edit, then reopen: the existing file must survive untouched
      (f-write-text "*.jsonl merge=union\n# local override\n" 'utf-8 path)
      (let ((graph (org-glance-test-merge:reopen graph)))
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
      (let ((graph (org-glance-test-merge:reopen graph)))
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
    (let ((graph (org-glance-test-merge:reopen graph)))
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

(provide 'test-merge)
;;; test-merge.el ends here
