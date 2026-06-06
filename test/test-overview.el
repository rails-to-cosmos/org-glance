;;; test-overview.el --- Tests for the v2 graph-backed overview  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:overview-render ()
  "Overview renders one heading per live headline, with state/tags/planning/id."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "o1" "* TODO Alpha :work:" "SCHEDULED: <2025-01-10 Fri>")
                             (org-glance-test:headline "o2" "* Beta :home:"))
    (let ((text (org-glance-overview-v2:render graph)))
      (should (s-contains? "* TODO Alpha" text))
      (should (s-contains? ":work:" text))
      (should (s-contains? "SCHEDULED: <2025-01-10 Fri>" text))
      (should (s-contains? ":ORG_GLANCE_ID: o1" text))
      (should (s-contains? "* Beta" text))
      (should (s-contains? ":ORG_GLANCE_ID: o2" text)))))

(ert-deftest org-glance-test:overview-render-tag-filter ()
  "A tag argument restricts the overview to matching headlines."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "o1" "* Alpha :work:")
                             (org-glance-test:headline "o2" "* Beta :home:"))
    (let ((text (org-glance-overview-v2:render graph 'work)))
      (should (s-contains? ":ORG_GLANCE_ID: o1" text))
      (should (not (s-contains? ":ORG_GLANCE_ID: o2" text))))))

(ert-deftest org-glance-test:overview-id-at-point ()
  "The headline id under point is read from its ORG_GLANCE_ID property."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "o1" "* Alpha :work:"))
    (with-temp-buffer
      (insert (org-glance-overview-v2:render graph))
      (delay-mode-hooks (org-mode))
      (goto-char (point-min))
      (re-search-forward "Alpha")
      (should (string= "o1" (org-glance-overview-v2:id-at-point))))))

(ert-deftest org-glance-test:overview-materialize-at-point ()
  "Materializing from the overview opens the headline under point."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "o1" "* TODO Alpha"))
    (let ((org-glance-graph-v2 graph) (opened nil))
      (with-temp-buffer
        (insert (org-glance-overview-v2:render graph))
        (delay-mode-hooks (org-mode))
        (goto-char (point-min))
        (re-search-forward "Alpha")
        (cl-letf (((symbol-function 'switch-to-buffer) (lambda (b &rest _) (setq opened b) b)))
          (org-glance-overview-v2:materialize)))
      (should (bufferp opened)))))

(ert-deftest org-glance-test:overview-write-file ()
  "The overview is materialized to a file under the store."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "o1" "* Alpha"))
    (let ((file (org-glance-overview-v2:write graph)))
      (should (f-exists? file))
      (should (string= file (org-glance-overview-v2:file graph)))
      (should (s-contains? ":ORG_GLANCE_ID: o1" (f-read-text file 'utf-8))))))

;;; Filtering

(ert-deftest org-glance-test:overview-filter-by-tag ()
  "A plist `(:tags ...)' filter and the legacy bare-tag shim both restrict by tag."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "o1" "* Alpha :work:")
                             (org-glance-test:headline "o2" "* Beta :home:"))
    ;; Plist, bare symbol, bare string, and (since tags are stored downcased)
    ;; mixed-case spellings all match.
    (dolist (filter (list '(:tags ("work")) 'work "work" 'Work "WORK" '(:tags ("Work"))))
      (let ((text (org-glance-overview-v2:render graph filter)))
        (should (s-contains? ":ORG_GLANCE_ID: o1" text))
        (should-not (s-contains? ":ORG_GLANCE_ID: o2" text))))))

(ert-deftest org-glance-test:overview-filter-by-state ()
  "`:state'/`:done' filter on todo state; `(:state nil)' differs from omitting it.
No `org-done-keywords' binding here: the `:done' clause derives the done set
itself, so the filter is correct even outside an Org buffer (the bug the review
caught)."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "o1" "* TODO Alpha")
                             (org-glance-test:headline "o2" "* DONE Beta")
                             (org-glance-test:headline "o3" "* Gamma"))
    (cl-flet ((n (filter)
                (length (seq-filter (org-glance-overview-v2:spec-predicate filter)
                                    (org-glance-graph-v2:headlines graph)))))
      (should (= 1 (n '(:state "TODO"))))
      (should (= 1 (n '(:done t))))
      (should (= 2 (n '(:done nil))))
      ;; (:state nil) keeps only the stateless headline ...
      (should (= 1 (n '(:state nil))))
      ;; ... whereas omitting :state keeps everything.
      (should (= 3 (n nil))))))

(ert-deftest org-glance-test:overview-done-keywords-per-overview ()
  "A `:done-keywords' clause redefines \"done\" for that overview only."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "o1" "* TODO Alpha")
                             (org-glance-test:headline "o2" "* DONE Beta"))
    (cl-flet ((ids (filter)
                (mapcar #'org-glance-headline-metadata-v2:id
                        (seq-filter (org-glance-overview-v2:spec-predicate filter)
                                    (org-glance-graph-v2:headlines graph)))))
      ;; Default: DONE is done.
      (should (equal '("o2") (ids '(:done t))))
      (should (equal '("o1") (ids '(:done nil))))
      ;; This overview declares TODO (not DONE) to be done.
      (should (equal '("o1") (ids '(:done t :done-keywords ("TODO")))))
      (should (equal '("o2") (ids '(:done nil :done-keywords ("TODO")))))
      ;; Per-overview :done-keywords wins over the ambient `org-done-keywords'.
      (let ((org-done-keywords '("DONE")))
        (should (equal '("o1") (ids '(:done t :done-keywords ("TODO")))))))
    ;; :done-keywords participates in the cache key (distinct overviews) ...
    (should-not (string= (org-glance-overview-v2:spec-key '(:done t))
                         (org-glance-overview-v2:spec-key '(:done t :done-keywords ("TODO")))))
    ;; ... but folds to "all" (no-op) without :done.
    (should (string= "all" (org-glance-overview-v2:spec-key '(:done-keywords ("TODO")))))))

(ert-deftest org-glance-test:overview-filter-by-field ()
  "Arbitrary metadata fields compose with AND (here: priority + linked?)."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "o1" "* [#A] Alpha" "- [[https://example.com][L]]")
                             (org-glance-test:headline "o2" "* [#B] Beta"))
    (let ((kept (seq-filter (org-glance-overview-v2:spec-predicate '(:priority 65 :linked t))
                            (org-glance-graph-v2:headlines graph))))
      (should (= 1 (length kept)))
      (should (string= "o1" (org-glance-headline-metadata-v2:id (car kept)))))))

(ert-deftest org-glance-test:overview-filter-where ()
  "A raw `:where' predicate filters, and ANDs with declarative clauses."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "o1" "* Alpha :work:")
                             (org-glance-test:headline "o2" "* Beta :work:")
                             (org-glance-test:headline "o3" "* Gamma :home:"))
    (let ((only-o1 (list :where (lambda (m) (string= "o1" (org-glance-headline-metadata-v2:id m))))))
      (should (= 1 (length (seq-filter (org-glance-overview-v2:spec-predicate only-o1)
                                       (org-glance-graph-v2:headlines graph)))))
      ;; :where ANDed with :tags -- o1 matches both, o3 fails the tag, o2 fails :where
      (let ((spec (append only-o1 '(:tags ("work")))))
        (should (= 1 (length (seq-filter (org-glance-overview-v2:spec-predicate spec)
                                         (org-glance-graph-v2:headlines graph)))))))))

(ert-deftest org-glance-test:overview-filter-rejects-unknown-key ()
  "An unrecognised filter key errors loudly rather than silently matching all."
  (should-error (org-glance-overview-v2:spec-predicate '(:bogus 1)))
  (should-error (org-glance-overview-v2:spec-key '(:bogus 1))))

;;; Cache keys

(ert-deftest org-glance-test:overview-spec-key-stable ()
  "Cache keys are order-independent, filesystem-safe, and signal uncacheability."
  ;; key/tag order independence
  (should (string= (org-glance-overview-v2:spec-key '(:tags ("b" "a") :state "TODO"))
                   (org-glance-overview-v2:spec-key '(:state "TODO" :tags ("a" "b")))))
  ;; :tag folds into :tags
  (should (string= (org-glance-overview-v2:spec-key '(:tag "a"))
                   (org-glance-overview-v2:spec-key '(:tags ("a")))))
  ;; empty filter (and an empty tag list) is "all"; :where is uncacheable
  (should (string= "all" (org-glance-overview-v2:spec-key nil)))
  (should (string= "all" (org-glance-overview-v2:spec-key '(:tags nil))))
  (should-not (org-glance-overview-v2:spec-key (list :where #'ignore)))
  ;; a path-hostile value never escapes its segment
  (let ((key (org-glance-overview-v2:spec-key '(:id "a/../b"))))
    (should-not (s-contains? "/" key))
    (should (string-match-p "-[0-9a-f]\\{40\\}$" key)))
  ;; distinct specs must NOT collide even when values contain the k=v / & / ,
  ;; delimiters used to build the readable slug
  (should-not (string= (org-glance-overview-v2:spec-key '(:id "a" :title "b"))
                       (org-glance-overview-v2:spec-key '(:id "a&title=b"))))
  (should-not (string= (org-glance-overview-v2:spec-key '(:tags ("a" "b")))
                       (org-glance-overview-v2:spec-key '(:tags ("a,b"))))))

;;; Cache freshness + invalidation

(ert-deftest org-glance-test:overview-fresh-p ()
  "`:fresh?' is mtime-based against headlines.jsonl, biased to rebuild."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "o1" "* Alpha"))
    (let ((file (org-glance-overview-v2:write graph))
          (src (org-glance-graph-v2:headline-meta-path graph)))
      (set-file-times src (time-subtract (current-time) 100))
      (should (org-glance-overview-v2:fresh? graph file))
      (set-file-times src (time-add (current-time) 100))
      (should-not (org-glance-overview-v2:fresh? graph file))
      ;; a missing cache file is never fresh
      (should-not (org-glance-overview-v2:fresh? graph (f-join dir "absent.org"))))))

(ert-deftest org-glance-test:overview-cache-hit-skips-render ()
  "A fresh cache is served without re-reading the graph or re-rendering."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "o1" "* Alpha"))
    (let ((file (org-glance-overview-v2:cached-file graph)))   ; warm
      (set-file-times (org-glance-graph-v2:headline-meta-path graph)
                      (time-subtract (current-time) 100))
      (let ((renders 0))
        (cl-letf (((symbol-function 'org-glance-overview-v2:render)
                   (lambda (&rest _) (cl-incf renders) "")))
          (should (string= file (org-glance-overview-v2:cached-file graph)))
          (should (= 0 renders)))))))

(ert-deftest org-glance-test:overview-cache-invalidated-by-add ()
  "Adding a headline bumps headlines.jsonl, so the next access rebuilds."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "o1" "* Alpha"))
    (org-glance-overview-v2:cached-file graph)                 ; warm
    (org-glance-graph-v2:add graph (org-glance-test:headline "o2" "* Beta"))
    (set-file-times (org-glance-graph-v2:headline-meta-path graph)
                    (time-add (current-time) 100))             ; guarantee staleness
    (let ((file (org-glance-overview-v2:cached-file graph)))
      (should (s-contains? ":ORG_GLANCE_ID: o2" (f-read-text file 'utf-8))))))

(ert-deftest org-glance-test:overview-where-uncacheable ()
  "A `:where' filter renders to a transient file every time, never the cache."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "o1" "* Alpha")
                             (org-glance-test:headline "o2" "* Beta"))
    (let* ((spec (list :where (lambda (m) (string= "o1" (org-glance-headline-metadata-v2:id m)))))
           (file (org-glance-overview-v2:cached-file graph spec)))
      (should (s-contains? ":ORG_GLANCE_ID: o1" (f-read-text file 'utf-8)))
      (should-not (s-contains? ":ORG_GLANCE_ID: o2" (f-read-text file 'utf-8)))
      ;; it must NOT reuse the unfiltered overview file
      (should-not (string= file (org-glance-overview-v2:file graph)))
      ;; and it re-renders even when the graph is unchanged
      (set-file-times (org-glance-graph-v2:headline-meta-path graph)
                      (time-subtract (current-time) 100))
      (let ((renders 0))
        (cl-letf (((symbol-function 'org-glance-overview-v2:render)
                   (lambda (&rest _) (cl-incf renders) "")))
          (org-glance-overview-v2:cached-file graph spec)
          (should (= 1 renders)))))))

(ert-deftest org-glance-test:overview-filtered-uses-separate-dir ()
  "Filtered overviews live in their own directory; unfiltered stays at overview.org."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "o1" "* Alpha :work:"))
    (let ((filtered (org-glance-overview-v2:cached-file graph '(:tags ("work")))))
      (should (s-contains? (org-glance-overview-v2:cache-path graph) filtered))
      (should (f-exists? filtered))
      (should-not (string= filtered (org-glance-overview-v2:file graph))))
    (should (string= (org-glance-overview-v2:cached-file graph)
                     (org-glance-overview-v2:file graph)))))

(ert-deftest org-glance-test:overview-open-stale-headline-errors ()
  "Acting on a heading whose headline was deleted gives a user-error, not a crash."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "o1" "* Alpha"))
    (let ((org-glance-graph-v2 graph)
          (snapshot (org-glance-overview-v2:render graph)))   ; rendered while o1 was live
      (org-glance-graph-v2:delete graph "o1")                 ; now the snapshot is stale
      (with-temp-buffer
        (insert snapshot)
        (org-glance--org-mode)
        (goto-char (point-min))
        (re-search-forward "Alpha")
        (should-error (org-glance-overview-v2:open) :type 'user-error)
        (should-error (org-glance-overview-v2:extract) :type 'user-error)))))

(ert-deftest org-glance-test:overview-v2-tags ()
  "Tag candidates are the distinct, sorted tags of the graph's live headlines."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "o1" "* Alpha :work:")
                             (org-glance-test:headline "o2" "* Beta :home:work:")
                             (org-glance-test:headline "o3" "* Gamma"))
    (should (equal '("home" "work") (org-glance-overview-v2:tags graph)))))

(ert-deftest org-glance-test:overview-interactive-tag-filter ()
  "Invoking the overview interactively prompts for a tag and FILTERS by it
\(regression: the prompted tag never reached the v2 visit); empty input means
the unfiltered overview."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "o1" "* Alpha :work:"))
    (let ((org-glance-graph-v2 graph)
          (org-glance-use-graph-v2 t)
          (visited 'unset))
      (cl-letf (((symbol-function 'org-glance-overview-v2:visit)
                 (lambda (_graph filter) (setq visited filter)))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "work")))
        (call-interactively #'org-glance-overview-v2)
        (should (equal "work" visited))
        ;; the v1 entry point (the C-x j o transient action) dispatches into
        ;; the same prompting path
        (setq visited 'unset)
        (org-glance-overview)
        (should (equal "work" visited)))
      ;; empty input -> unfiltered
      (cl-letf (((symbol-function 'org-glance-overview-v2:visit)
                 (lambda (_graph filter) (setq visited filter)))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "")))
        (call-interactively #'org-glance-overview-v2)
        (should (null visited))))))

;;; Keymap

(ert-deftest org-glance-test:overview-keymap-bindings ()
  "The overview keymap mirrors v1 navigation (n/p heading, f/b sibling) + actions."
  (let ((map org-glance-overview-v2-mode-map))
    (should (eq (lookup-key map (kbd "n")) #'org-next-visible-heading))
    (should (eq (lookup-key map (kbd "p")) #'org-previous-visible-heading))
    (should (eq (lookup-key map (kbd "f")) #'org-forward-heading-same-level))
    (should (eq (lookup-key map (kbd "b")) #'org-backward-heading-same-level))
    (should (eq (lookup-key map (kbd ",")) #'beginning-of-buffer))
    (should (eq (lookup-key map (kbd ".")) #'end-of-buffer))
    (should (eq (lookup-key map (kbd "RET")) #'org-glance-overview-v2:materialize))
    (should (eq (lookup-key map (kbd "m")) #'org-glance-overview-v2:materialize))
    (should (eq (lookup-key map (kbd "o")) #'org-glance-overview-v2:open))
    (should (eq (lookup-key map (kbd "e")) #'org-glance-overview-v2:extract))
    (should (eq (lookup-key map (kbd "a")) #'org-glance-agenda-v2))
    (should (eq (lookup-key map (kbd "g")) #'org-glance-overview-v2:refresh))
    (should (eq (lookup-key map (kbd "q")) #'quit-window))))

(provide 'test-overview)
;;; test-overview.el ends here
