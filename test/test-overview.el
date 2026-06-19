;;; test-overview.el --- Tests for the graph-backed overview  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:overview-render ()
  "Overview renders one heading per live headline, with state/tags/planning/id."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "o1" "* TODO Alpha :work:" "SCHEDULED: <2025-01-10 Fri>")
                             (org-glance-test:headline "o2" "* Beta :home:"))
    (let ((text (org-glance-overview:render graph)))
      (should (s-contains? "* TODO Alpha" text))
      (should (s-contains? ":work:" text))
      (should (s-contains? "SCHEDULED: <2025-01-10 Fri>" text))
      (should (s-contains? ":ORG_GLANCE_ID: o1" text))
      (should (s-contains? "* Beta" text))
      (should (s-contains? ":ORG_GLANCE_ID: o2" text)))))

(ert-deftest org-glance-test:overview-render-tag-filter ()
  "A tag argument restricts the overview to matching headlines."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "o1" "* Alpha :work:")
                             (org-glance-test:headline "o2" "* Beta :home:"))
    (let ((text (org-glance-overview:render graph 'work)))
      (should (s-contains? ":ORG_GLANCE_ID: o1" text))
      (should (not (s-contains? ":ORG_GLANCE_ID: o2" text))))))

(ert-deftest org-glance-test:overview-id-at-point ()
  "The headline id under point is read from its ORG_GLANCE_ID property."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "o1" "* Alpha :work:"))
    (with-temp-buffer
      (insert (org-glance-overview:render graph))
      (delay-mode-hooks (org-mode))
      (goto-char (point-min))
      (re-search-forward "Alpha")
      (should (string= "o1" (org-glance-overview:id-at-point))))))

(ert-deftest org-glance-test:overview-materialize-at-point ()
  "Materializing from the overview opens the headline under point."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "o1" "* TODO Alpha"))
    (let ((org-glance-graph graph) (opened nil))
      (with-temp-buffer
        (insert (org-glance-overview:render graph))
        (delay-mode-hooks (org-mode))
        (goto-char (point-min))
        (re-search-forward "Alpha")
        (cl-letf (((symbol-function 'switch-to-buffer) (lambda (b &rest _) (setq opened b) b)))
          (org-glance-overview:materialize)))
      (should (bufferp opened)))))

(ert-deftest org-glance-test:overview-write-file ()
  "The overview is materialized to a file under the store."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "o1" "* Alpha"))
    (let ((file (org-glance-overview:write graph)))
      (should (f-exists? file))
      (should (string= file (org-glance-overview:file graph)))
      (should (s-contains? ":ORG_GLANCE_ID: o1" (f-read-text file 'utf-8))))))

;;; Filtering

(ert-deftest org-glance-test:overview-filter-by-tag ()
  "A plist `(:tags ...)' filter and the legacy bare-tag shim both restrict by tag."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "o1" "* Alpha :work:")
                             (org-glance-test:headline "o2" "* Beta :home:"))
    ;; Plist, bare symbol, bare string, and (since tags are stored downcased)
    ;; mixed-case spellings all match.
    (dolist (filter (list '(:tags ("work")) 'work "work" 'Work "WORK" '(:tags ("Work"))))
      (let ((text (org-glance-overview:render graph filter)))
        (should (s-contains? ":ORG_GLANCE_ID: o1" text))
        (should-not (s-contains? ":ORG_GLANCE_ID: o2" text))))))

(ert-deftest org-glance-test:overview-filter-by-state ()
  "`:state'/`:done' filter on todo state; `(:state nil)' differs from omitting it.
No `org-done-keywords' binding here: the `:done' clause derives the done set
itself, so the filter is correct even outside an Org buffer (the bug the review
caught)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "o1" "* TODO Alpha")
                             (org-glance-test:headline "o2" "* DONE Beta")
                             (org-glance-test:headline "o3" "* Gamma"))
    (cl-flet ((n (filter)
                (length (seq-filter (org-glance-filter:predicate filter)
                                    (org-glance-graph:headlines graph)))))
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
    (org-glance-graph:add graph
                             (org-glance-test:headline "o1" "* TODO Alpha")
                             (org-glance-test:headline "o2" "* DONE Beta"))
    (cl-flet ((ids (filter)
                (mapcar #'org-glance-headline-metadata:id
                        (seq-filter (org-glance-filter:predicate filter)
                                    (org-glance-graph:headlines graph)))))
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
    (should-not (string= (org-glance-overview:spec-key '(:done t))
                         (org-glance-overview:spec-key '(:done t :done-keywords ("TODO")))))
    ;; ... but folds to "all" (no-op) without :done.
    (should (string= "all" (org-glance-overview:spec-key '(:done-keywords ("TODO")))))))

(ert-deftest org-glance-test:overview-filter-by-field ()
  "Arbitrary metadata fields compose with AND (here: priority + linked?)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "o1" "* [#A] Alpha" "- [[https://example.com][L]]")
                             (org-glance-test:headline "o2" "* [#B] Beta"))
    (let ((kept (seq-filter (org-glance-filter:predicate '(:priority 65 :linked t))
                            (org-glance-graph:headlines graph))))
      (should (= 1 (length kept)))
      (should (string= "o1" (org-glance-headline-metadata:id (car kept)))))))

(ert-deftest org-glance-test:overview-filter-where ()
  "A raw `:where' predicate filters, and ANDs with declarative clauses."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "o1" "* Alpha :work:")
                             (org-glance-test:headline "o2" "* Beta :work:")
                             (org-glance-test:headline "o3" "* Gamma :home:"))
    (let ((only-o1 (list :where (lambda (m) (string= "o1" (org-glance-headline-metadata:id m))))))
      (should (= 1 (length (seq-filter (org-glance-filter:predicate only-o1)
                                       (org-glance-graph:headlines graph)))))
      ;; :where ANDed with :tags -- o1 matches both, o3 fails the tag, o2 fails :where
      (let ((spec (append only-o1 '(:tags ("work")))))
        (should (= 1 (length (seq-filter (org-glance-filter:predicate spec)
                                         (org-glance-graph:headlines graph)))))))))

(ert-deftest org-glance-test:overview-filter-rejects-unknown-key ()
  "An unrecognised filter key errors loudly rather than silently matching all."
  (should-error (org-glance-filter:predicate '(:bogus 1)))
  (should-error (org-glance-overview:spec-key '(:bogus 1))))

;;; Cache keys

(ert-deftest org-glance-test:overview-spec-key-stable ()
  "Cache keys are order-independent, filesystem-safe, and signal uncacheability."
  ;; key/tag order independence
  (should (string= (org-glance-overview:spec-key '(:tags ("b" "a") :state "TODO"))
                   (org-glance-overview:spec-key '(:state "TODO" :tags ("a" "b")))))
  ;; :tag folds into :tags
  (should (string= (org-glance-overview:spec-key '(:tag "a"))
                   (org-glance-overview:spec-key '(:tags ("a")))))
  ;; empty filter (and an empty tag list) is "all"; :where is uncacheable
  (should (string= "all" (org-glance-overview:spec-key nil)))
  (should (string= "all" (org-glance-overview:spec-key '(:tags nil))))
  (should-not (org-glance-overview:spec-key (list :where #'ignore)))
  ;; a path-hostile value never escapes its segment
  (let ((key (org-glance-overview:spec-key '(:id "a/../b"))))
    (should-not (s-contains? "/" key)))
  ;; distinct specs MAY share a (lossy, hashed) key -- correctness is enforced
  ;; by the SPEC sidecar (see overview-cache-collision-rebuilds), but their
  ;; identities must always differ
  (should-not (string= (org-glance-filter:identity '(:id "a" :title "b"))
                       (org-glance-filter:identity '(:id "a&title=b"))))
  (should-not (string= (org-glance-filter:identity '(:tags ("a" "b")))
                       (org-glance-filter:identity '(:tags ("a,b"))))))

(ert-deftest org-glance-test:overview-spec-key-compact ()
  "Cache directory names are short hashes of the canonical identity: fixed
width, lowercase hex, whatever the filter carries.  The readable filter lives
in the SPEC sidecar, not in the name."
  ;; canonical: the bare-tag shorthand and the full form share a key
  (should (string= (org-glance-overview:spec-key 'task)
                   (org-glance-overview:spec-key '(:tags ("task")))))
  ;; fixed-width lowercase hex -- non-ASCII, whitespace, overlong values and
  ;; path-hostile characters all hash away
  (dolist (filter (list '(:tags ("task"))
                        '(:tags ("work") :state "TODO")
                        '(:title-contains "Приготовить ужин")
                        (list :title (make-string 200 ?x))
                        '(:id "a/../b")))
    (should (string-match-p "\\`[0-9a-f]\\{12\\}\\'"
                            (org-glance-overview:spec-key filter))))
  ;; the hash is of the IDENTITY, so value boundaries survive: specs the old
  ;; readable slug conflated get distinct keys
  (should-not (string= (org-glance-overview:spec-key '(:tags ("a" "b")))
                       (org-glance-overview:spec-key '(:tags ("a,b")))))
  (should-not (string= (org-glance-overview:spec-key '(:id "a" :title "b"))
                       (org-glance-overview:spec-key '(:id "a&title=b")))))

(ert-deftest org-glance-test:overview-cache-collision-rebuilds ()
  "Two distinct specs that share a directory name never serve each other's
content: the SPEC sidecar mismatch forces a rebuild, and re-requesting the
owning spec hits its cache again.  Hash keys make a real collision
astronomically rare, so one is forced here by pinning `spec-key'."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (let ((s1 '(:id "a" :title "b"))
          (s2 '(:id "a&title=b")))
      (cl-letf (((symbol-function 'org-glance-overview:spec-key)
                 (lambda (_filter) "deadbeef0000")))
        ;; same directory name, different identities
        (should (string= (org-glance-overview:spec-key s1)
                         (org-glance-overview:spec-key s2)))
        (org-glance-overview:cached-file graph s1)             ; s1 owns the dir
        (set-file-times (org-glance-graph:headline-meta-path graph)
                        (time-subtract (current-time) 100))       ; cache is fresh
        (let ((renders 0))
          (cl-letf (((symbol-function 'org-glance-overview:render)
                     ;; a real render always starts with the header -- a
                     ;; headerless cache would be rejected as outdated
                     (lambda (&rest _) (cl-incf renders) org-glance-overview:header)))
            (org-glance-overview:cached-file graph s1)         ; owner -> hit
            (should (= 0 renders))
            (org-glance-overview:cached-file graph s2)         ; intruder -> rebuild
            (should (= 1 renders))
            (org-glance-overview:cached-file graph s2)         ; now s2 owns -> hit
            (should (= 1 renders))))))))

(ert-deftest org-glance-test:overview-cache-outdated-header-rebuilds ()
  "A cache written by an older org-glance (pre-rename `-v2' prop-line) is
rebuilt even when mtime-fresh -- its prop-line would enable a mode that no
longer exists."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* Alpha"))
    (let ((file (org-glance-overview:cached-file graph '(:tags ("work")))))
      ;; Simulate the pre-rename cache: same content, old prop-line mode.
      (f-write-text (s-replace "mode: org-glance-overview"
                               "mode: org-glance-overview-v2"
                               (f-read-text file 'utf-8))
                    'utf-8 file)
      (set-file-times (org-glance-graph:headline-meta-path graph)
                      (time-subtract (current-time) 100))      ; cache is fresh
      (should-not (org-glance-overview--header-current? file))
      (should (s-contains? "mode: org-glance-overview -*-"
                           (f-read-text (org-glance-overview:cached-file graph '(:tags ("work")))
                                        'utf-8))))))

;;; Cache freshness + invalidation

(ert-deftest org-glance-test:overview-fresh-p ()
  "`:fresh?' is mtime-based against headlines.jsonl, biased to rebuild."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "o1" "* Alpha"))
    (let ((file (org-glance-overview:write graph))
          (src (org-glance-graph:headline-meta-path graph)))
      (set-file-times src (time-subtract (current-time) 100))
      (should (org-glance-overview:fresh? graph file))
      (set-file-times src (time-add (current-time) 100))
      (should-not (org-glance-overview:fresh? graph file))
      ;; a missing cache file is never fresh
      (should-not (org-glance-overview:fresh? graph (f-join dir "absent.org"))))))

(ert-deftest org-glance-test:overview-cache-hit-skips-render ()
  "A fresh cache is served without re-reading the graph or re-rendering."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "o1" "* Alpha"))
    (let ((file (org-glance-overview:cached-file graph)))   ; warm
      (set-file-times (org-glance-graph:headline-meta-path graph)
                      (time-subtract (current-time) 100))
      (let ((renders 0))
        (cl-letf (((symbol-function 'org-glance-overview:render)
                   (lambda (&rest _) (cl-incf renders) "")))
          (should (string= file (org-glance-overview:cached-file graph)))
          (should (= 0 renders)))))))

(ert-deftest org-glance-test:overview-cache-invalidated-by-add ()
  "Adding a headline bumps headlines.jsonl, so the next access rebuilds."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "o1" "* Alpha"))
    (org-glance-overview:cached-file graph)                 ; warm
    (org-glance-graph:add graph (org-glance-test:headline "o2" "* Beta"))
    (set-file-times (org-glance-graph:headline-meta-path graph)
                    (time-add (current-time) 100))             ; guarantee staleness
    (let ((file (org-glance-overview:cached-file graph)))
      (should (s-contains? ":ORG_GLANCE_ID: o2" (f-read-text file 'utf-8))))))

(ert-deftest org-glance-test:overview-where-uncacheable ()
  "A `:where' filter renders to a transient file every time, never the cache."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "o1" "* Alpha")
                             (org-glance-test:headline "o2" "* Beta"))
    (let* ((spec (list :where (lambda (m) (string= "o1" (org-glance-headline-metadata:id m)))))
           (file (org-glance-overview:cached-file graph spec)))
      (should (s-contains? ":ORG_GLANCE_ID: o1" (f-read-text file 'utf-8)))
      (should-not (s-contains? ":ORG_GLANCE_ID: o2" (f-read-text file 'utf-8)))
      ;; it must NOT reuse the unfiltered overview file
      (should-not (string= file (org-glance-overview:file graph)))
      ;; and it re-renders even when the graph is unchanged
      (set-file-times (org-glance-graph:headline-meta-path graph)
                      (time-subtract (current-time) 100))
      (let ((renders 0))
        (cl-letf (((symbol-function 'org-glance-overview:render)
                   (lambda (&rest _) (cl-incf renders) "")))
          (org-glance-overview:cached-file graph spec)
          (should (= 1 renders)))))))

(ert-deftest org-glance-test:overview-filtered-uses-separate-dir ()
  "Filtered overviews live in their own directory; unfiltered stays at overview.org."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "o1" "* Alpha :work:"))
    (let ((filtered (org-glance-overview:cached-file graph '(:tags ("work")))))
      (should (s-contains? (org-glance-overview:cache-path graph) filtered))
      (should (f-exists? filtered))
      (should-not (string= filtered (org-glance-overview:file graph))))
    (should (string= (org-glance-overview:cached-file graph)
                     (org-glance-overview:file graph)))))

(ert-deftest org-glance-test:overview-open-stale-headline-errors ()
  "Acting on a heading whose headline was deleted gives a user-error, not a crash."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "o1" "* Alpha"))
    (let ((org-glance-graph graph)
          (snapshot (org-glance-overview:render graph)))   ; rendered while o1 was live
      (org-glance-graph:delete graph "o1")                 ; now the snapshot is stale
      (with-temp-buffer
        (insert snapshot)
        (org-glance--org-mode)
        (goto-char (point-min))
        (re-search-forward "Alpha")
        (should-error (org-glance-overview:open) :type 'user-error)
        (should-error (org-glance-overview:extract) :type 'user-error)))))

(ert-deftest org-glance-test:overview-filter-title-contains ()
  "`:title-contains' matches case-insensitively and composes with other clauses;
spellings differing only in case share one cache key, distinct from `:title'."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "o1" "* TODO Pay electricity bill :home:")
                             (org-glance-test:headline "o2" "* TODO Read a Bill of materials :work:")
                             (org-glance-test:headline "o3" "* Walk :home:"))
    (cl-flet ((ids (filter)
                (mapcar #'org-glance-headline-metadata:id
                        (seq-filter (org-glance-filter:predicate filter)
                                    (org-glance-graph:headlines graph)))))
      (should (equal '("o1" "o2") (ids '(:title-contains "BILL"))))
      (should (equal '("o1") (ids '(:title-contains "bill" :tags ("home")))))))
  (should (string= (org-glance-overview:spec-key '(:title-contains "Bill"))
                   (org-glance-overview:spec-key '(:title-contains "bill"))))
  (should-not (string= (org-glance-overview:spec-key '(:title-contains "bill"))
                       (org-glance-overview:spec-key '(:title "bill")))))

(ert-deftest org-glance-test:overview-filter-refinement ()
  "The `/' refinement commands compose a clause onto the CURRENT buffer's
filter (same dimension replaces), and `clear' returns to the unfiltered view."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "o1" "* TODO Alpha :work:"))
    (let ((org-glance-graph graph)
          (visited 'unset))
      (cl-letf (((symbol-function 'org-glance-overview:visit)
                 (lambda (_graph filter) (setq visited filter))))
        (with-temp-buffer
          (setq-local org-glance-overview--spec '(:tags ("work")))
          ;; narrow by state: composes onto the tag filter
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "TODO")))
            (org-glance-overview:filter-by-state))
          (should (equal '(:tags ("work") :state "TODO") visited))
          ;; narrow by substring
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "alp")))
            (org-glance-overview:filter-by-substring))
          (should (equal '(:tags ("work") :title-contains "alp") visited))
          ;; re-filtering the same dimension REPLACES it
          (setq-local org-glance-overview--spec '(:state "TODO"))
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "DONE")))
            (org-glance-overview:filter-by-state))
          (should (equal '(:state "DONE") visited))
          ;; empty input aborts instead of filtering
          (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
            (should-error (org-glance-overview:filter-by-substring) :type 'user-error))
          ;; clear -> unfiltered
          (org-glance-overview:filter-clear)
          (should (null visited)))))))

(ert-deftest org-glance-test:overview-tags ()
  "Tag candidates are the distinct, sorted tags of the graph's live headlines."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "o1" "* Alpha :work:")
                             (org-glance-test:headline "o2" "* Beta :home:work:")
                             (org-glance-test:headline "o3" "* Gamma"))
    (should (equal '("home" "work") (org-glance-overview:tags graph)))))

(ert-deftest org-glance-test:overview-interactive-tag-filter ()
  "Invoking the overview interactively prompts for a tag and overlays it on the
ambient `org-glance-filter-spec' (so the overview honours the global filter);
empty input means just the ambient filter."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "o1" "* Alpha :work:"))
    (let ((org-glance-graph graph)
          (org-glance-filter-spec '(:done nil))   ; the active default
          (visited 'unset))
      (cl-letf (((symbol-function 'org-glance-overview:visit)
                 (lambda (_graph filter) (setq visited filter)))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "work")))
        (call-interactively #'org-glance-overview)
        ;; tag overlaid on the ambient (active) filter
        (should (equal '(:done nil :tags ("work")) visited)))
      ;; empty input -> just the ambient filter
      (cl-letf (((symbol-function 'org-glance-overview:visit)
                 (lambda (_graph filter) (setq visited filter)))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "")))
        (call-interactively #'org-glance-overview)
        (should (equal '(:done nil) visited)))
      ;; a cleared ambient filter falls back to the bare tag / nil
      (let ((org-glance-filter-spec nil))
        (cl-letf (((symbol-function 'org-glance-overview:visit)
                   (lambda (_graph filter) (setq visited filter)))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _) "work")))
          (call-interactively #'org-glance-overview)
          (should (equal '(:tags ("work")) visited)))))))

;;; View coherence: no overview may show outdated results

(ert-deftest org-glance-test:overview-live-update-on-save ()
  "A materialized save patches every open overview buffer: in place when the
headline still matches the buffer's filter, dropping it when it no longer does."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "a1" "* TODO Alpha :work:")
                             (org-glance-test:headline "b1" "* TODO Beta :work:"))
    (let* ((org-glance-graph graph)
           (all (org-glance-overview:visit graph nil))
           (todo (org-glance-overview:visit graph '(:state "TODO"))))
      (unwind-protect
          (progn
            (should (s-contains? "TODO Alpha" (with-current-buffer all (buffer-string))))
            (should (s-contains? "TODO Alpha" (with-current-buffer todo (buffer-string))))
            ;; simulate editing Alpha via materialization + save
            (with-temp-buffer
              (insert "* DONE Alpha :work:\n:PROPERTIES:\n:ORG_GLANCE_ID: a1\n:END:\n")
              (setq-local org-glance-material--graph graph
                          org-glance-material--id "a1")
              (org-glance-material:sync))
            ;; "all": patched in place, persisted, not left modified
            (with-current-buffer all
              (should (s-contains? "DONE Alpha" (buffer-string)))
              (should-not (s-contains? "TODO Alpha" (buffer-string)))
              (should-not (buffer-modified-p))
              (should (s-contains? "DONE Alpha" (f-read-text buffer-file-name 'utf-8))))
            ;; state=TODO: Alpha no longer matches -> dropped; Beta untouched
            (with-current-buffer todo
              (should-not (s-contains? "Alpha" (buffer-string)))
              (should (s-contains? "TODO Beta" (buffer-string)))))
        (kill-buffer all)
        (kill-buffer todo)))))

(ert-deftest org-glance-test:overview-live-update-newly-matching ()
  "A save that makes a headline newly match a filtered overview rebuilds it."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a1" "* TODO Alpha"))
    (let* ((org-glance-graph graph)
           (done (org-glance-overview:visit graph '(:state "DONE"))))
      (unwind-protect
          (progn
            (should-not (s-contains? "Alpha" (with-current-buffer done (buffer-string))))
            (with-temp-buffer
              (insert "* DONE Alpha\n:PROPERTIES:\n:ORG_GLANCE_ID: a1\n:END:\n")
              (setq-local org-glance-material--graph graph
                          org-glance-material--id "a1")
              (org-glance-material:sync))
            (with-current-buffer done
              (should (s-contains? "DONE Alpha" (buffer-string)))))
        (kill-buffer done)))))

(ert-deftest org-glance-test:overview-stale-buffer-refreshes-on-display ()
  "The lazy net: an overview made stale by any other store mutation rebuilds
when it is (re)displayed or selected."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a1" "* Alpha"))
    (let* ((org-glance-graph graph)
           (all (org-glance-overview:visit graph nil)))
      (unwind-protect
          (progn
            ;; mutate the store BEHIND the views (no materialized save involved)
            (org-glance-graph:add graph (org-glance-test:headline "b1" "* Beta"))
            (set-file-times (org-glance-graph:headline-meta-path graph)
                            (time-add (current-time) 100)) ; guarantee staleness
            (should-not (s-contains? "Beta" (with-current-buffer all (buffer-string))))
            (with-current-buffer all
              (org-glance-overview--refresh-when-stale))
            (should (s-contains? "Beta" (with-current-buffer all (buffer-string)))))
        (kill-buffer all)))))

;;; Keymap

(ert-deftest org-glance-test:overview-keymap-bindings ()
  "The overview keymap mirrors v1 navigation (n/p heading, f/b sibling) + actions."
  (let ((map org-glance-overview-mode-map))
    (should (eq (lookup-key map (kbd "n")) #'org-next-visible-heading))
    (should (eq (lookup-key map (kbd "p")) #'org-previous-visible-heading))
    (should (eq (lookup-key map (kbd "f")) #'org-forward-heading-same-level))
    (should (eq (lookup-key map (kbd "b")) #'org-backward-heading-same-level))
    (should (eq (lookup-key map (kbd ",")) #'beginning-of-buffer))
    (should (eq (lookup-key map (kbd ".")) #'end-of-buffer))
    (should (eq (lookup-key map (kbd "RET")) #'org-glance-overview:materialize))
    (should (eq (lookup-key map (kbd "m")) #'org-glance-overview:materialize))
    (should (eq (lookup-key map (kbd "o")) #'org-glance-overview:open))
    (should (eq (lookup-key map (kbd "e")) #'org-glance-overview:extract))
    (should (eq (lookup-key map (kbd "a")) #'org-glance-agenda))
    (should (eq (lookup-key map (kbd "g")) #'org-glance-overview:refresh))
    (should (eq (lookup-key map (kbd "/")) #'org-glance-overview-filter))
    (should (eq (lookup-key map (kbd "q")) #'quit-window))))

(provide 'test-overview)
;;; test-overview.el ends here
