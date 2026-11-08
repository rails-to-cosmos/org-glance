;;; test-tag-config.el --- Shared JSON tag settings -*- lexical-binding: t -*-
(require 'test-helpers)

(cl-defmacro org-glance-test:with-tag-config (target configs &rest body)
  "Run BODY with CONFIGS as JSON tag objects in TARGET, or a temporary graph."
  (declare (indent 2))
  `(with-temp-directory config-root
     (let* ((org-glance-graph (or ,target (org-glance-graph config-root)))
            (root (org-glance-graph:directory org-glance-graph))
            (snapshot (org-glance-config:snapshot org-glance-graph)))
       (org-glance-config-file:write
        root :expected (org-glance-config-file:revision snapshot)
        :change (lambda (object)
                  (let ((tags (make-hash-table :test 'equal)))
                    (dolist (entry ,configs)
                      (puthash (car entry) (json-parse-string (cdr entry)) tags))
                    (puthash "tags" tags object) object)))
       (org-glance-config:refresh org-glance-graph)
       ,@body)))

(cl-defun org-glance-test:one-config (tag contents)
  "Return a one-entry `with-tag-config' alist mapping TAG to CONTENTS."
  (list (cons tag contents)))

(defconst org-glance-test:book-config
  "{\"title\":\"Book\",\"todoCycles\":[{\"active\":[\"TODO\",\"READING\"],\"inactive\":[\"READ\",\"ABANDONED\"]}],\"template\":\"* Book\\n:PROPERTIES:\\n:LOCATION: %^{Where}\\n:END:\\n*** Notes\\n    %?\"}"
  "Full book config: cycle, a property prompt, and a sub-heading skeleton.")

(defconst org-glance-test:book-config-min
  "{\"title\":\"Book\",\"todoCycles\":[{\"active\":[\"TODO\",\"READING\"],\"inactive\":[\"READ\"]}],\"template\":\"* Book\"}"
  "Minimal single-tag book config: cycle only, an empty capture entry.")

(ert-deftest org-glance-test:tag-config-degrades-without-config ()
  "With no config, the capture template is byte-identical to the default."
  (let ((org-glance-graph nil))
    (should (string= "* Hi%?  :task:" (org-glance-capture:template 'task "Hi")))))

(ert-deftest org-glance-test:tag-config-resolve ()
  "A per-tag file resolves to a struct; an unknown tag resolves to nil."
  (org-glance-test:with-tag-config nil (org-glance-test:one-config "book" org-glance-test:book-config)
    (let ((config (org-glance-tag-config:resolve org-glance-graph 'book)))
      (should (org-glance-tag-config? config))
      (should (eq 'book (org-glance-tag-config:tag config)))
      (should (equal "Book" (org-glance-tag-config:title config)))
      (should (equal "TODO READING | READ ABANDONED" (org-glance-tag-config:todo config)))
      (should (s-contains? "Notes" (org-glance-tag-config:template config))))
    (should (null (org-glance-tag-config:resolve org-glance-graph 'nonexistent)))))

(ert-deftest org-glance-test:tag-config-done-keywords ()
  "The done-set is everything after the last `|', derived by org itself."
  (should (equal '("READ" "ABANDONED")
                 (org-glance-tag-config:done-keywords "TODO READING | READ ABANDONED")))
  (should (equal '("DONE") (org-glance-tag-config:done-keywords "TODO | DONE")))
  (should (null (org-glance-tag-config:done-keywords nil)))
  (should (null (org-glance-tag-config:done-keywords ""))))

(ert-deftest org-glance-test:tag-config-active-only-cycle-stays-active ()
  "Legacy active-only cycles retain their meaning in native Org presentation."
  (dolist (cycle '("PLAN" "PLAN |" "PLAN(p!) |"))
    (should-not (member "PLAN" (org-glance-tag-config:done-keywords cycle)))
    (with-temp-buffer
      (insert (org-glance-tag-config--preamble-lines (list (cons "TODO" cycle)))
              "* PLAN Task\n")
      (delay-mode-hooks (org-mode))
      (should (member "PLAN" org-not-done-keywords))
      (should-not (member "PLAN" org-done-keywords))))
  (org-glance-test:with-graph graph
    (org-glance-test:with-tag-config graph
        (org-glance-test:one-config "book" "{\"todoCycles\":[{\"active\":[\"PLAN\"],\"inactive\":[]}],\"template\":\"* Book\"}")
      (org-glance-test:capture graph (org-glance-test:org-with-id "* PLAN Task :book:" "active-only"))
      (org-glance-test:with-material (buffer graph "active-only")
        (should (member "PLAN" org-not-done-keywords))
        (should-not (member "PLAN" org-done-keywords))
        (org-glance-test:save)
        (should-not (string-match-p "#\\+TODO:" (f-read-text (plist-get org-glance-material--base :path) 'utf-8-unix)))
        (goto-char (point-min))
        (let ((org-inhibit-logging t)) (org-todo "DONE"))
        (org-glance-test:save)
        (should (string-prefix-p "* DONE Task" (f-read-text (plist-get org-glance-material--base :path) 'utf-8-unix))))
      (should (equal "PLAN |" (org-glance-tag-config:todo (org-glance-tag-config:resolve graph 'book)))))))

(ert-deftest org-glance-test:tag-config-render-from-config ()
  "Render keeps tag, skeleton and prompts, prepends `#+TODO:', yields one `%?'."
  (org-glance-test:with-tag-config nil (org-glance-test:one-config "book" org-glance-test:book-config)
    (let* ((template (org-glance-capture:template 'book "Dune")))
      (should (s-contains? ":book:" template))
      (should (s-contains? "#+TODO: TODO READING | READ ABANDONED" template))
      (should (s-contains? "Notes" template))
      (should (s-contains? "%^{Where}" template))
      (should (s-contains? "Dune" template))
      (should (= 1 (s-count-matches "%\\?" template))))))

(ert-deftest org-glance-test:tag-config-render-single-capture-point ()
  "Render appends no `%?' when a kept drawer property already holds one."
  (org-glance-test:with-tag-config nil
      (org-glance-test:one-config
       "note"
       "{\"title\":\"Note\",\"template\":\"* Note\\n:PROPERTIES:\\n:RECORD:  %?\\n:END:\\nbody\"}")
    (let ((template (org-glance-capture:template 'note "X")))
      (should (= 1 (s-count-matches "%\\?" template)))
      (should (s-contains? ":RECORD:  %?" template)))))

(ert-deftest org-glance-test:tag-config-render-bare-capture-point ()
  "A bare `* %?' skeleton renders the default entry without `#+TODO:'."
  (org-glance-test:with-tag-config nil
      (org-glance-test:one-config
       "task"
       "{\"template\":\"* %?\"}")
    (let* ((template (org-glance-capture:template 'task "Buy milk")))
      (should (s-contains? "* Buy milk%?" template))   ; TITLE fills heading, %? survives
      (should (s-contains? ":task:" template))
      (should (= 1 (s-count-matches "%\\?" template)))
      (should (s-contains? "#+TODO: TODO | DONE" template)))))

(ert-deftest org-glance-test:tag-config-cycle-for-filter ()
  "Every relevant tag cycle remains a separate TODO sequence."
  (org-glance-test:with-tag-config nil
      (list (cons "book" "{\"title\":\"Book\",\"todoCycles\":[{\"active\":[\"TODO\",\"READING\"],\"inactive\":[\"READ\"]}],\"template\":\"* Book\"}")
            (cons "film" "{\"title\":\"Film\",\"todoCycles\":[{\"active\":[\"TODO\",\"WATCHING\"],\"inactive\":[\"WATCHED\"]}],\"template\":\"* Film\"}"))
    (let ((book (org-glance-tag-config:done-keywords
                 (org-glance-tag-config:cycle-for-filter org-glance-graph '(:tags ("book")))))
          (both (org-glance-tag-config:done-keywords
                 (org-glance-tag-config:cycle-for-filter org-glance-graph '(:tags ("book" "film"))))))
      (should (member "READ" book))
      (should-not (member "WATCHED" book))
      (should (member "READ" both))
      (should (member "WATCHED" both)))))

(ert-deftest org-glance-test:tag-config-not-in-content-tags ()
  "Tag discovery lists neither configured tags nor the retired `class' marker."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "h1" "* TODO A :work:"))
    (org-glance-test:with-tag-config graph (org-glance-test:one-config "book" org-glance-test:book-config)
      (should (member "work" (org-glance-graph:tags graph)))
      (should-not (member "book" (org-glance-graph:tags graph)))
      (should-not (member "class" (org-glance-graph:tags graph)))
      (should (org-glance-tag-config:resolve org-glance-graph 'book)))))

(ert-deftest org-glance-test:tag-config-overview-todo-header ()
  "The overview emits TODO lines for each tag present in its scope."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "b1" "* READING Dune :book:"))
    (org-glance-test:with-tag-config graph (org-glance-test:one-config "book" org-glance-test:book-config)
      (let ((text (org-glance-overview:render graph '(:tags ("book")))))
        (should (s-contains? "#+TODO: TODO READING | READ ABANDONED" text))
        (should (s-contains? "Dune" text)))
      (let ((text (org-glance-overview:render graph nil)))
        (should (s-contains? "#+TODO: TODO READING | READ ABANDONED" text))))))

(ert-deftest org-glance-test:tag-config-system-and-tag-cycles ()
  "System and per-tag TODO lines drive overview, parsing and capture."
  (org-glance-test:with-graph graph
    (org-glance-test:publish-config graph
      "{\"format\":1,\"todoCycles\":[{\"active\":[\"DELEGATED\",\"REVIEW\",\"TODO\"],\"inactive\":[\"DONE\",\"CANCELLED\"]}]}")
    (org-glance-test:with-tag-config graph
        (list (cons "book" "{\"todoCycles\":[{\"active\":[\"READING\"],\"inactive\":[\"READ\"]}],\"template\":\"* Book\"}")
              (cons "note" "{\"todoCycles\":[{\"active\":[\"READING\"],\"inactive\":[\"READ\"]}],\"template\":\"* Note\"}")
              (cons "film" "{\"todoCycles\":[{\"active\":[\"WATCHING\"],\"inactive\":[\"WATCHED\"]}],\"template\":\"* Film\"}"))
      (let ((cycle (org-glance-tag-config:cycle-for-filter
                    graph '(:tags ("wrike" "book" "film")))))
        (should (equal (split-string cycle "\n")
                       '("DELEGATED REVIEW TODO | DONE CANCELLED"
                         "READING | READ" "WATCHING | WATCHED")))
        (should (equal '("DONE" "CANCELLED" "READ" "WATCHED")
                       (org-glance-tag-config:done-keywords cycle))))
      (should (equal "DELEGATED REVIEW TODO | DONE CANCELLED"
                     (car (split-string (org-glance-tag-config:cycle-for-filter graph '(:tags nil)) "\n"))))
      (let ((text (org-glance-overview:render graph '(:tags ("wrike" "book" "note")))))
        (should (s-contains? "#+TODO:" text)))
      (let ((template (let ((org-glance-graph graph))
                        (org-glance-capture:template 'wrike "Review this"))))
        (should (s-contains? "#+TODO: DELEGATED REVIEW TODO | DONE CANCELLED\n"
                                 template)))
      (let ((org-todo-keywords
             (org-glance-tag-config:cycle->keywords
              (org-glance-tag-config:cycle-for-filter graph '(:tags ("wrike"))))))
        (org-glance-graph:add
         graph (org-glance-test:headline "review-1" "* REVIEW Inspect :wrike:")))
      (should (equal "REVIEW" (org-glance-headline-metadata:state
                                (org-glance-graph:live-meta graph "review-1"))))
      (org-glance-test:with-material (buffer graph "review-1")
        (should (equal "REVIEW" (org-get-todo-state))))
      (should (equal "Inspect" (org-glance-headline:title
                                (org-glance-graph:headline graph "review-1"))))
      (org-glance-graph:reindex graph)
      (should (equal "REVIEW" (org-glance-headline-metadata:state
                                (org-glance-graph:live-meta graph "review-1")))))))

(ert-deftest org-glance-test:tag-config-system-edit-refreshes-overview ()
  "A shared JSON edit invalidates a cached Overview at runtime."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "work" "* TODO Work :wrike:"))
    (let ((filter '(:tags ("wrike"))))
      (let ((file (org-glance-overview:cached-file graph filter)))
        (should (s-contains? "#+TODO: TODO | DONE" (f-read-text file 'utf-8)))
        (org-glance-test:publish-config graph
          "{\"format\":1,\"todoCycles\":[{\"active\":[\"REVIEW\",\"TODO\"],\"inactive\":[\"DONE\"]}]}")
        (should (s-contains? "REVIEW" (f-read-text (org-glance-overview:cached-file graph filter) 'utf-8)))))))

(ert-deftest org-glance-test:tag-config-materialize-state-roundtrip ()
  "A per-tag state survives a material save without folding into the title."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "d1" "* TODO Dune :book:"))
    (org-glance-test:with-tag-config graph (org-glance-test:one-config "book" org-glance-test:book-config-min)
      (org-glance-test:with-open buf (org-glance-material:open graph "d1")
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "TODO")
          (replace-match "READING")
          (org-glance-test:save))
        (let ((m (org-glance-graph:get-headline graph "d1")))
          (should (equal "READING" (org-glance-headline-metadata:state m)))
          (should (equal "Dune" (org-glance-headline-metadata:title m))))))))



(defconst org-glance-test:book-config-no-prompts
  "{\"title\":\"Book\",\"todoCycles\":[{\"active\":[\"TODO\",\"READING\"],\"inactive\":[\"READ\",\"ABANDONED\"]}],\"template\":\"* Book\\n%?\"}"
  "Book config with no `%^{...}' prompts or sub-headings, for batch capture.")

(ert-deftest org-glance-test:tag-config-capture-book ()
  "Capturing a configured-tag book runs `org-capture' end to end into the graph.
The `#+TODO:' preamble splits from the entry; the headline keeps tag and
skeleton and drops config-internal drawer keys."
  (org-glance-test:session
    (org-glance-test:with-tag-config org-glance-graph (org-glance-test:one-config "book" org-glance-test:book-config-no-prompts)
      (org-glance-capture 'book "Dune" :finalize t)
      (let* ((headlines (org-glance-graph:headlines org-glance-graph))
             (meta (car headlines)))
        (should (= 1 (length headlines)))
        (should (equal "Dune" (org-glance-headline-metadata:title meta)))
        (should (member "book" (org-glance-headline-metadata:tags meta)))
        (let* ((id (org-glance-headline-metadata:id meta))
               (headline (org-glance-graph:headline org-glance-graph id))
               (contents (org-glance-headline:contents headline)))
          (should-not (s-contains? ":TAG:" contents))
          (should-not (s-contains? ":TODO_KEYWORDS:" contents)))))))

(ert-deftest org-glance-test:tag-config-capture-unconfigured ()
  "Capturing an unconfigured tag still works (the degradation path)."
  (org-glance-test:session
    (org-glance-capture 'task "Buy milk" :finalize t)
    (let* ((headlines (org-glance-graph:headlines org-glance-graph))
           (meta (car headlines)))
      (should (= 1 (length headlines)))
      (should (equal "Buy milk" (org-glance-headline-metadata:title meta)))
      (should (member "task" (org-glance-headline-metadata:tags meta))))))

(ert-deftest org-glance-test:capture-split-preamble ()
  "The preamble splitter separates `#+TODO:' from the org entry."
  (let ((split (org-glance-capture--split-preamble
                "#+TODO: A B | C\n* heading :t:")))
    (should (equal "#+TODO: A B | C" (car split)))
    (should (equal "* heading :t:" (cdr split))))
  (let ((split (org-glance-capture--split-preamble "* plain :t:")))
    (should (null (car split)))
    (should (equal "* plain :t:" (cdr split)))))



(ert-deftest org-glance-test:tag-config-materialize-knows-keywords ()
  "A configured-tag material buffer knows the tag's todo states natively.
READING parses as a state; `org-todo' works and the blob gains no `#+TODO:'."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "b1" "* READING Dune :book:"))
    (org-glance-test:with-tag-config graph (org-glance-test:one-config "book" org-glance-test:book-config-min)
      (org-glance-test:with-open buf (org-glance-material:open graph "b1")
        (with-current-buffer buf
          (goto-char (point-min))
          (should (equal "READING" (org-get-todo-state)))
          (should (member "READ" org-done-keywords))
          (should (member "READING" org-not-done-keywords)))))))

(ert-deftest org-glance-test:tag-config-change-todo-live-cycle ()
  "`change-todo-live' cycles a configured tag's own states, persisting each."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "b1" "* TODO Dune :book:"))
    (org-glance-test:with-tag-config graph (org-glance-test:one-config "book" org-glance-test:book-config-min)
      (should (equal "READING" (org-glance-test:change-todo-live graph "b1")))
      (should (equal "READING" (org-glance-test:field graph "b1" state)))
      (should (equal "READ" (org-glance-test:change-todo-live graph "b1")))
      (should (s-contains? "* READ Dune" (org-glance-graph:get-content graph "b1"))))))

(ert-deftest org-glance-test:tag-config-fields-table ()
  "The field table is the pragma set's single source, in step with the struct.
The load-time guard rejects a mismatch; both preamble builders derive from it."
  (should (org-glance-tag-config--check-fields
           (cdr (cl-struct-slot-info 'org-glance-tag-config))
           org-glance-tag-config:fields))
  (should-error (org-glance-tag-config--check-fields
                 (cdr (cl-struct-slot-info 'org-glance-tag-config))
                 (butlast org-glance-tag-config:fields))
                :type 'error)
  ;; the `t' arg is EMITTABLE-only: TITLE is parsed but never emitted.
  (should (equal '((title . "TITLE") (todo . "TODO"))
                 (org-glance-tag-config--pragma-slots)))
  (should (equal '((todo . "TODO")) (org-glance-tag-config--pragma-slots t))))

(ert-deftest org-glance-test:tag-config-preamble ()
  "`:preamble' renders a config's emittable pragmas.
`:preamble-for-filter' emits each distinct relevant TODO sequence."
  (org-glance-test:with-tag-config nil
      (list (cons "book" "{\"title\":\"Book\",\"todoCycles\":[{\"active\":[\"TODO\",\"READING\"],\"inactive\":[\"READ\"]}],\"template\":\"* Book\"}")
            (cons "note" "{\"title\":\"Note\",\"todoCycles\":[{\"active\":[\"TODO\",\"READING\"],\"inactive\":[\"READ\"]}],\"template\":\"* Note\"}")
            (cons "film" "{\"title\":\"Film\",\"todoCycles\":[{\"active\":[\"TODO\",\"WATCHING\"],\"inactive\":[\"WATCHED\"]}],\"template\":\"* Film\"}")
            (cons "bare" "{\"title\":\"Bare\",\"template\":\"* Bare\"}"))
    (should (equal "#+TODO: TODO READING | READ\n"
                   (org-glance-tag-config:preamble
                    (org-glance-tag-config:resolve org-glance-graph 'book))))
    (should-not (org-glance-tag-config:preamble
                 (org-glance-tag-config:resolve org-glance-graph 'bare)))
    (should (equal
             (org-glance-tag-config:preamble-for-filter org-glance-graph '(:tags ("book")))
             (org-glance-tag-config:preamble-for-filter org-glance-graph '(:tags ("book" "note")))))))

(provide 'test-tag-config)
;;; test-tag-config.el ends here
