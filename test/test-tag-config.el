;;; test-tag-config.el --- Separate optional per-tag config store -*- lexical-binding: t -*-
(require 'test-helpers)

(cl-defmacro org-glance-test:with-tag-config (configs &rest body)
  "Write CONFIGS into a temp config dir, point the override there, run BODY.
CONFIGS is an alist (TAG-STRING . FILE-CONTENTS); each becomes `<dir>/<tag>.org'.
Resets the module cache around BODY so reads see exactly CONFIGS."
  (declare (indent 1))
  `(with-temp-directory dir
     (let ((org-glance-tag-config-dir dir))
       (dolist (cfg ,configs)
         (f-write-text (cdr cfg) 'utf-8 (f-join dir (concat (car cfg) ".org"))))
       (org-glance-tag-config--invalidate)
       (unwind-protect (progn ,@body)
         (org-glance-tag-config--invalidate)))))

(cl-defun org-glance-test:one-config (tag contents)
  "The one-element CONFIGS alist for the `with-tag-config' macro."
  (list (cons tag contents)))

(defconst org-glance-test:book-config
  "#+TITLE: Book
#+TODO:  TODO READING | READ ABANDONED

* Book
:PROPERTIES:
:LOCATION: %^{Where}
:END:
*** Notes
    %?
"
  "Full book config: cycle, a property prompt, and a sub-heading skeleton.")

(defconst org-glance-test:book-config-min
  "#+TITLE: Book\n#+TODO: TODO READING | READ\n\n* Book\n"
  "Minimal single-tag book config: cycle only, an empty capture entry.")


(ert-deftest org-glance-test:tag-config-degrades-without-config ()
  "With no config, the capture template is byte-identical to the default."
  (let ((org-glance-tag-config-dir nil)
        (org-glance-graph nil))
    (org-glance-tag-config--invalidate)
    (should (string= "* Hi%?  :task:" (org-glance-capture:template 'task "Hi")))))


(ert-deftest org-glance-test:tag-config-resolve ()
  "A per-tag file resolves to a struct; an unknown tag resolves to nil."
  (org-glance-test:with-tag-config (org-glance-test:one-config "book" org-glance-test:book-config)
    (let ((config (org-glance-tag-config:resolve nil 'book)))
      (should (org-glance-tag-config? config))
      (should (eq 'book (org-glance-tag-config:tag config)))
      (should (equal "Book" (org-glance-tag-config:title config)))
      (should (equal "TODO READING | READ ABANDONED" (org-glance-tag-config:todo config)))
      (should (s-contains? "Notes" (org-glance-tag-config:template config))))
    (should (null (org-glance-tag-config:resolve nil 'nonexistent)))))


(ert-deftest org-glance-test:tag-config-done-keywords ()
  "The done-set is everything after the last `|', derived by org itself."
  (should (equal '("READ" "ABANDONED")
                 (org-glance-tag-config:done-keywords "TODO READING | READ ABANDONED")))
  (should (equal '("DONE") (org-glance-tag-config:done-keywords "TODO | DONE")))
  (should (null (org-glance-tag-config:done-keywords nil)))
  (should (null (org-glance-tag-config:done-keywords ""))))


(ert-deftest org-glance-test:tag-config-render-from-config ()
  "Render keeps the captured tag + skeleton + prompts, prepends the cycle as a
`#+TODO:' file keyword, and yields exactly one `%?'."
  (org-glance-test:with-tag-config (org-glance-test:one-config "book" org-glance-test:book-config)
    (let* ((org-glance-graph nil)
           (template (org-glance-capture:template 'book "Dune")))
      (should (s-contains? ":book:" template))
      (should (s-contains? "#+TODO: TODO READING | READ ABANDONED" template))
      (should (s-contains? "Notes" template))
      (should (s-contains? "%^{Where}" template))
      (should (s-contains? "Dune" template))
      (should (= 1 (s-count-matches "%\\?" template))))))

(ert-deftest org-glance-test:tag-config-render-single-capture-point ()
  "A `%?' in a KEPT drawer property is the skeleton's own capture point; render
must NOT append a second one (org-capture honours only the first)."
  (org-glance-test:with-tag-config
      (org-glance-test:one-config
       "note"
       "#+TITLE: Note\n\n* Note\n:PROPERTIES:\n:RECORD:  %?\n:END:\nbody\n")
    (let ((template (org-glance-capture:template 'note "X")))
      (should (= 1 (s-count-matches "%\\?" template)))
      (should (s-contains? ":RECORD:  %?" template)))))

(ert-deftest org-glance-test:tag-config-render-bare-capture-point ()
  "A minimal `* %?' skeleton with no pragmas renders to the default entry:
TITLE fills the heading, `%?' survives, no `#+TODO:' preamble is emitted."
  (org-glance-test:with-tag-config
      (org-glance-test:one-config
       "task"
       "# only a capture template below\n\n* %?\n")
    (let* ((org-glance-graph nil)
           (template (org-glance-capture:template 'task "Buy milk")))
      (should (s-contains? "* Buy milk%?" template))   ; TITLE fills heading, %? survives
      (should (s-contains? ":task:" template))
      (should (= 1 (s-count-matches "%\\?" template)))
      (should-not (s-contains? "#+TODO:" template)))))


(ert-deftest org-glance-test:tag-config-cycle-for-filter ()
  "A single configured cycle wins; 0 or >1 distinct cycles fall back to nil."
  (org-glance-test:with-tag-config
      (list (cons "book" "#+TITLE: Book\n#+TODO: TODO READING | READ\n\n* Book\n")
            (cons "film" "#+TITLE: Film\n#+TODO: TODO WATCHING | WATCHED\n\n* Film\n"))
    (should (equal "TODO READING | READ"
                   (org-glance-tag-config:cycle-for-filter nil '(:tags ("book")))))
    (should (null (org-glance-tag-config:cycle-for-filter nil '(:tags ("book" "film")))))
    (should (null (org-glance-tag-config:cycle-for-filter nil '(:tags ("task")))))))


(ert-deftest org-glance-test:tag-config-not-in-content-tags ()
  "A configured tag never appears in the content graph's tag discovery, and
`class' (the old reserved marker) is gone entirely."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "h1" "* TODO A :work:"))
    (org-glance-test:with-tag-config (org-glance-test:one-config "book" org-glance-test:book-config)
      (should (member "work" (org-glance-graph:tags graph)))
      (should-not (member "book" (org-glance-graph:tags graph)))
      (should-not (member "class" (org-glance-graph:tags graph)))
      (should (org-glance-tag-config:resolve nil 'book)))))


(ert-deftest org-glance-test:tag-config-overview-todo-header ()
  "The overview emits `#+TODO:' for a single configured tag, and omits it otherwise."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "b1" "* READING Dune :book:"))
    (org-glance-test:with-tag-config (org-glance-test:one-config "book" org-glance-test:book-config)
      (let ((text (org-glance-overview:render graph '(:tags ("book")))))
        (should (s-contains? "#+TODO: TODO READING | READ ABANDONED" text))
        (should (s-contains? "Dune" text)))
      (let ((text (org-glance-overview:render graph nil)))
        (should-not (s-contains? "#+TODO:" text))))))


(ert-deftest org-glance-test:tag-config-materialize-state-roundtrip ()
  "A per-tag todo state survives materialize -> edit -> save: it must NOT fold into
the title via a keyword-naive reparse (the blob has no #+TODO; the cycle is bound
per-tag at sync)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "d1" "* TODO Dune :book:"))
    (org-glance-test:with-tag-config (org-glance-test:one-config "book" org-glance-test:book-config-min)
      (org-glance-test:with-open buf (org-glance-material:open graph "d1")
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward "TODO")
          (replace-match "READING")
          (org-glance-material:sync))
        (let ((m (org-glance-graph:get-headline graph "d1")))
          (should (equal "READING" (org-glance-headline-metadata:state m)))
          (should (equal "Dune" (org-glance-headline-metadata:title m))))))))


(ert-deftest org-glance-test:tag-config-lint ()
  "The lint flags a leftover :TODO_KEYWORDS:/:TAG: drawer and a missing entry,
and is silent on a clean per-tag file."
  (with-temp-buffer
    (insert "#+TITLE: t\n\n:PROPERTIES:\n:TAG: book\n:TODO_KEYWORDS: TODO | DONE\n:END:\n")
    (delay-mode-hooks (org-mode))
    (let ((issues (org-glance-tag-config--lint)))
      (should (cl-some (lambda (s) (s-contains? ":TODO_KEYWORDS:" s)) issues))
      (should (cl-some (lambda (s) (s-contains? ":TAG:" s)) issues))
      (should (cl-some (lambda (s) (s-contains? "no capture entry" s)) issues))))
  (with-temp-buffer
    (insert "#+TITLE: Book\n#+TODO: TODO | DONE\n\n* Book\n*** Notes\n%?\n")
    (delay-mode-hooks (org-mode))
    (should-not (org-glance-tag-config--lint))))


(defconst org-glance-test:book-config-no-prompts
  "#+TITLE: Book\n#+TODO:  TODO READING | READ ABANDONED\n\n* Book\n%?\n"
  "Book config without `%^{...}' prompts and without sub-headings --
safe for batch-mode capture tests (no interactive prompts, single headline).")

(ert-deftest org-glance-test:tag-config-capture-book ()
  "Capturing a book with a configured tag produces a valid headline in the graph.
Exercises the real `org-glance-capture' -> `org-capture' -> finalize -> ingest
pipeline end-to-end: the `#+TODO:' preamble must be split from the entry so
org-capture accepts the template, and the finalized headline must carry the tag,
the skeleton body, and no config-internal drawer keys."
  (org-glance-test:session
    (org-glance-test:with-tag-config (org-glance-test:one-config "book" org-glance-test:book-config-no-prompts)
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


(ert-deftest org-glance-test:tag-config-migrate-legacy ()
  "Opening a graph with a legacy `config/tags.org' splits it into per-tag files
\(heading -> #+TITLE, :TODO_KEYWORDS: -> #+TODO, subtree minus those keys ->
entry) and backs the legacy file up, leaving resolution intact."
  (org-glance-test:with-graph graph
    (let ((legacy (org-glance-graph:config-file graph "tags.org")))
      (org-glance-test:write
       legacy
       (concat "#+TITLE: tags\n\n"
               "* Book\n:PROPERTIES:\n:TAG: book\n"
               ":TODO_KEYWORDS: TODO READING | READ\n:LOCATION: %^{Where}\n:END:\n"
               "*** Notes\n    %?\n"
               "* Film\n:PROPERTIES:\n:TAG: film\n:TODO_KEYWORDS: TODO | SEEN\n:END:\n"))
      (org-glance-tag-config--invalidate)
      (org-glance-tag-config--migrate-on-open graph)
      (should (f-exists? (org-glance-tag-config:file graph 'book)))
      (should (f-exists? (org-glance-tag-config:file graph 'film)))
      (should-not (f-exists? legacy))
      (should (f-exists? (concat legacy ".bak")))
      (let ((book (org-glance-tag-config:resolve graph 'book)))
        (should (equal "TODO READING | READ" (org-glance-tag-config:todo book)))
        (should (equal "Book" (org-glance-tag-config:title book)))
        (should (s-contains? "%^{Where}" (org-glance-tag-config:template book)))
        (should (s-contains? "Notes" (org-glance-tag-config:template book)))
        (should-not (s-contains? ":TAG:" (org-glance-tag-config:template book)))
        (should-not (s-contains? ":TODO_KEYWORDS:" (org-glance-tag-config:template book))))
      (f-write-text "#+TITLE: mine\n#+TODO: X | Y\n\n* mine\n" 'utf-8
                    (org-glance-tag-config:file graph 'book))
      (org-glance-tag-config--migrate-on-open graph)
      (should (equal "X | Y" (org-glance-tag-config:todo
                              (progn (org-glance-tag-config--invalidate)
                                     (org-glance-tag-config:resolve graph 'book))))))))


(ert-deftest org-glance-test:tag-config-materialize-knows-keywords ()
  "Materializing a configured-tag headline makes the blob buffer natively recognise
the tag's states (READING is a state, not folded into the title) -- so `org-todo'
and faces work in place, with no `#+TODO:' in the kept-clean blob."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "b1" "* READING Dune :book:"))
    (org-glance-test:with-tag-config (org-glance-test:one-config "book" org-glance-test:book-config-min)
      (org-glance-test:with-open buf (org-glance-material:open graph "b1")
        (with-current-buffer buf
          (goto-char (point-min))
          (should (equal "READING" (org-get-todo-state)))
          (should (member "READ" org-done-keywords))
          (should (member "READING" org-not-done-keywords)))))))

(ert-deftest org-glance-test:tag-config-change-todo-live-cycle ()
  "`change-todo-live' cycles a configured tag's OWN states (TODO -> READING ->
READ), persisting each -- not the global TODO/DONE."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "b1" "* TODO Dune :book:"))
    (org-glance-test:with-tag-config (org-glance-test:one-config "book" org-glance-test:book-config-min)
      (should (equal "READING" (org-glance-test:change-todo-live graph "b1")))
      (should (equal "READING" (org-glance-test:field graph "b1" state)))
      (should (equal "READ" (org-glance-test:change-todo-live graph "b1")))
      (should (s-contains? "* READ Dune" (org-glance-graph:get-content graph "b1"))))))

(ert-deftest org-glance-test:tag-config-fields-table ()
  "The field table is the single source of the pragma set: it stays in step
with the struct (guarded at load), and both preamble builders derive from it,
so a new pragma is one row plus one slot instead of a parse line, a capture
arm and an overview arm."
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
  "`:preamble' renders a config's emittable pragmas; `:preamble-for-filter'
renders only what the filtered tags AGREE on -- one distinct value or nothing."
  (org-glance-test:with-tag-config
      (list (cons "book" "#+TITLE: Book\n#+TODO: TODO READING | READ\n\n* Book\n")
            (cons "note" "#+TITLE: Note\n#+TODO: TODO READING | READ\n\n* Note\n")
            (cons "film" "#+TITLE: Film\n#+TODO: TODO WATCHING | WATCHED\n\n* Film\n")
            (cons "bare" "#+TITLE: Bare\n\n* Bare\n"))
    (should (equal "#+TODO: TODO READING | READ\n"
                   (org-glance-tag-config:preamble
                    (org-glance-tag-config:resolve nil 'book))))
    (should-not (org-glance-tag-config:preamble
                 (org-glance-tag-config:resolve nil 'bare)))
    (should (equal "#+TODO: TODO READING | READ\n"
                   (org-glance-tag-config:preamble-for-filter nil '(:tags ("book")))))
    (should (equal "#+TODO: TODO READING | READ\n"
                   (org-glance-tag-config:preamble-for-filter nil '(:tags ("book" "note")))))
    (should-not (org-glance-tag-config:preamble-for-filter nil '(:tags ("book" "film"))))
    (should-not (org-glance-tag-config:preamble-for-filter nil '(:tags ("bare"))))))

(provide 'test-tag-config)
;;; test-tag-config.el ends here
