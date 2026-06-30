;;; test-tag-config.el --- Separate optional per-tag config store -*- lexical-binding: t -*-
(require 'test-helpers)

(cl-defmacro org-glance-test:with-tag-config (contents &rest body)
  "Write CONTENTS to a temp `tags.org', point the config override at it, run BODY.
Resets the module cache around BODY so reads see exactly CONTENTS."
  (declare (indent 1))
  `(with-temp-directory dir
     (let ((org-glance-tag-config-file (f-join dir "tags.org")))
       (f-write-text ,contents 'utf-8 org-glance-tag-config-file)
       (org-glance-tag-config--invalidate)
       (unwind-protect (progn ,@body)
         (org-glance-tag-config--invalidate)))))

(defconst org-glance-test:book-config
  "#+TITLE: tags

* Book
:PROPERTIES:
:TAG:            book
:TODO_KEYWORDS:  TODO READING | READ ABANDONED
:LOCATION:       %^{Where}
:END:
*** Notes
    %?
")

;;; Degradation (the identity element)

(ert-deftest org-glance-test:tag-config-degrades-without-config ()
  "With no config, the capture template is byte-identical to the default."
  (let ((org-glance-tag-config-file nil)
        (org-glance-graph nil))
    (org-glance-tag-config--invalidate)
    (should (string= "* Hi%?  :task:" (org-glance-capture:template 'task "Hi")))))

;;; Resolve / parse

(ert-deftest org-glance-test:tag-config-resolve ()
  "A `:TAG:' config resolves to a struct; an unknown tag resolves to nil."
  (org-glance-test:with-tag-config org-glance-test:book-config
    (let ((config (org-glance-tag-config:resolve nil 'book)))
      (should (org-glance-tag-config? config))
      (should (eq 'book (org-glance-tag-config:tag config)))
      (should (equal "TODO READING | READ ABANDONED" (org-glance-tag-config:todo config)))
      (should (org-glance-headline? (org-glance-tag-config:headline config))))
    (should (null (org-glance-tag-config:resolve nil 'nonexistent)))))

;;; Todo cycle -> done-set

(ert-deftest org-glance-test:tag-config-done-keywords ()
  "The done-set is everything after the last `|', derived by org itself."
  (should (equal '("READ" "ABANDONED")
                 (org-glance-tag-config:done-keywords "TODO READING | READ ABANDONED")))
  (should (equal '("DONE") (org-glance-tag-config:done-keywords "TODO | DONE")))
  (should (null (org-glance-tag-config:done-keywords nil)))
  (should (null (org-glance-tag-config:done-keywords ""))))

;;; Render (capture template from the config skeleton + #+TODO)

(ert-deftest org-glance-test:tag-config-render-from-config ()
  "Render keeps the captured tag + skeleton + prompts, prepends the cycle as a
`#+TODO:' file keyword, strips config metadata, and yields exactly one `%?'."
  (org-glance-test:with-tag-config org-glance-test:book-config
    (let* ((org-glance-graph nil)
           (template (org-glance-capture:template 'book "Dune")))
      ;; instance carries the captured tag; the cycle is a FILE keyword, not a drawer prop
      (should (s-contains? ":book:" template))
      (should (s-contains? "#+TODO: TODO READING | READ ABANDONED" template))
      (should-not (s-contains? ":TODO_KEYWORDS:" template))
      ;; config metadata stripped from the instance
      (should-not (s-contains? ":TAG:" template))
      ;; skeleton + property default/prompt preserved
      (should (s-contains? "Notes" template))
      (should (s-contains? "%^{Where}" template))
      ;; title pre-filled; exactly one capture point
      (should (s-contains? "Dune" template))
      (should (= 1 (s-count-matches "%\\?" template))))))

(ert-deftest org-glance-test:tag-config-render-single-capture-point ()
  "A `%?' in a KEPT drawer property is the skeleton's own capture point; render
must NOT append a second one (org-capture honours only the first)."
  (org-glance-test:with-tag-config
      "#+TITLE: t

* Note
:PROPERTIES:
:TAG:     note
:RECORD:  %?
:END:
body
"
    (let ((template (org-glance-capture:template 'note "X")))
      (should (= 1 (s-count-matches "%\\?" template)))
      (should (s-contains? ":RECORD:  %?" template)))))

;;; Multi-tag resolution

(ert-deftest org-glance-test:tag-config-cycle-for-filter ()
  "A single configured cycle wins; 0 or >1 distinct cycles fall back to nil."
  (org-glance-test:with-tag-config
      "#+TITLE: t

* Book
:PROPERTIES:
:TAG:            book
:TODO_KEYWORDS:  TODO READING | READ
:END:

* Film
:PROPERTIES:
:TAG:            film
:TODO_KEYWORDS:  TODO WATCHING | WATCHED
:END:
"
    (should (equal "TODO READING | READ"
                   (org-glance-tag-config:cycle-for-filter nil '(:tags ("book")))))
    (should (null (org-glance-tag-config:cycle-for-filter nil '(:tags ("book" "film")))))
    (should (null (org-glance-tag-config:cycle-for-filter nil '(:tags ("task")))))))

;;; No tag-namespace pollution (the whole point of decoupling)

(ert-deftest org-glance-test:tag-config-not-in-content-tags ()
  "A configured tag never appears in the content graph's tag discovery, and
`class' (the old reserved marker) is gone entirely."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "h1" "* TODO A :work:"))
    (org-glance-test:with-tag-config org-glance-test:book-config
      (should (member "work" (org-glance-graph:tags graph)))
      (should-not (member "book" (org-glance-graph:tags graph)))
      (should-not (member "class" (org-glance-graph:tags graph)))
      (should (org-glance-tag-config:resolve nil 'book)))))

;;; Overview: per-tag #+TODO header

(ert-deftest org-glance-test:tag-config-overview-todo-header ()
  "The overview emits `#+TODO:' for a single configured tag, and omits it otherwise."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "b1" "* READING Dune :book:"))
    (org-glance-test:with-tag-config org-glance-test:book-config
      (let ((text (org-glance-overview:render graph '(:tags ("book")))))
        (should (s-contains? "#+TODO: TODO READING | READ ABANDONED" text))
        (should (s-contains? "Dune" text)))
      (let ((text (org-glance-overview:render graph nil)))
        (should-not (s-contains? "#+TODO:" text))))))

;;; Materialize round-trip preserves a per-tag state (W1 regression)

(ert-deftest org-glance-test:tag-config-materialize-state-roundtrip ()
  "A per-tag todo state survives materialize -> edit -> save: it must NOT fold into
the title via a keyword-naive reparse (the blob has no #+TODO; the cycle is bound
per-tag at sync)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "d1" "* TODO Dune :book:"))
    (org-glance-test:with-tag-config
        "#+TITLE: t\n\n* Book\n:PROPERTIES:\n:TAG: book\n:TODO_KEYWORDS: TODO READING | READ\n:END:\n"
      (let ((buf (org-glance-material:open graph "d1")))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (goto-char (point-min))
                (re-search-forward "TODO")
                (replace-match "READING")
                (org-glance-material:sync))
              (let ((m (org-glance-graph:get-headline graph "d1")))
                (should (equal "READING" (org-glance-headline-metadata:state m)))
                (should (equal "Dune" (org-glance-headline-metadata:title m)))))
          (when (buffer-live-p buf) (kill-buffer buf)))))))

;;; On-save lint (advisory)

(ert-deftest org-glance-test:tag-config-lint ()
  "The lint flags a duplicate :TAG: and the :TODO:-not-:TODO_KEYWORDS: gotcha,
and is silent on a clean file."
  (with-temp-buffer
    (insert "#+TITLE: t\n\n"
            "* A\n:PROPERTIES:\n:TAG: book\n:TODO_KEYWORDS: TODO | DONE\n:END:\n"
            "* B\n:PROPERTIES:\n:TAG: book\n:END:\n"
            "* C\n:PROPERTIES:\n:TAG: film\n:TODO: TODO | DONE\n:END:\n")
    (delay-mode-hooks (org-mode))
    (let ((issues (org-glance-tag-config--lint)))
      (should (cl-some (lambda (s) (s-contains? "duplicate :TAG: book" s)) issues))
      (should (cl-some (lambda (s) (and (s-contains? "film" s) (s-contains? "TODO_KEYWORDS" s))) issues))
      ;; the clean book headline (A) raises nothing on its own account
      (should-not (cl-some (lambda (s) (s-contains? "tag book:" s)) issues))))
  (with-temp-buffer
    (insert "#+TITLE: t\n\n* A\n:PROPERTIES:\n:TAG: book\n:TODO_KEYWORDS: TODO | DONE\n:END:\n")
    (delay-mode-hooks (org-mode))
    (should-not (org-glance-tag-config--lint))))

;;; Capture: the full flow (template + org-capture + graph ingest)

(defconst org-glance-test:book-config-no-prompts
  "#+TITLE: tags

* Book
:PROPERTIES:
:TAG:            book
:TODO_KEYWORDS:  TODO READING | READ ABANDONED
:END:
%?
"
  "Book config without `%^{...}' prompts and without sub-headings --
safe for batch-mode capture tests (no interactive prompts, single headline).")

(ert-deftest org-glance-test:tag-config-capture-book ()
  "Capturing a book with a configured tag produces a valid headline in the graph.
Exercises the real `org-glance-capture' -> `org-capture' -> finalize -> ingest
pipeline end-to-end: the `#+TODO:' preamble must be split from the entry so
org-capture accepts the template, and the finalized headline must carry the tag,
the skeleton body, and no config-internal drawer keys."
  (org-glance-test:session
    (org-glance-test:with-tag-config org-glance-test:book-config-no-prompts
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

(provide 'test-tag-config)
;;; test-tag-config.el ends here
