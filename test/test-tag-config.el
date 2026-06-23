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
:REQUIRES:       author
:VERSION:        1
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
      (should (equal '(author) (org-glance-tag-config:requires config)))
      (should (= 1 (org-glance-tag-config:version config)))
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
      (should-not (s-contains? "REQUIRES" template))
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

(provide 'test-tag-config)
;;; test-tag-config.el ends here
