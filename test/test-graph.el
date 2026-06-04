;;; test-graph.el --- Tests for the v2 graph store  -*- lexical-binding: t -*-

(require 'test-helpers)

(cl-defmacro org-glance-test:with-graph (graph &rest body)
  "Create a graph in a fresh temp directory, bind it to GRAPH, run BODY."
  (declare (indent 1))
  `(with-temp-directory dir
     (let ((,graph (org-glance-graph-v2 dir)))
       ,@body)))

(cl-defun org-glance-test:headline (id &rest lines)
  "Build an `org-glance-headline-v2' carrying ID.
LINES is the heading, then optional planning (SCHEDULED:/DEADLINE:/CLOSED:)
lines, then optional body.  The ORG_GLANCE_ID drawer is placed after the
heading and any planning lines -- where org expects a property drawer --
so the id parses correctly whether or not a body or planning is present."
  (let* ((rest (cdr lines))
         (planning (seq-take-while
                    (lambda (l) (string-match-p "^\\(SCHEDULED\\|DEADLINE\\|CLOSED\\):" l))
                    rest))
         (body (seq-drop rest (length planning))))
    (apply #'org-glance-headline-v2--from-lines
           (append (list (car lines))
                   planning
                   (list ":PROPERTIES:"
                         (format ":ORG_GLANCE_ID: %s" id)
                         ":END:")
                   body))))

(ert-deftest org-glance-test:graph-add-get ()
  "A headline added to the graph is retrievable by id with its fields intact."
  (org-glance-test:with-graph graph
    (let ((headline (org-glance-test:headline "id1" "* TODO foo :a:b:")))
      (org-glance-graph-v2:add graph headline)
      (let ((meta (org-glance-graph-v2:get-headline graph "id1")))
        (should (org-glance-headline-metadata-v2? meta))
        (should (string= "id1"  (org-glance-headline-metadata-v2:id meta)))
        (should (string= "TODO" (org-glance-headline-metadata-v2:state meta)))
        (should (string= "foo"  (org-glance-headline-metadata-v2:title meta)))
        (should (string= (org-glance-headline-v2:hash headline)
                         (org-glance-headline-metadata-v2:hash meta)))
        (should (equal ["a" "b"] (org-glance-headline-metadata-v2:tags meta)))))))

(ert-deftest org-glance-test:graph-get-missing ()
  "Unknown ids return nil."
  (org-glance-test:with-graph graph
    (should (null (org-glance-graph-v2:get-headline graph "nope")))))

(ert-deftest org-glance-test:graph-add-returns-graph ()
  "`add' is chainable: it returns the graph."
  (org-glance-test:with-graph graph
    (should (eq graph (org-glance-graph-v2:add graph (org-glance-test:headline "id1" "* foo"))))))

(ert-deftest org-glance-test:graph-multiple-headlines ()
  "Several headlines coexist and are independently retrievable."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "a" "* TODO alpha")
                             (org-glance-test:headline "b" "* DONE beta"))
    (should (string= "alpha" (org-glance-headline-metadata-v2:title (org-glance-graph-v2:get-headline graph "a"))))
    (should (string= "beta"  (org-glance-headline-metadata-v2:title (org-glance-graph-v2:get-headline graph "b"))))))

(ert-deftest org-glance-test:graph-latest-wins ()
  "Re-adding an id appends a record; the most recent one wins (reverse scan)."
  (org-glance-test:with-graph graph
    (let ((headline (org-glance-test:headline "id1" "* TODO foo")))
      (org-glance-graph-v2:add graph headline)
      (org-glance-graph-v2:add graph (org-glance-headline-v2--copy headline :state "DONE"))
      (should (string= "DONE" (org-glance-headline-metadata-v2:state (org-glance-graph-v2:get-headline graph "id1")))))))

(ert-deftest org-glance-test:graph-delete-tombstone ()
  "Deleting an id makes `get-headline' report a tombstone."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (should (org-glance-headline-metadata-v2? (org-glance-graph-v2:get-headline graph "id1")))
    (org-glance-graph-v2:delete graph "id1")
    (should (eq 'tombstone (org-glance-graph-v2:get-headline graph "id1")))))

(ert-deftest org-glance-test:graph-delete-idempotent ()
  "Deleting an absent or already-deleted id is a no-op (no extra record)."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:delete graph "ghost")
    (should (null (org-glance-graph-v2:get-headline graph "ghost")))
    (org-glance-graph-v2:add graph (org-glance-test:headline "id1" "* foo"))
    (org-glance-graph-v2:delete graph "id1")
    (org-glance-graph-v2:delete graph "id1")
    (should (eq 'tombstone (org-glance-graph-v2:get-headline graph "id1")))))

(ert-deftest org-glance-test:graph-scheduled-roundtrip ()
  "A scheduled headline serializes (regression: schedule/deadline must be coerced
to raw strings, not org-element timestamp objects, or `json-serialize' crashes)."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "sid"
                                                             "* TODO sched"
                                                             "SCHEDULED: <2025-01-10 Fri>"))
    (let ((meta (org-glance-graph-v2:get-headline graph "sid")))
      (should (org-glance-headline-metadata-v2? meta))
      (should (string= "<2025-01-10 Fri>" (org-glance-headline-metadata-v2:schedule meta))))))

(ert-deftest org-glance-test:graph-capture-assigns-unique-ids ()
  "Capturing assigns a fresh, unique-per-namespace id to each id-less headline."
  (org-glance-test:with-graph graph
    (with-temp-buffer
      (org-mode)
      (insert "* TODO foo :a:\n* TODO bar :b:\n")
      (org-glance-graph-v2:capture graph (current-buffer)))
    (let* ((headlines (org-glance-graph-v2:headlines graph))
           (ids (mapcar #'org-glance-headline-metadata-v2:id headlines)))
      (should (= 2 (length headlines)))
      (should (-none? #'null ids))
      (should (= 2 (length (-uniq ids)))))))

(ert-deftest org-glance-test:graph-capture-preserves-existing-id ()
  "Capturing keeps an already-present ORG_GLANCE_ID."
  (org-glance-test:with-graph graph
    (with-temp-buffer
      (org-mode)
      (insert "* TODO foo\n:PROPERTIES:\n:ORG_GLANCE_ID: keep-me\n:END:\n")
      (org-glance-graph-v2:capture graph (current-buffer)))
    (should (org-glance-headline-metadata-v2? (org-glance-graph-v2:get-headline graph "keep-me")))))

(ert-deftest org-glance-test:graph-headlines-skips-tombstones ()
  "`headlines' returns live records only, newest per id."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "a" "* foo")
                             (org-glance-test:headline "b" "* bar"))
    (org-glance-graph-v2:delete graph "a")
    (should (equal '("b") (mapcar #'org-glance-headline-metadata-v2:id
                                  (org-glance-graph-v2:headlines graph))))))

(ert-deftest org-glance-test:graph-content-roundtrip ()
  "Headline contents persist to the data store and reconstruct fully."
  (org-glance-test:with-graph graph
    (let ((headline (org-glance-test:headline "rt1" "* TODO foo" "body line")))
      (org-glance-graph-v2:add graph headline)
      (should (s-contains? "body line" (org-glance-graph-v2:get-content graph "rt1")))
      (let ((restored (org-glance-graph-v2:headline graph "rt1")))
        (should (org-glance-headline-v2? restored))
        (should (string= "rt1" (org-glance-headline-v2:id restored)))
        (should (string= (org-glance-headline-v2:hash headline)
                         (org-glance-headline-v2:hash restored)))))))

(ert-deftest org-glance-test:graph-content-missing ()
  "Reading content for an unknown id yields nil, not an error."
  (org-glance-test:with-graph graph
    (should (null (org-glance-graph-v2:get-content graph "nope")))
    (should (null (org-glance-graph-v2:headline graph "nope")))))

(ert-deftest org-glance-test:graph-headline-tombstoned ()
  "`headline' returns nil for a deleted id even though its blob remains."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "id1" "* foo" "body"))
    (should (org-glance-headline-v2? (org-glance-graph-v2:headline graph "id1")))
    (org-glance-graph-v2:delete graph "id1")
    (should (null (org-glance-graph-v2:headline graph "id1")))
    ;; the low-level blob is still present (append-only store)
    (should (s-contains? "body" (org-glance-graph-v2:get-content graph "id1")))))

(provide 'test-graph)
;;; test-graph.el ends here
