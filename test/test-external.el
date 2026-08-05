;;; test-external.el --- Tests for the external-write notification file  -*- lexical-binding: t -*-

;; The cross-repo contract with `glance': an outside writer edits a blob and
;; names its id in `meta/EXTERNAL.jsonl'; `org-glance-graph:refresh-external'
;; folds those entries back into the WAL and shortens the file.  See the
;; commentary in `org-glance-graph.el'.

(require 'test-helpers)

(cl-defun org-glance-test:external-write (graph &rest ids)
  "Append a notification line for each of IDS to GRAPH's `EXTERNAL.jsonl'.
Spells the frozen line by hand rather than through a JSON encoder, so a change
to the field names or their order fails here."
  (let ((path (org-glance-graph:external-path graph)))
    (f-mkdir-full-path (f-dirname path))
    (f-append-text
     (mapconcat (lambda (id)
                  (format "{\"id\":\"%s\",\"at\":\"2026-08-03T04:21:07Z\"}\n" id))
                ids "")
     'utf-8 path)))

(cl-defun org-glance-test:external-text (graph)
  "GRAPH's `EXTERNAL.jsonl' as text, or nil when there is no file."
  (let ((path (org-glance-graph:external-path graph)))
    (when (f-exists? path) (f-read-text path 'utf-8))))

(cl-defun org-glance-test:edit-blob (graph id from to)
  "Rewrite ID's blob in GRAPH, replacing FROM with TO -- an outside writer's edit.
Writes the file directly, without going through `org-glance-graph:add', which is
exactly what the daemon does: the blob moves and the WAL does not."
  (let ((path (org-glance-graph:content-path graph id)))
    (f-write-text (s-replace from to (f-read-text path 'utf-8)) 'utf-8 path)))

(ert-deftest org-glance-test:external-refresh-folds-the-edit-in ()
  "A blob edited outside Emacs reaches the WAL through `refresh-external'."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    ;; The WAL still says what it said: a blob write is not a record.
    (should (string= "TODO" (org-glance-test:field graph "id1" state)))
    (org-glance-test:external-write graph "id1")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (string= "DONE" (org-glance-test:field graph "id1" state)))))

(ert-deftest org-glance-test:external-refresh-truncates ()
  "The file is emptied once its ids have landed, and the file itself stays."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:external-write graph "id1")
    (org-glance-graph:refresh-external graph)
    (should (string= "" (org-glance-test:external-text graph)))
    (should (f-exists? (org-glance-graph:external-path graph)))))

(ert-deftest org-glance-test:external-refresh-is-idempotent ()
  "Re-running a refresh over the same ids is the crash rule: it costs a record
equal to the one already there and changes no answer."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (org-glance-test:external-write graph "id1")
    (org-glance-graph:refresh-external graph)
    ;; A crash before the truncation leaves the line: the next run repeats it.
    (org-glance-test:external-write graph "id1")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (string= "DONE" (org-glance-test:field graph "id1" state)))
    (should (equal '("id1") (org-glance-test:ids graph)))))

(ert-deftest org-glance-test:external-refresh-dedupes-ids ()
  "One id named several times is re-derived once."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:external-write graph "id1" "id1" "id1")
    (should (= 1 (org-glance-graph:refresh-external graph)))))

(ert-deftest org-glance-test:external-refresh-skips-unknown-and-deleted ()
  "An id the store never had, and one it tombstoned, are skipped -- with the
file still cleared, since neither has a record to replace."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-graph:delete graph "id1")
    (org-glance-test:external-write graph "id1" "ghost")
    (should (= 0 (org-glance-graph:refresh-external graph)))
    (should (eq 'tombstone (org-glance-graph:get-headline graph "id1")))
    (should (string= "" (org-glance-test:external-text graph)))))

(ert-deftest org-glance-test:external-refresh-keeps-a-good-id-past-a-bad-line ()
  "A line no JSON reader can parse costs its own entry and no other.
The writer's append is one `write', so the only way to get one is a partial
write at the end of the file -- which is where this puts it."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (org-glance-test:external-write graph "id1")
    (f-append-text "{\"id\":\"tor" 'utf-8 (org-glance-graph:external-path graph))
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (string= "DONE" (org-glance-test:field graph "id1" state)))
    (should (string= "" (org-glance-test:external-text graph)))))

(ert-deftest org-glance-test:external-refresh-keeps-what-arrived-meanwhile ()
  "Only the lines the run folded are dropped: a line appended while it ran
survives for the next refresh, so a concurrent writer loses nothing."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-graph:add graph (org-glance-test:headline "id2" "* TODO bar"))
    (org-glance-test:external-write graph "id1")
    (let ((read (symbol-function 'org-glance-graph--read-external)))
      (cl-letf (((symbol-function 'org-glance-graph--read-external)
                 (lambda (g) (prog1 (funcall read g)
                          (org-glance-test:external-write g "id2")))))
        (org-glance-graph:refresh-external graph)))
    (should (string-match-p "id2" (org-glance-test:external-text graph)))
    (should-not (string-match-p "id1" (org-glance-test:external-text graph)))))

(ert-deftest org-glance-test:external-refresh-without-a-file ()
  "A store no external writer ever touched refreshes nothing and makes nothing."
  (org-glance-test:with-graph graph
    (should (= 0 (org-glance-graph:refresh-external graph)))
    (should-not (f-exists? (org-glance-graph:external-path graph)))))

(ert-deftest org-glance-test:external-refresh-reads-the-tags-cycle ()
  "A state only a tag's `#+TODO:' cycle declares is re-derived as a STATE.
Without the cycle in scope the keyword folds into the title, exactly as it
would in `org-glance-material:sync' -- which binds it for this reason."
  (org-glance-test:with-graph graph
    ;; The tag is the file NAME under the store's `config/tags/'.
    (org-glance-test:write (org-glance-graph:config-file graph "tags/book.org")
                           "#+TITLE: Book\n#+TODO: TODO READING | READ\n\n* Book\n")
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* foo :book:"))
    (org-glance-test:edit-blob graph "id1" "* foo" "* READING foo")
    (org-glance-test:external-write graph "id1")
    (org-glance-graph:refresh-external graph)
    (should (string= "READING" (org-glance-test:field graph "id1" state)))
    (should (string= "foo" (org-glance-test:field graph "id1" title)))))

(ert-deftest org-glance-test:external-invalidates-overview-cache ()
  "The notification file joins the overview cache's source list, so an outside
edit invalidates a rendered overview the way a WAL append does."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (let ((file (org-glance-overview:write graph)))
      (org-glance-test:store-mtime graph -100)
      (should (org-glance-overview:fresh? graph file))
      (org-glance-test:external-write graph "id1")
      (should-not (org-glance-overview:fresh? graph file)))))

(ert-deftest org-glance-test:external-read-folds-without-asking ()
  "A plain READ folds pending external writes in: nobody calls the command, and
`get-headline' already answers with the edit; the file is cleared as usual."
  (let ((org-glance-graph-external-poll-seconds 0))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
      (org-glance-test:external-write graph "id1")
      (should (string= "DONE" (org-glance-test:field graph "id1" state)))
      (should (string= "" (org-glance-test:external-text graph))))))

(ert-deftest org-glance-test:external-read-fold-is-throttled ()
  "The read-path stat is throttled by `org-glance-graph-external-poll-seconds':
inside the interval a fresh notification waits, and the command folds it on
demand regardless."
  (org-glance-test:with-graph graph
    (let ((org-glance-graph-external-poll-seconds 0))
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-test:field graph "id1" state))          ; stamps the check
    (let ((org-glance-graph-external-poll-seconds 3600))
      (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
      (org-glance-test:external-write graph "id1")
      (should (string= "TODO" (org-glance-test:field graph "id1" state)))
      (should (= 1 (org-glance-graph:refresh-external graph)))   ; never throttled
      (should (string= "DONE" (org-glance-test:field graph "id1" state))))))

(ert-deftest org-glance-test:external-read-fold-does-not-reenter ()
  "The fold's OWN reads do not fold again: one refresh, not a recursion."
  (let ((org-glance-graph-external-poll-seconds 0))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
      (org-glance-test:external-write graph "id1")
      (let ((calls 0)
            (real (symbol-function 'org-glance-graph:refresh-external)))
        (cl-letf (((symbol-function 'org-glance-graph:refresh-external)
                   (lambda (&rest args) (cl-incf calls) (apply real args))))
          (should (string= "DONE" (org-glance-test:field graph "id1" state))))
        (should (= 1 calls))))))

(ert-deftest org-glance-test:external-read-fold-survives-a-failure ()
  "A fold that signals never breaks the read -- even under `debug-on-error'.
The read serves what the WAL has and the line stays for the next attempt."
  (let ((org-glance-graph-external-poll-seconds 0)
        (debug-on-error t))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
      (org-glance-test:external-write graph "id1")
      (cl-letf (((symbol-function 'org-glance-graph--read-external)
                 (lambda (&rest _) (error "boom"))))
        (should (string= "TODO" (org-glance-test:field graph "id1" state))))
      (should (string-match-p "id1" (org-glance-test:external-text graph)))
      (should (string= "DONE" (org-glance-test:field graph "id1" state))))))

(provide 'test-external)
;;; test-external.el ends here
