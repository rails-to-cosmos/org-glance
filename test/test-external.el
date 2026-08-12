;;; test-external.el --- Tests for the external-write notification file  -*- lexical-binding: t -*-

;; The cross-repo contract with `glance': an outside writer edits or removes a
;; blob and names its id in `meta/EXTERNAL.jsonl';
;; `org-glance-graph:refresh-external' folds those entries back into the WAL --
;; a re-derived record for a write, a tombstone for a delete -- and shortens the
;; file.  See the commentary in `org-glance-graph.el'.

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

(cl-defun org-glance-test:external-delete (graph &rest ids)
  "Append a DELETE notification line for each of IDS to GRAPH's `EXTERNAL.jsonl'.
Hand-spelled like `org-glance-test:external-write', whose two fields it repeats:
the third is the whole difference between the two line shapes."
  (let ((path (org-glance-graph:external-path graph))
        (shape "{\"id\":\"%s\",\"at\":\"2026-08-03T04:21:07Z\",\"tombstone\":true}\n"))
    (f-mkdir-full-path (f-dirname path))
    (f-append-text (mapconcat (lambda (id) (format shape id)) ids "") 'utf-8 path)))

(cl-defun org-glance-test:external-raw (graph line)
  "Append LINE verbatim to GRAPH's `EXTERNAL.jsonl'.
For a shape neither speller above can make: a field this reader knows nothing
about, or a `tombstone' that is not JSON true."
  (let ((path (org-glance-graph:external-path graph)))
    (f-mkdir-full-path (f-dirname path))
    (f-append-text line 'utf-8 path)))

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

;;; Deletes

(ert-deftest org-glance-test:external-refresh-folds-a-delete-in ()
  "A tombstone line deletes the entry: `get-headline' answers the symbol and the
read-only collapse answers nil.  The file is emptied and kept, as a write's is."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:external-delete graph "id1")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (eq 'tombstone (org-glance-graph:get-headline graph "id1")))
    (should-not (org-glance-graph:live-meta graph "id1"))
    (should (string= "" (org-glance-test:external-text graph)))
    (should (f-exists? (org-glance-graph:external-path graph)))))

(ert-deftest org-glance-test:external-refresh-deletes-a-blob-that-is-gone ()
  "A delete needs no blob.  The daemon moves the bytes out of the tree BEFORE it
says so, so a write's `no stored blob' skip must never reach a tombstone line."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (f-delete (org-glance-graph:content-path graph "id1"))
    (org-glance-test:external-delete graph "id1")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (eq 'tombstone (org-glance-graph:get-headline graph "id1")))))

(ert-deftest org-glance-test:external-refresh-folds-a-write-and-a-delete ()
  "Two ids of two kinds fold in one pass, and the batch is ONE append -- the
crash rule rests on the whole fold landing before the file is shortened."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-graph:add graph (org-glance-test:headline "id2" "* TODO bar"))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (org-glance-test:external-write graph "id1")
    (org-glance-test:external-delete graph "id2")
    (let ((appends 0)
          (real (symbol-function 'org-glance-graph--append)))
      (cl-letf (((symbol-function 'org-glance-graph--append)
                 (lambda (&rest args) (cl-incf appends) (apply real args))))
        (should (= 2 (org-glance-graph:refresh-external graph))))
      (should (= 1 appends)))
    (should (string= "DONE" (org-glance-test:field graph "id1" state)))
    (should (eq 'tombstone (org-glance-graph:get-headline graph "id2")))
    (should (equal '("id1") (org-glance-test:ids graph)))
    (should (string= "" (org-glance-test:external-text graph)))))

(ert-deftest org-glance-test:external-refresh-takes-the-last-sighting ()
  "A write then a delete for one id is the DELETE: a blob edited and then
removed inside one window must leave no live record pointing at bytes that
have gone -- the drift the third field exists to close."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (org-glance-test:external-write graph "id1")
    (org-glance-test:external-delete graph "id1")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (eq 'tombstone (org-glance-graph:get-headline graph "id1")))))

(ert-deftest org-glance-test:external-refresh-reads-the-last-sighting-either-way ()
  "The rule is the LAST sighting rather than a tombstone winning: a delete then
a write for one id is the write, and the entry stays live."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (org-glance-test:external-delete graph "id1")
    (org-glance-test:external-write graph "id1")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (string= "DONE" (org-glance-test:field graph "id1" state)))))

(ert-deftest org-glance-test:external-refresh-skips-a-delete-it-cannot-make ()
  "A delete naming an id the store never had, and one naming an already
tombstoned id, each cost the line and nothing else: no record, no count."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-graph:delete graph "id1")
    (let ((before (org-glance-test:count-records graph)))
      (org-glance-test:external-delete graph "id1" "ghost")
      (should (= 0 (org-glance-graph:refresh-external graph)))
      (should (= before (org-glance-test:count-records graph))))
    (should (eq 'tombstone (org-glance-graph:get-headline graph "id1")))
    (should (string= "" (org-glance-test:external-text graph)))))

(ert-deftest org-glance-test:external-refresh-bumps-the-tag-removal-counter ()
  "A folded delete reaches the tag sidecar the way `graph:delete' does: the
batch is appended while the id is still live, so the before-append hook can
still resolve its tags."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A :x:"))
    (org-glance-test:external-delete graph "a")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (= 1 (plist-get (cdr (assoc "x" (org-glance-tag-metrics--read graph)))
                            :removals)))))

;;; Cross-version compatibility

(ert-deftest org-glance-test:external-json-true-is-t ()
  "The platform fact the kind test rests on: `json-parse-string' maps JSON true
to t, and false to the NON-NIL symbol plist truthiness would read as a delete."
  (should (eq t (plist-get (json-parse-string "{\"tombstone\":true}"
                                              :object-type 'plist)
                           :tombstone)))
  (should (eq :false (plist-get (json-parse-string "{\"tombstone\":false}"
                                                   :object-type 'plist)
                                :tombstone))))

(ert-deftest org-glance-test:external-refresh-ignores-an-unknown-key ()
  "A key this reader does not know is inert and the line folds as a write.
That is what a NEW glance rests on when it meets an OLD org-glance, and what
keeps a field invented later from being read as a delete."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (org-glance-test:external-raw
     graph "{\"id\":\"id1\",\"at\":\"2026-08-03T04:21:07Z\",\"op\":\"delete\"}\n")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (string= "DONE" (org-glance-test:field graph "id1" state)))
    (should (org-glance-graph:live-meta graph "id1"))))

(ert-deftest org-glance-test:external-refresh-deletes-on-json-true-alone ()
  "Only JSON true is a delete: `false' and the STRING \"true\" are ordinary
writes, so plist truthiness cannot creep into the kind test."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-graph:add graph (org-glance-test:headline "id2" "* TODO bar"))
    (org-glance-test:external-raw
     graph (concat "{\"id\":\"id1\",\"at\":\"2026-08-03T04:21:07Z\","
                   "\"tombstone\":false}\n"
                   "{\"id\":\"id2\",\"at\":\"2026-08-03T04:21:07Z\","
                   "\"tombstone\":\"true\"}\n"))
    (should (= 2 (org-glance-graph:refresh-external graph)))
    (should (org-glance-graph:live-meta graph "id1"))
    (should (org-glance-graph:live-meta graph "id2"))))

;;; Known hazards, pinned
;;
;; Two costs the fold is shipped with, each written up in docs/invariants.org
;; (H1, H2) and in this module's commentary.  These tests pin TODAY's behaviour
;; so closing either hazard turns its own case red and names the decision.

(ert-deftest org-glance-test:external-truncate-race-eats-a-tombstone ()
  "H1: two Emacsen folding one store take no lock, and a lost tombstone never
self-heals.  E2 reads 74 characters; E1 folds and empties the file; the writer
appends the 91-character tombstone; E2 truncates by the 74 it counted against
the file E1 already emptied, leaving a fragment no reader parses.  The record
stays LIVE, and every later fold leaves it that way."
  (org-glance-test:with-graph graph
    (let ((id "e3b0c442-98fc-1c14-9afb-f4c8996fb924")
          (path (org-glance-graph:external-path graph)))
      (org-glance-graph:add graph (org-glance-test:headline id "* TODO foo"))
      (org-glance-test:external-write graph id)
      (pcase-let ((`(,e2-text . ,_) (org-glance-graph--read-external graph)))
        (should (= 74 (length e2-text)))
        (org-glance-graph:refresh-external graph)          ; E1 empties the file
        (org-glance-test:external-delete graph id)         ; the writer's tombstone
        (should (= 91 (length (org-glance-test:external-text graph))))
        (org-glance-graph--truncate-external graph (length e2-text)))
      (should (string= "tombstone\":true}\n" (f-read-text path 'utf-8)))
      ;; the fragment parses as nothing, so no later fold ever sees the delete
      (should (= 0 (org-glance-graph:refresh-external graph)))
      (should (org-glance-graph:live-meta graph id)))))

(ert-deftest org-glance-test:external-truncate-race-spares-a-write ()
  "H1's other half: the SAME race over a write note costs nothing lasting.
The blob on disk is still the truth, so the next edit of that id appends
another note and the record catches up."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:external-write graph "id1")
    (pcase-let ((`(,e2-text . ,_) (org-glance-graph--read-external graph)))
      (org-glance-graph:refresh-external graph)
      (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
      (org-glance-test:external-write graph "id1")        ; the note this race eats
      (org-glance-graph--truncate-external graph (length e2-text)))
    (should (string= "" (org-glance-test:external-text graph)))
    (should (string= "TODO" (org-glance-test:field graph "id1" state)))
    ;; the next edit says so again, and the record catches up
    (org-glance-test:external-write graph "id1")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (string= "DONE" (org-glance-test:field graph "id1" state)))))

(ert-deftest org-glance-test:external-unparseable-file-is-never-shortened ()
  "H1's residue: a file whose every line is unparseable names no entry, so the
fold's `(when entries ...)' guard never truncates and the fragment survives
every later fold.  What a truncate race leaves behind, it leaves for good."
  (org-glance-test:with-graph graph
    (org-glance-test:external-raw graph "tombstone\":true}\n")
    (dotimes (_ 3)
      (should (= 0 (org-glance-graph:refresh-external graph)))
      (should (string= "tombstone\":true}\n" (org-glance-test:external-text graph))))))

(ert-deftest org-glance-test:external-delete-is-undone-by-an-open-buffer ()
  "H2: the fold's tombstone arm inserts the spec and touches no buffer, so an
open material buffer survives a folded delete and its next save appends a LIVE
record over the tombstone -- and writes the blob back.  `material:delete'
discards that buffer instead (see
`org-glance-test:material-delete-referrer-aware'), because discarding a DIRTY
one needs a human and a fold has none.  What does not come back is the blob's
occurrence history, which went with the directory."
  (org-glance-test:with-graph graph
    (let* ((id "id1")
           (path (org-glance-graph:content-path graph id))
           (snapshot (f-join (f-dirname path) "occurrences" "20260803T042107.org")))
      (org-glance-graph:add graph (org-glance-test:headline id "* TODO foo"))
      (org-glance-test:write snapshot "* DONE foo\n")
      (org-glance-test:with-material (buffer graph id)
        (f-delete (f-dirname path) t)                ; the daemon takes the blob dir
        (org-glance-test:external-delete graph id)
        (should (= 1 (org-glance-graph:refresh-external graph)))
        (should (eq 'tombstone (org-glance-graph:get-headline graph id)))
        (should (buffer-live-p buffer))
        (goto-char (point-max))
        (insert "typed after the delete\n")
        ;; org's own question, since the directory went with the blob
        (org-glance-test:answering ((y-or-n-p t))
          (org-glance-test:save)))
      (should (org-glance-graph:live-meta graph id))
      (should (f-exists? path))
      (should-not (f-exists? snapshot)))))

(ert-deftest org-glance-test:external-delete-after-a-save-strands-the-blob ()
  "H2's visible half.  A save landing BEFORE the fold reads leaves the tombstone
standing over a blob that is back on disk -- the one interleaving a store-level
instrument can see, since `glance''s scan counts a blob no live record names."
  (org-glance-test:with-graph graph
    (let* ((id "id1")
           (path (org-glance-graph:content-path graph id)))
      (org-glance-graph:add graph (org-glance-test:headline id "* TODO foo"))
      (org-glance-test:with-material (buffer graph id)
        (f-delete (f-dirname path) t)
        (org-glance-test:external-delete graph id)
        (goto-char (point-max))
        (insert "typed before the fold\n")
        (org-glance-test:answering ((y-or-n-p t))
          (org-glance-test:save)))
      (should (= 1 (org-glance-graph:refresh-external graph)))
      (should (eq 'tombstone (org-glance-graph:get-headline graph id)))
      (should (f-exists? path)))))

(provide 'test-external)
;;; test-external.el ends here
