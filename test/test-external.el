;;; test-external.el --- Tests for the external-write notification file  -*- lexical-binding: t -*-

;; The cross-repo contract with `glance' (invariants 33-34): it names edited
;; and removed blob ids in `meta/EXTERNAL.jsonl' and a fold moves a CURSOR past
;; them, rewriting nothing.  Commentary in `src/data/org-glance-graph.el'.

(require 'test-helpers)

(cl-defun org-glance-test:external-line (id &optional tombstone)
  "The frozen notification line naming ID, a DELETE when TOMBSTONE.
Spells the line by hand, with no JSON encoder in between, so a change to the
field names or their order fails here.  ONE speller for both shapes: the third
field is the whole difference between them, and two copies could drift apart."
  (format "{\"id\":\"%s\",\"at\":\"2026-08-03T04:21:07Z\"%s}\n"
          id (if tombstone ",\"tombstone\":true" "")))

(cl-defun org-glance-test:external-write (graph &rest ids)
  "Append a notification line for each of IDS to GRAPH's `EXTERNAL.jsonl'."
  (let ((path (org-glance-graph:external-path graph)))
    (f-mkdir-full-path (f-dirname path))
    (f-append-text (mapconcat #'org-glance-test:external-line ids "") 'utf-8 path)))

(cl-defun org-glance-test:external-delete (graph &rest ids)
  "Append a DELETE notification line for each of IDS to GRAPH's `EXTERNAL.jsonl'."
  (let ((path (org-glance-graph:external-path graph)))
    (f-mkdir-full-path (f-dirname path))
    (f-append-text
     (mapconcat (lambda (id) (org-glance-test:external-line id t)) ids "")
     'utf-8 path)))

(defconst org-glance-test:external-filler-id "aaaaaaaaaaaaaaaaaaaa"
  "A 20-character id, so its WRITE line is a TOMBSTONE's own length.
What a generation is filled with where a case has to re-lay its first line at
that line's exact size.  ONE id however many lines, since the fold keeps one
entry per id: filling a file costs a fold one re-derivation.")

(defconst org-glance-test:external-write-bytes
  (string-bytes (org-glance-test:external-line "id1"))
  "Bytes in a WRITE line naming a three-character id.
`org-glance-test:external-line' is fixed-width, so a case names an offset or a
file size with this instead of the number it happens to be.")

(defconst org-glance-test:external-line-bytes
  (string-bytes (org-glance-test:external-line org-glance-test:external-filler-id))
  "Bytes in a WRITE line naming `org-glance-test:external-filler-id'.
A TOMBSTONE naming a three-character id is this long too, which is what lets a
case re-lay a generation's first line at that line's exact size; the two are
asserted equal where the trick is used.")

(cl-defun org-glance-test:external-fill (graph n)
  "Append N filler WRITE lines to GRAPH's live notification file."
  (apply #'org-glance-test:external-write graph
         (make-list n org-glance-test:external-filler-id)))

(cl-defun org-glance-test:external-generation (graph n text)
  "Write TEXT as GRAPH's rotated notification generation N; return its path.
Built by hand instead of by rotating, so the DRAIN ORDER can be asked about with
none of rotation's own conditions standing between the question and it."
  (let ((path (org-glance-graph--external-generation-path graph n)))
    (org-glance-test:write path text)
    path))

(cl-defun org-glance-test:generation-name (graph n &optional cursor)
  "Basename of GRAPH's rotated notification generation N, or of its CURSOR.
Off `--external-generation-path' and `--external-cursor-path', so a case names
a generation the way rotation spells one and no literal here can drift from the
format string."
  (let ((path (org-glance-graph--external-generation-path graph n)))
    (f-filename (if cursor (org-glance-graph--external-cursor-path path) path))))

(cl-defun org-glance-test:rotate-now (graph)
  "Push GRAPH's live notification file past the cap, fold it, and rotate.
Rotation's own condition and nothing between the call and the moves: the survey
entry `org-glance-test:spend' hands back is the reading a fold would have given
it, so what it says about the live file is what rotation reads.  The two lines
name `id1', which every case using this has already added."
  (org-glance-test:external-write graph "id1" "id1")
  (org-glance-graph--rotate-external-maybe
   graph (list (org-glance-test:spend
                graph (org-glance-graph:external-path graph)))))

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

(cl-defun org-glance-test:external-pending (graph)
  "The notification bytes of GRAPH no fold has taken yet, decoded.
Every source's tail in drain order.  Empty means the cursors have caught up --
which is what \"the file was emptied\" used to mean, back when a fold rewrote it."
  (mapconcat (lambda (path) (car (org-glance-graph--external-tail path)))
             (org-glance-graph--external-sources graph) ""))

(cl-defun org-glance-test:external-cursor (graph)
  "How far GRAPH's live notification file has been folded, in bytes."
  (org-glance-graph--external-folded (org-glance-graph:external-path graph)))

(cl-defun org-glance-test:external-cursor-path (graph)
  "Path of the cursor beside GRAPH's live notification file."
  (org-glance-graph--external-cursor-path (org-glance-graph:external-path graph)))

(cl-defun org-glance-test:external-cursor-text (graph)
  "GRAPH's live cursor file as text, or nil when there is none.
What the cursor RECORDS, where `org-glance-test:external-cursor' is what a fold
makes of it -- the two come apart the moment the digest refuses the offset."
  (let ((path (org-glance-test:external-cursor-path graph)))
    (when (f-exists? path) (f-read-text path 'utf-8))))

(cl-defun org-glance-test:external-cursor-at (graph offset)
  "GRAPH's live cursor text with its offset rewritten to OFFSET.
AN HONEST FIXTURE for a number the cursor never named: the digests stay the ones
that file's own bytes hash to, so nothing but the offset can refuse the line."
  (pcase (split-string (s-trim (org-glance-test:external-cursor-text graph)) nil t)
    (`(,_ ,window ,prefix) (format "%d %s %s\n" offset window prefix))))

(cl-defun org-glance-test:spend (graph path)
  "Move PATH's cursor in GRAPH to the end, as a fold reading it whole would.
Return the `--external-survey' entry that reading leaves, so a case can hand
rotation the pass it just simulated.  The digest comes out of
`--external-tail', which is the rule itself: a cursor is written with the digest
of the bytes some read TOOK.  A file with nothing owed gets no cursor, which is
what a fold does too."
  (let ((tail (org-glance-graph--external-tail path)))
    (when (and (car tail) (not (string-empty-p (car tail))))
      (apply #'org-glance-graph--set-external-cursor graph path (cdr tail)))
    (list path nil tail)))

(cl-defun org-glance-test:external-two-lines (graph)
  "Two entries, `b' edited outside, and `id1''s line written but not folded.
The setup both re-laying cases share; folding it leaves the one-line cursor each
of them then moves the bytes under."
  (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
  (org-glance-graph:add graph (org-glance-test:headline "b" "* TODO bar"))
  (org-glance-test:edit-blob graph "b" "* TODO bar" "* DONE bar")
  (org-glance-test:external-write graph "id1"))

(cl-defun org-glance-test:external-relaid (graph)
  "Re-lay GRAPH's notification file with a line inserted ahead of the cursor.
An UNFOLDED line for `b' goes AHEAD of the folded line for `id1' -- what a hand
edit, a restore from a backup or a botched rotation leaves.  Both ids are the
same length and the stamp is fixed-width, so the recorded offset still lands on
a line boundary and nothing but the digest can tell that anything moved."
  (org-glance-test:write (org-glance-graph:external-path graph)
                         (concat (org-glance-test:external-line "b")
                                 (org-glance-test:external-line "id1"))))

(cl-defun org-glance-test:external-refolds-the-relaid-file (graph)
  "Assert GRAPH's re-laid notification file is owed WHOLE and folds to the end.
The tail both re-laying cases share: one line was folded and one line is still
recorded, of a file whose first line is another one now -- so the offset is
refused, the file is taken whole, and the line that moved ahead of the cursor is
adopted.

THE POLL REFUSES IT TOO, and by the WINDOW where a size comparison sees nothing:
an insertion ahead of the offset shifts every byte after it, so the bytes the
cursor claims are another line's now and the recorded offset no longer ends
one."
  (let ((path (org-glance-graph:external-path graph)))
    (should (org-glance-graph--external-pending-p graph))
    (should-not (equal (plist-get (org-glance-graph--external-cursor path) :window)
                       (org-glance-graph--external-window
                        path org-glance-test:external-write-bytes))))
  (should (string-prefix-p (format "%d " org-glance-test:external-write-bytes)
                           (org-glance-test:external-cursor-text graph)))
  (should (= 0 (org-glance-test:external-cursor graph)))
  (should (string= (org-glance-test:external-text graph)
                   (org-glance-test:external-pending graph)))
  (should (= 2 (org-glance-graph:refresh-external graph)))
  (should (string= "DONE" (org-glance-test:field graph "b" state)))
  (should (= (f-size (org-glance-graph:external-path graph))
             (org-glance-test:external-cursor graph))))

(cl-defun org-glance-test:git-ignores (graph &rest paths)
  "Ask git which of PATHS the store's `.gitignore' covers; a t/nil per path.
Initialises a repository over GRAPH's own directory and asks `git check-ignore',
which needs no commit and no index.

THE PATTERNS ARE THE WHOLE QUESTION ONLY BECAUSE NOTHING HERE IS TRACKED: git
applies no ignore rule to a path it already holds, so this same call over a
store that committed the notification family answers nil for every one of them,
and no case in this file asks it that way."
  (let ((default-directory (org-glance-graph:directory graph)))
    (call-process "git" nil nil nil "init" "--quiet" ".")
    (mapcar (lambda (path)
              (eq 0 (call-process "git" nil nil nil "check-ignore" "-q" path)))
            paths)))

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
    (should (string= "TODO" (org-glance-test:field graph "id1" state)))
    (org-glance-test:external-write graph "id1")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (string= "DONE" (org-glance-test:field graph "id1" state)))))

(ert-deftest org-glance-test:external-refresh-spends-the-bytes-and-keeps-them ()
  "The fold moves a CURSOR and rewrites nothing: the line stays on disk byte for
byte, stops being pending, and the cursor lands on the file's own size."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:external-write graph "id1")
    (let ((text (org-glance-test:external-text graph))
          (path (org-glance-graph:external-path graph)))
      (org-glance-graph:refresh-external graph)
      (should (string= text (org-glance-test:external-text graph)))
      (should (string= "" (org-glance-test:external-pending graph)))
      (should (= (f-size path) (org-glance-test:external-cursor graph))))))

(ert-deftest org-glance-test:external-cursor-counts-bytes ()
  "The cursor is in BYTES where the fold reads CHARACTERS.  A line carrying a
multibyte id leaves it past that line's character count, and the fold after it
still starts on a line boundary.

AND A FILE THAT ONLY GREW KEEPS ITS CURSOR: the digest guards the offset and
charges growth nothing, so the append below leaves the recorded offset where it
was and the fold after it takes the new line alone."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (org-glance-test:external-raw
     graph "{\"id\":\"café\",\"at\":\"2026-08-03T04:21:07Z\"}\n")
    (org-glance-graph:refresh-external graph)
    (let ((text (org-glance-test:external-text graph))
          (spent (org-glance-test:external-cursor graph)))
      (should (= (string-bytes text) spent))
      (should (> spent (length text)))
      (should (string= "" (org-glance-test:external-pending graph)))
      (org-glance-test:external-write graph "id1")
      (should (= spent (org-glance-test:external-cursor graph)))
      (should (org-glance-graph--external-pending-p graph))
      (should (string= (org-glance-test:external-line "id1")
                       (org-glance-test:external-pending graph))))
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (string= "DONE" (org-glance-test:field graph "id1" state)))))

(ert-deftest org-glance-test:external-refresh-is-idempotent ()
  "Re-running a refresh over the same ids is the crash rule: it costs a record
equal to the one already there and changes no answer."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (org-glance-test:external-write graph "id1")
    (org-glance-graph:refresh-external graph)
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
  "An id the store never had, and one it tombstoned, are skipped -- with their
bytes spent all the same, since neither has a record to replace."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-graph:delete graph "id1")
    (org-glance-test:external-write graph "id1" "ghost")
    (should (= 0 (org-glance-graph:refresh-external graph)))
    (should (eq 'tombstone (org-glance-graph:get-headline graph "id1")))
    (should (string= "" (org-glance-test:external-pending graph)))))

(ert-deftest org-glance-test:external-refresh-without-a-file ()
  "A store no external writer ever touched refreshes nothing and makes nothing."
  (org-glance-test:with-graph graph
    (should (= 0 (org-glance-graph:refresh-external graph)))
    (should-not (f-exists? (org-glance-graph:external-path graph)))
    (should-not (f-exists? (org-glance-test:external-cursor-path graph)))))

(ert-deftest org-glance-test:external-refresh-reads-the-tags-cycle ()
  "A state only a tag's `#+TODO:' cycle declares is re-derived as a STATE.
Without the cycle in scope the keyword folds into the title, exactly as it
would in `org-glance-material:sync' -- which binds it for this reason."
  (org-glance-test:with-graph graph
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
`get-headline' already answers with the edit; the cursor has spent the line."
  (let ((org-glance-graph-external-poll-seconds 0))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
      (org-glance-test:external-write graph "id1")
      (should (string= "DONE" (org-glance-test:field graph "id1" state)))
      (should (string= "" (org-glance-test:external-pending graph))))))

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
The read serves what the WAL has and the line stays pending for the next
attempt, the cursor never having moved."
  (let ((org-glance-graph-external-poll-seconds 0)
        (debug-on-error t))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
      (org-glance-test:external-write graph "id1")
      (cl-letf (((symbol-function 'org-glance-graph--read-external)
                 (lambda (&rest _) (error "boom"))))
        (should (string= "TODO" (org-glance-test:field graph "id1" state))))
      (should (string-match-p "id1" (org-glance-test:external-pending graph)))
      (should (string= "DONE" (org-glance-test:field graph "id1" state))))))

(ert-deftest org-glance-test:external-refresh-folds-a-delete-in ()
  "A tombstone line deletes the entry: `get-headline' answers the symbol and the
read-only collapse answers nil.  The line is spent and kept, as a write's is."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:external-delete graph "id1")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (eq 'tombstone (org-glance-graph:get-headline graph "id1")))
    (should-not (org-glance-graph:live-meta graph "id1"))
    (should (string= "" (org-glance-test:external-pending graph)))
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
crash rule rests on the whole fold landing before the cursor moves."
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
    (should (string= "" (org-glance-test:external-pending graph)))))

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
  "The rule is the LAST sighting, and a tombstone wins by being last: a delete then
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
tombstoned id, each cost their bytes and nothing else: no record, no count."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-graph:delete graph "id1")
    (let ((before (org-glance-test:count-records graph)))
      (org-glance-test:external-delete graph "id1" "ghost")
      (should (= 0 (org-glance-graph:refresh-external graph)))
      (should (= before (org-glance-test:count-records graph))))
    (should (eq 'tombstone (org-glance-graph:get-headline graph "id1")))
    (should (string= "" (org-glance-test:external-pending graph)))))

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

(ert-deftest org-glance-test:external-json-true-is-t ()
  "The platform fact the kind test rests on: `json-parse-string' maps JSON true
to t, and false to the NON-NIL symbol plist truthiness would read as a delete."
  (should (eq t (plist-get (json-parse-string "{\"tombstone\":true}"
                                              :object-type 'plist)
                           :tombstone)))
  (should (eq :false (plist-get (json-parse-string "{\"tombstone\":false}"
                                                   :object-type 'plist)
                                :tombstone))))

(ert-deftest org-glance-test:external-refresh-deletes-on-json-true-alone ()
  "ONLY JSON TRUE IS A DELETE, over the three shapes that are not it.  A key this
reader knows nothing about is INERT and its line folds as a write, which is what
a NEW glance rests on when it meets an OLD org-glance; `false' and the STRING
\"true\" are ordinary writes, so plist truthiness cannot creep into the kind
test.  Three ids, three shapes, one fold, and every record still live."
  (org-glance-test:with-graph graph
    (dolist (id '("id1" "id2" "id3"))
      (org-glance-graph:add graph (org-glance-test:headline id "* TODO foo")))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (org-glance-test:external-raw
     graph (concat "{\"id\":\"id1\",\"at\":\"2026-08-03T04:21:07Z\","
                   "\"op\":\"delete\"}\n"
                   "{\"id\":\"id2\",\"at\":\"2026-08-03T04:21:07Z\","
                   "\"tombstone\":false}\n"
                   "{\"id\":\"id3\",\"at\":\"2026-08-03T04:21:07Z\","
                   "\"tombstone\":\"true\"}\n"))
    (should (= 3 (org-glance-graph:refresh-external graph)))
    (should (string= "DONE" (org-glance-test:field graph "id1" state)))
    (dolist (id '("id1" "id2" "id3"))
      (should (org-glance-graph:live-meta graph id)))))

(ert-deftest org-glance-test:external-race-spares-the-tombstone ()
  "The race that ate a tombstone, run again.  E2 reads 74 characters; E1 folds
them and moves the cursor; the writer appends the 91-character tombstone.  The
tombstone is PAST the cursor, where a size comparison counted it against a file
somebody had shortened, so the next fold takes it and the record is tombstoned."
  (org-glance-test:with-graph graph
    (let ((id "e3b0c442-98fc-1c14-9afb-f4c8996fb924"))
      (org-glance-graph:add graph (org-glance-test:headline id "* TODO foo"))
      (org-glance-test:external-write graph id)
      (should (= 74 (length (org-glance-test:external-pending graph))))
      (org-glance-graph:refresh-external graph)           ; E1 folds the write
      (org-glance-test:external-delete graph id)          ; the writer's tombstone
      (should (= 165 (length (org-glance-test:external-text graph))))
      (should (= 91 (length (org-glance-test:external-pending graph))))
      (should (= 1 (org-glance-graph:refresh-external graph)))
      (should (eq 'tombstone (org-glance-graph:get-headline graph id)))
      (should (string= "" (org-glance-test:external-pending graph))))))

(ert-deftest org-glance-test:external-keeps-a-note-that-arrived-mid-fold ()
  "The window that killed the compare-and-swap: a note landing after the fold
read and before it recorded its position.  The offset was measured BEFORE the
read, so the note is past it, the bytes this run took are the only ones spent,
and the note folds next time."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-graph:add graph (org-glance-test:headline "id2" "* TODO bar"))
    (org-glance-test:external-write graph "id1")
    (let ((read (symbol-function 'org-glance-graph--read-external)))
      (cl-letf (((symbol-function 'org-glance-graph--read-external)
                 (lambda (g &optional survey) (prog1 (funcall read g survey)
                          (org-glance-test:external-delete g "id2")))))
        (should (= 1 (org-glance-graph:refresh-external graph)))))
    (should (string= (org-glance-test:external-line "id2" t)
                     (org-glance-test:external-pending graph)))
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (eq 'tombstone (org-glance-graph:get-headline graph "id2")))))

(ert-deftest org-glance-test:external-keeps-a-byte-identical-note ()
  "The hole a prefix test could not see: a note arriving byte-identical to the
text a fold just read.  Two equal strings are indistinguishable and were dropped
together; an OFFSET counts them instead, so the second survives and does its
work -- here the edit that reaches the WAL through it."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:external-write graph "id1")
    (let ((read (symbol-function 'org-glance-graph--read-external)))
      (cl-letf (((symbol-function 'org-glance-graph--read-external)
                 (lambda (g &optional survey) (prog1 (funcall read g survey)
                          (org-glance-test:external-write g "id1")))))
        (should (= 1 (org-glance-graph:refresh-external graph)))))
    (should (string= "{\"id\":\"id1\",\"at\":\"2026-08-03T04:21:07Z\"}\n"
                     (org-glance-test:external-pending graph)))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (string= "DONE" (org-glance-test:field graph "id1" state)))
    (should (string= "" (org-glance-test:external-pending graph)))))

(ert-deftest org-glance-test:external-double-fold-is-a-no-op ()
  "Two Emacsen folding one range is what no lock costs, and it costs work alone.
E1 folds and moves the cursor; E2, holding the read it took before that, folds
the same range and appends a record equal to E1's, which latest-per-id cannot
tell apart."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (org-glance-test:external-write graph "id1")
    (let ((stale (org-glance-graph--read-external graph))
          (before (org-glance-test:count-records graph)))
      (should (= 1 (org-glance-graph:refresh-external graph)))
      (cl-letf (((symbol-function 'org-glance-graph--read-external)
                 (lambda (_graph &optional _survey) stale)))
        (should (= 1 (org-glance-graph:refresh-external graph))))
      (should (= (+ before 2) (org-glance-test:count-records graph))))
    (should (string= "DONE" (org-glance-test:field graph "id1" state)))
    (should (equal '("id1") (org-glance-test:ids graph)))
    (should (string= "" (org-glance-test:external-pending graph)))))

(ert-deftest org-glance-test:external-a-cursor-that-says-nothing-refolds ()
  "FIVE WAYS A CURSOR SAYS NOTHING, one ladder.  It is GONE, it is GARBLED (a git
conflict marker), it carries NO DIGEST at all, it carries ONE (the shape written
before the window digest existed), or it names a NEGATIVE offset.  Each reads as
0 and costs a re-fold and nothing else -- the file is read whole again, its
records land again, and every answer is the one it already was.

A SIXTH WAY has a case to itself, `--an-offset-past-the-end-is-refused': an
offset past the bytes hashes what the size hashes, so that rung needs its own
digests and its own assertion about the clamp before it means anything."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (org-glance-test:external-write graph "id1")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (let ((cursor (org-glance-test:external-cursor-path graph)))
      (dolist (rung (list nil "<<<<<<< HEAD\n7\n" "7\n" "7 da39a3ee\n"
                          "-1 da39a3ee da39a3ee\n"))
        (if rung
            (f-write-text rung 'utf-8 cursor)
          (f-delete cursor))
        (should (= 0 (org-glance-test:external-cursor graph)))
        (should (string= (org-glance-test:external-text graph)
                         (org-glance-test:external-pending graph)))
        (should (= 1 (org-glance-graph:refresh-external graph)))))
    (should (string= "DONE" (org-glance-test:field graph "id1" state)))
    (should (equal '("id1") (org-glance-test:ids graph)))))

(ert-deftest org-glance-test:external-a-rewrite-ahead-of-the-cursor-refolds ()
  "A REWRITE AHEAD OF THE OFFSET, the door the offset alone could not close.
Unfolded lines land AHEAD of the offset and the cursor is left where it was, so
the fold resumes MID-LINE: the fragment fails `json-parse-string' and is skipped
in silence, the line in front of it is never seen at all, and the cursor
advances past both.  The digest catches it -- the recorded number still stands
in the file, and the fold refuses it and starts over.

A GIT UNION MERGE WAS THIS CASE'S ORIGINAL CAUSE and is no longer one: the
notification family is git-ignored (`--ensure-gitignore'), so nothing merges it
and no cursor is fast-forwarded by a sync.  What still lays these bytes out
again is single-writer and local -- a hand edit, a restore from a backup, a
botched rotation -- which no ignore rule can rule out, and which is what the
digest is for now.

The rewrite lands BETWEEN two folds here; `--a-rewrite-inside-the-fold-window-'
is the same bytes landing INSIDE one, which is the whole difference between the
two cases and the reason they share a tail."
  (org-glance-test:with-graph graph
    (org-glance-test:external-two-lines graph)
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (= org-glance-test:external-write-bytes
               (org-glance-test:external-cursor graph)))
    (org-glance-test:external-relaid graph)
    (org-glance-test:external-refolds-the-relaid-file graph)))

(ert-deftest org-glance-test:external-a-rewrite-inside-the-fold-window-refolds ()
  "THE DIGEST IS TAKEN WHERE THE BYTES ARE READ, and this is the door that
stays open when it is taken anywhere else.  The FOLD WINDOW runs from
`--external-tail''s read to `--set-external-cursor''s write and spans every blob
read, every org re-parse and the `graph:insert' between them, so anything
re-laying the file lands under a fold already in flight.  Hashed at WRITE time
the cursor describes the file AS REWRITTEN -- self-consistent with bytes nobody
folded, and the guard passes forever.  Hashed at READ time, out of the one
buffer the fold sliced its lines from, it describes what was folded, so it no
longer holds and the file is taken whole.

Reproduced end to end with a real `git reset --hard' before it was written down
and before the family was ignored; the shape survives its cause, a hand edit
landing here costing the same fold.  It is the only case in either repo that
goes red under a write-time digest."
  (org-glance-test:with-graph graph
    (org-glance-test:external-two-lines graph)
    (let ((insert (symbol-function 'org-glance-graph:insert)))
      (cl-letf (((symbol-function 'org-glance-graph:insert)
                 (lambda (g specs)
                   (org-glance-test:external-relaid graph)
                   (funcall insert g specs))))
        (should (= 1 (org-glance-graph:refresh-external graph)))))
    (org-glance-test:external-refolds-the-relaid-file graph)))

(ert-deftest org-glance-test:external-a-same-size-rewrite-of-a-drained-file-is-pending ()
  "THE CASE A SIZE COMPARISON LOST.  `Data.Org.External.noteLine' spells
FIXED-WIDTH lines -- one length for every write and another for every delete --
so a whole-file replacement that leaves the size where it was is this file's
TYPICAL re-laying.  What re-lays it is a hand edit, a
restore from a backup, a botched rotation, and a union merge on a store that
tracked the family before the ignore existed.  A drained 58-byte tombstone for
one id, replaced by a 58-byte
tombstone for another: the offset still equals the size, the cursor still ends a
line, and only the WINDOW can tell.  Measured under the size comparison, the
second id stayed live through twenty polls with its tombstone unread."
  (let ((org-glance-graph-external-poll-seconds 0))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "aaa" "* TODO foo"))
      (org-glance-graph:add graph (org-glance-test:headline "bbb" "* TODO bar"))
      (org-glance-test:external-delete graph "aaa")
      (should (= 1 (org-glance-graph:refresh-external graph)))
      (let* ((path (org-glance-graph:external-path graph))
             (size (f-size path)))
        (should (org-glance-graph--external-drained? path))
        ;; THE POLL'S WINDOW IS THE FOLD'S OWN: minted out of the whole file,
        ;; read back out of 4 KiB.  Two spellings of one rule, asserted equal.
        (should (string= (plist-get (org-glance-graph--external-cursor path) :window)
                         (org-glance-graph--external-window path size)))
        (org-glance-test:write path (org-glance-test:external-line "bbb" t))
        (should (= size (f-size path)))
        (should-not (org-glance-graph--external-drained? path))
        (should (org-glance-graph--external-pending-p graph)))
      ;; A PLAIN READ folds it: the poll was the whole of what stood between
      ;; this tombstone and the record it names.
      (should (eq 'tombstone (org-glance-graph:get-headline graph "bbb")))
      (should (string= "" (org-glance-test:external-pending graph))))))

(ert-deftest org-glance-test:external-a-torn-final-line-leaves-the-file-pending ()
  "A CURSOR LANDS ON A LINE BOUNDARY, and one that does not is one the bytes
moved under.  The only way to reach a mid-line offset honestly is the writer's
own torn line: the fold spends it with the rest, so the cursor stops on a byte
that is not a newline and the poll refuses it.

THAT REFUSAL IS THE SAFE DIRECTION -- it costs a fold that finds nothing owed --
which is why the assertion is the POLL's and not the fold's, where it would
re-fold the whole file forever instead.  The next whole line puts the boundary
back.

A TORN LINE THAT RIDES INTO A ROTATION NEVER GETS ONE, nothing appending to a
generation, so that generation reads pending for as long as it exists: one fold
per poll interval that reads every source and takes nothing.  Pure cost and no
loss, every byte of it having been folded, and it ends at the next rotation --
`--external-folded-whole?' makes no newline assertion, for exactly this reason.

THE LINE COSTS ITS OWN ENTRY AND NO OTHER, which is the same fixture's other
half: the good id ahead of it folds, the fragment is skipped in silence, and the
bytes are spent all the same."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (org-glance-test:external-write graph "id1")
    (org-glance-test:external-raw graph "{\"id\":\"tor")   ; the writer's torn line
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (string= "DONE" (org-glance-test:field graph "id1" state)))
    (let ((path (org-glance-graph:external-path graph)))
      (should (= (f-size path) (org-glance-test:external-cursor graph)))
      (should-not (org-glance-graph--external-window path (f-size path)))
      (should-not (org-glance-graph--external-drained? path))
      (should (org-glance-graph--external-pending-p graph))
      ;; and the fold it costs takes NOTHING: the prefix still holds
      (should (string= "" (org-glance-test:external-pending graph)))
      (should (= 0 (org-glance-graph:refresh-external graph)))
      (should (= (f-size path) (org-glance-test:external-cursor graph)))
      (org-glance-test:external-raw graph "\n")
      (should (= 0 (org-glance-graph:refresh-external graph)))
      (should (org-glance-graph--external-drained? path)))))

(ert-deftest org-glance-test:external-the-fold-catches-what-the-window-passed ()
  "THE BOUND'S KNOWN COST, and the half that closes it.  A same-length edit
further back than `org-glance-graph--external-window-bytes' leaves the window
byte-identical, so the POLL reads the file as drained -- invariant 34's
residual, written down here ahead of anybody discovering it.  The FOLD verifies the WHOLE
prefix out of the buffer it already holds, so the moment anything at all is owed
the drift is caught and the file is taken whole.  That is what makes the cheap
check safe HERE: this is the LIVE file, which the writer appends to, so the next
byte owed is what closes the residual.

NOTHING IS EVER OWED ON A ROTATED GENERATION, where the same residual is
permanent -- `--rotation-spares-a-generation-re-laid-mid-fold' is that half, and
why rotation asks the whole prefix, which this window would pass."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-graph:add graph (org-glance-test:headline "id2" "* TODO bar"))
    (org-glance-test:edit-blob graph "id2" "* TODO bar" "* DONE bar")
    (let* ((path (org-glance-graph:external-path graph))
           (line (org-glance-test:external-line "id1")))
      (org-glance-test:write path (apply #'concat (make-list 200 line)))
      (should (= 1 (org-glance-graph:refresh-external graph)))
      (should (> (f-size path) (* 2 org-glance-graph--external-window-bytes)))
      ;; THE POLL'S WINDOW IS THE FOLD'S OWN, on a file where the bound bites:
      ;; the last 4 KiB before the offset, minted whole and read bounded.
      (should (string= (plist-get (org-glance-graph--external-cursor path) :window)
                       (org-glance-graph--external-window path (f-size path))))
      ;; THE FAR-BACK EDIT: the first line names the other id, same length, and
      ;; every byte the window covers is where it was.
      (org-glance-test:write path (concat (org-glance-test:external-line "id2")
                                          (substring (f-read-text path 'utf-8)
                                                     (length line))))
      (should (org-glance-graph--external-drained? path))          ; the residual
      (org-glance-test:external-write graph "id1")                 ; anything owed
      (should (= 2 (org-glance-graph:refresh-external graph)))
      (should (string= "DONE" (org-glance-test:field graph "id2" state))))))

(ert-deftest org-glance-test:external-is-out-of-the-union-resolver ()
  "THE RESOLVER NAMES THE WAL POSITIVELY, so every other JSONL family in
`meta/' is out of it by construction.  The resolver's rewrite would re-lay bytes
under a live fold cursor, and a notification QUEUE is no WAL segment to union in
the first place.  Three families stand here and only the first is touched: the
WAL's open segment, the notification file with its generation, and glance's own
`COMPLETIONS.jsonl', which shares this directory and no rule with either.  An
open heals the WAL and leaves the other two byte for byte, markers and all,
where the fold's own skip-a-bad-line rule costs those lines and no others.

MARKERS CANNOT REACH THE NOTIFICATION FAMILY BY SYNC ANY MORE -- it is ignored
(`--ensure-gitignore') -- so what this fixture writes by hand is what a store
tracked before the ignore, or a hand edit, can still leave.  The exclusion is
worth exactly as much either way: the resolver must not rewrite a file a cursor
points into."
  (org-glance-test:with-graph graph
    (let* ((org-glance-conflict-resolution 'union)
           (meta (org-glance-graph:meta-path graph))
           (open (org-glance-graph:headline-meta-path graph))
           (external (org-glance-graph:external-path graph))
           (completions (f-join meta "COMPLETIONS.jsonl"))
           (generation (org-glance-graph--external-generation-path graph 1))
           (marked (org-glance-test:conflict-open
                    (org-glance-test:external-line "id1")
                    (org-glance-test:external-line "id2"))))
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-test:write open (concat (f-read-text open 'utf-8)
                                          (org-glance-test:conflict-open "" "")))
      (dolist (path (list external completions))
        (org-glance-test:write path marked))
      (org-glance-test:external-generation graph 1 marked)
      (should (equal (list open) (org-glance-graph--conflicted-jsonl-files graph)))
      (let ((graph (org-glance-test:reopen graph)))
        (should-not (s-contains? "<<<<<<<" (f-read-text open 'utf-8)))
        (dolist (path (list external completions generation))
          (should (string= marked (f-read-text path 'utf-8))))
        (should (org-glance-graph:live-meta graph "id1"))))))

(ert-deftest org-glance-test:external-drains-a-generation-before-the-live-file ()
  "THE ORDER IS THE RULE: a rotated generation is drained to completion before
the live file, so ids reach the fold in the order the writer left them.  Here a
WRITE for one id sits in the generation and its TOMBSTONE in the live file --
last sighting wins, so drained in order the record ends tombstoned.  Read the
other way round the write is last and the record stays live, which is what makes
this case the one that dies when `--external-sources' is reversed."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (org-glance-test:external-generation graph 1 (org-glance-test:external-line "id1"))
    (org-glance-test:external-delete graph "id1")
    (should (equal (list (org-glance-graph--external-generation-path graph 1)
                         (org-glance-graph:external-path graph))
                   (org-glance-graph--external-sources graph)))
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (eq 'tombstone (org-glance-graph:get-headline graph "id1")))
    (should-not (org-glance-graph:live-meta graph "id1"))
    (should (string= "" (org-glance-test:external-pending graph)))))

(ert-deftest org-glance-test:external-unparseable-bytes-are-spent ()
  "A file whose every line is unparseable names no entry and is spent anyway:
the cursor moves by BYTES, so a fragment costs one fold where every later one
would read it again -- and the bytes themselves stay where they were."
  (org-glance-test:with-graph graph
    (org-glance-test:external-raw graph "tombstone\":true}\n")
    (should (= 0 (org-glance-graph:refresh-external graph)))
    (should (string= "" (org-glance-test:external-pending graph)))
    (should (string= "tombstone\":true}\n" (org-glance-test:external-text graph)))))

(ert-deftest org-glance-test:external-rotates-when-the-cursor-has-caught-up ()
  "Rotation renames the file and its cursor together, so the generation is born
fully folded and the live path is free for the writer to create again."
  (let ((org-glance-graph-external-max-bytes 60))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-test:external-write graph "id1" "id1")     ; two lines, past 60
      (should (= 1 (org-glance-graph:refresh-external graph)))
      (should-not (f-exists? (org-glance-graph:external-path graph)))
      (should-not (f-exists? (org-glance-test:external-cursor-path graph)))
      (should (equal (list (org-glance-test:generation-name graph 1))
                     (org-glance-graph--external-generations graph)))
      (should (string= "" (org-glance-test:external-pending graph)))
      (org-glance-test:external-write graph "id1")
      (should (string= "{\"id\":\"id1\",\"at\":\"2026-08-03T04:21:07Z\"}\n"
                       (org-glance-test:external-pending graph)))
      (should (= 1 (org-glance-graph:refresh-external graph))))))

(ert-deftest org-glance-test:external-holds-a-file-under-the-rotation-size ()
  "A file the cursor has caught up with is left alone while it is small: nothing
rotates until it is worth rotating."
  (let ((org-glance-graph-external-max-bytes (* 1024 1024)))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-test:external-write graph "id1")
      (should (= 1 (org-glance-graph:refresh-external graph)))
      (should (f-exists? (org-glance-graph:external-path graph)))
      (should-not (org-glance-graph--external-generations graph)))))

(ert-deftest org-glance-test:external-folds-a-line-that-landed-in-a-rotated-file ()
  "THE ROTATION HAZARD, handled.  The writer opens the live path per line, so a
rename landing between its open and its write puts that line in the ROTATED
file.  A generation is drained ahead of the live one, so the line is folded
there, and the name it was written under costs nothing."
  (let ((org-glance-graph-external-max-bytes 60))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-graph:add graph (org-glance-test:headline "id2" "* TODO bar"))
      (org-glance-test:edit-blob graph "id2" "* TODO bar" "* DONE bar")
      (org-glance-test:external-write graph "id1" "id1")
      (org-glance-graph:refresh-external graph)             ; this pass rotates
      (let ((rotated (f-join (org-glance-graph:meta-path graph)
                             (car (org-glance-graph--external-generations graph)))))
        (f-append-text "{\"id\":\"id2\",\"at\":\"2026-08-03T04:21:07Z\"}\n"
                       'utf-8 rotated))
      (should (string-match-p "id2" (org-glance-test:external-pending graph)))
      (should (= 1 (org-glance-graph:refresh-external graph)))
      (should (string= "DONE" (org-glance-test:field graph "id2" state)))
      (should (string= "" (org-glance-test:external-pending graph))))))

(ert-deftest org-glance-test:external-carries-two-generations ()
  "A generation is NEVER retired on the pass that made it -- an open the writer
took before the rename may still be about to write into it.  The older one goes
at the START of the next rotation, so at most two FOLDED-WHOLE ones stand in the
LIVE directory and each survives a whole rotation cycle.  Its cursor goes with
it, so nothing in `meta/' names a generation that is not there."
  (let ((org-glance-graph-external-max-bytes 60))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (dolist (round '(1 2 3))
        (org-glance-test:external-write graph "id1" "id1")
        (should (= 1 (org-glance-graph:refresh-external graph)))
        (should (= (min round 2)
                   (length (org-glance-graph--external-generations graph)))))
      (should (equal (list (org-glance-test:generation-name graph 2)
                           (org-glance-test:generation-name graph 3))
                     (org-glance-graph--external-generations graph)))
      (should-not (f-exists?
                   (org-glance-graph--external-cursor-path
                    (org-glance-graph--external-generation-path graph 1))))
      (should (f-exists? (org-glance-graph--external-spent-path
                          graph (org-glance-test:generation-name graph 1 t)))))))

(ert-deftest org-glance-test:external-rotates-under-an-append ()
  "THE CAP BOUNDS THE FILE, and a store being written to is where that has to
hold.  Rotation reads the offset THE FOLD TOOK.  Asking the file its size again
afterwards left it undrained whenever a line landed inside the fold window, and one append per fold deferred the rename for as long as the
writing lasted, which is a cap that bounds nothing.

The line that arrived mid-fold rides into the generation and is folded there,
generations being drained ahead of the live file -- so bounding the file costs
no line."
  (let ((org-glance-graph-external-max-bytes 60))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-graph:add graph (org-glance-test:headline "id2" "* TODO bar"))
      (org-glance-test:edit-blob graph "id2" "* TODO bar" "* DONE bar")
      (org-glance-test:external-write graph "id1" "id1")   ; 82 bytes, past the cap
      (let ((insert (symbol-function 'org-glance-graph:insert)))
        (cl-letf (((symbol-function 'org-glance-graph:insert)
                   (lambda (g specs)
                     (org-glance-test:external-write graph "id2")
                     (funcall insert g specs))))
          (should (= 1 (org-glance-graph:refresh-external graph)))))
      (should-not (f-exists? (org-glance-graph:external-path graph)))
      (should (equal (list (org-glance-test:generation-name graph 1))
                     (org-glance-graph--external-generations graph)))
      (should (string= (org-glance-test:external-line "id2")
                       (org-glance-test:external-pending graph)))
      (should (= 1 (org-glance-graph:refresh-external graph)))
      (should (string= "DONE" (org-glance-test:field graph "id2" state)))
      (should (string= "" (org-glance-test:external-pending graph))))))

(ert-deftest org-glance-test:external-rotation-spares-an-undrained-generation ()
  "ROTATION RETIRES ON A CURSOR AND NOT ON A POSITION.  A generation carrying no
cursor has been folded by nobody, and taking it out of the live directory leaves
every tombstone in it unfolded until somebody puts it back.

Three generations stand here: the OLDEST carries a tombstone no cursor has
spent, the MIDDLE one is drained, and the NEWEST is spared by position whatever
this rule says.  The rotation keeps the undrained one and retires the drained
one, and the fold after it makes the delete that generation always named.

TWO CASES FOR ONE RULE, and the TIMING is what each pins.  Here the undrained
generation is on disk when the survey lists the sources, so it HAS an entry and
rotation reads a refused verdict off it; in `--spares-a-generation-that-appeared-
mid-fold' it appears after the listing and has NO entry at all, which is the
fallback path.  Merged, whichever one ran second would prove nothing."
  (let ((org-glance-graph-external-max-bytes 60))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-graph:add graph (org-glance-test:headline "id2" "* TODO bar"))
      (let ((stale (org-glance-test:external-generation
                    graph 1 (org-glance-test:external-line "id2" t)))
            (retired (org-glance-test:external-generation
                      graph 2 (org-glance-test:external-line "id1")))
            (newest (org-glance-test:external-generation graph 3 "")))
        (org-glance-test:spend graph retired)
        (org-glance-test:rotate-now graph)
        (should (f-exists? stale))
        (should-not (f-exists? retired))
        (should (f-exists? (org-glance-graph--external-spent-path
                            graph (org-glance-test:generation-name graph 2))))
        (should (f-exists? newest))
        (should (equal (list (org-glance-test:generation-name graph 1)
                             (org-glance-test:generation-name graph 3)
                             (org-glance-test:generation-name graph 4))
                       (org-glance-graph--external-generations graph)))
        ;; the tombstone the position rule would have retired is still owed,
        ;; and folds
        (should (string-match-p "tombstone" (org-glance-test:external-pending graph)))
        (should (= 1 (org-glance-graph:refresh-external graph)))
        (should (eq 'tombstone (org-glance-graph:get-headline graph "id2")))
        (should (string= "" (org-glance-test:external-pending graph)))))))

(ert-deftest org-glance-test:external-rotation-spares-a-re-laid-generation ()
  "ROTATION ASKS `--external-folded-whole?', being the one place in this file
where being wrong cannot be repaired by folding again -- a retired generation is
no longer a source, so its bytes wait for a reader who noticed, where the next
fold would have come on its own.  A generation folded to its end and then RE-LAID at the same
length -- the fixed-width shape any rewrite of this file leaves -- is not folded
whole, and retiring it would take the line it carries now out of reach.  A bare
size comparison called it drained and deleted it outright.

THE RE-LAYING HERE IS INSIDE THE WINDOW, a one-line file being shorter than one,
so even the poll's bounded question would catch it.  `--spares-a-generation-re-
laid-mid-fold' is the same shape OUTSIDE the window, where only the prefix can."
  (let ((org-glance-graph-external-max-bytes 60))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-graph:add graph (org-glance-test:headline "id2" "* TODO bar"))
      (org-glance-test:edit-blob graph "id2" "* TODO bar" "* DONE bar")
      (let ((relaid (org-glance-test:external-generation
                     graph 1 (org-glance-test:external-line "id1")))
            (newest (org-glance-test:external-generation graph 2 "")))
        (org-glance-test:spend graph relaid)
        ;; SAME LENGTH, another id: both are three characters and the stamp is
        ;; fixed-width, so nothing but the window can tell the file moved.
        (org-glance-test:write relaid (org-glance-test:external-line "id2"))
        (should (= org-glance-test:external-write-bytes (f-size relaid)))
        (org-glance-test:rotate-now graph)
        (should (f-exists? relaid))
        (should (f-exists? newest))
        ;; the line the position rule -- and the size comparison -- would have
        ;; unlinked is still owed, and folds
        (should (= 1 (org-glance-graph:refresh-external graph)))
        (should (string= "DONE" (org-glance-test:field graph "id2" state)))
        (should (string= "" (org-glance-test:external-pending graph)))))))

(ert-deftest org-glance-test:external-rotation-spares-a-generation-re-laid-mid-fold ()
  "THE EXACT QUESTION IS ROTATION'S, and this is the case that says why.  A
same-length re-laying further back than `--external-window-bytes' leaves the
window byte-identical, so `--external-drained?' blesses it for as long as the
file exists: nothing appends to a rotated generation, so no fold ever comes
along to put a prefix check in front of it.  Rotation asks
`--external-folded-whole?' over the whole prefix, SPARES what it refuses and
drops that generation's cursor (`--external-refold'), so the next fold takes the
file whole and the tombstone lands.  A bare size comparison unlinked it.

Every generation here is made by ROTATION and the re-lay is driven from inside
`graph:insert', the seam `--a-rewrite-inside-the-fold-window-refolds' declares
real: the third fold appends past the cap, re-lays the oldest generation while
it runs -- same size, a tombstone as its first line, the last 4 KiB
byte-identical -- and then rotates."
  (let ((org-glance-graph-external-max-bytes 6000))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id2" "* TODO bar"))
      (org-glance-graph:add graph (org-glance-test:headline
                                   org-glance-test:external-filler-id "* TODO fill"))
      ;; TWO ROTATIONS: a generation is a candidate only once another stands
      ;; behind it, the newest being spared by position.
      (dotimes (_ 2)
        (org-glance-test:external-fill graph 120)
        (should (= 1 (org-glance-graph:refresh-external graph))))
      (let* ((gen1 (org-glance-graph--external-generation-path graph 1))
             (tomb (org-glance-test:external-line "id2" t))
             (size (* 120 org-glance-test:external-line-bytes))
             window)
        ;; THE FIXTURE'S WHOLE TRICK, asserted here so a change to either speller
        ;; reddens this case:
        ;; filler's WRITE line and this TOMBSTONE are the same length, so the
        ;; re-lay moves the first line and leaves every byte behind it.
        (should (= (length tomb) org-glance-test:external-line-bytes))
        (should (= size (f-size gen1)))
        (should (org-glance-graph--external-folded-whole? gen1))
        (setq window (plist-get (org-glance-graph--external-cursor gen1) :window))
        (org-glance-test:external-fill graph 120)
        (let ((insert (symbol-function 'org-glance-graph:insert)))
          (cl-letf (((symbol-function 'org-glance-graph:insert)
                     (lambda (g specs)
                       (org-glance-test:write
                        gen1 (concat tomb (substring (f-read-text gen1 'utf-8)
                                                     (length tomb))))
                       (funcall insert g specs))))
            (should (= 1 (org-glance-graph:refresh-external graph)))))
        (should (f-exists? gen1))
        (should (= size (f-size gen1)))
        ;; THE BOUND CANNOT SEE IT AND SAYS SO: same size, and every byte the
        ;; window covers is where it was, so the digest the cursor carried still
        ;; holds.  The whole prefix is what refuses.
        (should (string= window (org-glance-graph--external-window gen1 size)))
        (should-not (org-glance-graph--external-folded-whole? gen1))
        ;; AND THE REFUSAL RE-FOLDS IT: the cursor that described the old bytes
        ;; is gone, so the generation is owed whole.
        (should-not (f-exists? (org-glance-graph--external-cursor-path gen1)))
        (should (string-match-p "tombstone" (org-glance-test:external-pending graph)))
        (should (= 2 (org-glance-graph:refresh-external graph)))
        (should (eq 'tombstone (org-glance-graph:get-headline graph "id2")))
        (should (string= "" (org-glance-test:external-pending graph)))))))

(ert-deftest org-glance-test:external-rotation-spares-a-generation-that-appeared-mid-fold ()
  "ABSENCE OF EVIDENCE NEVER LICENSES A MOVE.  A generation that appeared
after `--read-external' listed the sources was folded by nobody on this pass, so
the rotation it runs into knows nothing about it -- and the only safe answer to
that is to leave it standing, whatever its number says about its age.

Here another Emacs's rotation lands from inside `graph:insert', low-numbered so
`butlast' names it, carrying a tombstone with no cursor beside it.  A drained
generation stands behind it and GOES on the same pass, which is what keeps this
case from passing on a rotation that retired nothing at all, and a third is
spared by position whatever any rule says."
  (let ((org-glance-graph-external-max-bytes 60))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-graph:add graph (org-glance-test:headline "id2" "* TODO bar"))
      (let ((newcomer (org-glance-graph--external-generation-path graph 1))
            (retired (org-glance-test:external-generation
                      graph 2 (org-glance-test:external-line "id1")))
            (newest (org-glance-test:external-generation graph 3 "")))
        (org-glance-test:spend graph retired)
        (org-glance-test:external-write graph "id1" "id1")   ; past the cap
        (let ((insert (symbol-function 'org-glance-graph:insert)))
          (cl-letf (((symbol-function 'org-glance-graph:insert)
                     (lambda (g specs)
                       (org-glance-test:write
                        newcomer (org-glance-test:external-line "id2" t))
                       (funcall insert g specs))))
            (should (= 1 (org-glance-graph:refresh-external graph)))))
        (should (f-exists? newcomer))
        (should (f-exists? newest))
        (should-not (f-exists? retired))
        (should (= 1 (org-glance-graph:refresh-external graph)))
        (should (eq 'tombstone (org-glance-graph:get-headline graph "id2")))))))

(ert-deftest org-glance-test:external-rotation-spares-an-unreadable-generation ()
  "A FAILED READ IS NOT AN ANSWER, and reading it as one cost a tombstone.
`--external-bytes' answered every question over the buffer it was left with, and
`ignore-errors' left the EMPTY buffer for an unreadable file as readily as for
an absent one -- so `--external-folded-whole?' took both halves of its equality
off that buffer, `0 = 0' said every byte was folded, and rotation destroyed a
file it had never read.  Measured over this very fixture: readable, spared;
`chmod 000', unlinked, with the tombstone gone.

The generation here carries an UNFOLDED TOMBSTONE and nothing else, which is the
whole of what was at stake.  Unreadable it is neither folded whole nor moved;
readable again it folds and the record goes."
  (let ((org-glance-graph-external-max-bytes 60))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-graph:add graph (org-glance-test:headline "id2" "* TODO bar"))
      (let ((locked (org-glance-test:external-generation
                     graph 1 (org-glance-test:external-line "id2" t)))
            (newest (org-glance-test:external-generation graph 2 "")))
        (unwind-protect
            (progn
              (set-file-modes locked 0)
              (should-not (org-glance-graph--external-folded-whole? locked))
              (should (= 0 (org-glance-graph--external-folded locked)))
              (org-glance-test:rotate-now graph)
              (should (f-exists? locked))
              (should (f-exists? newest))
              (should-not (f-exists? (org-glance-graph--external-spent-path
                                      graph (org-glance-test:generation-name graph 1)))))
          (set-file-modes locked #o600))
        ;; readable again, the tombstone is owed and folds
        (should (string-match-p "tombstone" (org-glance-test:external-pending graph)))
        (should (= 1 (org-glance-graph:refresh-external graph)))
        (should (eq 'tombstone (org-glance-graph:get-headline graph "id2")))))))

(ert-deftest org-glance-test:external-rotation-refuses-a-name-that-is-not-a-file ()
  "`directory-files' NAMES EVERY ENTRY, so a DIRECTORY and a DANGLING SYMLINK
wearing a generation's name are candidates like any other.  Both are refused by
the one predicate rotation already asks: the read fails, so nothing says they
are folded whole and nothing moves them.  Measured against the swallowed read
and the unlink together -- both answered FOLDED WHOLE on `0 = 0', and rotation
removed the empty directory and the dangling link alike.

A REAL DRAINED GENERATION GOES ON THE SAME PASS, so this cannot pass on a
rotation that moved nothing at all."
  (let ((org-glance-graph-external-max-bytes 60))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (let* ((meta (org-glance-graph:meta-path graph))
             (dir (org-glance-graph--external-generation-path graph 1))
             (link (org-glance-graph--external-generation-path graph 2))
             (drained (org-glance-test:external-generation
                       graph 3 (org-glance-test:external-line "id1")))
             (newest (org-glance-test:external-generation graph 4 "")))
        (f-mkdir-full-path dir)
        (make-symbolic-link (f-join meta "gone.jsonl") link)
        (org-glance-test:spend graph drained)
        (should-not (org-glance-graph--external-folded-whole? dir))
        (should-not (org-glance-graph--external-folded-whole? link))
        (org-glance-test:rotate-now graph)
        (should (f-directory? dir))
        (should (file-symlink-p link))
        (should (f-exists? newest))
        (should-not (f-exists? drained))
        (should (f-exists? (org-glance-graph--external-spent-path
                            graph (org-glance-test:generation-name graph 3))))))))

(ert-deftest org-glance-test:external-rotation-moves-a-spent-generation-aside ()
  "ROTATION MOVES A SPENT GENERATION; IT NEVER UNLINKS ONE.  The bytes land
under `meta/spent/' with its cursor beside them, byte for byte what left
`meta/', so a wrong answer from the predicate that licensed the move costs disk
where it used to cost a tombstone.

THREE ROTATIONS, since the newest generation is spared by position and the one
under it is what `butlast' first names."
  (let ((org-glance-graph-external-max-bytes 60))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (dotimes (_ 3)
        (org-glance-test:external-write graph "id1" "id1")
        (should (= 1 (org-glance-graph:refresh-external graph))))
      (let ((gen1 (org-glance-graph--external-generation-path graph 1))
            (moved (org-glance-graph--external-spent-path
                    graph (org-glance-test:generation-name graph 1))))
        (should-not (f-exists? gen1))
        (should (f-exists? moved))
        (should (string= (apply #'concat
                                (make-list 2 (org-glance-test:external-line "id1")))
                         (f-read-text moved 'utf-8)))
        (should (f-exists? (org-glance-graph--external-spent-path
                            graph (org-glance-test:generation-name graph 1 t))))))))

(ert-deftest org-glance-test:external-retire-refuses-a-destination-that-exists ()
  "A SECOND RETIREMENT OF ONE GENERATION KEEPS THE FIRST ONE'S BYTES.
`--external-move' renames with clobbering OFF, so a name already under `spent/'
is refused and both ends stay where they are.  That refusal is what the whole
move-never-unlink rule rests on: an overwrite spends exactly the bytes the move
exists to keep, and it spends them under the one name that proves a generation
was retired twice.

THE HAND REPAIR IS HOW IT IS REACHED.  The `.jsonl' alone goes back into
`meta/', its old cursor stays under `spent/', and the retirement after that
meets a destination it must not take -- so the fresh cursor is refused too and
stays in `meta/', which is the orphan the repair produces."
  (org-glance-test:with-graph graph
    (let* ((name (org-glance-test:generation-name graph 1))
           (gen (org-glance-test:external-generation
                 graph 1 (org-glance-test:external-line "id1")))
           (kept (org-glance-graph--external-spent-path graph name))
           (kept-cursor (org-glance-graph--external-cursor-path kept)))
      (org-glance-test:spend graph gen)
      (should (org-glance-graph--external-retire graph name))
      (should (string= (org-glance-test:external-line "id1")
                       (f-read-text kept 'utf-8)))
      (should (string-prefix-p (format "%d " org-glance-test:external-write-bytes)
                               (f-read-text kept-cursor 'utf-8)))
      ;; the repair, then a second generation of that number: different bytes
      ;; and a different offset, so an overwrite cannot pass for a no-op
      (org-glance-test:external-generation
       graph 1 (concat (org-glance-test:external-line "id1")
                       (org-glance-test:external-line "id2")))
      (org-glance-test:spend graph gen)
      (should-not (org-glance-graph--external-retire graph name))
      (should (string= (org-glance-test:external-line "id1")
                       (f-read-text kept 'utf-8)))
      (should (string-prefix-p (format "%d " org-glance-test:external-write-bytes)
                               (f-read-text kept-cursor 'utf-8)))
      (should (f-exists? gen))
      (should (f-exists? (org-glance-graph--external-cursor-path gen))))))

(ert-deftest org-glance-test:external-retire-moves-the-cursor-first ()
  "THE CURSOR GOES FIRST AND THE FILE AFTER IT, and a crash between the two is
the whole of why the order is written down.  Interrupted at exactly that point
here: the cursor is under `spent/' and the generation is still in `meta/', where
it carries no offset, is a source like any other and re-folds WHOLE -- a no-op
by the crash rule, every line in it having been folded once already.

REVERSED, the same crash puts the FILE out of the fold's reach and leaves the
OFFSET in `meta/' beside a name that is not there: the lines sit in `spent/'
where no fold reads them, and pending reports nothing owed."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (let* ((name (org-glance-test:generation-name graph 1))
           (gen (org-glance-test:external-generation
                 graph 1 (org-glance-test:external-line "id1")))
           (cursor (org-glance-graph--external-cursor-path gen))
           (spent (org-glance-graph--external-spent-path graph name))
           (move (symbol-function 'org-glance-graph--external-move))
           (left 1))
      (org-glance-test:spend graph gen)
      (should (f-exists? cursor))
      ;; the first move lands and the second never runs, whichever it is
      (cl-letf (((symbol-function 'org-glance-graph--external-move)
                 (lambda (from to)
                   (when (> left 0) (cl-decf left) (funcall move from to)))))
        (should-not (org-glance-graph--external-retire graph name)))
      (should (f-exists? gen))
      (should-not (f-exists? cursor))
      (should (f-exists? (org-glance-graph--external-cursor-path spent)))
      (should-not (f-exists? spent))
      ;; a source again, owed whole, and the fold takes it
      (should (member gen (org-glance-graph--external-sources graph)))
      (should (string= (org-glance-test:external-line "id1")
                       (org-glance-test:external-pending graph)))
      (should (= 1 (org-glance-graph:refresh-external graph)))
      (should (string= "DONE" (org-glance-test:field graph "id1" state))))))

(ert-deftest org-glance-test:external-a-spent-generation-is-out-of-the-fold ()
  "WHAT THE MOVE COSTS, said out loud: a retired generation is NO LONGER A
SOURCE.  `--external-generations' asks `directory-files' about `meta/' alone, so
`spent/' leaves the fold's reach by construction -- which is the point, a fold
that kept reading every generation it ever retired would pay for the accumulation
on every poll.

THE REPAIR IS THE `.jsonl' ALONE.  Moved back into `meta/' without its cursor
the generation carries no offset, so the next fold takes it WHOLE and every line
in it lands again -- idempotent by the crash rule.  A reader who suspects the
predicate was wrong has that, where the unlink left them nothing."
  (let ((org-glance-graph-external-max-bytes 60))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (dotimes (_ 3)
        (org-glance-test:external-write graph "id1" "id1")
        (should (= 1 (org-glance-graph:refresh-external graph))))
      (let ((moved (org-glance-graph--external-spent-path
                    graph (org-glance-test:generation-name graph 1)))
            (gen1 (org-glance-graph--external-generation-path graph 1)))
        (should-not (member moved (org-glance-graph--external-sources graph)))
        ;; the blob moves and the retired lines say nothing about it
        (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
        (should (string= "" (org-glance-test:external-pending graph)))
        (should (= 0 (org-glance-graph:refresh-external graph)))
        (should (string= "TODO" (org-glance-test:field graph "id1" state)))
        ;; the `.jsonl' alone comes back, and the whole of it folds
        (rename-file moved gen1)
        (should (member gen1 (org-glance-graph--external-sources graph)))
        (should (string-match-p "id1" (org-glance-test:external-pending graph)))
        (should (= 1 (org-glance-graph:refresh-external graph)))
        (should (string= "DONE" (org-glance-test:field graph "id1" state)))))))

(ert-deftest org-glance-test:external-nothing-prunes-a-spent-generation ()
  "NOTHING PRUNES `spent/' ON ITS OWN, a deliberate trade: a 1 MiB cap times a few generations is nothing against one lost
tombstone.  Four rotations here and every retired file is still on disk, where
`--external-generations' holds the live directory at two.

`org-glance-graph:clear-spent-external' is the door, and `rm -r' on the same
directory is the other."
  (let ((org-glance-graph-external-max-bytes 60))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (dotimes (_ 5)
        (org-glance-test:external-write graph "id1" "id1")
        (should (= 1 (org-glance-graph:refresh-external graph))))
      (should (= 2 (length (org-glance-graph--external-generations graph))))
      (dolist (n '(1 2 3))
        (should (f-exists? (org-glance-graph--external-spent-path
                            graph (org-glance-test:generation-name graph n)))))
      ;; three generations and the three cursors that rode along with them
      (should (= 6 (org-glance-graph:clear-spent-external graph)))
      (should-not (f-directory? (org-glance-graph--external-spent-path graph)))
      (should (= 0 (org-glance-graph:clear-spent-external graph)))
      ;; and the live directory is untouched by it
      (should (= 2 (length (org-glance-graph--external-generations graph)))))))

(ert-deftest org-glance-test:external-the-family-is-git-ignored ()
  "THE NOTIFICATION FAMILY IS A LOCAL HINT and does not sync.  What has to reach
another machine is the WAL RECORD a fold produces, and that syncs on its own; a
notification is a byte offset's worth of local state between the local daemon
and the local Emacs.  Ignoring it removes the git merge door outright rather
than degrading it -- no file here has two writers, so no offset ever shifts
under a cursor by sync, and the conflict resolver has nothing of this family to
reach.  What it gives up is hazard H3, a daemon here and an Emacs there.

GIT ITSELF IS THE ORACLE: the rules are git's, and a matcher spelled here would
be a second implementation of them agreeing with itself.  Four things stand for
the family -- the live file, a generation, a cursor of each, and the `spent/'
directory -- and the WAL's own open segment stands for what must keep syncing."
  (skip-unless (executable-find "git"))
  (org-glance-test:with-graph graph
    ;; a directory pattern needs a directory to match, which retirement makes
    (f-mkdir-full-path (org-glance-graph--external-spent-path graph))
    (should (equal '(t t t t t t nil)
                   (org-glance-test:git-ignores
                    graph
                    (org-glance-graph:external-path graph)
                    (org-glance-graph--external-generation-path graph 1)
                    (org-glance-test:external-cursor-path graph)
                    (org-glance-graph--external-cursor-path
                     (org-glance-graph--external-generation-path graph 1))
                    (org-glance-graph--external-spent-path graph)
                    (org-glance-graph--external-spent-path
                     graph (org-glance-test:generation-name graph 1))
                    (org-glance-graph:headline-meta-path graph))))))

(ert-deftest org-glance-test:external-the-gitignore-retrofits-an-old-store ()
  "THE RETROFIT IS AN APPEND, where every other bootstrap marker is written only
when absent.  A store made before the notification family existed carries the
one-line `cache/' file, and write-if-absent would leave every one of them
syncing a hint the other machine cannot use.  Each missing line joins the end,
a hand-written line is left where it is, and a second open adds nothing.

IGNORING DOES NOT UNTRACK: a store that already committed these files goes on
syncing them until `git rm --cached' is run by hand, which is the reader's own
step and cannot be taken from here.  NOTHING ASSERTS THAT HALF, and it cannot be
asserted in the shape the case above uses: `org-glance-test:git-ignores' runs
`check-ignore' over a repository holding nothing, where the patterns are the
whole answer, and the same call over a tracked family answers the other way.
Measured with real git -- patterns appended to a store whose family is tracked,
`check-ignore' exiting 1, `status' reporting the file modified, and the commit
landing its bytes; `git rm --cached' is what makes `check-ignore' name the rule."
  (org-glance-test:with-graph graph
    (let ((path (f-join (org-glance-graph:store-path graph) ".gitignore")))
      (should (string= (concat (mapconcat #'identity
                                          org-glance-graph--gitignore-lines "\n")
                               "\n")
                       (f-read-text path 'utf-8)))
      (org-glance-test:write path "cache/\n# mine\n")
      (let* ((graph (org-glance-test:reopen graph))
             (text (f-read-text path 'utf-8))
             (lines (split-string text "\n" t)))
        (should (s-prefix? "cache/\n# mine\n" text))
        (dolist (line org-glance-graph--gitignore-lines)
          (should (member line lines)))
        (should (= 1 (cl-count "cache/" lines :test #'string=)))
        (org-glance-test:reopen graph)
        (should (string= text (f-read-text path 'utf-8)))))))

(ert-deftest org-glance-test:external-an-offset-past-the-end-is-refused ()
  "AN OFFSET PAST THE END IS REFUSED BEFORE ANYTHING IS HASHED, and it takes a
test of its own because no digest can answer it.  `--external-digests' CLAMPS
its range, so on a file at or under the window every offset from the SIZE up to
the WINDOW WIDTH hashes exactly what the size does -- and what the size hashes
is what a cursor minted there recorded.  Asserted first, so the rest of this
case is about the offset alone: the pair a cursor at the width is verified
against IS the pair this file's own bytes make.

Measured before the refusal was written: a cursor rewritten to `4096 <same>
<same>' read as 4096 bytes folded.  Nothing was lost by it -- the file polls
pending forever and an append re-folds it whole -- but three documents said an
offset past the end hashes a strict prefix, which cannot match, and the ladder
rung standing for that rule passed on a bogus digest, where the offset was the
thing under test."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (org-glance-test:external-write graph "id1")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (let* ((path (org-glance-graph:external-path graph))
           (past org-glance-graph--external-window-bytes))
      (should (= org-glance-test:external-write-bytes (f-size path)))
      (should (< org-glance-test:external-write-bytes past))
      (should (equal (org-glance-graph--external-bytes
                      path (lambda () (org-glance-graph--external-digests
                                  org-glance-test:external-write-bytes)))
                     (org-glance-graph--external-bytes
                      path (lambda () (org-glance-graph--external-digests past)))))
      (org-glance-test:write (org-glance-test:external-cursor-path graph)
                             (org-glance-test:external-cursor-at graph past))
      (should (= 0 (org-glance-test:external-cursor graph)))
      (should-not (org-glance-graph--external-folded-whole? path))
      (should (string= (org-glance-test:external-text graph)
                       (org-glance-test:external-pending graph)))
      (should (= 1 (org-glance-graph:refresh-external graph))))))

;;; Known hazards: these pin TODAY's behaviour -- H2 in docs/invariants.org.

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
