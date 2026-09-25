;;; test-external.el --- Tests for the external-write notification file  -*- lexical-binding: t -*-

;; Pins the `glance' external-writer contract: invariants 33-34, hazards H1-H3.

(require 'test-helpers)

(cl-defun org-glance-test:external-line (id &optional tombstone)
  "Return the frozen notification line naming ID, a DELETE when TOMBSTONE.
Spelled by hand with no JSON encoder, so a field-name or order change fails
here; ONE speller serves both shapes, which differ only in the third field."
  (format "{\"id\":\"%s\",\"at\":\"2026-08-03T04:21:07Z\"%s}\n"
          id (if tombstone ",\"tombstone\":true" "")))

(cl-defun org-glance-test--external-append (graph text)
  "Append TEXT to GRAPH's live external-notification file."
  (let ((path (org-glance-graph:external-path graph)))
    (f-mkdir-full-path (f-dirname path))
    (f-append-text text 'utf-8 path)))

(cl-defun org-glance-test:external-write (graph &rest ids)
  "Append a notification line for each of IDS to GRAPH's `EXTERNAL.jsonl'."
  (org-glance-test--external-append
   graph (mapconcat #'org-glance-test:external-line ids "")))

(cl-defun org-glance-test:external-delete (graph &rest ids)
  "Append a DELETE line for each of IDS to GRAPH's `EXTERNAL.jsonl'."
  (org-glance-test--external-append
   graph (mapconcat (lambda (id) (org-glance-test:external-line id t)) ids "")))

(defconst org-glance-test:external-filler-id "aaaaaaaaaaaaaaaaaaaa"
  "Filler id whose WRITE line is as long as a three-character id's TOMBSTONE.
ONE id for every line, so filling a file costs a fold one re-derivation.")

(defconst org-glance-test:external-write-bytes
  (string-bytes (org-glance-test:external-line "id1"))
  "Bytes in a WRITE line naming a three-character id.
Lines are fixed-width; a case names an offset or file size with this.")

(defconst org-glance-test:external-line-bytes
  (string-bytes (org-glance-test:external-line org-glance-test:external-filler-id))
  "Bytes in a WRITE line naming `org-glance-test:external-filler-id'.
A three-character id's TOMBSTONE is as long, which lets a case re-lay a first
line at its exact size; the two are asserted equal where used.")

(cl-defun org-glance-test:external-fill (graph n)
  "Append N filler WRITE lines to GRAPH's live notification file."
  (apply #'org-glance-test:external-write graph
         (make-list n org-glance-test:external-filler-id)))

(cl-defun org-glance-test:external-generation (graph n text)
  "Write TEXT as GRAPH's rotated notification generation N; return its path.
Built by hand instead of by rotating, so no rotation condition intervenes."
  (let ((path (org-glance-graph--external-generation-path graph n)))
    (org-glance-test:write path text)
    path))

(cl-defun org-glance-test:generation-name (graph n &optional cursor)
  "Return the basename of GRAPH's generation N, or of its cursor when CURSOR.
Spelled by rotation's own path functions, so no literal drifts from them."
  (let ((path (org-glance-graph--external-generation-path graph n)))
    (f-filename (if cursor (org-glance-graph--external-cursor-path path) path))))

(cl-defun org-glance-test:rotate-now (graph)
  "Push GRAPH's live notification file past the cap, fold it, and rotate.
Rotation gets the survey entry `org-glance-test:spend' returns, as from a fold.
The two lines name `id1', which every caller has already added."
  (org-glance-test:external-write graph "id1" "id1")
  (org-glance-graph--rotate-external-maybe
   graph (list (org-glance-test:spend
                graph (org-glance-graph:external-path graph)))))

(cl-defun org-glance-test:external-raw (graph line)
  "Append LINE verbatim to GRAPH's `EXTERNAL.jsonl'.
For shapes the spellers cannot make: an unknown field, a non-true `tombstone'."
  (org-glance-test--external-append graph line))

(cl-defun org-glance-test:external-text (graph)
  "Return GRAPH's `EXTERNAL.jsonl' as text, or nil when there is no file."
  (let ((path (org-glance-graph:external-path graph)))
    (when (f-exists? path) (f-read-text path 'utf-8))))

(cl-defun org-glance-test:external-pending (graph)
  "Return GRAPH's notification text no fold has taken yet, in drain order.
Empty when every cursor has caught up."
  (mapconcat (lambda (path) (car (org-glance-graph--external-tail path)))
             (org-glance-graph--external-sources graph) ""))

(cl-defun org-glance-test:external-cursor (graph)
  "Return how far GRAPH's live notification file has been folded, in bytes."
  (org-glance-graph--external-folded (org-glance-graph:external-path graph)))

(cl-defun org-glance-test:external-cursor-path (graph)
  "Return the path of the cursor beside GRAPH's live notification file."
  (org-glance-graph--external-cursor-path (org-glance-graph:external-path graph)))

(cl-defun org-glance-test:external-cursor-text (graph)
  "Return GRAPH's live cursor file as text, or nil when there is none.
The raw record; `org-glance-test:external-cursor' is what a fold makes of it."
  (let ((path (org-glance-test:external-cursor-path graph)))
    (when (f-exists? path) (f-read-text path 'utf-8))))

(cl-defun org-glance-test:external-cursor-at (graph offset)
  "Return GRAPH's live cursor text with its offset rewritten to OFFSET.
The digests stay the file's own, so only the offset can refuse the line."
  (pcase (split-string (s-trim (org-glance-test:external-cursor-text graph)) nil t)
    (`(,_ ,window ,prefix) (format "%d %s %s\n" offset window prefix))))

(cl-defun org-glance-test:spend (graph path)
  "Move PATH's cursor in GRAPH to the end, as a fold reading it whole would.
Return the `--external-survey' entry that reading leaves.  The digests come
from `--external-tail'; a file owing nothing gets no cursor, as in a fold."
  (let ((tail (org-glance-graph--external-tail path)))
    (when (and (car tail) (not (string-empty-p (car tail))))
      (apply #'org-glance-graph--set-external-cursor path (cdr tail)))
    (list path nil tail)))

(cl-defun org-glance-test:external-two-lines (graph)
  "Add `id1' and `b' to GRAPH, edit `b' outside, and write an `id1' line.
The setup both re-laying cases share; folding it leaves a one-line cursor."
  (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
  (org-glance-graph:add graph (org-glance-test:headline "b" "* TODO bar"))
  (org-glance-test:edit-blob graph "b" "* TODO bar" "* DONE bar")
  (org-glance-test:external-write graph "id1"))

(cl-defun org-glance-test:external-relaid (graph)
  "Re-lay GRAPH's notification file with a line inserted ahead of the cursor.
An UNFOLDED `b' line lands AHEAD of the folded `id1' line; same-length ids keep
the offset on a line boundary, so only the digest can tell."
  (org-glance-test:write (org-glance-graph:external-path graph)
                         (concat (org-glance-test:external-line "b")
                                 (org-glance-test:external-line "id1"))))

(cl-defun org-glance-test:external-refolds-the-relaid-file (graph)
  "Assert GRAPH's re-laid notification file is owed WHOLE and folds to the end.
The tail both re-laying cases share: the fold refuses the offset and adopts the
moved line; the poll refuses it by WINDOW, which a size check passes."
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
Runs `git check-ignore' in a fresh repository over GRAPH's directory, which
tracks nothing, so the patterns are the whole answer; a tracked path says nil."
  (let ((default-directory (org-glance-graph:directory graph)))
    (call-process "git" nil nil nil "init" "--quiet" ".")
    (mapcar (lambda (path)
              (eq 0 (call-process "git" nil nil nil "check-ignore" "-q" path)))
            paths)))

(cl-defun org-glance-test:edit-blob (graph id from to)
  "Replace FROM with TO in ID's blob in GRAPH, as an outside writer does.
Bypasses `org-glance-graph:add': the blob moves and the WAL does not."
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
  "The cursor counts BYTES: a multibyte id leaves it past the character count,
and the next fold starts on a line boundary.  A file that only grew keeps its
cursor, and the fold after the append takes the new line alone."
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
  "Unknown and tombstoned ids are skipped, their bytes spent all the same."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-graph:delete graph "id1")
    (org-glance-test:external-write graph "id1" "ghost")
    (should (= 0 (org-glance-graph:refresh-external graph)))
    (should (eq 'tombstone (org-glance-graph:get-headline graph "id1")))
    (should (string= "" (org-glance-test:external-pending graph)))))

(ert-deftest org-glance-test:external-refresh-adopts-an-unknown-stored-blob ()
  "A WRITE for an unindexed blob derives its metadata, including new tags."
  (org-glance-test:with-graph graph
    (org-glance-test:write
     (org-glance-graph:content-path graph "fresh")
     (org-glance-test:org-with-id "* TODO fresh :newtag:" "fresh"))
    (org-glance-test:external-write graph "fresh")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (equal '("newtag")
                   (org-glance-headline-metadata:tag-strings
                    (org-glance-graph:live-meta graph "fresh"))))))

(ert-deftest org-glance-test:external-refresh-without-a-file ()
  "A store no external writer ever touched refreshes nothing and makes nothing."
  (org-glance-test:with-graph graph
    (should (= 0 (org-glance-graph:refresh-external graph)))
    (should-not (f-exists? (org-glance-graph:external-path graph)))
    (should-not (f-exists? (org-glance-test:external-cursor-path graph)))))

(ert-deftest org-glance-test:external-refresh-reads-the-tags-cycle ()
  "A state only a tag's `#+TODO:' cycle declares is re-derived as a STATE,
never folded into the title."
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
  "A plain READ folds pending notes in and spends their bytes (invariant 33)."
  (let ((org-glance-graph-external-poll-seconds 0))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
      (org-glance-test:external-write graph "id1")
      (should (string= "DONE" (org-glance-test:field graph "id1" state)))
      (should (string= "" (org-glance-test:external-pending graph))))))

(ert-deftest org-glance-test:external-read-fold-is-throttled ()
  "The read-path fold is throttled by `org-glance-graph-external-poll-seconds';
`org-glance-graph:refresh-external' never is."
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
  "The fold's OWN reads never fold again: one refresh per read."
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
  "A fold that signals never breaks the read, even under `debug-on-error': the
read serves the WAL and the line stays pending, its cursor unmoved."
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
  "A tombstone folds with its blob already gone, as the daemon leaves it: the
write arm's `no stored blob' skip never reaches it."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (f-delete (org-glance-graph:content-path graph "id1"))
    (org-glance-test:external-delete graph "id1")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (eq 'tombstone (org-glance-graph:get-headline graph "id1")))))

(ert-deftest org-glance-test:external-refresh-folds-a-write-and-a-delete ()
  "A write and a delete fold in one pass as ONE append (invariant 33)."
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
  "A write then a delete for one id folds as the DELETE, leaving no live record
over a blob that has gone."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (org-glance-test:external-write graph "id1")
    (org-glance-test:external-delete graph "id1")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (eq 'tombstone (org-glance-graph:get-headline graph "id1")))))

(ert-deftest org-glance-test:external-refresh-reads-the-last-sighting-either-way ()
  "A delete then a write for one id folds as the WRITE: the last sighting wins."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
    (org-glance-test:external-delete graph "id1")
    (org-glance-test:external-write graph "id1")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (string= "DONE" (org-glance-test:field graph "id1" state)))))

(ert-deftest org-glance-test:external-refresh-skips-a-delete-it-cannot-make ()
  "A delete of an unknown or tombstoned id spends its bytes and nothing else:
no record, no count."
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
  "A folded delete bumps the tag removal counter as `graph:delete' does: the
batch appends while the id is live, so the before-append hook sees its tags."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* A :x:"))
    (org-glance-test:external-delete graph "a")
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (= 1 (plist-get (cdr (assoc "x" (org-glance-tag-metrics--read graph)))
                            :removals)))))

(ert-deftest org-glance-test:external-json-true-is-t ()
  "JSON true parses to t and false to the non-nil `:false'.
The kind test's `eq t' rests on this platform fact (invariant 33)."
  (should (eq t (plist-get (json-parse-string "{\"tombstone\":true}"
                                              :object-type 'plist)
                           :tombstone)))
  (should (eq :false (plist-get (json-parse-string "{\"tombstone\":false}"
                                                   :object-type 'plist)
                                :tombstone))))

(ert-deftest org-glance-test:external-refresh-deletes-on-json-true-alone ()
  "ONLY JSON true is a delete: an unknown key (inert, for compatibility),
`false' and the string \"true\" each fold as a write; every record stays live."
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
  "H1 replayed: a tombstone appended after a fold lies PAST the cursor, and the
next fold takes it."
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
  "A note landing between a fold's read and its cursor write stays pending:
only the bytes the fold read are spent, and the note folds next time."
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
  "A note byte-identical to the text a fold just read survives it: the OFFSET
counts bytes, so the second copy folds and carries its edit to the WAL."
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
  "Two Emacsen folding one range cost only work (invariant 7): the stale second
fold appends a record equal to the first's."
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
  "A cursor that is gone, garbled, short of two digests, or negative reads as 0
and costs a re-fold, every answer unchanged.  The past-the-end rung has its own
case, `--an-offset-past-the-end-is-refused', needing the file's own digests."
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
  "Bytes re-laid ahead of the offset BETWEEN two folds re-fold the file whole.
The digest refuses the offset, which would resume mid-line and skip lines.  The
same bytes INSIDE a fold: `--a-rewrite-inside-the-fold-window-refolds'."
  (org-glance-test:with-graph graph
    (org-glance-test:external-two-lines graph)
    (should (= 1 (org-glance-graph:refresh-external graph)))
    (should (= org-glance-test:external-write-bytes
               (org-glance-test:external-cursor graph)))
    (org-glance-test:external-relaid graph)
    (org-glance-test:external-refolds-the-relaid-file graph)))

(ert-deftest org-glance-test:external-a-rewrite-inside-the-fold-window-refolds ()
  "Bytes re-laid INSIDE a fold re-fold the file whole: the digest is minted at
READ time, over the bytes folded; the one case a write-time digest reddens."
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
  "A drained file replaced whole at the same size polls pending by its WINDOW,
and a plain read folds the new tombstone.  Fixed-width lines make this the
typical re-laying, which a size comparison cannot see."
  (let ((org-glance-graph-external-poll-seconds 0))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "aaa" "* TODO foo"))
      (org-glance-graph:add graph (org-glance-test:headline "bbb" "* TODO bar"))
      (org-glance-test:external-delete graph "aaa")
      (should (= 1 (org-glance-graph:refresh-external graph)))
      (let* ((path (org-glance-graph:external-path graph))
             (size (f-size path)))
        (should (org-glance-graph--external-drained? path))
        ;; The poll's window equals the fold's: two spellings of one rule.
        (should (string= (plist-get (org-glance-graph--external-cursor path) :window)
                         (org-glance-graph--external-window path size)))
        (org-glance-test:write path (org-glance-test:external-line "bbb" t))
        (should (= size (f-size path)))
        (should-not (org-glance-graph--external-drained? path))
        (should (org-glance-graph--external-pending-p graph)))
      (should (eq 'tombstone (org-glance-graph:get-headline graph "bbb")))
      (should (string= "" (org-glance-test:external-pending graph))))))

(ert-deftest org-glance-test:external-a-torn-final-line-leaves-the-file-pending ()
  "A writer's torn final line leaves the cursor mid-line: the poll alone reads
it pending, the fold it costs takes nothing, and a newline restores drained.
The good line ahead folds; the fragment's bytes are spent."
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
      (should (string= "" (org-glance-test:external-pending graph)))
      (should (= 0 (org-glance-graph:refresh-external graph)))
      (should (= (f-size path) (org-glance-test:external-cursor graph)))
      (org-glance-test:external-raw graph "\n")
      (should (= 0 (org-glance-graph:refresh-external graph)))
      (should (org-glance-graph--external-drained? path)))))

(ert-deftest org-glance-test:external-the-fold-catches-what-the-window-passed ()
  "A same-length edit behind the window passes the poll (invariant 34's
residual); the next fold owing anything checks the whole prefix and re-folds.
A rotated generation: `--rotation-spares-a-generation-re-laid-mid-fold'."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
    (org-glance-graph:add graph (org-glance-test:headline "id2" "* TODO bar"))
    (org-glance-test:edit-blob graph "id2" "* TODO bar" "* DONE bar")
    (let* ((path (org-glance-graph:external-path graph))
           (line (org-glance-test:external-line "id1")))
      (org-glance-test:write path (apply #'concat (make-list 200 line)))
      (should (= 1 (org-glance-graph:refresh-external graph)))
      (should (> (f-size path) (* 2 org-glance-graph--external-window-bytes)))
      (should (string= (plist-get (org-glance-graph--external-cursor path) :window)
                       (org-glance-graph--external-window path (f-size path))))
      (org-glance-test:write path (concat (org-glance-test:external-line "id2")
                                          (substring (f-read-text path 'utf-8)
                                                     (length line))))
      (should (org-glance-graph--external-drained? path))          ; the residual
      (org-glance-test:external-write graph "id1")                 ; anything owed
      (should (= 2 (org-glance-graph:refresh-external graph)))
      (should (string= "DONE" (org-glance-test:field graph "id2" state))))))

(ert-deftest org-glance-test:external-is-out-of-the-union-resolver ()
  "The union resolver heals the WAL alone (invariant 8's allowlist): the
notification file, its generation and glance's `COMPLETIONS.jsonl' keep their
markers byte for byte."
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
  "A rotated generation drains before the live file: its WRITE then the live
file's TOMBSTONE for one id ends tombstoned.  Reversing `--external-sources'
turns this red."
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
  "An unparseable file names no entry and is spent anyway, its bytes kept."
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
  "A drained file under `org-glance-graph-external-max-bytes' never rotates."
  (let ((org-glance-graph-external-max-bytes (* 1024 1024)))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-test:external-write graph "id1")
      (should (= 1 (org-glance-graph:refresh-external graph)))
      (should (f-exists? (org-glance-graph:external-path graph)))
      (should-not (org-glance-graph--external-generations graph)))))

(ert-deftest org-glance-test:external-folds-a-line-that-landed-in-a-rotated-file ()
  "A line landing in a just-rotated generation, a rename having come between
the writer's open and write, folds from there."
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
  "A generation is never retired on the pass that made it: at most two stand in
`meta/', the older retired at the start of the next rotation with its cursor."
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
  "Rotation fires on the offset the fold took, so an append landing mid-fold
still rotates; that line rides into the generation and folds from there."
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
  "Rotation retires by cursor alone: an unfolded oldest generation stays, a
drained one moves to `spent/', and the next fold makes the owed delete.  Its
twin `--spares-a-generation-that-appeared-mid-fold' covers an unlisted one."
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
        (should (string-match-p "tombstone" (org-glance-test:external-pending graph)))
        (should (= 1 (org-glance-graph:refresh-external graph)))
        (should (eq 'tombstone (org-glance-graph:get-headline graph "id2")))
        (should (string= "" (org-glance-test:external-pending graph)))))))

(ert-deftest org-glance-test:external-rotation-spares-a-re-laid-generation ()
  "Rotation spares a generation re-laid at the same length after its fold, per
`--external-folded-whole?', and the line it now carries folds.  The re-lay is
INSIDE the window; `--spares-a-generation-re-laid-mid-fold' is OUTSIDE it."
  (let ((org-glance-graph-external-max-bytes 60))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id1" "* TODO foo"))
      (org-glance-graph:add graph (org-glance-test:headline "id2" "* TODO bar"))
      (org-glance-test:edit-blob graph "id2" "* TODO bar" "* DONE bar")
      (let ((relaid (org-glance-test:external-generation
                     graph 1 (org-glance-test:external-line "id1")))
            (newest (org-glance-test:external-generation graph 2 "")))
        (org-glance-test:spend graph relaid)
        (org-glance-test:write relaid (org-glance-test:external-line "id2"))
        (should (= org-glance-test:external-write-bytes (f-size relaid)))
        (org-glance-test:rotate-now graph)
        (should (f-exists? relaid))
        (should (f-exists? newest))
        (should (= 1 (org-glance-graph:refresh-external graph)))
        (should (string= "DONE" (org-glance-test:field graph "id2" state)))
        (should (string= "" (org-glance-test:external-pending graph)))))))

(ert-deftest org-glance-test:external-rotation-spares-a-generation-re-laid-mid-fold ()
  "A generation re-laid mid-fold behind the window passes `--external-drained?'
forever; rotation's whole-prefix `--external-folded-whole?' spares it and drops
its cursor (`--external-refold'), so the next fold lands the tombstone."
  (let ((org-glance-graph-external-max-bytes 6000))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "id2" "* TODO bar"))
      (org-glance-graph:add graph (org-glance-test:headline
                                   org-glance-test:external-filler-id "* TODO fill"))
      ;; Two rotations: the newest generation is never a retirement candidate.
      (dotimes (_ 2)
        (org-glance-test:external-fill graph 120)
        (should (= 1 (org-glance-graph:refresh-external graph))))
      (let* ((gen1 (org-glance-graph--external-generation-path graph 1))
             (tomb (org-glance-test:external-line "id2" t))
             (size (* 120 org-glance-test:external-line-bytes))
             window)
        ;; Equal lengths, asserted so a speller change reddens this case.
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
        (should (string= window (org-glance-graph--external-window gen1 size)))
        (should-not (org-glance-graph--external-folded-whole? gen1))
        (should-not (f-exists? (org-glance-graph--external-cursor-path gen1)))
        (should (string-match-p "tombstone" (org-glance-test:external-pending graph)))
        (should (= 2 (org-glance-graph:refresh-external graph)))
        (should (eq 'tombstone (org-glance-graph:get-headline graph "id2")))
        (should (string= "" (org-glance-test:external-pending graph)))))))

(ert-deftest org-glance-test:external-rotation-spares-a-generation-that-appeared-mid-fold ()
  "A generation appearing mid-fold, after the sources were listed, is spared:
absence of evidence never licenses a move.  A drained generation retires on the
same pass, so a rotation that moved nothing fails here."
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
  "An unreadable generation is neither folded whole nor moved by rotation; once
readable, its unfolded tombstone folds."
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
        (should (string-match-p "tombstone" (org-glance-test:external-pending graph)))
        (should (= 1 (org-glance-graph:refresh-external graph)))
        (should (eq 'tombstone (org-glance-graph:get-headline graph "id2")))))))

(ert-deftest org-glance-test:external-rotation-refuses-a-name-that-is-not-a-file ()
  "Rotation leaves a directory or dangling symlink wearing a generation's name
in place, its read failing; a real drained one retires on the same pass."
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
  "Rotation moves a spent generation and its cursor into `meta/spent/' byte for
byte and never unlinks one.  Three rotations: the newest is spared by position."
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
  "A second retirement of one generation keeps the first one's bytes: the move
refuses an existing `spent/' name, leaving both ends in place.  Reached by the
hand repair, which strands the old cursor under `spent/'."
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
      ;; Repaired: new bytes and offset, so an overwrite cannot pass as a no-op.
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
  "Retirement moves the cursor before the file: interrupted between them, the
generation stays a source with no cursor and re-folds whole, a no-op."
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
      (should (member gen (org-glance-graph--external-sources graph)))
      (should (string= (org-glance-test:external-line "id1")
                       (org-glance-test:external-pending graph)))
      (should (= 1 (org-glance-graph:refresh-external graph)))
      (should (string= "DONE" (org-glance-test:field graph "id1" state))))))

(ert-deftest org-glance-test:external-a-spent-generation-is-out-of-the-fold ()
  "A retired generation leaves `--external-sources'; its `.jsonl' moved back
alone into `meta/' is the repair, and the next fold takes it whole."
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
        (org-glance-test:edit-blob graph "id1" "* TODO foo" "* DONE foo")
        (should (string= "" (org-glance-test:external-pending graph)))
        (should (= 0 (org-glance-graph:refresh-external graph)))
        (should (string= "TODO" (org-glance-test:field graph "id1" state)))
        (rename-file moved gen1)
        (should (member gen1 (org-glance-graph--external-sources graph)))
        (should (string-match-p "id1" (org-glance-test:external-pending graph)))
        (should (= 1 (org-glance-graph:refresh-external graph)))
        (should (string= "DONE" (org-glance-test:field graph "id1" state)))))))

(ert-deftest org-glance-test:external-nothing-prunes-a-spent-generation ()
  "Nothing prunes `spent/' on its own: `org-glance-graph:clear-spent-external'
removes every retired generation and cursor, leaving the live directory alone."
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
      (should (= 6 (org-glance-graph:clear-spent-external graph)))
      (should-not (f-directory? (org-glance-graph--external-spent-path graph)))
      (should (= 0 (org-glance-graph:clear-spent-external graph)))
      (should (= 2 (length (org-glance-graph--external-generations graph)))))))

(ert-deftest org-glance-test:external-the-family-is-git-ignored ()
  "The store's `.gitignore' covers the notification family and spares the WAL's
open segment, at the cost of hazard H3.  Git itself is the oracle; a matcher
spelled here would only agree with itself."
  (skip-unless (executable-find "git"))
  (org-glance-test:with-graph graph
    ;; a directory pattern needs a directory to match, which retirement makes
    (f-mkdir-full-path (org-glance-graph--external-spent-path graph))
    (should (equal '(t t t t t t t nil)
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
                    (f-join (org-glance-graph:meta-path graph)
                            "COMPLETIONS.jsonl")
                    (org-glance-graph:headline-meta-path graph))))))

(ert-deftest org-glance-test:external-the-gitignore-retrofits-an-old-store ()
  "Opening a store appends each missing `.gitignore' line, keeping hand-written
ones in place, and a second open adds nothing.  Untracking a committed family
is the reader's `git rm --cached', which no case asserts."
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
  "An offset past the end is refused before anything is hashed.  The clamp in
`--external-digests' makes it hash what the size does, asserted first, so only
the offset is under test."
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
  "H2: a folded delete spares the id's open material buffer, and its next save
appends a LIVE record and writes the blob back, minus the occurrence history.
`org-glance-test:material-delete-referrer-aware' pins the discarding path."
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
  "H2's visible half: a save landing BEFORE the fold leaves a tombstone over a
blob back on disk, which `glance''s scan counts as unindexed."
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
