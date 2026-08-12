;;; test-material.el --- Tests for graph-backed materialize/sync  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:material-completing-read ()
  "Selection lists live graph headlines and resolves the chosen metadata."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "a" "* TODO Alpha :x:")
                             (org-glance-test:headline "b" "* DONE Beta :y:"))
    (org-glance-test:offering (offered (caar offered))
      (let ((meta (org-glance-material:completing-read graph)))
        (should (org-glance-headline-metadata? meta))
        (should (string= "Alpha" (org-glance-headline-metadata:title meta)))))))

(ert-deftest org-glance-test:material-label-cleans-links ()
  "The completing-read label renders link markup as its description (or target)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "a" "* TODO [[id:x][Alpha]] :w:")
                             (org-glance-test:headline "b" "* TODO [[https:example.com]]"))
    (let* ((metas (org-glance-graph:headlines graph))
           (labels (mapcar #'org-glance-material:label metas)))
      (should (member "[w] Alpha" labels))
      (should (member "https:example.com" labels))
      (should-not (cl-some (lambda (l) (s-contains? "[[" l)) labels)))))

;; (`material-active-filter' removed: a subset of the next test, and
;; mutation-testing the `:filter' path found no unique kill.)

(ert-deftest org-glance-test:material-done-keywords-custom ()
  "`org-done-keywords' redefines what counts as active for selection."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "a" "* TODO Todo")
                             (org-glance-test:headline "b" "* DONE Done"))
    (org-glance-test:offering (offered (caar offered))
      (should (string= "a" (org-glance-headline-metadata:id
                            (org-glance-material:completing-read
                             graph :filter #'org-glance-headline-metadata:active?))))
      (should (= 1 (length offered))))
    (let ((org-done-keywords '("TODO")))
      (org-glance-test:offering (offered (caar offered))
        (should (string= "b" (org-glance-headline-metadata:id
                              (org-glance-material:completing-read
                               graph :filter #'org-glance-headline-metadata:active?))))
        (should (= 1 (length offered)))))))

(ert-deftest org-glance-test:material-kill-drops-pending-log-note ()
  "Killing a materialized buffer with a running clock must not leave a queued
log note behind.  Org\'s own `org-check-running-clock\' (`kill-buffer-hook\')
clocks out while the buffer dies; with `org-log-note-clock-out\' set that queues
`org-add-log-note\' on `post-command-hook\' with a marker into THAT buffer, and
the command loop then errors \"Marker does not point anywhere\"."
  (require 'org-clock)
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "ck" "* TODO Task"))
    (let ((org-log-note-clock-out t)
          (buf (org-glance-material:open graph "ck")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (goto-char (point-min))
              (org-clock-in))
            ;; preconditions: without these the should-nots below pass vacuously
            (should (org-clocking-p))
            (should (memq #'org-glance-material--cancel-pending-log-note
                          (buffer-local-value 'kill-buffer-hook buf)))
            ;; the kill and the hook that follows it are ONE command
            (let ((this-command 'kill-buffer))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                (kill-buffer buf))
              (should-not (buffer-live-p buf))
              (should-not (memq 'org-add-log-note post-command-hook))
              (should-not org-log-setup)
              (run-hooks 'post-command-hook))
            (let ((other (generate-new-buffer " *other-note*")))
              (unwind-protect
                  (let ((org-log-setup t))
                    (with-current-buffer other
                      (set-marker org-log-note-marker (point-min) other))
                    ;; runs in a DIFFERENT buffer: this note must survive.
                    (with-temp-buffer
                      (org-glance-material--cancel-pending-log-note))
                    (should org-log-setup)
                    (should (eq (marker-buffer org-log-note-marker) other)))
                (kill-buffer other)
                (setq org-log-setup nil)
                (set-marker org-log-note-marker nil))))
        (when (buffer-live-p buf) (kill-buffer buf))
        (ignore-errors (when (org-clocking-p) (org-clock-out)))))))

(ert-deftest org-glance-test:material-save-affordance ()
  "A materialized buffer visits its content-blob FILE, runs the minor mode,
and is editable; saving persists to the graph and survives re-materialize.
This guards the interactive save path users actually hit."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "s1" "* TODO foo"))
    (org-glance-test:with-material (buffer graph "s1")
      (should org-glance-material-mode)
      (should-not buffer-read-only)
      (should buffer-file-name)            ; a real file -> the standard save works
      (should (s-contains? "TODO foo" (buffer-string)))
      (org-glance-test:sed "TODO" "DONE")
      (org-glance-test:save))
    (should (string= "DONE" (org-glance-test:field graph "s1" state)))
    (org-glance-test:with-material (buffer graph "s1")
      (should (s-contains? "DONE foo" (buffer-string))))))

(ert-deftest org-glance-test:material-edit-updates-metadata ()
  "Editing the heading and saving updates the stored metadata projection."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "u1" "* TODO foo"))
    (org-glance-test:with-material (buffer graph "u1")
      (org-glance-test:sed "TODO foo" "DONE bar")
      (org-glance-test:save))
    (let ((meta (org-glance-graph:get-headline graph "u1")))
      (should (string= "DONE" (org-glance-headline-metadata:state meta)))
      (should (string= "bar" (org-glance-headline-metadata:title meta))))))

(ert-deftest org-glance-test:material-save-e2e ()
  "End-to-end interactive save: `org-glance-materialize' opens the buffer;
editing and invoking `C-x C-s' (the standard save command) persists to the
graph.  Drives the real command + the real keybinding."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "e2e1" "* TODO foo" "body"))
    (let ((org-glance-graph graph))
      (cl-letf (((symbol-function 'completing-read) (lambda (_p coll &rest _) (caar coll))))
        (save-window-excursion
          (org-glance-materialize)
          (unwind-protect
              (progn
                (should (string-prefix-p "*org-glance: " (buffer-name)))
                (should org-glance-material-mode)
                (should-not buffer-read-only)
                (should buffer-file-name)
                (org-glance-test:sed "TODO" "DONE")
                (let ((inhibit-message t))
                  (call-interactively (key-binding (kbd "C-x C-s")))))
            (when (string-prefix-p "*org-glance: " (buffer-name))
              (set-buffer-modified-p nil)
              (kill-buffer))))))
    (should (string= "DONE" (org-glance-test:field graph "e2e1" state)))))

(ert-deftest org-glance-test:material-id-change-skips-metadata ()
  "If the ORG_GLANCE_ID is edited, saving does not corrupt the metadata index
for the original id (the sync hook skips a mismatched id)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "m1" "* TODO foo"))
    (org-glance-test:with-material (buffer graph "m1")
      (org-glance-test:sed "^:ORG_GLANCE_ID:.*$" ":ORG_GLANCE_ID: changed")
      (org-glance-test:save))
    (should (string= "foo" (org-glance-test:field graph "m1" title)))))

(ert-deftest org-glance-test:material-open-missing ()
  "Materializing an unknown id errors."
  (org-glance-test:with-graph graph
    (should-error (org-glance-material:open graph "nope") :type 'user-error)))

(ert-deftest org-glance-test:open-link-single ()
  "A single non-org-glance link is opened without prompting."
  (let ((headline (org-glance-test:headline "o1" "* foo" "[[https://example.com][ex]]"))
        (called nil))
    (cl-letf (((symbol-function 'org-open-at-point) (lambda (&rest _) (setq called t))))
      (org-glance-material:open-link headline))
    (should called)))

(ert-deftest org-glance-test:open-link-multiple ()
  "With several links, the chosen one is opened."
  (let ((headline (org-glance-test:headline "o3" "* foo"
                                            "[[https://a.example][AAA]]"
                                            "[[https://b.example][BBB]]"))
        (line nil))
    (cl-letf (((symbol-function 'completing-read) (lambda (_p _coll &rest _) "BBB"))
              ((symbol-function 'org-open-at-point)
               (lambda (&rest _) (setq line (buffer-substring-no-properties
                                             (line-beginning-position) (line-end-position))))))
      (org-glance-material:open-link headline))
    (should (string-match-p "b.example" line))))

(ert-deftest org-glance-test:open-link-none ()
  "A headline with no openable links errors."
  (let ((headline (org-glance-test:headline "o2" "* foo" "no links here")))
    (should-error (org-glance-material:open-link headline) :type 'user-error)))

(ert-deftest org-glance-test:open-link-skips-org-glance ()
  "org-glance-* internal links are not offered as openable."
  (let ((headline (org-glance-test:headline "o4" "* foo" "[[org-glance-overview:task][task]]")))
    (should-error (org-glance-material:open-link headline) :type 'user-error)))

(ert-deftest org-glance-test:extract-helper ()
  "Extracting a known key copies its value to the kill ring."
  (let ((headline (org-glance-test:headline "e1" "* foo" "- key: val")))
    (should (string= "val" (org-glance-material:extract headline "key")))
    (should (string= "val" (current-kill 0)))))

(ert-deftest org-glance-test:extract-none ()
  "Extracting from a headline with no key-value pairs errors."
  (let ((headline (org-glance-test:headline "e2" "* foo" "no pairs here")))
    (should-error (org-glance-material:extract headline) :type 'user-error)))

(ert-deftest org-glance-test:extract-command ()
  "The command selects a headline from the graph then extracts a pair."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "e1" "* foo" "- key: val"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p coll &rest _) (if (assoc "key" coll) "key" (caar coll)))))
      (let ((org-glance-graph graph))
        (should (string= "val" (org-glance-extract)))))))

(ert-deftest org-glance-test:open-filters-nonlinked ()
  "open offers only linked headlines."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "L" "* foo" "[[https://x.example][x]]")
                             (org-glance-test:headline "P" "* bar" "no link here"))
    (let ((org-glance-graph graph) (called nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p coll &rest _) (should (= 1 (length coll))) (caar coll)))
                ((symbol-function 'org-open-at-point) (lambda (&rest _) (setq called t))))
        (org-glance-open))
      (should called))))

(ert-deftest org-glance-test:extract-filters-nonpropertized ()
  "extract offers only headlines with key-value pairs."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "K" "* foo" "- k: v")
                             (org-glance-test:headline "N" "* bar" "no pairs"))
    (let ((org-glance-graph graph))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p coll &rest _)
                   (if (assoc "k" coll) "k"
                     (progn (should (= 1 (length coll))) (caar coll))))))
        (should (string= "v" (org-glance-extract)))))))

(ert-deftest org-glance-test:link-materialize-stale-id-errors ()
  "Following a stale org-glance link errors loudly, not popping a picker."
  (let ((org-glance-graph nil))
    (should-error (org-glance-link:material "no-such-id") :type 'user-error)
    (should-error (org-glance-link:open "no-such-id") :type 'user-error))
  (org-glance-test:with-graph graph
    (let ((org-glance-graph graph))
      (should-error (org-glance-link:material "no-such-id") :type 'user-error)
      (should-error (org-glance-link:open "no-such-id") :type 'user-error))))

(ert-deftest org-glance-test:material-datetime-mode-enabled ()
  "Materialized buffers enable `org-glance-datetime-mode'."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "R" "* TODO water"))
    (org-glance-test:with-material (buffer graph "R")
      (should org-glance-datetime-mode))))

(ert-deftest org-glance-test:material-snapshot-on-repeat ()
  "Completing a repetition snapshots the done state as an occurrence file --
NEVER a new headline -- stamped by the completed occurrence's timestamp."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "R" "* TODO water flowers :house:"
                                                             "SCHEDULED: <2026-06-07 Sun +1d>"))
    (org-glance-test:with-repeat (buffer graph "R" 7)
        (progn
          (org-glance-test:complete-repetition)
        (should (= 1 (length (org-glance-graph:headlines graph))))
        (let ((occ (org-glance-graph:occurrences graph "R")))
          (should (= 1 (length occ)))
          (should (string-prefix-p "2026-06-07" (caar occ)))
          (should (s-contains? "DONE water flowers" (f-read-text (cdar occ) 'utf-8))))
        (should (string= "TODO" (org-get-todo-state)))))))

(ert-deftest org-glance-test:material-cleanup-after-repeat ()
  "After repeating, the live headline is trimmed to header + pinned blocks."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "R" "* TODO routine"
                                                             "SCHEDULED: <2026-06-07 Sun +1d>"
                                                             "#+begin_pin"
                                                             "keep me"
                                                             "#+end_pin"
                                                             "transient note"))
    (org-glance-test:with-repeat (buffer graph "R" 7)
        (progn
          (org-glance-test:complete-repetition)
        (should (s-contains? "keep me" (buffer-string)))
        (should-not (s-contains? "transient note" (buffer-string)))
        (should (s-contains? ":ORG_GLANCE_ID: R" (buffer-string)))
        (should (s-contains? "transient note"
                             (f-read-text (cdar (org-glance-graph:occurrences graph "R"))
                                          'utf-8)))))))

(ert-deftest org-glance-test:material-no-snapshot-when-depth-zero ()
  "Depth 0 (the default): no snapshot, no trim -- repeating changes nothing else."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "R" "* TODO water"
                                                             "SCHEDULED: <2026-06-07 Sun +1d>"
                                                             "transient note"))
    (org-glance-test:with-repeat (buffer graph "R" 0)
        (progn
          (org-glance-test:complete-repetition)
        (should (= 1 (length (org-glance-graph:headlines graph))))
        (should (null (org-glance-graph:occurrences graph "R")))
        (should (s-contains? "transient note" (buffer-string)))))))

(ert-deftest org-glance-test:material-hides-reserved-properties ()
  "Bookkeeping drawer lines are concealed (overlays, never deleted); a drawer
whose every property is reserved hides entirely; nil custom shows all; the
concealment survives a save."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline-props "vis" "* TODO Visible" '(("AUTHOR" . "Tolkien")))
      (org-glance-test:headline "bare" "* TODO Bare"))
    (cl-flet ((line-invisible? (re)
                (save-excursion
                  (goto-char (point-min))
                  (re-search-forward re)
                  (invisible-p (line-beginning-position)))))
      (org-glance-test:with-material (buf graph "vis")
        (should (line-invisible? ":ORG_GLANCE_ID:"))
        (should-not (line-invisible? ":AUTHOR:"))
        (should-not (line-invisible? ":PROPERTIES:"))
        (should (s-contains? ":ORG_GLANCE_ID: vis" (buffer-string)))
        (goto-char (point-max))
        (insert "\nnote")
        (org-glance-test:save)
        (should (line-invisible? ":ORG_GLANCE_ID:")))
      (org-glance-test:with-material (buf graph "bare")
        (should (line-invisible? ":PROPERTIES:"))
        (should (line-invisible? ":ORG_GLANCE_ID:"))
        (should (line-invisible? ":END:")))
      (let ((org-glance-material-hidden-properties nil))
        (org-glance-test:with-material (buf graph "bare")
          (should-not (line-invisible? ":ORG_GLANCE_ID:")))))))

(ert-deftest org-glance-test:material-reserved-properties-managed ()
  "A hand edit to a reserved property is reverted on save with a warning:
the original id survives to disk and metadata, a hand-added ORG_GLANCE_HASH is
removed, and ordinary edits persist under the original id."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline-props "vis" "* TODO Visible" '(("AUTHOR" . "Tolkien"))))
    (let (warnings)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (_type msg &rest _) (push msg warnings))))
        (org-glance-test:with-material (buf graph "vis")
          (org-glance-test:sed ":ORG_GLANCE_ID: vis" ":ORG_GLANCE_ID: hacked")
          (org-glance-test:sed ":AUTHOR: Tolkien"
                               ":ORG_GLANCE_HASH: fake\n:AUTHOR: Le Guin")
          (org-glance-test:save)
          (should (s-contains? ":ORG_GLANCE_ID: vis" (buffer-string)))
          (should-not (s-contains? "hacked" (buffer-string)))
          (should-not (s-contains? "ORG_GLANCE_HASH" (buffer-string)))
          (should (= 2 (length warnings)))
          (should (cl-some (lambda (w) (s-contains? "ORG_GLANCE_ID" w)) warnings))
          (should (cl-some (lambda (w) (s-contains? "ORG_GLANCE_HASH" w)) warnings))
          ;; a CLEAN save is silent (pins the equal-gate: always-revert would warn)
          (set-buffer-modified-p t)
          (org-glance-test:save)
          (should (= 2 (length warnings)))
          (goto-char (point-min))
          (re-search-forward ":ORG_GLANCE_ID: vis")
          (delete-region (line-beginning-position) (1+ (line-end-position)))
          (should-not (s-contains? ":ORG_GLANCE_ID:" (buffer-string)))
          (org-glance-test:save)
          (should (s-contains? ":ORG_GLANCE_ID: vis" (buffer-string)))
          (should (= 3 (length warnings)))))
      (should (s-contains? ":ORG_GLANCE_ID: vis" (org-glance-graph:get-content graph "vis")))
      (should (s-contains? "Le Guin" (org-glance-graph:get-content graph "vis")))
      (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "vis"))))))

(ert-deftest org-glance-test:metadata-repeated-predicate ()
  "`repeated?' reads a NONZERO repeater cookie off schedule/deadline strings."
  (cl-flet ((rep? (&rest lines)
              (org-glance-headline-metadata:repeated?
               (org-glance-headline:metadata
                (apply #'org-glance-test:headline "x" lines)))))
    (should (rep? "* TODO A" "SCHEDULED: <2026-06-07 Sun +1d>"))
    (should (rep? "* TODO B" "DEADLINE: <2026-06-07 Sun ++2w>"))
    (should-not (rep? "* TODO C" "SCHEDULED: <2026-06-07 Sun>"))
    (should-not (rep? "* TODO D"))
    (should-not (rep? "* TODO E" "SCHEDULED: <2026-06-07 Sun +0d>"))))  ; disarmed

(ert-deftest org-glance-test:material-history-picker ()
  "`C-c h' completing-reads an occurrence stamp and opens it READ-ONLY;
without history it errors with a hint."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "R" "* TODO daily"
                                                             "SCHEDULED: <2026-06-07 Sun +1d>"))
    (org-glance-test:with-repeat (buffer graph "R" 3)
      (progn
        (should (eq (key-binding (kbd "C-c h")) #'org-glance-material:history))
        (should-error (org-glance-material:history) :type 'user-error)  ; no history yet
        (org-glance-test:complete-repetition)
        (org-glance-test:with-shown (shown)
          (org-glance-test:offering (offered (car offered))          ; newest stamp
            (org-glance-material:history))
          (with-current-buffer shown
            (should buffer-read-only)
            (should (s-contains? "DONE daily" (buffer-string)))
            (should (s-contains? "org-glance-occurrence" (buffer-name)))))))))

(ert-deftest org-glance-test:material-snapshot-prunes-to-depth ()
  "The newest DEPTH snapshots survive; older ones are pruned on each write."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "R" "* TODO daily"
                                                             "SCHEDULED: <2026-06-07 Sun +1d>"))
    (org-glance-test:with-repeat (buffer graph "R" 2)
      (progn
        (dotimes (_ 4)                       ; complete four consecutive occurrences
          (org-glance-test:complete-repetition))
        (let ((occ (org-glance-graph:occurrences graph "R")))
          (should (= 2 (length occ)))                       ; pruned to depth
          (should (equal (sort (mapcar #'car occ) #'string>) (mapcar #'car occ)))
          (should (string-prefix-p "2026-06-10" (caar occ))))))))

(ert-deftest org-glance-test:materialize-honors-filter ()
  "`org-glance-materialize' restricts its candidate list to `org-glance-filter-spec'.
Drives the real command; the chosen-headline `open' is stubbed so no file IO
runs and only the offered candidate set is observed."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "mt" "* TODO Todo" "body")
                             (org-glance-test:headline "md" "* DONE Done" "body"))
    (let ((org-glance-graph graph))
      (cl-letf (((symbol-function 'org-glance-material:open) (lambda (&rest _) (current-buffer)))
                ((symbol-function 'switch-to-buffer) #'ignore))
        (org-glance-test:offering (offered (caar offered))
          (let ((org-glance-filter-spec '(:done nil)))     ; active
            (org-glance-materialize) (should (equal '("mt") (org-glance-test:offered-ids offered))))
          (let ((org-glance-filter-spec '(:done t)))       ; done
            (org-glance-materialize) (should (equal '("md") (org-glance-test:offered-ids offered))))
          (let ((org-glance-filter-spec nil))              ; all
            (org-glance-materialize) (should (equal '("md" "mt") (org-glance-test:offered-ids offered))))
          (let ((org-glance-filter-spec '(:state "DONE"))) ; exact state
            (org-glance-materialize) (should (equal '("md") (org-glance-test:offered-ids offered)))))))))

(ert-deftest org-glance-test:materialize-default-filter-is-active ()
  "With NO binding, `org-glance-filter-spec' defaults to active: DONE excluded.
Pins the design default so an accidental change to the defvar fails loudly."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "dt" "* TODO Todo" "body")
                             (org-glance-test:headline "dd" "* DONE Done" "body"))
    (let ((org-glance-graph graph))
      (cl-letf (((symbol-function 'org-glance-material:open) (lambda (&rest _) (current-buffer)))
                ((symbol-function 'switch-to-buffer) #'ignore))
        (org-glance-test:offering (offered (caar offered))
          (org-glance-materialize)            ; no `org-glance-filter-spec' binding
          (should (equal '("dt") (org-glance-test:offered-ids offered))))))))

(ert-deftest org-glance-test:open-honors-filter ()
  "`org-glance-open' composes `org-glance-filter-spec' with its linked? constraint."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "ta" "* TODO A" "[[https://a.example][a]]")
                             (org-glance-test:headline "da" "* DONE B" "[[https://b.example][b]]")
                             (org-glance-test:headline "tn" "* TODO C" "no link"))
    (let ((org-glance-graph graph))
      (cl-letf (((symbol-function 'org-glance-material:open-link) #'ignore))
        (org-glance-test:offering (offered (caar offered))
          (let ((org-glance-filter-spec '(:done nil)))
            (org-glance-open) (should (equal '("ta") (org-glance-test:offered-ids offered))))
          (let ((org-glance-filter-spec '(:done t)))
            (org-glance-open) (should (equal '("da") (org-glance-test:offered-ids offered))))
          (let ((org-glance-filter-spec nil))
            (org-glance-open) (should (equal '("da" "ta") (org-glance-test:offered-ids offered)))))))))

(ert-deftest org-glance-test:extract-honors-filter ()
  "`org-glance-extract' composes `org-glance-filter-spec' with its propertized? constraint."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "tk" "* TODO A" "- k: v")
                             (org-glance-test:headline "dk" "* DONE B" "- k: v")
                             (org-glance-test:headline "tn" "* TODO C" "no pairs"))
    (let ((org-glance-graph graph))
      (cl-letf (((symbol-function 'org-glance-material:extract) #'ignore))
        (org-glance-test:offering (offered (caar offered))
          (let ((org-glance-filter-spec '(:done nil)))
            (org-glance-extract) (should (equal '("tk") (org-glance-test:offered-ids offered))))
          (let ((org-glance-filter-spec '(:done t)))
            (org-glance-extract) (should (equal '("dk") (org-glance-test:offered-ids offered))))
          (let ((org-glance-filter-spec nil))
            (org-glance-extract) (should (equal '("dk" "tk") (org-glance-test:offered-ids offered)))))))))

(ert-deftest org-glance-test:material-change-todo-live-global ()
  "`change-todo-live' advances the state via org's own algorithm, persists it
through the save->sync path, and finalizes with the new state (Tier A, no note)."
  (org-glance-test:with-todo-done
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "c1" "* TODO Alpha"))
      (should (equal "DONE" (org-glance-test:change-todo-live graph "c1")))
      (should (equal "DONE" (org-glance-test:field graph "c1" state)))
      (should (s-contains? "* DONE Alpha" (org-glance-graph:get-content graph "c1")))
      (should (equal "" (org-glance-test:change-todo-live graph "c1")))
      (should (equal "" (org-glance-test:field graph "c1" state)))
      (should-not (get-file-buffer
                   (f-join (org-glance-graph:headline-data-path graph "c1") "data.org"))))))

(ert-deftest org-glance-test:material-change-todo-live-closed ()
  "Completing to DONE with time-logging adds a CLOSED timestamp, exactly like org."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))) (org-log-done 'time))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "c1" "* TODO Alpha"))
      (should (equal "DONE" (org-glance-test:change-todo-live graph "c1")))
      (should (s-contains? "CLOSED:" (org-glance-graph:get-content graph "c1"))))))

(ert-deftest org-glance-test:material-change-todo-live-unknown-id ()
  "`change-todo-live' on an unknown id signals rather than corrupting the store."
  (org-glance-test:with-graph graph
    (should-error (org-glance-test:change-todo-live graph "nope") :type 'user-error)))

(ert-deftest org-glance-test:material-change-todo-live-note ()
  "Tier B: an interactive LOGBOOK note is honoured -- committing (`C-c C-c') stores
the note, aborting (`C-c C-k') discards it, and BOTH keep the state + CLOSED
(native `C-c C-t' semantics).  Drives the deferred note flow synchronously."
  (dolist (case '((:label "commit" :abort nil :note t)
                  (:label "abort"  :abort t   :note nil)))
    (org-glance-test:with-note-origin (origin)
      (let ((org-todo-keywords '((sequence "TODO" "DONE")))
            (org-log-done 'note) (org-log-into-drawer t)
            (this-command 'org-glance-test-cct)
            (finalized 'unset))
        (org-glance-test:with-graph graph
          (org-glance-graph:add graph (org-glance-test:headline "n1" "* TODO Alpha"))
          (with-current-buffer origin
            (org-glance-material:change-todo-live
             graph "n1" nil (lambda (s) (setq finalized s))))
          (ignore-errors (run-hooks 'post-command-hook))
          (let ((nb (get-buffer "*Org Note*")))
            (should nb)
            (with-current-buffer nb
              (unless (plist-get case :abort) (insert "the reason"))
              (let ((org-note-abort (plist-get case :abort)))
                (ignore-errors (funcall org-finish-function)))))
          (with-timeout (3) (while (eq finalized 'unset) (sit-for 0.02)))
          (let ((blob (org-glance-graph:get-content graph "n1")))
            (should (equal "DONE" finalized))
            (should (equal "DONE" (org-glance-test:field graph "n1" state)))
            (should (s-contains? "CLOSED:" blob))            ; state+CLOSED always kept
            (should (eq (plist-get case :note)               ; note only on commit
                        (and (s-contains? "the reason" blob) t)))))))))

(ert-deftest org-glance-test:material-set-todo-bulk ()
  "`set-todo-bulk' sets each id to STATE, persists it, and reports changed/skipped."
  (org-glance-test:with-todo-done
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph
                            (org-glance-test:headline "m1" "* TODO A")
                            (org-glance-test:headline "m2" "* TODO B")
                            (org-glance-test:headline "m3" "* TODO C"))
      (let ((result 'unset))
        (org-glance-material:set-todo-bulk
         graph '("m1" "m2") "DONE"
         (lambda (changed skipped) (setq result (list changed skipped))))
        (should (equal '("m1" "m2") (car result)))      ; both set
        (should (null (cadr result)))                    ; none skipped
        (should (equal "DONE" (org-glance-test:field graph "m1" state)))
        (should (equal "DONE" (org-glance-test:field graph "m2" state)))
        (should (equal "TODO" (org-glance-headline-metadata:state  ; untouched
                               (org-glance-graph:get-headline graph "m3"))))))))

(ert-deftest org-glance-test:material-set-todo-bulk-full-timestamp-logging ()
  "Bulk records the LOGBOOK state-change entry for EVERY row under timestamp
logging (flushed synchronously, org-agenda style), never discarded, and leaves
nothing dangling on `post-command-hook'."
  ;; `DONE(!)' rides in via `org-todo-keywords': a global
  ;; `org-todo-log-states' would not reach the buffer -- it is buffer-local.
  (let ((org-todo-keywords '((sequence "TODO" "DONE(!)")))
        (org-log-into-drawer nil) (this-command 'org-glance-test-bulk))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph
                            (org-glance-test:headline "l1" "* TODO A")
                            (org-glance-test:headline "l2" "* TODO B"))
      (org-glance-material:set-todo-bulk graph '("l1" "l2") "DONE" #'ignore)
      (should-not org-log-setup)                                ; nothing left queued
      (should-not (memq #'org-add-log-note post-command-hook))  ; hook is clean
      (run-hooks 'post-command-hook)                            ; must NOT error
      (dolist (id '("l1" "l2"))                                 ; EVERY row logged
        (let ((blob (org-glance-graph:get-content graph id)))
          (should (equal "DONE" (org-glance-test:field graph id state)))
          (should (s-contains? "State \"DONE\"" blob)))))))     ; LOGBOOK entry present

(ert-deftest org-glance-test:material-set-todo-bulk-skips-unsaved ()
  "Bulk leaves a materialized buffer with unsaved edits untouched (skipped)."
  (org-glance-test:with-todo-done
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "u1" "* TODO A"))
      (org-glance-test:with-material (buf graph "u1")
        (let ((result 'unset))
          (goto-char (point-max)) (insert "dirty\n")
          (org-glance-material:set-todo-bulk
           graph '("u1") "DONE" (lambda (c s) (setq result (list c s))))
          (should (null (car result)))                       ; not changed
          (should (equal '("u1" . "unsaved changes") (car (cadr result))))
          (should (equal "TODO" (org-glance-test:field graph "u1" state)))
          (should (buffer-modified-p buf)))))))               ; edits preserved

(ert-deftest org-glance-test:material-set-todo-bulk-note-sequential ()
  "Under note logging, bulk prompts for a note PER ROW, sequentially, and records
each one -- never discarding, never dangling on a killed buffer."
  (org-glance-test:with-note-origin (origin)
    (let ((org-todo-keywords '((sequence "TODO" "DONE")))
          (org-log-done 'note) (org-log-into-drawer t)
          (this-command 'org-glance-test-bulk)
          (finalized 'unset) (notes 0))
      (org-glance-test:with-graph graph
        (org-glance-graph:add graph
                              (org-glance-test:headline "n1" "* TODO A")
                              (org-glance-test:headline "n2" "* TODO B"))
        (with-current-buffer origin
          (org-glance-material:set-todo-bulk
           graph '("n1" "n2") "DONE" (lambda (c _s) (setq finalized c))))
        ;; Pump the timer: each commit releases the next row's prompt.
        (with-timeout (5)
          (while (eq finalized 'unset)
            (let ((nb (get-buffer "*Org Note*")))
              (if (not (buffer-live-p nb))
                  (sit-for 0.02)
                (with-current-buffer nb
                  (insert (format "reason %d" (cl-incf notes)))
                  (let ((org-note-abort nil)) (funcall org-finish-function)))))))
        (should (equal 2 notes))                       ; one prompt PER row
        (should (equal '("n1" "n2") finalized))        ; both recorded, in order
        (dolist (pair '(("n1" . "reason 1") ("n2" . "reason 2")))
          (let ((blob (org-glance-graph:get-content graph (car pair))))
            (should (equal "DONE" (org-glance-headline-metadata:state
                                   (org-glance-graph:get-headline graph (car pair)))))
            (should (s-contains? (cdr pair) blob))      ; the note text landed
            (should (s-contains? "CLOSED:" blob))))))))  ; and CLOSED stayed

(ert-deftest org-glance-test:material-set-todo-bulk-repeater-no-dangling ()
  "A repeating task set DONE in bulk reschedules and leaves no dangling log note."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))) (org-log-repeat 'time)
        (this-command 'org-glance-test-bulk))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph
                            (org-glance-test:headline "rp1" "* TODO A"
                                                      "SCHEDULED: <2026-07-03 Fri +1w>"))
      (org-glance-material:set-todo-bulk graph '("rp1") "DONE" #'ignore)
      (should-not org-log-setup)
      (run-hooks 'post-command-hook)                            ; must NOT error
      (should (s-contains? "+1w" (org-glance-graph:get-content graph "rp1"))))))

(ert-deftest org-glance-test:material-encrypted-decrypt-roundtrip ()
  "Materializing an encrypted headline prompts for the password, shows plaintext,
and re-encrypts on save so `data.org' never holds plaintext and edits round-trip."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-headline:encrypt
                                 (org-glance-test:headline "enc" "* TODO Secret" "plainbody")
                                 "pw"))
    (should (s-contains? "aes-encrypted" (org-glance-graph:get-content graph "enc")))
    (org-glance-test:answering ((read-passwd "pw"))
      (org-glance-test:with-material (buffer graph "enc")
        (should (s-contains? "aes-encrypted" (buffer-string)))
        (org-glance-material:decrypt)
        (should (string= "pw" org-glance-material--password))
        ;; hardening keeps plaintext off disk entirely (invariant 14).
        (should (null buffer-auto-save-file-name))
        (should backup-inhibited)
        (should (null create-lockfiles))
        (org-glance-test:answering ((y-or-n-p nil))
          (org-glance-material:lock))
        (should (string= "pw" org-glance-material--password))
        (org-glance-test:answering ((y-or-n-p t))
          (org-glance-material:lock))
        (should (null org-glance-material--password))
        (org-glance-material--set-password "pw")   ; restore for the save below
        (should (save-excursion (goto-char (point-min)) (re-search-forward "plainbody" nil t)))
        (should-not (save-excursion (goto-char (point-min)) (re-search-forward "aes-encrypted" nil t)))
        (org-glance-test:sed "plainbody" "editedbody")
        (org-glance-test:save)
        (should (s-contains? "aes-encrypted" (org-glance-graph:get-content graph "enc")))
        (should (org-glance-test:field graph "enc" encrypted?))
        (should (save-excursion (goto-char (point-min)) (re-search-forward "editedbody" nil t)))
        (should-not (buffer-modified-p)))
      (org-glance-test:with-material (buffer graph "enc")
        (should-not (save-excursion (goto-char (point-min)) (re-search-forward "editedbody" nil t)))
        (org-glance-material:decrypt)
        (should (save-excursion (goto-char (point-min)) (re-search-forward "editedbody" nil t)))))))

(ert-deftest org-glance-test:material-crypt-set-roundtrip ()
  "`org-glance-material:crypt-set' encrypts a plaintext headline's stored
blob and decrypts it back, flipping the `encrypted?' projection each way, and
refuses to re-encrypt an already-encrypted headline."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "sec" "* TODO Secret" "plainbody"))
    (should-not (org-glance-test:field graph "sec" encrypted?))
    (should (org-glance-material:crypt-set graph "sec" t "pw"))
    (should (s-contains? "aes-encrypted" (org-glance-graph:get-content graph "sec")))
    (should (org-glance-test:field graph "sec" encrypted?))
    (should-error (org-glance-material:crypt-set graph "sec" t "pw"))  ; already
    (should (org-glance-material:crypt-set graph "sec" nil "pw"))
    (let ((blob (org-glance-graph:get-content graph "sec")))
      (should-not (s-contains? "aes-encrypted" blob))
      (should (s-contains? "plainbody" blob)))
    (should-not (org-glance-test:field graph "sec" encrypted?))))

(ert-deftest org-glance-test:material-crypt-set-guards-unsaved ()
  "`crypt-set' refuses when the blob is open with unsaved edits."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "sec" "* TODO Secret" "body"))
    (org-glance-test:with-material (buffer graph "sec")
      (org-glance-test:sed "body" "edited")   ; dirty, unsaved
      (should-error (org-glance-material:crypt-set graph "sec" t "pw")))))

(ert-deftest org-glance-test:material-history-depth-property ()
  "ORG_GLANCE_REPEAT_HISTORY_DEPTH on the headline overrides the global depth:
a positive value enables history under a global 0, inf disables pruning under
a global 1, and 0 disables under a global t."
  (org-glance-test:with-graph graph
    (cl-flet ((repeater (id depth)
                (org-glance-test:headline-props
                 id "* TODO daily"
                 `(("ORG_GLANCE_REPEAT_HISTORY_DEPTH" . ,depth))
                 "SCHEDULED: <2026-06-07 Sun +1d>")))
      (org-glance-graph:add graph
        (repeater "on" "2") (repeater "inf" "inf") (repeater "off" "0")
        (repeater "junk" "whenever")))
    (org-glance-test:with-repeat (buffer graph "on" 0)
      (progn (dotimes (_ 3) (org-glance-test:complete-repetition))
             (should (= 2 (length (org-glance-graph:occurrences graph "on"))))))
    (org-glance-test:with-repeat (buffer graph "inf" 1)
      (progn (dotimes (_ 3) (org-glance-test:complete-repetition))
             (should (= 3 (length (org-glance-graph:occurrences graph "inf"))))))
    (org-glance-test:with-repeat (buffer graph "off" t)
      (progn (org-glance-test:complete-repetition)
             (should (null (org-glance-graph:occurrences graph "off")))))
    (org-glance-test:with-repeat (buffer graph "junk" t)
      (progn (org-glance-test:complete-repetition)
             (should (null (org-glance-graph:occurrences graph "junk")))))))

(ert-deftest org-glance-test:material-snapshot-unlimited-depth ()
  "Depth t keeps every occurrence -- no pruning."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "R" "* TODO daily"
                                                             "SCHEDULED: <2026-06-07 Sun +1d>"))
    (org-glance-test:with-repeat (buffer graph "R" t)
      (progn
        (dotimes (_ 4)
          (org-glance-test:complete-repetition))
        (should (= 4 (length (org-glance-graph:occurrences graph "R"))))))))

(ert-deftest org-glance-test:material-crypt-whole-body-no-region ()
  "`C-c #' with no region in a PLAINTEXT buffer encrypts the whole body:
one crypt block wrapping it, sealed ciphertext on disk after save."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "wb" "* TODO Secretish"
                                                             "line one" "line two"))
    (org-glance-test:answering ((read-passwd "pw"))
      (org-glance-test:with-material (buffer graph "wb")
        (deactivate-mark)
        (org-glance-material:crypt)                     ; the C-c # dispatcher
        (should (= 1 (s-count-matches "#\\+begin_crypt" (buffer-string))))
        (should (s-contains? "line one" (buffer-string)))   ; still plaintext in buffer
        ;; the SEAL route refuses unsaved edits (invariant 11).
        (should-error (org-glance-material:crypt) :type 'user-error)
        (should (= 1 (s-count-matches "#\\+begin_crypt" (buffer-string))))
        (should (string= "pw" org-glance-material--password))
        (org-glance-test:save)
        (org-glance-material:crypt)
        (should-not (s-contains? "line one" (buffer-string)))
        (should (= 1 (s-count-matches "#\\+begin_crypt" (buffer-string))))
        (should-not org-glance-material--password)))
    (let ((blob (org-glance-graph:get-content graph "wb")))
      (should (s-contains? "aes-encrypted" blob))
      (should-not (s-contains? "line one" blob)))
    (should (org-glance-test:field graph "wb" encrypted?))))

(ert-deftest org-glance-test:material-delete-referrer-aware ()
  "Delete tombstones after confirmation; the prompt names referrers; declining
keeps the headline; the referrer's edge dangles harmlessly afterwards."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "gone" "* TODO Doomed")
      (org-glance-test:headline "ref" "* TODO Referrer"
        "[[org-glance-material:gone][Doomed]]"))
    (let (prompt)
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (p) (setq prompt p) nil)))
        (should-not (org-glance-material:delete graph "gone")))
      (should (s-contains? "Referrer" prompt))
      (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "gone")))
      (let ((buf (org-glance-material:open graph "gone")))
        (org-glance-test:answering ((yes-or-no-p t))
          (should (org-glance-material:delete graph "gone")))
        (should-not (buffer-live-p buf)))
      (should (eq 'tombstone (org-glance-graph:get-headline graph "gone")))
      (should (equal '("ref") (org-glance-test:filter-ids graph '(:refers-to "gone"))))
      (should (equal '(("gone" . nil))
                     (org-glance-test:field graph "ref" relations)))
      (should (equal "gone" (org-glance-graph:title-or-id graph "gone")))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (p) (setq prompt p) t)))
        (should (org-glance-material:delete graph "ref")))
      (should-not (s-contains? "reference" prompt))
      (org-glance-graph:compact graph)
      (should-not (f-exists? (org-glance-graph:headline-data-path graph "gone"))))))

(ert-deftest org-glance-test:material-crypt-set-purges-occurrences ()
  "Encrypting a headline deletes its PLAINTEXT occurrence snapshots (they are
copies of content the user just declared secret)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "R" "* TODO daily"
                                                             "SCHEDULED: <2026-06-07 Sun +1d>"))
    (org-glance-test:with-repeat (buffer graph "R" 3)
      (org-glance-test:complete-repetition))
    (should (= 1 (length (org-glance-graph:occurrences graph "R"))))
    (should (org-glance-material:crypt-set graph "R" t "pw"))
    (should (null (org-glance-graph:occurrences graph "R")))))

(ert-deftest org-glance-test:material-crypt-rekey ()
  "`crypt-rekey' re-keys an encrypted headline -- the new password decrypts,
the old no longer does -- and refuses a plaintext headline or a wrong OLD."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-headline:encrypt
                                 (org-glance-test:headline "k" "* TODO Secret" "body") "old"))
    (org-glance-graph:add graph (org-glance-test:headline "p" "* TODO Plain" "b"))
    (should-error (org-glance-material:crypt-rekey graph "p" "x" "y"))  ; not encrypted
    (should (org-glance-material:crypt-rekey graph "k" "old" "new"))
    (should (s-contains? "aes-encrypted" (org-glance-graph:get-content graph "k")))
    (should-error (org-glance-material:crypt-rekey graph "k" "old" "z"))  ; wrong OLD now
    (let ((hl (org-glance-graph:headline graph "k")))
      (should-error (org-glance-headline:decrypt hl "old"))                   ; old dead
      (should (s-contains? "body" (org-glance-headline:contents
                                   (org-glance-headline:decrypt hl "new"))))))) ; new works

(ert-deftest org-glance-test:material-crypt-legacy-upgrade ()
  "A legacy whole-body-cipher blob opens with its password, and the first save
silently upgrades the stored format to crypt blocks (same password works)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:legacy-encrypt
                                 (org-glance-test:headline "leg" "* TODO Old" "old secret")
                                 "pw"))
    (should-not (s-contains? "#+begin_crypt" (org-glance-graph:get-content graph "leg")))
    (org-glance-test:answering ((read-passwd "pw"))
      (org-glance-test:with-material (buffer graph "leg")
        (org-glance-material:decrypt)
        (should (s-contains? "old secret" (buffer-string)))   ; legacy branch decrypted
        (org-glance-test:sed "old secret" "new secret")
        (org-glance-test:save)
        (let ((blob (org-glance-graph:get-content graph "leg")))
          (should (s-contains? "#+begin_crypt" blob))         ; upgraded at rest
          (should (s-contains? "aes-encrypted" blob))
          (should-not (s-contains? "new secret" blob)))
        (should (s-contains? "new secret" (buffer-string))))  ; buffer plaintext again
      (org-glance-test:with-material (buffer graph "leg")     ; round-trips post-upgrade
        (should-not (s-contains? "new secret" (buffer-string)))   ; as-is: sealed
        (org-glance-material:decrypt)
        (should (s-contains? "new secret" (buffer-string)))))))

(ert-deftest org-glance-test:material-crypt-region-command ()
  "`crypt-region' wraps a body region; save seals ONLY it: the public rest stays
plaintext at rest and the metadata keeps `linked?' alongside `encrypted?'."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "mix" "* TODO Mixed"
                                                    "public [[https://example.com][site]]"
                                                    "secret line"))
    (org-glance-test:answering ((read-passwd "pw"))
      (org-glance-test:with-material (buffer graph "mix")
        (goto-char (point-min))
        (re-search-forward "secret line")
        (org-glance-material:crypt-region (match-beginning 0) (match-end 0))
        (org-glance-test:save)
        (let ((blob (org-glance-graph:get-content graph "mix")))
          (should (s-contains? "example.com" blob))           ; public link at rest
          (should (s-contains? "#+begin_crypt" blob))
          (should (s-contains? "aes-encrypted" blob))
          (should-not (s-contains? "secret line" blob)))
        (let ((meta (org-glance-graph:get-headline graph "mix")))
          (should (org-glance-headline-metadata:encrypted? meta))
          (should (org-glance-headline-metadata:linked? meta)))
        (should (s-contains? "secret line" (buffer-string)))))))  ; buffer plaintext

(ert-deftest org-glance-test:material-crypt-key-unwraps-block-at-point ()
  "`C-c #' with point inside a PLAINTEXT crypt block unwraps that block --
the dispatch arm that used to need `C-u'.  In a SEALED buffer the same key
unseals instead, since point sits inside the ciphertext block there."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-headline:encrypt
                                 (org-glance-test:headline "u2" "* TODO Secret"
                                   "plainbody")
                                 "pw"))
    (org-glance-test:answering ((read-passwd "pw"))
      (org-glance-test:with-material (buffer graph "u2")
        (goto-char (point-min))
        (search-forward "#+begin_crypt")                 ; inside the SEALED block
        (funcall (key-binding (kbd "C-c #")))            ; -> unseals, keeps the block
        (should (s-contains? "plainbody" (buffer-string)))
        (should (= 1 (length (org-glance--crypt-block-regions))))
        (goto-char (point-min))
        (search-forward "plainbody")                     ; inside the PLAIN block now
        (funcall (key-binding (kbd "C-c #")))            ; -> unwraps it
        (should-not (s-contains? "#+begin_crypt" (buffer-string)))
        (should (s-contains? "plainbody" (buffer-string)))
        (should-not org-glance-material--encrypted)))))  ; last block gone: public

(ert-deftest org-glance-test:material-crypt-unwrap-last-goes-public ()
  "Unwrapping the last crypt block and saving stores plaintext; `encrypted?'
flips off, so the next open needs no password."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-headline:encrypt
                                 (org-glance-test:headline "pub" "* TODO Was-secret" "plainbody")
                                 "pw"))
    (org-glance-test:answering ((read-passwd "pw"))
      (org-glance-test:with-material (buffer graph "pub")
        (goto-char (point-min))
        (search-forward "#+begin_crypt")                      ; on the sealed block
        (org-glance-material:crypt-unwrap)
        (org-glance-test:save)
        (let ((blob (org-glance-graph:get-content graph "pub")))
          (should (s-contains? "plainbody" blob))
          (should-not (s-contains? "#+begin_crypt" blob))
          (should-not (s-contains? "aes-encrypted" blob)))
        (should-not (org-glance-test:field graph "pub" encrypted?))))))

(ert-deftest org-glance-test:material-set-project-dir ()
  "`set-project-dir' writes then clears the `ORG_GLANCE_PROJECT_DIR' drawer
property on the materialized headline.  (Who READS it -- the `llm' plugin --
tests that end in its own repo.)"
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "d" "* TODO Doc" "body"))
    (org-glance-test:with-material (buffer graph "d")
      (org-glance-material:set-project-dir "/tmp/proj-x")
      (should (equal "/tmp/proj-x"
                     (org-glance-headline:node-property
                      "ORG_GLANCE_PROJECT_DIR" (org-glance-graph:headline graph "d"))))
      ;; `read-directory-name' always hands over a trailing slash.
      (org-glance-material:set-project-dir "/tmp/proj-y/")
      (should (equal "/tmp/proj-y"
                     (org-glance-headline:node-property
                      "ORG_GLANCE_PROJECT_DIR" (org-glance-graph:headline graph "d"))))
      (org-glance-material:set-project-dir nil)
      (should-not (org-glance-headline:node-property
                   "ORG_GLANCE_PROJECT_DIR" (org-glance-graph:headline graph "d"))))))

(ert-deftest org-glance-test:material-duplicate ()
  "`material:duplicate' copies the blob under a fresh id: body, planning,
state and tags kept, ORG_GLANCE_ID replaced, source untouched."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* TODO Alpha :x:"
                                "SCHEDULED: <2026-08-01 Sat>" "body line"))
    (let* ((new (org-glance-material:duplicate graph "a"))
           (meta (org-glance-graph:get-headline graph new)))
      (should-not (equal "a" new))
      (should (= 2 (length (org-glance-graph:headlines graph))))
      (should (equal "Alpha" (org-glance-headline-metadata:title meta)))
      (should (equal "TODO" (org-glance-headline-metadata:state meta)))
      (should (equal '("x") (org-glance-headline-metadata:tag-strings meta)))
      (should (s-contains? "2026-08-01" (org-glance-headline-metadata:schedule meta)))
      (let ((content (with-temp-buffer
                       (insert-file-contents (org-glance-graph:content-path graph new))
                       (buffer-string))))
        (should (s-contains? "body line" content))
        (should (string-match-p (concat ":ORG_GLANCE_ID:[ \t]+" (regexp-quote new)) content))
        (should-not (string-match-p ":ORG_GLANCE_ID:[ \t]+a$" content))))))

(ert-deftest org-glance-test:material-case-duplicate-tags ()
  "Case-duplicate tags collapse everywhere: parse (one canonical tag in
metadata), legacy records (tag-strings dedupes), retag (canonical compare,
both directions), and the material save (heading rewritten to one tag)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                          (org-glance-test:headline "a" "* TODO A :Food:food:"))
    (should (equal '("food") (org-glance-test:field graph "a" tag-strings)))
    (let ((meta (org-glance-headline-metadata:deserialize
                 '(:id "old" :state "" :title "Old" :tags ["Food" "food"]
                   :hash "h" :schedule nil :deadline nil :priority nil
                   :linked nil :propertized nil :encrypted nil))))
      (should (equal '("food") (org-glance-headline-metadata:tag-strings meta))))
    (should-not (org-glance-material:retag graph "a" "FOOD"))
    (should (org-glance-material:retag graph "a" "food" :remove t))
    (should-not (org-glance-test:field graph "a" tag-strings))
    (org-glance-graph:add graph
                          (org-glance-test:headline "b" "* TODO B :Food:food:"))
    (org-glance-test:with-material (buf graph "b")
      (set-buffer-modified-p t)                 ; force the save hooks to run
      (org-glance-test:save)
      (org-glance-material--goto-first-heading)
      (should (equal '("food") (org-get-tags nil t))))
    (let ((content (with-temp-buffer
                     (insert-file-contents (org-glance-graph:content-path graph "b"))
                     (buffer-string))))
      (should (s-contains? ":food:" content))
      (should-not (s-contains? "Food" content)))))

(ert-deftest org-glance-test:material-extract-here ()
  "`C-c e' in a material buffer copies a body KEY: value to the kill ring."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* TODO A" "- author: Tolkien"))
    (org-glance-test:with-material (buf graph "a")
      (should (eq (key-binding (kbd "C-c e")) #'org-glance-material:extract-here))
      (org-glance-test:offering (offered (caar offered))
        (org-glance-material:extract-here)
        (should (assoc "author" offered)))
      (should (equal "Tolkien" (current-kill 0))))))

(ert-deftest org-glance-test:material-open-reuses-live-buffer ()
  "Re-materializing returns the live wired buffer as-is; an encrypted
headline is NOT re-prompted for its password."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-headline:encrypt
                                 (org-glance-test:headline "enc" "* TODO Secret" "plainbody")
                                 "pw"))
    (org-glance-test:answering ((read-passwd "pw"))
      (org-glance-test:with-material (buf graph "enc" :decrypt t)
        (should (s-contains? "plainbody" (buffer-string)))
        (cl-letf (((symbol-function 'read-passwd)
                   (lambda (&rest _) (error "must not re-prompt"))))
          (should (eq buf (org-glance-material:open graph "enc")))
          (should (eq buf (org-glance-material:open graph "enc" :decrypt t))))))))

(ert-deftest org-glance-test:material-interval ()
  "The body's first active range projects to metadata, the overview line and
the table cell; `C-c i' inserts/replaces it live, `C-u C-c i' removes it."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "trip" "* DONE Petersburg :travel:"
                                "<2021-12-18 Sat>--<2021-12-19 Sun>")
      (org-glance-test:headline "flat" "* TODO No dates :travel:"))
    (let ((meta (org-glance-graph:get-headline (org-glance-test:reopen graph) "trip")))
      (should (equal '("<2021-12-18 Sat>" "<2021-12-19 Sun>")
                     (org-glance-headline-metadata:range meta))))
    (should-not (org-glance-test:field graph "flat" range))
    (should (s-contains? "<2021-12-18 Sat>--<2021-12-19 Sun>"
                         (org-glance-overview:render-headline
                          graph (org-glance-graph:get-headline graph "trip"))))
    (should (equal "2021-12-18..2021-12-19"
                   (alist-get 'interval
                              (alist-get 'cells (org-glance-table--row
                                                 (org-glance-graph:get-headline graph "trip"))))))
    (org-glance-test:with-material (buf graph "flat")
      (cl-letf (((symbol-function 'org-read-date)
                 (let ((n 0))
                   (lambda (&rest _)
                     (encode-time 0 0 0 (if (= (cl-incf n) 1) 18 19) 12 2021)))))
        (org-glance-material:set-interval))
      (should (s-contains? "<2021-12-18 Sat>--<2021-12-19 Sun>" (buffer-string)))
      (cl-letf (((symbol-function 'org-read-date)
                 (lambda (&rest _) (encode-time 0 0 0 20 12 2021))))
        (org-glance-material:set-interval))
      (should (s-contains? "<2021-12-20 Mon>--<2021-12-20 Mon>" (buffer-string)))
      (should-not (s-contains? "2021-12-18" (buffer-string)))
      (org-glance-material:set-interval '(4))
      (should-not (s-contains? "--<" (buffer-string)))
      (should-error (org-glance-material:set-interval '(4)) :type 'user-error)
      (set-buffer-modified-p t)
      (org-glance-test:save))
    (should-not (org-glance-test:field graph "flat" range))))

(ert-deftest org-glance-test:material-interval-body-scoped ()
  "Only a BODY range is the interval: ranges in the title, planning line,
property drawer or a crypt block never project, and `C-c i' never edits
them -- it inserts a fresh body line instead."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline-props
       "p" "* TODO Trip <2020-01-01 Wed>--<2020-01-02 Thu>"
       '(("DATES" . "<2020-02-01 Sat>--<2020-02-02 Sun>"))
       "SCHEDULED: <2020-03-01 Sun>--<2020-03-02 Mon>"))
    (should-not (org-glance-test:field graph "p" range))
    (org-glance-test:with-material (buf graph "p")
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "#+begin_crypt\n<2020-04-01 Wed>--<2020-04-02 Thu>\n#+end_crypt\n")
      (should-error (org-glance-material:set-interval '(4)) :type 'user-error)
      (cl-letf (((symbol-function 'org-read-date)
                 (lambda (&rest _) (encode-time 0 0 0 5 5 2020))))
        (org-glance-material:set-interval))
      (let ((text (buffer-string)))
        (should (s-contains? "<2020-05-05 Tue>--<2020-05-05 Tue>" text))
        (should (s-contains? "Trip <2020-01-01 Wed>--<2020-01-02 Thu>" text))
        (should (s-contains? ":DATES: <2020-02-01 Sat>--<2020-02-02 Sun>" text))
        (should (s-contains? "SCHEDULED: <2020-03-01 Sun>--<2020-03-02 Mon>" text))
        (should (s-contains? "<2020-04-01 Wed>--<2020-04-02 Thu>" text)))
      (set-buffer-modified-p t)
      (org-glance-test:save))
    (should (equal '("<2020-05-05 Tue>" "<2020-05-05 Tue>")
                   (org-glance-test:field graph "p" range)))))

(ert-deftest org-glance-test:material-open-link-here ()
  "`C-c j' opens a link from the LIVE buffer -- unsaved links count."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO A" "body"))
    (org-glance-test:with-material (buf graph "a")
      (should (eq (key-binding (kbd "C-c j")) #'org-glance-material:open-link-here))
      (goto-char (point-max))
      (insert "\n[[https://example.com/unsaved][Unsaved]]\n")   ; NOT saved
      (let (opened)
        (cl-letf (((symbol-function 'org-open-at-point)
                   (lambda (&rest _)
                     (setq opened (buffer-substring-no-properties
                                   (point) (min (point-max) (+ (point) 40)))))))
          (org-glance-material:open-link-here))
        (should (s-contains? "example.com/unsaved" opened))))))

(ert-deftest org-glance-test:material-opens-encrypted-as-is ()
  "An encrypted headline materializes AS-IS: no password prompt, ciphertext
on screen, unhardened; `:decrypt' (the transient's `-d') opens it unsealed,
and `C-c #' toggles the seal of an as-is buffer after the fact."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-headline:encrypt
                                 (org-glance-test:headline "e" "* TODO Secret" "plainbody")
                                 "pw"))
    (cl-letf (((symbol-function 'read-passwd)
               (lambda (&rest _) (error "must not prompt"))))
      (org-glance-test:with-material (buf graph "e")
        (should (s-contains? "aes-encrypted" (buffer-string)))
        (should-not (s-contains? "plainbody" (buffer-string)))
        (should-not org-glance-material--encrypted)          ; unwired until asked
        (should (eq (key-binding (kbd "C-c #")) #'org-glance-material:crypt))))
    (org-glance-test:answering ((read-passwd "pw"))
      (org-glance-test:with-material (buf graph "e")
        (funcall (key-binding (kbd "C-c #")))
        (should (s-contains? "plainbody" (buffer-string)))
        (should org-glance-material--encrypted)
        (funcall (key-binding (kbd "C-c #")))                ; toggle: re-seal
        (should-not (s-contains? "plainbody" (buffer-string)))
        (should (s-contains? "aes-encrypted" (buffer-string)))
        (should-not org-glance-material--password)           ; and forgotten
        (should-not (buffer-modified-p))                     ; == the bytes on disk
        (funcall (key-binding (kbd "C-c #")))                ; toggle: unseal again
        (should (s-contains? "plainbody" (buffer-string)))
        (cl-letf (((symbol-function 'read-passwd)
                   (lambda (&rest _) (error "must not re-prompt"))))
          (org-glance-material:decrypt))))
    ;; a bare `C-c #' unseals here; a second wrap would double-encrypt.
    (org-glance-test:answering ((read-passwd "pw"))
      (org-glance-test:with-material (buf graph "e")
        (org-glance-material:crypt)
        (should (= 1 (length (org-glance--crypt-block-regions))))
        (should (s-contains? "plainbody" (buffer-string)))))))

(ert-deftest org-glance-test:link-label-from-item-prefix ()
  "A link with NO description is labelled by the `KEY:\' text introducing it in
its list item -- `- Local: [[file:~/x]]\' offers \"Local\", never the raw URL.
A description still wins, and a link outside any list keeps the raw link."
  (with-temp-buffer
    (insert "* TODO Project\n"
            "- Local: [[file:/tmp/jupyterhub/]]\n"
            "- Docs: [[https://example.com/docs][Handbook]]\n"
            "- [[https://example.com/bare]]\n"
            "Loose [[https://example.com/loose]]\n")
    (org-glance--org-mode)
    (let ((labels (mapcar (lambda (e) (car (last (car e)))) (org-glance--link-paths))))
      (should (member "Local" labels))
      (should (member "Handbook" labels))
      (should-not (member "Docs" labels))
      (should (member "https://example.com/bare" labels))
      (should (member "https://example.com/loose" labels)))))

(ert-deftest org-glance-test:link-label-prefix-nested-path ()
  "The introducing text is the link\'s OWN path component, under its ancestry:
a nested `- Local: [[...]]\' reads (\"Remote\" \"Local\")."
  (with-temp-buffer
    (insert "* TODO Project\n"
            "- Remote\n"
            "  - Local: [[file:/tmp/x]]\n")
    (org-glance--org-mode)
    (let ((paths (mapcar #'car (org-glance--link-paths))))
      (should (equal '(("Remote" "Local")) paths)))))

(ert-deftest org-glance-test:material-open-link-nested ()
  "Nested link lists are picked level by level: the first prompt offers the
top items, the second the chosen branch's children; a lone candidate at any
depth is taken without asking."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "p" "* TODO Project"
        "- [[file+emacs:/tmp][Local]]"
        "- Remote"
        "  - [[https://example.com/gh][Remote (Github)]]"
        "  - [[https://example.com/gl][Remote (Gitlab)]]"
        "- Solo"
        "  - [[https://example.com/only][Only child]]"))
    (org-glance-test:with-material (buf graph "p")
      (cl-flet ((open-with (answers)
                  (let ((asked nil) (opened nil))
                    (cl-letf (((symbol-function 'completing-read)
                               (lambda (_p coll &rest _)
                                 (push (append coll nil) asked)
                                 (pop answers)))
                              ((symbol-function 'org-open-at-point)
                                (lambda (&rest _)
                                  (setq opened (buffer-substring-no-properties
                                                (point) (line-end-position))))))
                      (org-glance-material:open-link-here))
                    (cons (nreverse asked) opened))))
        (pcase-let ((`(,asked . ,opened) (open-with '("Remote" "Remote (Gitlab)"))))
          (should (equal '(("Local" "Remote" "Solo")
                           ("Remote (Github)" "Remote (Gitlab)"))
                         asked))
          (should (s-contains? "example.com/gl" opened)))
        (pcase-let ((`(,asked . ,opened) (open-with '("Local"))))
          (should (= 1 (length asked)))
          (should (s-contains? "file+emacs:/tmp" opened)))
        (pcase-let ((`(,asked . ,opened) (open-with '("Solo"))))
          (should (= 1 (length asked)))
          (should (s-contains? "example.com/only" opened)))))))

(ert-deftest org-glance-test:material-open-link-identical-paths ()
  "Two links that end up with the SAME path (same description, same depth)
are offered by their targets instead of recursing forever."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "d" "* TODO Docs"
        "- [[https://example.com/a][docs]]"
        "- [[https://example.com/b][docs]]"))
    (org-glance-test:with-material (buf graph "d")
      (let (offered opened)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p coll &rest _) (setq offered (append coll nil))
                     (cadr coll)))
                  ((symbol-function 'org-open-at-point)
                   (lambda (&rest _)
                     (setq opened (buffer-substring-no-properties
                                   (point) (line-end-position))))))
          (org-glance-material:open-link-here))
        (should (equal '("https://example.com/a" "https://example.com/b") offered))
        (should (s-contains? "example.com/b" opened))))))

(provide 'test-material)
;;; test-material.el ends here
