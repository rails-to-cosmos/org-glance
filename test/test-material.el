;;; test-material.el --- Tests for graph-backed materialize/sync  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:material-completing-read ()
  "Selection lists live graph headlines and resolves the chosen metadata."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "a" "* TODO Alpha :x:")
                             (org-glance-test:headline "b" "* DONE Beta :y:"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _) (caar collection))))
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
      ;; raw link markup never leaks into a label
      (should-not (cl-some (lambda (l) (s-contains? "[[" l)) labels)))))

(ert-deftest org-glance-test:material-active-filter ()
  "The commands filter selection to active headlines.
No `org-done-keywords' binding: `completing-read' resolves the done set itself,
so the filter is correct even from this non-Org command context."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "a" "* TODO Todo")
                             (org-glance-test:headline "b" "* DONE Done"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p coll &rest _)
                 (should (= 1 (length coll)))   ; DONE excluded
                 (caar coll))))
      (let ((meta (org-glance-material:completing-read
                   graph :filter #'org-glance-headline-metadata:active?)))
        (should (string= "a" (org-glance-headline-metadata:id meta)))))))

(ert-deftest org-glance-test:material-done-keywords-custom ()
  "`org-done-keywords' redefines what counts as active for selection."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "a" "* TODO Todo")
                             (org-glance-test:headline "b" "* DONE Done"))
    ;; Default: DONE is done, so the active (selectable) headline is the TODO one.
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p coll &rest _) (should (= 1 (length coll))) (caar coll))))
      (should (string= "a" (org-glance-headline-metadata:id
                            (org-glance-material:completing-read
                             graph :filter #'org-glance-headline-metadata:active?)))))
    ;; Declaring TODO done flips it: now the DONE headline is the active one.
    (let ((org-done-keywords '("TODO")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p coll &rest _) (should (= 1 (length coll))) (caar coll))))
        (should (string= "b" (org-glance-headline-metadata:id
                              (org-glance-material:completing-read
                               graph :filter #'org-glance-headline-metadata:active?))))))))

(ert-deftest org-glance-test:material-completing-read-filter ()
  "The FILTER predicate narrows the candidate list."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "a" "* TODO Alpha")
                             (org-glance-test:headline "b" "* DONE Beta"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (= 1 (length collection)))
                 (caar collection))))
      (let ((meta (org-glance-material:completing-read
                   graph
                   :filter (lambda (m) (string= "TODO" (org-glance-headline-metadata:state m))))))
        (should (string= "a" (org-glance-headline-metadata:id meta)))))))

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
      (org-glance-material:apply))
    (should (string= "DONE" (org-glance-headline-metadata:state
                             (org-glance-graph:get-headline graph "s1"))))
    ;; persisted: re-materializing shows the edited state
    (org-glance-test:with-material (buffer graph "s1")
      (should (s-contains? "DONE foo" (buffer-string))))))

(ert-deftest org-glance-test:material-edit-updates-metadata ()
  "Editing the heading and saving updates the stored metadata projection."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "u1" "* TODO foo"))
    (org-glance-test:with-material (buffer graph "u1")
      (org-glance-test:sed "TODO foo" "DONE bar")
      (org-glance-material:apply))
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
    (should (string= "DONE" (org-glance-headline-metadata:state
                             (org-glance-graph:get-headline graph "e2e1"))))))

(ert-deftest org-glance-test:material-save-via-save-buffer ()
  "`save-buffer' (the standard save mechanism, whatever key invokes it) syncs the
materialized buffer through `write-contents-functions' -- robust to configs that
rebind/shadow C-x C-s, and no \"not visiting a file\" prompt on this non-file buffer."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "sb1" "* TODO foo"))
    (org-glance-test:with-material (buffer graph "sb1")
      (org-glance-test:sed "TODO" "DONE")
      (set-buffer-modified-p t)
      (let ((inhibit-message t)) (save-buffer)))
    (should (string= "DONE" (org-glance-headline-metadata:state
                             (org-glance-graph:get-headline graph "sb1"))))))

(ert-deftest org-glance-test:material-id-change-skips-metadata ()
  "If the ORG_GLANCE_ID is edited, saving does not corrupt the metadata index
for the original id (the sync hook skips a mismatched id)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "m1" "* TODO foo"))
    (org-glance-test:with-material (buffer graph "m1")
      (org-glance-test:sed "^:ORG_GLANCE_ID:.*$" ":ORG_GLANCE_ID: changed")
      (let ((inhibit-message t)) (org-glance-material:apply)))
    ;; the original id's metadata is left untouched
    (should (string= "foo" (org-glance-headline-metadata:title
                            (org-glance-graph:get-headline graph "m1"))))))

(ert-deftest org-glance-test:material-open-missing ()
  "Materializing an unknown id errors."
  (org-glance-test:with-graph graph
    (should-error (org-glance-material:open graph "nope") :type 'user-error)))

;;; open / extract (read-only commands)

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
  ;; Uninitialized: still a loud user-error, no picker.
  (let ((org-glance-graph nil))
    (should-error (org-glance-link:materialize "no-such-id") :type 'user-error)
    (should-error (org-glance-link:open "no-such-id") :type 'user-error))
  ;; Initialized but the id is unknown: not-found user-error.
  (org-glance-test:with-graph graph
    (let ((org-glance-graph graph))
      (should-error (org-glance-link:materialize "no-such-id") :type 'user-error)
      (should-error (org-glance-link:open "no-such-id") :type 'user-error))))

(ert-deftest org-glance-test:material-datetime-mode-enabled ()
  "Materialized buffers enable `org-glance-datetime-mode'."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "R" "* TODO water"))
    (org-glance-test:with-material (buffer graph "R")
      (should org-glance-datetime-mode))))

(ert-deftest org-glance-test:material-clone-on-repeat ()
  "Completing a repeated headline preserves the done state as a new headline."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "R" "* TODO water flowers :house:"
                                                             "SCHEDULED: <2026-06-07 Sun +1d>"))
    (let ((org-glance-clone-on-repeat-p t)
          (org-log-repeat nil)
          (org-log-done nil))
      (org-glance-test:with-material (buffer graph "R")
        (goto-char (point-min))
        (org-todo "DONE")
        (let* ((headlines (org-glance-graph:headlines graph))
               (clone (cl-find-if (lambda (m) (not (string= "R" (org-glance-headline-metadata:id m))))
                                  headlines)))
          (should (= 2 (length headlines)))
          (should clone)
          ;; The clone preserves the completed state...
          (should (string= "DONE" (org-glance-headline-metadata:state clone)))
          (should (member "house" (append (org-glance-headline-metadata:tags clone) nil)))
          ;; ...with its repeater disarmed, so it never repeats again.
          (should (s-contains? "+0d" (org-glance-graph:get-content
                                      graph (org-glance-headline-metadata:id clone))))
          ;; The live headline repeated forward.
          (should (string= "TODO" (org-get-todo-state))))))))

(ert-deftest org-glance-test:material-cleanup-after-repeat ()
  "After repeating, the live headline is trimmed to header + pinned blocks."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "R" "* TODO routine"
                                                             "SCHEDULED: <2026-06-07 Sun +1d>"
                                                             "#+begin_pin"
                                                             "keep me"
                                                             "#+end_pin"
                                                             "transient note"))
    (let ((org-glance-clone-on-repeat-p t)
          (org-log-repeat nil)
          (org-log-done nil))
      (org-glance-test:with-material (buffer graph "R")
        (goto-char (point-min))
        (org-todo "DONE")
        (should (s-contains? "keep me" (buffer-string)))
        (should-not (s-contains? "transient note" (buffer-string)))
        (should (s-contains? ":ORG_GLANCE_ID: R" (buffer-string)))))))

(ert-deftest org-glance-test:material-no-clone-when-disabled ()
  "Without `org-glance-clone-on-repeat-p', repeating leaves the graph alone."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "R" "* TODO water"
                                                             "SCHEDULED: <2026-06-07 Sun +1d>"
                                                             "transient note"))
    (let ((org-glance-clone-on-repeat-p nil)
          (org-log-repeat nil)
          (org-log-done nil))
      (org-glance-test:with-material (buffer graph "R")
        (goto-char (point-min))
        (org-todo "DONE")
        (should (= 1 (length (org-glance-graph:headlines graph))))
        (should (s-contains? "transient note" (buffer-string)))))))

(cl-defmacro org-glance-test--seen-ids (&rest body)
  "Stub `completing-read' to capture the offered headline ids (sorted) in `seen',
returning the first candidate, then run BODY.  `seen' is bound around BODY."
  (declare (indent 0))
  `(let ((seen nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_p coll &rest _)
                  (setq seen (sort (mapcar (lambda (c) (org-glance-headline-metadata:id (cdr c))) coll)
                                   #'string<))
                  (caar coll))))
       ,@body)))

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
        (org-glance-test--seen-ids
          (let ((org-glance-filter-spec '(:done nil)))     ; active
            (org-glance-materialize) (should (equal '("mt") seen)))
          (let ((org-glance-filter-spec '(:done t)))       ; done
            (org-glance-materialize) (should (equal '("md") seen)))
          (let ((org-glance-filter-spec nil))              ; all
            (org-glance-materialize) (should (equal '("md" "mt") seen)))
          (let ((org-glance-filter-spec '(:state "DONE"))) ; exact state
            (org-glance-materialize) (should (equal '("md") seen))))))))

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
        (org-glance-test--seen-ids
          (org-glance-materialize)            ; no `org-glance-filter-spec' binding
          (should (equal '("dt") seen)))))))

(ert-deftest org-glance-test:open-honors-filter ()
  "`org-glance-open' composes `org-glance-filter-spec' with its linked? constraint."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "ta" "* TODO A" "[[https://a.example][a]]")
                             (org-glance-test:headline "da" "* DONE B" "[[https://b.example][b]]")
                             (org-glance-test:headline "tn" "* TODO C" "no link"))
    (let ((org-glance-graph graph))
      (cl-letf (((symbol-function 'org-glance-material:open-link) #'ignore))
        (org-glance-test--seen-ids
          ;; active + linked -> "ta" (DONE filtered out; "tn" lacks a link)
          (let ((org-glance-filter-spec '(:done nil)))
            (org-glance-open) (should (equal '("ta") seen)))
          ;; done + linked -> "da"
          (let ((org-glance-filter-spec '(:done t)))
            (org-glance-open) (should (equal '("da") seen)))
          ;; all + linked -> "ta","da" (still not "tn")
          (let ((org-glance-filter-spec nil))
            (org-glance-open) (should (equal '("da" "ta") seen))))))))

(ert-deftest org-glance-test:extract-honors-filter ()
  "`org-glance-extract' composes `org-glance-filter-spec' with its propertized? constraint."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "tk" "* TODO A" "- k: v")
                             (org-glance-test:headline "dk" "* DONE B" "- k: v")
                             (org-glance-test:headline "tn" "* TODO C" "no pairs"))
    (let ((org-glance-graph graph))
      (cl-letf (((symbol-function 'org-glance-material:extract) #'ignore))
        (org-glance-test--seen-ids
          ;; active + propertized -> "tk" (DONE filtered; "tn" lacks pairs)
          (let ((org-glance-filter-spec '(:done nil)))
            (org-glance-extract) (should (equal '("tk") seen)))
          ;; done + propertized -> "dk"
          (let ((org-glance-filter-spec '(:done t)))
            (org-glance-extract) (should (equal '("dk") seen)))
          ;; all + propertized -> "tk","dk" (still not "tn")
          (let ((org-glance-filter-spec nil))
            (org-glance-extract) (should (equal '("dk" "tk") seen))))))))

;;; TODO state change: materialize -> org-todo -> sync (exactly C-c C-t)

(ert-deftest org-glance-test:material-change-todo-live-global ()
  "`change-todo-live' advances the state via org's own algorithm, persists it
through the save->sync path, and finalizes with the new state (Tier A, no note)."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))) (org-log-done nil))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "c1" "* TODO Alpha"))
      ;; TODO -> DONE
      (should (equal "DONE" (org-glance-test:change-todo-live graph "c1")))
      (should (equal "DONE" (org-glance-headline-metadata:state
                             (org-glance-graph:get-headline graph "c1"))))
      (should (s-contains? "* DONE Alpha" (org-glance-graph:get-content graph "c1")))
      ;; DONE -> (none)
      (should (equal "" (org-glance-test:change-todo-live graph "c1")))
      (should (equal "" (org-glance-headline-metadata:state
                         (org-glance-graph:get-headline graph "c1"))))
      ;; the fresh background material buffer is not leaked
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
          ;; deliver + finish the note (C-c C-c commit, or C-c C-k abort)
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
            (should (equal "DONE" (org-glance-headline-metadata:state
                                   (org-glance-graph:get-headline graph "n1"))))
            (should (s-contains? "CLOSED:" blob))            ; state+CLOSED always kept
            (should (eq (plist-get case :note)               ; note only on commit
                        (and (s-contains? "the reason" blob) t)))))))))

;;; Bulk TODO state change: materialize -> org-todo -> sync, per row, no note

(ert-deftest org-glance-test:material-set-todo-bulk ()
  "`set-todo-bulk' sets each id to STATE, persists it, and reports changed/skipped."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))) (org-log-done nil))
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
        (should (equal "DONE" (org-glance-headline-metadata:state
                               (org-glance-graph:get-headline graph "m1"))))
        (should (equal "DONE" (org-glance-headline-metadata:state
                               (org-glance-graph:get-headline graph "m2"))))
        (should (equal "TODO" (org-glance-headline-metadata:state  ; untouched
                               (org-glance-graph:get-headline graph "m3"))))))))

(ert-deftest org-glance-test:material-set-todo-bulk-full-timestamp-logging ()
  "Bulk records the LOGBOOK state-change entry for EVERY row under timestamp
logging (flushed synchronously, org-agenda style), never discarded, and leaves
nothing dangling on `post-command-hook'."
  ;; `DONE(!)' logs a timestamp on entering DONE; the notation rides in via
  ;; `org-todo-keywords' so the materialized buffer actually derives the log
  ;; (a bare global `org-todo-log-states' would not -- it is buffer-local).
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
          (should (equal "DONE" (org-glance-headline-metadata:state
                                 (org-glance-graph:get-headline graph id))))
          (should (s-contains? "State \"DONE\"" blob)))))))     ; LOGBOOK entry present

(ert-deftest org-glance-test:material-set-todo-bulk-skips-unsaved ()
  "Bulk leaves a materialized buffer with unsaved edits untouched (skipped)."
  (let ((org-todo-keywords '((sequence "TODO" "DONE"))) (org-log-done nil))
    (org-glance-test:with-graph graph
      (org-glance-graph:add graph (org-glance-test:headline "u1" "* TODO A"))
      (org-glance-test:with-material (buf graph "u1")
        (let ((result 'unset))
          (goto-char (point-max)) (insert "dirty\n")
          (org-glance-material:set-todo-bulk
           graph '("u1") "DONE" (lambda (c s) (setq result (list c s))))
          (should (null (car result)))                       ; not changed
          (should (equal '("u1" . "unsaved changes") (car (cadr result))))
          (should (equal "TODO" (org-glance-headline-metadata:state
                                 (org-glance-graph:get-headline graph "u1"))))
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
        ;; Drive the sequential prompts: whenever `*Org Note*' appears, fill and
        ;; commit it; the next row's prompt follows once the timer advances.
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
    ;; Seeded blob on disk is ciphertext.
    (should (s-contains? "aes-encrypted" (org-glance-graph:get-content graph "enc")))
    (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) "pw")))
      (org-glance-test:with-material (buffer graph "enc")
        ;; Body is decrypted in the buffer; ciphertext never shown.
        (should (string= "pw" org-glance-material--password))
        ;; Hardening: plaintext cannot leak to auto-save/backup/lockfiles.
        (should (null buffer-auto-save-file-name))
        (should backup-inhibited)
        (should (null create-lockfiles))
        ;; Lock forgets the password (next save would re-prompt).
        (org-glance-material:lock)
        (should (null org-glance-material--password))
        (org-glance-material--set-password "pw")   ; restore for the save below
        (should (save-excursion (goto-char (point-min)) (re-search-forward "plainbody" nil t)))
        (should-not (save-excursion (goto-char (point-min)) (re-search-forward "aes-encrypted" nil t)))
        ;; Edit + save: before-save encrypts, after-save decrypts back.
        (org-glance-test:sed "plainbody" "editedbody")
        (let ((inhibit-message t)) (save-buffer))
        ;; Disk stays ciphertext; metadata still flagged encrypted.
        (should (s-contains? "aes-encrypted" (org-glance-graph:get-content graph "enc")))
        (should (org-glance-headline-metadata:encrypted?
                 (org-glance-graph:get-headline graph "enc")))
        ;; Buffer decrypted back to plaintext with the edit, not left dirty.
        (should (save-excursion (goto-char (point-min)) (re-search-forward "editedbody" nil t)))
        (should-not (buffer-modified-p)))
      ;; Reopen: the edited plaintext round-trips through the ciphertext blob.
      (org-glance-test:with-material (buffer graph "enc")
        (should (save-excursion (goto-char (point-min)) (re-search-forward "editedbody" nil t)))))))

(ert-deftest org-glance-test:llm-spawns-in-data-dir ()
  "`org-glance-llm' launches `agnostic-llm' in the chosen headline's data dir.
The directory is created if absent so the CLI has a working directory."
  (org-glance-test:session
    (org-glance-graph:add org-glance-graph
                          (org-glance-test:headline "llmid" "* TODO Alpha :x:"))
    (let ((spawned 'unset))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p coll &rest _) (caar coll)))
                ((symbol-function 'agnostic-llm)
                 (lambda (&optional dir) (setq spawned dir))))
        (org-glance-llm))
      (let ((expected (org-glance-graph:headline-data-path org-glance-graph "llmid")))
        (should (equal (file-truename spawned) (file-truename expected)))
        (should (file-directory-p spawned))))))

(provide 'test-material)
;;; test-material.el ends here
