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

(ert-deftest org-glance-test:material-open-apply ()
  "Materialize opens the blob editable; apply writes back a new version."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "m1" "* TODO foo"))
    (let ((buffer (org-glance-material:open graph "m1")))
      (unwind-protect
          (with-current-buffer buffer
            (should org-glance-material-mode)
            (should (s-contains? "TODO foo" (buffer-string)))
            (goto-char (point-min))
            (re-search-forward "TODO")
            (replace-match "DONE")
            (org-glance-material:apply))
        (kill-buffer buffer))
      (should (string= "DONE" (org-glance-headline-metadata:state
                               (org-glance-graph:get-headline graph "m1")))))))

(ert-deftest org-glance-test:material-save-affordance ()
  "A materialized buffer visits its content-blob FILE, runs the minor mode,
and is editable; saving persists to the graph and survives re-materialize.
This guards the interactive save path users actually hit."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "s1" "* TODO foo"))
    (let ((buffer (org-glance-material:open graph "s1")))
      (unwind-protect
          (with-current-buffer buffer
            (should org-glance-material-mode)
            (should-not buffer-read-only)
            (should buffer-file-name)            ; a real file -> the standard save works
            (goto-char (point-min))
            (re-search-forward "TODO")
            (replace-match "DONE")
            (org-glance-material:apply))
        (kill-buffer buffer))
      ;; persisted: re-materializing shows the edited state
      (let ((buffer2 (org-glance-material:open graph "s1")))
        (unwind-protect
            (with-current-buffer buffer2
              (should (s-contains? "DONE foo" (buffer-string))))
          (kill-buffer buffer2))))))

(ert-deftest org-glance-test:material-edit-updates-metadata ()
  "Editing the heading and saving updates the stored metadata, not just the blob."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "u1" "* TODO foo"))
    (let ((buffer (org-glance-material:open graph "u1")))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward "TODO foo")
            (replace-match "DONE bar")
            (org-glance-material:apply))
        (kill-buffer buffer)))
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
                (goto-char (point-min))
                (re-search-forward "TODO")
                (replace-match "DONE")
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
    (let ((buffer (org-glance-material:open graph "sb1")))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward "TODO")
            (replace-match "DONE")
            (set-buffer-modified-p t)
            (let ((inhibit-message t)) (save-buffer)))
        (set-buffer-modified-p nil)
        (kill-buffer buffer)))
    (should (string= "DONE" (org-glance-headline-metadata:state
                             (org-glance-graph:get-headline graph "sb1"))))))

(ert-deftest org-glance-test:material-id-change-skips-metadata ()
  "If the ORG_GLANCE_ID is edited, saving does not corrupt the metadata index
for the original id (the sync hook skips a mismatched id)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "m1" "* TODO foo"))
    (let ((buffer (org-glance-material:open graph "m1")))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward "^:ORG_GLANCE_ID:.*$")
            (replace-match ":ORG_GLANCE_ID: changed")
            (let ((inhibit-message t)) (org-glance-material:apply)))
        (kill-buffer buffer)))
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
    (let ((buffer (org-glance-material:open graph "R")))
      (unwind-protect
          (with-current-buffer buffer
            (should org-glance-datetime-mode))
        (kill-buffer buffer)))))

(ert-deftest org-glance-test:material-clone-on-repeat ()
  "Completing a repeated headline preserves the done state as a new headline."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "R" "* TODO water flowers :house:"
                                                             "SCHEDULED: <2026-06-07 Sun +1d>"))
    (let ((buffer (org-glance-material:open graph "R"))
          (org-glance-clone-on-repeat-p t)
          (org-log-repeat nil)
          (org-log-done nil))
      (unwind-protect
          (with-current-buffer buffer
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
              (should (string= "TODO" (org-get-todo-state)))))
        (kill-buffer buffer)))))

(ert-deftest org-glance-test:material-cleanup-after-repeat ()
  "After repeating, the live headline is trimmed to header + pinned blocks."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "R" "* TODO routine"
                                                             "SCHEDULED: <2026-06-07 Sun +1d>"
                                                             "#+begin_pin"
                                                             "keep me"
                                                             "#+end_pin"
                                                             "transient note"))
    (let ((buffer (org-glance-material:open graph "R"))
          (org-glance-clone-on-repeat-p t)
          (org-log-repeat nil)
          (org-log-done nil))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (org-todo "DONE")
            (should (s-contains? "keep me" (buffer-string)))
            (should-not (s-contains? "transient note" (buffer-string)))
            (should (s-contains? ":ORG_GLANCE_ID: R" (buffer-string))))
        (kill-buffer buffer)))))

(ert-deftest org-glance-test:material-no-clone-when-disabled ()
  "Without `org-glance-clone-on-repeat-p', repeating leaves the graph alone."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "R" "* TODO water"
                                                             "SCHEDULED: <2026-06-07 Sun +1d>"
                                                             "transient note"))
    (let ((buffer (org-glance-material:open graph "R"))
          (org-glance-clone-on-repeat-p nil)
          (org-log-repeat nil)
          (org-log-done nil))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (org-todo "DONE")
            (should (= 1 (length (org-glance-graph:headlines graph))))
            (should (s-contains? "transient note" (buffer-string))))
        (kill-buffer buffer)))))

(cl-defmacro org-glance-test:--seen-ids (&rest body)
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
        (org-glance-test:--seen-ids
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
        (org-glance-test:--seen-ids
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
        (org-glance-test:--seen-ids
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
        (org-glance-test:--seen-ids
          ;; active + propertized -> "tk" (DONE filtered; "tn" lacks pairs)
          (let ((org-glance-filter-spec '(:done nil)))
            (org-glance-extract) (should (equal '("tk") seen)))
          ;; done + propertized -> "dk"
          (let ((org-glance-filter-spec '(:done t)))
            (org-glance-extract) (should (equal '("dk") seen)))
          ;; all + propertized -> "tk","dk" (still not "tn")
          (let ((org-glance-filter-spec nil))
            (org-glance-extract) (should (equal '("dk" "tk") seen))))))))

(provide 'test-material)
;;; test-material.el ends here
