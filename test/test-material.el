;;; test-material.el --- Tests for v2 graph-backed materialize/sync  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:material-completing-read ()
  "Selection lists live graph headlines and resolves the chosen metadata."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "a" "* TODO Alpha :x:")
                             (org-glance-test:headline "b" "* DONE Beta :y:"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _) (caar collection))))
      (let ((meta (org-glance-material-v2:completing-read graph)))
        (should (org-glance-headline-metadata-v2? meta))
        (should (string= "Alpha" (org-glance-headline-metadata-v2:title meta)))))))

(ert-deftest org-glance-test:material-active-filter ()
  "The v2 commands filter selection to active headlines (v1 parity).
No `org-done-keywords' binding: `completing-read' resolves the done set itself,
so the filter is correct even from this non-Org command context."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "a" "* TODO Todo")
                             (org-glance-test:headline "b" "* DONE Done"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p coll &rest _)
                 (should (= 1 (length coll)))   ; DONE excluded
                 (caar coll))))
      (let ((meta (org-glance-material-v2:completing-read
                   graph :filter #'org-glance-headline-metadata-v2:active?)))
        (should (string= "a" (org-glance-headline-metadata-v2:id meta)))))))

(ert-deftest org-glance-test:material-done-keywords-custom ()
  "`org-done-keywords' redefines what counts as active for selection."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "a" "* TODO Todo")
                             (org-glance-test:headline "b" "* DONE Done"))
    ;; Default: DONE is done, so the active (selectable) headline is the TODO one.
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p coll &rest _) (should (= 1 (length coll))) (caar coll))))
      (should (string= "a" (org-glance-headline-metadata-v2:id
                            (org-glance-material-v2:completing-read
                             graph :filter #'org-glance-headline-metadata-v2:active?)))))
    ;; Declaring TODO done flips it: now the DONE headline is the active one.
    (let ((org-done-keywords '("TODO")))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p coll &rest _) (should (= 1 (length coll))) (caar coll))))
        (should (string= "b" (org-glance-headline-metadata-v2:id
                              (org-glance-material-v2:completing-read
                               graph :filter #'org-glance-headline-metadata-v2:active?))))))))

(ert-deftest org-glance-test:material-completing-read-filter ()
  "The FILTER predicate narrows the candidate list."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "a" "* TODO Alpha")
                             (org-glance-test:headline "b" "* DONE Beta"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (should (= 1 (length collection)))
                 (caar collection))))
      (let ((meta (org-glance-material-v2:completing-read
                   graph
                   :filter (lambda (m) (string= "TODO" (org-glance-headline-metadata-v2:state m))))))
        (should (string= "a" (org-glance-headline-metadata-v2:id meta)))))))

(ert-deftest org-glance-test:material-open-apply ()
  "Materialize opens the blob editable; apply writes back a new version."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "m1" "* TODO foo"))
    (let ((buffer (org-glance-material-v2:open graph "m1")))
      (unwind-protect
          (with-current-buffer buffer
            (should org-glance-material-v2-mode)
            (should (s-contains? "TODO foo" (buffer-string)))
            (goto-char (point-min))
            (re-search-forward "TODO")
            (replace-match "DONE")
            (org-glance-material-v2:apply))
        (kill-buffer buffer))
      (should (string= "DONE" (org-glance-headline-metadata-v2:state
                               (org-glance-graph-v2:get-headline graph "m1")))))))

(ert-deftest org-glance-test:material-save-affordance ()
  "A materialized buffer visits its content-blob FILE, runs the v2 minor mode,
and is editable; saving persists to the graph and survives re-materialize.
This guards the interactive save path users actually hit."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "s1" "* TODO foo"))
    (let ((buffer (org-glance-material-v2:open graph "s1")))
      (unwind-protect
          (with-current-buffer buffer
            (should org-glance-material-v2-mode)
            (should-not buffer-read-only)
            (should buffer-file-name)            ; a real file -> the standard save works
            (goto-char (point-min))
            (re-search-forward "TODO")
            (replace-match "DONE")
            (org-glance-material-v2:apply))
        (kill-buffer buffer))
      ;; persisted: re-materializing shows the edited state
      (let ((buffer2 (org-glance-material-v2:open graph "s1")))
        (unwind-protect
            (with-current-buffer buffer2
              (should (s-contains? "DONE foo" (buffer-string))))
          (kill-buffer buffer2))))))

(ert-deftest org-glance-test:material-edit-updates-metadata ()
  "Editing the heading and saving updates the stored metadata, not just the blob."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "u1" "* TODO foo"))
    (let ((buffer (org-glance-material-v2:open graph "u1")))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward "TODO foo")
            (replace-match "DONE bar")
            (org-glance-material-v2:apply))
        (kill-buffer buffer)))
    (let ((meta (org-glance-graph-v2:get-headline graph "u1")))
      (should (string= "DONE" (org-glance-headline-metadata-v2:state meta)))
      (should (string= "bar" (org-glance-headline-metadata-v2:title meta))))))

(ert-deftest org-glance-test:material-save-e2e ()
  "End-to-end interactive save: `org-glance:materialize' (flag on, no arg) opens
the buffer; editing and invoking `C-x C-s' (the standard save command) persists
to the graph.  Drives the real command + the real keybinding."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "e2e1" "* TODO foo" "body"))
    (let ((org-glance-use-graph-v2 t)
          (org-glance-graph-v2 graph))
      (cl-letf (((symbol-function 'completing-read) (lambda (_p coll &rest _) (caar coll))))
        (save-window-excursion
          (org-glance:materialize)
          (unwind-protect
              (progn
                (should (string-prefix-p "*org-glance: " (buffer-name)))
                (should org-glance-material-v2-mode)
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
    (should (string= "DONE" (org-glance-headline-metadata-v2:state
                             (org-glance-graph-v2:get-headline graph "e2e1"))))))

(ert-deftest org-glance-test:material-save-via-save-buffer ()
  "`save-buffer' (the standard save mechanism, whatever key invokes it) syncs the
materialized buffer through `write-contents-functions' -- robust to configs that
rebind/shadow C-x C-s, and no \"not visiting a file\" prompt on this non-file buffer."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "sb1" "* TODO foo"))
    (let ((buffer (org-glance-material-v2:open graph "sb1")))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward "TODO")
            (replace-match "DONE")
            (set-buffer-modified-p t)
            (let ((inhibit-message t)) (save-buffer)))
        (set-buffer-modified-p nil)
        (kill-buffer buffer)))
    (should (string= "DONE" (org-glance-headline-metadata-v2:state
                             (org-glance-graph-v2:get-headline graph "sb1"))))))

(ert-deftest org-glance-test:material-id-change-skips-metadata ()
  "If the ORG_GLANCE_ID is edited, saving does not corrupt the metadata index
for the original id (the sync hook skips a mismatched id)."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "m1" "* TODO foo"))
    (let ((buffer (org-glance-material-v2:open graph "m1")))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward "^:ORG_GLANCE_ID:.*$")
            (replace-match ":ORG_GLANCE_ID: changed")
            (let ((inhibit-message t)) (org-glance-material-v2:apply)))
        (kill-buffer buffer)))
    ;; the original id's metadata is left untouched
    (should (string= "foo" (org-glance-headline-metadata-v2:title
                            (org-glance-graph-v2:get-headline graph "m1"))))))

(ert-deftest org-glance-test:material-open-missing ()
  "Materializing an unknown id errors."
  (org-glance-test:with-graph graph
    (should-error (org-glance-material-v2:open graph "nope") :type 'user-error)))

;;; open / extract (read-only commands)

(ert-deftest org-glance-test:open-link-single ()
  "A single non-org-glance link is opened without prompting."
  (let ((headline (org-glance-test:headline "o1" "* foo" "[[https://example.com][ex]]"))
        (called nil))
    (cl-letf (((symbol-function 'org-open-at-point) (lambda (&rest _) (setq called t))))
      (org-glance-material-v2:open-link headline))
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
      (org-glance-material-v2:open-link headline))
    (should (string-match-p "b.example" line))))

(ert-deftest org-glance-test:open-link-none ()
  "A headline with no openable links errors."
  (let ((headline (org-glance-test:headline "o2" "* foo" "no links here")))
    (should-error (org-glance-material-v2:open-link headline) :type 'user-error)))

(ert-deftest org-glance-test:open-link-skips-org-glance ()
  "org-glance-* internal links are not offered as openable."
  (let ((headline (org-glance-test:headline "o4" "* foo" "[[org-glance-overview:task][task]]")))
    (should-error (org-glance-material-v2:open-link headline) :type 'user-error)))

(ert-deftest org-glance-test:extract-v2-helper ()
  "Extracting a known key copies its value to the kill ring."
  (let ((headline (org-glance-test:headline "e1" "* foo" "- key: val")))
    (should (string= "val" (org-glance-material-v2:extract headline "key")))
    (should (string= "val" (current-kill 0)))))

(ert-deftest org-glance-test:extract-v2-none ()
  "Extracting from a headline with no key-value pairs errors."
  (let ((headline (org-glance-test:headline "e2" "* foo" "no pairs here")))
    (should-error (org-glance-material-v2:extract headline) :type 'user-error)))

(ert-deftest org-glance-test:extract-v2-command ()
  "The command selects a headline from the graph then extracts a pair."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "e1" "* foo" "- key: val"))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_p coll &rest _) (if (assoc "key" coll) "key" (caar coll)))))
      (let ((org-glance-graph-v2 graph))
        (should (string= "val" (org-glance-extract-v2)))))))

;;; dispatch: no-arg (interactive) invocation must route to v2, not crash

;; Regression: the menu-invoked `org-glance:materialize' crashed with
;; (wrong-type-argument org-glance-headline nil) because dispatch keyed on
;; `called-interactively-p', which is nil under transient's advice wrapper.
;; Dispatch must key on the absence of a headline arg instead. A plain no-arg
;; funcall reproduces the transient path (caller is `apply', not
;; `funcall-interactively'), so `called-interactively-p' is nil here too.

(ert-deftest org-glance-test:materialize-dispatch-noarg ()
  "No-arg `org-glance:materialize' with the flag on routes to the v2 graph."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "d1" "* TODO foo"))
    (let ((org-glance-use-graph-v2 t)
          (org-glance-graph-v2 graph)
          (opened nil))
      (cl-letf (((symbol-function 'completing-read) (lambda (_p coll &rest _) (caar coll)))
                ((symbol-function 'switch-to-buffer) (lambda (buf &rest _) (setq opened buf) buf)))
        (org-glance:materialize))
      (should (bufferp opened)))))

(ert-deftest org-glance-test:open-dispatch-noarg ()
  "No-arg `org-glance:open' with the flag on routes to the v2 graph."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "d2" "* foo" "[[https://example.com][ex]]"))
    (let ((org-glance-use-graph-v2 t)
          (org-glance-graph-v2 graph)
          (called nil))
      (cl-letf (((symbol-function 'completing-read) (lambda (_p coll &rest _) (caar coll)))
                ((symbol-function 'org-open-at-point) (lambda (&rest _) (setq called t))))
        (org-glance:open))
      (should called))))

(ert-deftest org-glance-test:extract-dispatch-noarg ()
  "No-arg `org-glance:extract' with the flag on routes to the v2 graph."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "d3" "* foo" "- key: val"))
    (let ((org-glance-use-graph-v2 t)
          (org-glance-graph-v2 graph))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p coll &rest _) (if (assoc "key" coll) "key" (caar coll)))))
        (should (string= "val" (org-glance:extract)))))))

(ert-deftest org-glance-test:open-filters-nonlinked ()
  "open-v2 offers only linked headlines."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "L" "* foo" "[[https://x.example][x]]")
                             (org-glance-test:headline "P" "* bar" "no link here"))
    (let ((org-glance-use-graph-v2 t) (org-glance-graph-v2 graph) (called nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p coll &rest _) (should (= 1 (length coll))) (caar coll)))
                ((symbol-function 'org-open-at-point) (lambda (&rest _) (setq called t))))
        (org-glance:open))
      (should called))))

(ert-deftest org-glance-test:extract-filters-nonpropertized ()
  "extract-v2 offers only headlines with key-value pairs."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "K" "* foo" "- k: v")
                             (org-glance-test:headline "N" "* bar" "no pairs"))
    (let ((org-glance-use-graph-v2 t) (org-glance-graph-v2 graph))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p coll &rest _)
                   (if (assoc "k" coll) "k"
                     (progn (should (= 1 (length coll))) (caar coll))))))
        (should (string= "v" (org-glance:extract)))))))

(ert-deftest org-glance-test:link-materialize-stale-id-errors ()
  "Following a stale org-glance link errors loudly, not popping a picker."
  (let ((org-glance-tags (make-hash-table)))
    (should-error (org-glance-link:materialize "no-such-id") :type 'user-error)
    (should-error (org-glance-link:open "no-such-id") :type 'user-error)))

(provide 'test-material)
;;; test-material.el ends here
