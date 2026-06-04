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

(ert-deftest org-glance-test:material-apply-conflict ()
  "A concurrent graph change prompts before overwrite; declining aborts."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "m1" "* TODO foo"))
    (let ((buffer (org-glance-material-v2:open graph "m1")))
      (unwind-protect
          (progn
            ;; concurrent external change -> stored hash diverges from base-hash
            (org-glance-graph-v2:add graph (org-glance-test:headline "m1" "* DONE foo elsewhere"))
            (with-current-buffer buffer
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
                (should-error (org-glance-material-v2:apply) :type 'user-error))
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                (org-glance-material-v2:apply))))
        (kill-buffer buffer))
      ;; after accepting overwrite, the buffer's version (TODO foo) wins
      (should (string= "TODO" (org-glance-headline-metadata-v2:state
                               (org-glance-graph-v2:get-headline graph "m1")))))))

(ert-deftest org-glance-test:material-apply-id-guard ()
  "Apply refuses if the ORG_GLANCE_ID was edited away."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph (org-glance-test:headline "m1" "* TODO foo"))
    (let ((buffer (org-glance-material-v2:open graph "m1")))
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward "^:ORG_GLANCE_ID:.*$")
            (replace-match "")
            (should-error (org-glance-material-v2:apply) :type 'user-error))
        (kill-buffer buffer)))))

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

(ert-deftest org-glance-test:link-materialize-stale-id-errors ()
  "Following a stale org-glance link errors loudly, not popping a picker."
  (let ((org-glance-tags (make-hash-table)))
    (should-error (org-glance-link:materialize "no-such-id") :type 'user-error)
    (should-error (org-glance-link:open "no-such-id") :type 'user-error)))

(provide 'test-material)
;;; test-material.el ends here
