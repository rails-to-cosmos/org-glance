(require 'test-helpers)

(ert-deftest org-glance-test:capture ()
  (org-glance-test:session
    (org-glance-capture 'test "Hello")
    (org-capture-finalize)
    (let ((headlines (org-glance-graph:headlines org-glance-graph)))
      (should (= 1 (length headlines)))
      (let ((meta (car headlines)))
        (should (org-glance-headline-metadata:id meta))
        (should (string= "Hello" (org-glance-headline-metadata:title meta)))
        (should (member "test" (org-glance-headline-metadata:tag-strings meta)))
        (should (s-contains? "Hello" (org-glance-graph:get-content
                                      org-glance-graph
                                      (org-glance-headline-metadata:id meta))))))))

(ert-deftest org-glance-test:kill-buffer-noconfirm ()
  "`org-glance--kill-buffer-noconfirm' clears the modified flag and returns t.
Buffer-local on `kill-buffer-query-functions', it skips the kill confirmation."
  (with-temp-buffer
    (insert "scratch")
    (should (buffer-modified-p))
    (should (eq t (org-glance--kill-buffer-noconfirm)))
    (should-not (buffer-modified-p))))

(ert-deftest org-glance-test:capture-discards-temp-buffer ()
  "Capture finalize discards its temp buffer without a kill confirmation."
  (org-glance-test:session
    (org-glance-capture 'test "Hello")
    (org-capture-finalize)
    (should-not (cl-find-if (lambda (b)
                              (string-match-p "\\`org-glance-.*\\.org\\'"
                                              (buffer-name b)))
                            (buffer-list)))))

(ert-deftest org-glance-test:capture-tag-prompt-from-graph ()
  "The tag prompt offers graph tags, allows new ones, downcases, rejects blank."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "A" "* foo :task:" "")
                             (org-glance-test:headline "B" "* bar :work:" ""))
    (let ((org-glance-graph graph))
      (org-glance-test:offering (seen-candidates "Task")
        (should (eq 'task (org-glance-capture:completing-read-tag)))
        (should (equal '("task" "work") seen-candidates)))
      (org-glance-test:answering ((completing-read "fresh"))
        (should (eq 'fresh (org-glance-capture:completing-read-tag))))
      (org-glance-test:answering ((completing-read "  "))
        (should-error (org-glance-capture:completing-read-tag) :type 'user-error)))))

(ert-deftest org-glance-test:tag-validate-string ()
  "Valid org tags pass trimmed; a dash (or space, dot, empty) errors loudly."
  (should (equal "albertheijn" (org-glance-tag:validate-string " albertheijn ")))
  (should (equal "a_b@c#1%" (org-glance-tag:validate-string "a_b@c#1%")))
  (dolist (bad '("albert-heijn" "a b" "a.b" ""))
    (should-error (org-glance-tag:validate-string bad) :type 'user-error)))

(ert-deftest org-glance-test:tag-invalid-rejected-at-creation ()
  "Capture prompt, programmatic capture and retag add reject an unparsable tag.
Removal stays ungated (invariant 13)."
  (org-glance-test:session
    (org-glance-test:answering ((completing-read "albert-heijn"))
      (should-error (org-glance-capture:completing-read-tag) :type 'user-error))
    (should-error (org-glance-capture '(albert-heijn) "x") :type 'user-error)
    (org-glance-graph:add org-glance-graph
                          (org-glance-test:headline "a" "* TODO A :shop:"))
    (should-error (org-glance-material:retag org-glance-graph "a" "albert-heijn")
                  :type 'user-error)
    (should (equal '("shop")
                   (org-glance-test:field org-glance-graph "a" tag-strings)))
    (should (org-glance-material:retag org-glance-graph "a" "shop" :remove t))))

(ert-deftest org-glance-test:capture-refer-inserts-link ()
  "Capture buffers enable `org-glance-capture-mode'; `C-u @' reads kind first.
Finalize projects it into the captured headline's relations (invariant 5)."
  (org-glance-test:session
    (org-glance-graph:add org-glance-graph
                          (org-glance-test:headline "target" "* TODO Target headline"))
    (org-glance-capture 'test "Note")
    (should org-glance-capture-mode)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "refers ")
    (let (prompts)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt coll &rest _)
                   (push prompt prompts)
                   (if (string-prefix-p "Reference kind" prompt)
                       "depends on"
                     (caar coll)))))
        (org-glance-capture:refer '(4)))
      (should (equal '("Reference kind (empty for none): " "Refer to: ")
                     (nreverse prompts))))
    (should (s-contains? "depends on [[org-glance-material:target?kind=depends-on][Target headline]]"
                         (buffer-string)))
    (org-capture-finalize)
    (let ((captured (cl-find-if
                     (lambda (m) (string= "Note" (org-glance-headline-metadata:title m)))
                     (org-glance-graph:headlines org-glance-graph))))
      (should captured)
      (should (equal '(("target" . "depends-on"))
                     (org-glance-headline-metadata:relations captured))))))

(provide 'test-capture)
;;; test-capture.el ends here
