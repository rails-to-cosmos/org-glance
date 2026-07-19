(require 'test-helpers)

(ert-deftest org-glance-test:capture ()
  (org-glance-test:session
    (org-glance-capture 'test "Hello")
    (org-capture-finalize)
    ;; The headline lands in the graph with an id, title and tag.
    (let ((headlines (org-glance-graph:headlines org-glance-graph)))
      (should (= 1 (length headlines)))
      (let ((meta (car headlines)))
        (should (org-glance-headline-metadata:id meta))
        (should (string= "Hello" (org-glance-headline-metadata:title meta)))
        (should (member "test" (org-glance-headline-metadata:tag-strings meta)))
        ;; the captured body was persisted and is retrievable
        (should (s-contains? "Hello" (org-glance-graph:get-content
                                      org-glance-graph
                                      (org-glance-headline-metadata:id meta))))))))

(ert-deftest org-glance-test:kill-buffer-noconfirm ()
  "`org-glance--kill-buffer-noconfirm' clears the modified flag and returns t.
Installed buffer-locally on `kill-buffer-query-functions', it turns the built-in
`Buffer modified; kill anyway?' confirmation into a no-op for a discarded buffer."
  (with-temp-buffer
    (insert "scratch")
    (should (buffer-modified-p))
    (should (eq t (org-glance--kill-buffer-noconfirm)))
    (should-not (buffer-modified-p))))

(ert-deftest org-glance-test:capture-discards-temp-buffer ()
  "Capture finalize discards its temp buffer, leaving none behind.
The `kill-buffer-query-functions' guard clears the modified flag first, so the
discard raises no `Buffer modified; kill anyway?' confirmation."
  (org-glance-test:session
    (org-glance-capture 'test "Hello")
    (org-capture-finalize)
    (should-not (cl-find-if (lambda (b)
                              (string-match-p "\\`org-glance-.*\\.org\\'"
                                              (buffer-name b)))
                            (buffer-list)))))

(ert-deftest org-glance-test:capture-tag-prompt-from-graph ()
  "The interactive tag prompt sources candidates from the graph, allows new
tags, normalizes case, and rejects empty input."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "A" "* foo :task:" "")
                             (org-glance-test:headline "B" "* bar :work:" ""))
    (let ((org-glance-graph graph))
      (org-glance-test:offering (seen-candidates "Task")
        (should (eq 'task (org-glance-capture:completing-read-tag)))
        (should (equal '("task" "work") seen-candidates)))
      ;; A tag unknown to the graph is fine -- discovery is capture-driven.
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
  "Every tag-creation boundary rejects an org-unparsable tag loudly:
the capture prompt, programmatic capture, and the retag add path.
Removal stays ungated."
  (org-glance-test:session
    ;; capture prompt
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "albert-heijn")))
      (should-error (org-glance-capture:completing-read-tag) :type 'user-error))
    ;; programmatic capture
    (should-error (org-glance-capture '(albert-heijn) "x") :type 'user-error)
    ;; retag add path; the graph stays untouched
    (org-glance-graph:add org-glance-graph
                          (org-glance-test:headline "a" "* TODO A :shop:"))
    (should-error (org-glance-material:retag org-glance-graph "a" "albert-heijn")
                  :type 'user-error)
    (should (equal '("shop")
                   (org-glance-headline-metadata:tag-strings
                    (org-glance-graph:get-headline org-glance-graph "a"))))
    ;; removal of a valid tag still works
    (should (org-glance-material:retag org-glance-graph "a" "shop" :remove t))))

(provide 'test-capture)
;;; test-capture.el ends here
