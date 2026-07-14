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
        (should (member "test" (append (org-glance-headline-metadata:tags meta) nil)))
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
    (let ((org-glance-graph graph) (seen-candidates nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p coll &rest _) (setq seen-candidates coll) "Task")))
        (should (eq 'task (org-glance-capture:completing-read-tag))))
      (should (equal '("task" "work") seen-candidates))
      ;; A tag unknown to the graph is fine -- discovery is capture-driven.
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "fresh")))
        (should (eq 'fresh (org-glance-capture:completing-read-tag))))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "  ")))
        (should-error (org-glance-capture:completing-read-tag) :type 'user-error)))))

(provide 'test-capture)
;;; test-capture.el ends here
