(require 'test-helpers)

(ert-deftest org-glance-test:capture ()
  (org-glance-test:session
    (org-glance-capture-v2 'test "Hello")
    (org-capture-finalize)
    ;; The headline lands in the v2 graph with an id, title and tag.
    (let ((headlines (org-glance-graph-v2:headlines org-glance-graph-v2)))
      (should (= 1 (length headlines)))
      (let ((meta (car headlines)))
        (should (org-glance-headline-metadata-v2:id meta))
        (should (string= "Hello" (org-glance-headline-metadata-v2:title meta)))
        (should (member "test" (append (org-glance-headline-metadata-v2:tags meta) nil)))
        ;; the captured body was persisted and is retrievable
        (should (s-contains? "Hello" (org-glance-graph-v2:get-content
                                      org-glance-graph-v2
                                      (org-glance-headline-metadata-v2:id meta))))))))

(ert-deftest org-glance-test:capture-tag-prompt-from-graph ()
  "The interactive tag prompt sources candidates from the graph, allows new
tags, normalizes case, and rejects empty input."
  (org-glance-test:with-graph graph
    (org-glance-graph-v2:add graph
                             (org-glance-test:headline "A" "* foo :task:" "")
                             (org-glance-test:headline "B" "* bar :work:" ""))
    (let ((org-glance-graph-v2 graph) (seen-candidates nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_p coll &rest _) (setq seen-candidates coll) "Task")))
        (should (eq 'task (org-glance-capture-v2:completing-read-tag))))
      (should (equal '("task" "work") seen-candidates))
      ;; A tag unknown to the graph is fine -- discovery is capture-driven.
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "fresh")))
        (should (eq 'fresh (org-glance-capture-v2:completing-read-tag))))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "  ")))
        (should-error (org-glance-capture-v2:completing-read-tag) :type 'user-error)))))

(provide 'test-capture)
;;; test-capture.el ends here
