;; (org-glance-init-v2)

;; (org-glance-capture-v2 'test "Hello")
;; (org-glance-capture-v2 "test" "Hello")

(require 'test-helpers)

(ert-deftest org-glance-test:capture ()
  (org-glance-test:session
    (org-glance-capture-v2 'test "Hello")
    (org-capture-finalize)
    ;; Non-interactive capture does not touch the v1 tag registry.
    (should (= 0 (length (org-glance:tags))))
    ;; ...but the headline lands in the v2 graph with an id, title and tag.
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

(provide 'test-capture)
;;; test-capture.el ends here
