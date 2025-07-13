;; (org-glance-init-v2)

;; (org-glance-capture-v2 'test "Hello")
;; (org-glance-capture-v2 "test" "Hello")

(require 'test-helpers)

(ert-deftest org-glance-test:capture ()
  (org-glance-test:session
    (org-glance-capture-v2 'test "Hello")
    (org-capture-finalize)
    (should (= 0 (length (org-glance:tags))))))

(provide 'test-capture)
;;; test-capture.el ends here
