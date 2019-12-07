;;; org-glance-test.el --- Tests for org-glance
;;; Commentary:
;;
;;; Code:

(ert-deftest org-glance-test--feature-provided ()
  (should (featurep 'org-glance)))

(ert-deftest org-glance-test--should-extract-property ()
  (let* ((res (f-expand "template.org" resource-path))
         (user-input "1")
         (key :HELLO)
         (expected-val "WORLD")
         (actual-val (org-glance res
                                 :default-choice user-input
                                 :action (lambda (hl) (org-element-property key hl)))))
    (should (string= expected-val actual-val))))

(ert-deftest org-glance-test--should-raise-user-error-on-fnf ()
  (let* ((file (f-expand "file-not-found.org" resource-path)))
    (should-not (f-exists? file))
    (should (eq t
                (condition-case nil
                    (progn (org-glance file) nil)
                  (user-error t))))))

;;; org-glance-test.el ends here
