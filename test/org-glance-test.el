;;; org-glance-test.el --- Tests for org-glance
;;; Commentary:
;;
;;; Code:

(defun org-glance--load-resource (resource-name)
  (f-expand resource-name resource-path))

(ert-deftest org-glance-test--feature-provided ()
  (should (featurep 'org-glance)))

(ert-deftest org-glance-test--should-extract-property ()
  (let* ((res (org-glance--load-resource "simple.org"))
         (choice "Simple headline with properties")
         (key :HELLO)
         (expected-val "WORLD")
         (actual-val (org-glance :scope res
                                 :default-choice choice
                                 :action (lambda (hl) (org-element-property key hl)))))
    (should (string= expected-val actual-val))))

(ert-deftest org-glance-test--should-raise-user-error-on-fnf ()
  (let ((file (f-expand "file-not-found.org" resource-path)))
    (should-not (f-exists? file))
    (should (eq t
                (condition-case nil
                    (progn (org-glance :scope file) nil)
                  (user-error t))))))

;;; org-glance-test.el ends here
