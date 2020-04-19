;;; org-glance-test.el --- Tests for org-glance

;;; Commentary:
;; This package allows you to manage your org-mode entries as materialized views.

(require 'org-glance)

(ert-deftest org-glance-test--feature-provided ()
  (should (featurep 'org-glance)))

(ert-deftest org-glance-test--should-define-view ()
  "When define view by `org-glance-def-view' it should appear in `org-glance-views' variable."
  (let ((org-glance-views '()))
    (org-glance-def-view "org-glance-test")
    (should (equal org-glance-views '(org-glance-test)))))

;; Local Variables:
;; org-literate-test-selector: "^org-glance-test--*"
;; org-literate-test-buffer: "*org-glance-tests*"
;; End:

;;; org-glance-test.el ends here
