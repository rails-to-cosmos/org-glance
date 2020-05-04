;;; org-glance-test.el --- Tests for org-glance

;;; Commentary:
;; This package allows you to manage your org-mode entries as materialized views.

(eval-when-compile
  (require 'org)
  (require 'org-element)
  (require 'org-glance)
  (require 'with-simulated-input))

(ert-deftest org-glance-test--feature-provided ()
  (should (featurep 'org-glance)))

(ert-deftest org-glance-test--view-defined ()
  "When define view by `org-glance-def-view' it should appear in `org-glance-views' variable."
  (let ((org-glance-views '()))
    (org-glance-def-view "org-glance-test")
    (should (member 'org-glance-test org-glance-views))
    (should (member 'org-glance-test (hash-table-keys org-glance-view-scopes)))))

(ert-deftest org-glance-test--mv-sync ()
  "Matviews should materialize."
  (let* ((orig-file (make-temp-file "org-glance-test-"))
         (org-agenda-files (list orig-file))
         (view-name "testview")
         (view-contents "
* first :testview:
** a
** b
** c
* second :testview:
"))
    (with-temp-file orig-file (insert view-contents))
    (org-glance-def-view view-name)
    (with-simulated-input
        '("first RET")
      (org-glance-action-materialize view-name t))))

;; (hash-table-keys org-glance-view-scopes)
;; (gethash 'test-view org-glance-view-scopes)

;; Local Variables:
;; org-literate-test-selector: "^org-glance-test--*"
;; org-literate-test-buffer: "*org-glance-tests*"
;; End:

;;; org-glance-test.el ends here
