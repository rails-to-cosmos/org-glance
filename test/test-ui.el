;;; test-ui.el --- Tests for the org-glance transient UI  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:ui-filter-by-state ()
  "The transient state suffix sets the ambient filter's todo-state dimension."
  (let ((org-glance-filter-spec nil))
    (cl-letf (((symbol-function 'org-glance-filter:read-state) (lambda (&rest _) 'active)))
      (org-glance-form-action:filter-by-state)
      (should (equal '(:done nil) org-glance-filter-spec)))
    (cl-letf (((symbol-function 'org-glance-filter:read-state) (lambda (&rest _) "TODO")))
      (org-glance-form-action:filter-by-state)
      (should (equal '(:state "TODO") org-glance-filter-spec)))
    (cl-letf (((symbol-function 'org-glance-filter:read-state) (lambda (&rest _) 'all)))
      (org-glance-form-action:filter-by-state)
      (should (null org-glance-filter-spec)))))

(ert-deftest org-glance-test:ui-filter-by-substring ()
  "The transient substring suffix sets, then clears, the ambient substring."
  (let ((org-glance-filter-spec nil))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "foo")))
      (org-glance-form-action:filter-by-substring)
      (should (equal '(:title-contains "foo") org-glance-filter-spec)))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
      (org-glance-form-action:filter-by-substring)
      (should (null org-glance-filter-spec)))))

(ert-deftest org-glance-test:ui-filter-clear ()
  "The transient clear suffix drops the ambient filter entirely."
  (let ((org-glance-filter-spec '(:done nil)))
    (org-glance-form-action:filter-clear)
    (should (null org-glance-filter-spec))))

(provide 'test-ui)
;;; test-ui.el ends here
