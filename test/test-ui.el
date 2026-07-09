;;; test-ui.el --- Tests for the org-glance transient UI  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:ui-filter-by-state ()
  "The transient state suffix sets the ambient filter's todo-state dimension."
  (let ((org-glance-filter-spec nil))
    (dolist (c '((active (:done nil)) ("TODO" (:state "TODO")) (all nil)))
      (cl-letf (((symbol-function 'org-glance-filter:read-state)
                 (lambda (&rest _) (car c))))
        (org-glance-transient:filter-by-state)
        (should (equal (cadr c) org-glance-filter-spec))))))

(ert-deftest org-glance-test:ui-filter-by-substring ()
  "The transient substring suffix sets, then clears, the ambient substring."
  (let ((org-glance-filter-spec nil))
    (dolist (c '(("foo" (:title-contains "foo")) ("" nil)))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (car c))))
        (org-glance-transient:filter-by-substring)
        (should (equal (cadr c) org-glance-filter-spec))))))

(ert-deftest org-glance-test:ui-filter-clear ()
  "The transient clear suffix drops the ambient filter entirely."
  (let ((org-glance-filter-spec '(:done nil)))
    (org-glance-transient:filter-clear)
    (should (null org-glance-filter-spec))))

(provide 'test-ui)
;;; test-ui.el ends here
