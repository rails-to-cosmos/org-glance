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

(ert-deftest org-glance-test:ui-toggle-view ()
  "The transient view toggle round-trips `org-glance-overview-default-view'.
It flips between table and org-text view and maps a legacy value to canonical."
  (let ((org-glance-overview-default-view 'org-glance-table))
    (should (equal "table" (org-glance-transient--view-mode)))
    (org-glance-transient:toggle-view)
    (should (eq 'org-glance-overview org-glance-overview-default-view))
    (should (equal "org" (org-glance-transient--view-mode)))
    (org-glance-transient:toggle-view)
    (should (eq 'org-glance-table org-glance-overview-default-view)))
  (let ((org-glance-overview-default-view 'org))
    (org-glance-transient:toggle-view)
    (should (eq 'org-glance-table org-glance-overview-default-view))))

(ert-deftest org-glance-test:ui-autoloads-stub-transient-prefixes ()
  "Generated autoloads stub every transient prefix instead of copying it.
A copied `transient-define-prefix' defines the menu without loading its
suffixes, so opening it signals \"Suffix command ... is not defined\"."
  (let* ((src (expand-file-name
               "src" (locate-dominating-file
                      (locate-library "org-glance-ui") "org-glance.el")))
         (dir (make-temp-file "org-glance-autoloads-" t))
         (out (expand-file-name "autoloads.el" dir))
         forms)
    (unwind-protect
        (progn
          (let ((inhibit-message t))
            (loaddefs-generate (list (expand-file-name "data" src)
                                     (expand-file-name "view" src))
                               out))
          (with-temp-buffer
            (insert-file-contents out)
            (condition-case nil
                (while t (push (read (current-buffer)) forms))
              (end-of-file nil))))
      (delete-directory dir t))
    (should-not (seq-find (lambda (form)
                            (string-prefix-p "transient-define-"
                                             (symbol-name (car-safe form))))
                          forms))
    (should (member '(autoload 'org-glance-transient "org-glance-ui" nil t)
                    forms))))

(provide 'test-ui)
;;; test-ui.el ends here
