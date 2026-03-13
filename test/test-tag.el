;;; test-tag.el --- Tests for `org-glance-tag'  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:tag-validation ()
  (should (org-glance-tag? 'foo))
  (should (org-glance-tag? 'bar))
  (should-not (org-glance-tag? 'FOO))
  (should-not (org-glance-tag? 'Foo))
  (should-not (org-glance-tag? nil))
  (should-not (org-glance-tag? "foo")))

(ert-deftest org-glance-test:tag-string-conversion ()
  (should (string= (org-glance-tag:to-string 'foo) "foo"))
  (should (eq (org-glance-tag:from-string "FOO") 'foo))
  (should (eq (org-glance-tag:from-string "bar") 'bar)))

(ert-deftest org-glance-test:tag-registration ()
  (with-temp-directory dir
    (let ((tags (make-hash-table)))
      (org-glance-tag:register 'test tags :namespace dir)
      (should (org-glance-tag:exists? 'test tags))
      (org-glance-tag:remove 'test tags)
      (should-not (org-glance-tag:exists? 'test tags)))))

(ert-deftest org-glance-test:tag-read ()
  (should (eq (org-glance-tag:read "FOO") 'foo))
  (should (eq (org-glance-tag:read 'FOO) 'foo))
  (should (eq (org-glance-tag:read "bar") 'bar)))

(provide 'test-tag)
;;; test-tag.el ends here
