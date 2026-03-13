;;; test-utils.el --- Tests for `org-glance-utils'  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:encode-decode-roundtrip ()
  (let* ((original "Hello, world! Привет мир! 日本語")
         (encoded (org-glance--encode-string original))
         (decoded (org-glance--decode-string encoded)))
    (should (string= original decoded))
    (should-not (string= original encoded))))

(ert-deftest org-glance-test:encrypt-decrypt-roundtrip ()
  (let ((password "test-password")
        (original-text "Secret content here"))
    (with-temp-buffer
      (insert original-text)
      (org-glance--encrypt-region (point-min) (point-max) password)
      (should-not (string= original-text (buffer-substring-no-properties (point-min) (point-max))))
      (org-glance--decrypt-region (point-min) (point-max) password)
      (should (string= original-text (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest org-glance-test:parse-links ()
  (with-temp-buffer
    (org-mode)
    (insert "* Test\n")
    (insert "[[https://example.com][Example]]\n")
    (insert "[[https://test.org][Test Site]]\n")
    (let ((links (org-glance--parse-links)))
      (should (= (length links) 2)))))

(ert-deftest org-glance-test:key-value-pairs ()
  (with-temp-buffer
    (insert "- foo: bar\n")
    (insert "- baz: qux\n")
    (insert "not a pair\n")
    (let ((pairs (org-glance--buffer-key-value-pairs)))
      (should (= (length pairs) 2))
      (should (string= "bar" (alist-get "foo" pairs nil nil #'string=)))
      (should (string= "qux" (alist-get "baz" pairs nil nil #'string=))))))

(ert-deftest org-glance-test:valid-directory ()
  (with-temp-directory dir
    (should (org-glance--valid-directory? dir))
    (should-not (org-glance--valid-directory? "/nonexistent/path"))
    (should-not (org-glance--valid-directory? nil))))

(provide 'test-utils)
;;; test-utils.el ends here
