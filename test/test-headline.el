;;; test-headline.el --- Tests for the `org-glance-headline' model  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:headline-parser ()
  (let ((headline (org-glance-headline--from-lines
                    ""
                    ""
                    ""
                    "** [#A] bar :a:B:c:"
                    ":PROPERTIES:"
                    ":ORG_GLANCE_ID: bar"
                    ":END:")))
    (should (equal (org-glance-headline:tags headline) '(a b c)))
    (should (= (org-glance-headline:priority headline) 65))
    (should (string= (org-glance-headline:title headline) "bar"))
    (should (string= (org-glance-headline:state headline) ""))
    (should (string= (org-glance-headline:id headline) "bar"))
    (should (string= (org-glance-headline:tag-string headline) ":a:b:c:"))
    (should (not (org-glance-headline:encrypted? headline)))))

(ert-deftest org-glance-test:headline-active ()
  (let ((org-done-keywords (list "DONE")))
    (let ((headline (org-glance-headline--from-lines "* TODO Hello, world!")))
      (should (string= (org-glance-headline:state headline) "TODO"))
      (should (org-glance-headline:active? headline))
      (should (not (org-glance-headline:done? headline))))))

(ert-deftest org-glance-test:headline-properties ()
  (let ((headline (org-glance-headline--from-lines "* TODO Hello, world!" "- foo: bar")))
    (should (eq 1 (length (org-glance-headline:properties headline))))
    (should (string= "bar" (org-glance-headline:get-user-property "foo" headline)))))

(ert-deftest org-glance-test:headline-links ()
  (let ((headline (org-glance-headline--from-lines "* TODO Hello, world!" "[[https:duckduckgo.com][ddg]]")))
    (should (eq 1 (length (org-glance-headline:links headline))))))

(ert-deftest org-glance-test:headline-encryption ()
  (let* ((orig (org-glance-headline--from-lines "* TODO Hello, world!" "foo bar"))
         (password "password")
         (encrypted (org-glance-headline:encrypt orig password))
         (decrypted (org-glance-headline:decrypt encrypted password)))
    (should (not (org-glance-headline:encrypted? orig)))
    (should (org-glance-headline:encrypted? encrypted))
    (should (not (org-glance-headline:encrypted? decrypted)))
    (should (not (string= (org-glance-headline:contents orig) (org-glance-headline:contents encrypted))))
    (should (string= (org-glance-headline:contents decrypted) (org-glance-headline:contents orig)))))

(ert-deftest org-glance-test:headline-search ()
  (with-temp-buffer
    (insert "header\n")
    (insert "* foo\n")
    (insert "** bar\n")
    (insert "*** baz\n")
    (insert "** qux\n")
    (insert "*** quux\n")
    (insert ":PROPERTIES:\n")
    (insert ":ORG_GLANCE_ID: quux_id\n")
    (insert ":END:\n")

    (goto-char (point-min))

    (let ((existing-headline (org-glance-headline:search-forward "quux_id")))
      (should (string= (org-glance-headline:id existing-headline) "quux_id")))

    (let ((non-existing-headline (org-glance-headline:search-forward "bar")))
      (should (eq non-existing-headline nil)))))

(ert-deftest org-glance-test:headline-copy ()
  (let* ((orig (org-glance-headline--from-string "* TODO foo"))
         (copy (org-glance-headline--copy orig :state "DONE")))
    (should (string= (org-glance-headline:state orig) "TODO"))
    (should (string= (org-glance-headline:state copy) "DONE"))))

(ert-deftest org-glance-test:headline-indent-hash ()
  "Headline hash should change after indentation."
  (let* ((orig (org-glance-headline--from-string "*** foo"))
         (copy (org-glance-headline:reset-indent orig)))
    (should (not (string= (org-glance-headline:hash orig) (org-glance-headline:hash copy))))))

(ert-deftest org-glance-test:headline-title ()
  (let ((headline (org-glance-headline--from-string "* foo [[https:google.com]]")))
    (should (string= (org-glance-headline:title-clean headline) "foo https:google.com"))))

(ert-deftest org-glance-test:headline-log ()
  (let ((contents (-> (org-glance-headline--from-string "* foo")
                      (org-glance-headline:add-note "Log note")
                      (org-glance-headline:contents))))
    (should (s-join "\n" '("* foo" ":LOGBOOK:" "- Log note" ":END:")))))

(ert-deftest org-glance-test:headline-timestamps ()
  (let ((timestamps (-> (org-glance-headline--from-lines "* foo"
                                                          "<2025-01-01 Wed>"
                                                          "[2025-01-01 Wed]")
                        (org-glance-headline:timestamps-raw))))
    (should (= (length timestamps) 2))
    (should (member "<2025-01-01 Wed>" timestamps))))

(ert-deftest org-glance-test:headline-clocks ()
  (let ((clocks (-> (org-glance-headline--from-lines "* foo"
                                                      ":LOGBOOK:"
                                                      "- State \"STARTED\"    from \"PENDING\"    [2025-01-10 Fri 14:43]"
                                                      "CLOCK: [2025-01-10 Fri 14:43]"
                                                      "- State \"PENDING\"    from \"STARTED\"    [2025-01-10 Fri 14:43]"
                                                      "- State \"STARTED\"    from \"TODO\"       [2025-01-10 Fri 14:15]"
                                                      "CLOCK: [2025-01-10 Fri 14:15]--[2025-01-10 Fri 14:43] =>  0:28"
                                                      ":END:")
                    (org-glance-headline:clocks))))
    (should (= (length clocks) 2))))

(ert-deftest org-glance-test:headline-schedule ()
  (let ((schedule (-> (org-glance-headline--from-lines "* foo" "SCHEDULED: <2025-01-10 Fri>")
                      (org-glance-headline:schedule))))
    (should (string= schedule "<2025-01-10 Fri>"))))

(ert-deftest org-glance-test:headline-deadline ()
  (let ((deadline (-> (org-glance-headline--from-lines "* foo" "DEADLINE: <2025-01-10 Fri>")
                      (org-glance-headline:deadline))))
    (should (string= deadline "<2025-01-10 Fri>"))))

(ert-deftest org-glance-test:headline-hash-consistency ()
  (let* ((headline (org-glance-headline--from-string "* foo"))
         (overview (org-glance-headline:overview headline)))
    (should (string= (org-glance-headline:hash headline)
                     (org-glance-headline:hash (org-glance-headline--from-string overview))))))

(provide 'test-headline)
;;; test-headline.el ends here
