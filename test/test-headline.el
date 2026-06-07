;;; test-headline.el --- Tests for the `org-glance-headline-v2' model  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:headline-parser ()
  (let ((headline (org-glance-headline-v2--from-lines
                    ""
                    ""
                    ""
                    "** [#A] bar :a:B:c:"
                    ":PROPERTIES:"
                    ":ORG_GLANCE_ID: bar"
                    ":END:")))
    (should (equal (org-glance-headline-v2:tags headline) '(a b c)))
    (should (= (org-glance-headline-v2:priority headline) 65))
    (should (string= (org-glance-headline-v2:title headline) "bar"))
    (should (string= (org-glance-headline-v2:state headline) ""))
    (should (string= (org-glance-headline-v2:id headline) "bar"))
    (should (string= (org-glance-headline-v2:tag-string headline) ":a:b:c:"))
    (should (not (org-glance-headline-v2:encrypted? headline)))))

(ert-deftest org-glance-test:headline-active ()
  (let ((org-done-keywords (list "DONE")))
    (let ((headline (org-glance-headline-v2--from-lines "* TODO Hello, world!")))
      (should (string= (org-glance-headline-v2:state headline) "TODO"))
      (should (org-glance-headline-v2:active? headline))
      (should (not (org-glance-headline-v2:done? headline))))))

(ert-deftest org-glance-test:headline-properties ()
  (let ((headline (org-glance-headline-v2--from-lines "* TODO Hello, world!" "- foo: bar")))
    (should (eq 1 (length (org-glance-headline-v2:properties headline))))
    (should (string= "bar" (org-glance-headline-v2:get-user-property "foo" headline)))))

(ert-deftest org-glance-test:headline-links ()
  (let ((headline (org-glance-headline-v2--from-lines "* TODO Hello, world!" "[[https:duckduckgo.com][ddg]]")))
    (should (eq 1 (length (org-glance-headline-v2:links headline))))))

(ert-deftest org-glance-test:headline-encryption ()
  (let* ((orig (org-glance-headline-v2--from-lines "* TODO Hello, world!" "foo bar"))
         (password "password")
         (encrypted (org-glance-headline-v2:encrypt orig password))
         (decrypted (org-glance-headline-v2:decrypt encrypted password)))
    (should (not (org-glance-headline-v2:encrypted? orig)))
    (should (org-glance-headline-v2:encrypted? encrypted))
    (should (not (org-glance-headline-v2:encrypted? decrypted)))
    (should (not (string= (org-glance-headline-v2:contents orig) (org-glance-headline-v2:contents encrypted))))
    (should (string= (org-glance-headline-v2:contents decrypted) (org-glance-headline-v2:contents orig)))))

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

    (let ((existing-headline (org-glance-headline-v2:search-forward "quux_id")))
      (should (string= (org-glance-headline-v2:id existing-headline) "quux_id")))

    (let ((non-existing-headline (org-glance-headline-v2:search-forward "bar")))
      (should (eq non-existing-headline nil)))))

(ert-deftest org-glance-test:headline-copy ()
  (let* ((orig (org-glance-headline-v2--from-string "* TODO foo"))
         (copy (org-glance-headline-v2--copy orig :state "DONE")))
    (should (string= (org-glance-headline-v2:state orig) "TODO"))
    (should (string= (org-glance-headline-v2:state copy) "DONE"))))

(ert-deftest org-glance-test:headline-indent-hash ()
  "Headline hash should change after indentation."
  (let* ((orig (org-glance-headline-v2--from-string "*** foo"))
         (copy (org-glance-headline-v2:reset-indent orig)))
    (should (not (string= (org-glance-headline-v2:hash orig) (org-glance-headline-v2:hash copy))))))

(ert-deftest org-glance-test:headline-title ()
  (let ((headline (org-glance-headline-v2--from-string "* foo [[https:google.com]]")))
    (should (string= (org-glance-headline-v2:title-clean headline) "foo https:google.com"))))

(ert-deftest org-glance-test:headline-log ()
  (let ((contents (-> (org-glance-headline-v2--from-string "* foo")
                      (org-glance-headline-v2:add-note "Log note")
                      (org-glance-headline-v2:contents))))
    (should (s-join "\n" '("* foo" ":LOGBOOK:" "- Log note" ":END:")))))

(ert-deftest org-glance-test:headline-timestamps ()
  (let ((timestamps (-> (org-glance-headline-v2--from-lines "* foo"
                                                          "<2025-01-01 Wed>"
                                                          "[2025-01-01 Wed]")
                        (org-glance-headline-v2:timestamps-raw))))
    (should (= (length timestamps) 2))
    (should (member "<2025-01-01 Wed>" timestamps))))

(ert-deftest org-glance-test:headline-clocks ()
  (let ((clocks (-> (org-glance-headline-v2--from-lines "* foo"
                                                      ":LOGBOOK:"
                                                      "- State \"STARTED\"    from \"PENDING\"    [2025-01-10 Fri 14:43]"
                                                      "CLOCK: [2025-01-10 Fri 14:43]"
                                                      "- State \"PENDING\"    from \"STARTED\"    [2025-01-10 Fri 14:43]"
                                                      "- State \"STARTED\"    from \"TODO\"       [2025-01-10 Fri 14:15]"
                                                      "CLOCK: [2025-01-10 Fri 14:15]--[2025-01-10 Fri 14:43] =>  0:28"
                                                      ":END:")
                    (org-glance-headline-v2:clocks))))
    (should (= (length clocks) 2))))

(ert-deftest org-glance-test:headline-schedule ()
  (let ((schedule (-> (org-glance-headline-v2--from-lines "* foo" "SCHEDULED: <2025-01-10 Fri>")
                      (org-glance-headline-v2:schedule))))
    (should (string= schedule "<2025-01-10 Fri>"))))

(ert-deftest org-glance-test:headline-deadline ()
  (let ((deadline (-> (org-glance-headline-v2--from-lines "* foo" "DEADLINE: <2025-01-10 Fri>")
                      (org-glance-headline-v2:deadline))))
    (should (string= deadline "<2025-01-10 Fri>"))))

(ert-deftest org-glance-test:headline-hash-consistency ()
  (let* ((headline (org-glance-headline-v2--from-string "* foo"))
         (overview (org-glance-headline-v2:overview headline)))
    (should (string= (org-glance-headline-v2:hash headline)
                     (org-glance-headline-v2:hash (org-glance-headline-v2--from-string overview))))))

(provide 'test-headline)
;;; test-headline.el ends here
