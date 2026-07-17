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
    (should (not (org-glance-headline:encrypted? headline)))))

(ert-deftest org-glance-test:headline-properties ()
  (let ((headline (org-glance-headline--from-lines "* TODO Hello, world!" "- foo: bar")))
    (should (eq 1 (length (org-glance-headline:properties headline))))
    (should (string= "bar" (org-glance-headline:get-user-property "foo" headline)))))

(ert-deftest org-glance-test:headline-node-properties ()
  "`node-property' reads the `:PROPERTIES:' drawer (case-insensitively); a body
`KEY: value' pair is NOT a node property (that is `get-user-property')."
  (let ((headline (org-glance-headline--from-lines
                    "* TODO Book :read:"
                    ":PROPERTIES:"
                    ":ORG_GLANCE_ID: book-1"
                    ":TODO_KEYWORDS: TODO READING | READ"
                    ":END:"
                    "- author: Tolkien")))
    ;; drawer properties, matched case-insensitively
    (should (string= "book-1" (org-glance-headline:node-property "ORG_GLANCE_ID" headline)))
    (should (string= "TODO READING | READ" (org-glance-headline:node-property "todo_keywords" headline)))
    ;; absent -> nil
    (should (null (org-glance-headline:node-property "NOPE" headline)))
    ;; a body `KEY: value' is NOT a drawer/node property ...
    (should (null (org-glance-headline:node-property "author" headline)))
    ;; ... it is a user (body) property instead
    (should (string= "Tolkien" (org-glance-headline:get-user-property "author" headline)))
    ;; the drawer alist carries the keys, uppercased
    (should (assoc "TODO_KEYWORDS" (org-glance-headline:node-properties headline)))))

(ert-deftest org-glance-test:headline-links ()
  (let ((headline (org-glance-headline--from-lines "* TODO Hello, world!" "[[https:duckduckgo.com][ddg]]")))
    (should (eq 1 (length (org-glance-headline:links headline))))))

(ert-deftest org-glance-test:headline-encryption ()
  "Encrypt wraps the body in one sealed crypt block; decrypt keeps the markers
\(plaintext body), decrypt+unwrap restores the original bytes."
  (let* ((orig (org-glance-headline--from-lines "* TODO Hello, world!" "foo bar"))
         (password "password")
         (encrypted (org-glance-headline:encrypt orig password))
         (decrypted (org-glance-headline:decrypt encrypted password))
         (public (org-glance-headline:decrypt encrypted password t)))
    (should (not (org-glance-headline:encrypted? orig)))
    (should (org-glance-headline:encrypted? encrypted))
    (should (s-contains? "#+begin_crypt" (org-glance-headline:contents encrypted)))
    (should (not (s-contains? "foo bar" (org-glance-headline:contents encrypted))))
    ;; decrypt: blocks stay (rekey path), body plaintext again
    (should (not (org-glance-headline:encrypted? decrypted)))
    (should (s-contains? "#+begin_crypt" (org-glance-headline:contents decrypted)))
    (should (s-contains? "foo bar" (org-glance-headline:contents decrypted)))
    ;; decrypt + unwrap: byte-identical to the original
    (should (string= (org-glance-headline:contents public)
                     (org-glance-headline:contents orig)))))

(ert-deftest org-glance-test:headline-crypt-blocks-mixed ()
  "Several crypt blocks seal independently; plaintext between them stays public,
so an encrypted headline keeps honest `linked?' metadata.  Rekey (decrypt ->
encrypt) preserves the block structure; decrypt+unwrap restores the original."
  (let* ((orig (org-glance-headline--from-lines
                "* TODO Mixed"
                "public intro [[https://example.com][site]]"
                "#+begin_crypt" "secret one" "#+end_crypt"
                "public middle"
                "#+begin_crypt" "secret two" "#+end_crypt"))
         (enc (org-glance-headline:encrypt orig "pw"))
         (fresh (org-glance-headline--from-string (org-glance-headline:contents enc)))
         (meta (org-glance-headline:metadata fresh))
         (cipher (org-glance-headline:contents fresh)))
    ;; only the blocks sealed; public parts intact
    (should (s-contains? "example.com" cipher))
    (should (s-contains? "public middle" cipher))
    (should-not (s-contains? "secret one" cipher))
    (should-not (s-contains? "secret two" cipher))
    (should (= 2 (s-count-matches "#\\+begin_crypt" cipher)))
    ;; the metadata sees both facts at once -- the point of the feature
    (should (org-glance-headline-metadata:encrypted? meta))
    (should (org-glance-headline-metadata:linked? meta))
    ;; rekey preserves both blocks; the new password opens them
    (let* ((rekeyed (org-glance-headline:encrypt
                     (org-glance-headline:decrypt fresh "pw") "new"))
           (opened (org-glance-headline:contents
                    (org-glance-headline:decrypt rekeyed "new"))))
      (should (= 2 (s-count-matches "#\\+begin_crypt"
                                    (org-glance-headline:contents rekeyed))))
      (should (s-contains? "secret one" opened))
      (should (s-contains? "secret two" opened)))
    ;; decrypt + unwrap: fully public, no markers left
    (let ((public (org-glance-headline:contents
                   (org-glance-headline:decrypt fresh "pw" t))))
      (should (s-contains? "secret one" public))
      (should (s-contains? "secret two" public))
      (should-not (s-contains? "#+begin_crypt" public)))))

(ert-deftest org-glance-test:headline-crypt-legacy-layout ()
  "The pre-block whole-body cipher still detects as encrypted and decrypts."
  (let* ((orig (org-glance-headline--from-lines "* TODO Old" "old secret"))
         (legacy (org-glance-test:legacy-encrypt orig "pw")))
    (should (org-glance-headline:encrypted? legacy))
    (should-not (s-contains? "old secret" (org-glance-headline:contents legacy)))
    (should-not (s-contains? "#+begin_crypt" (org-glance-headline:contents legacy)))
    (should (s-contains? "old secret"
                         (org-glance-headline:contents
                          (org-glance-headline:decrypt legacy "pw"))))))

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

(ert-deftest org-glance-test:headline-planning ()
  (pcase-dolist (`(,accessor ,keyword)
                 '((org-glance-headline:schedule "SCHEDULED")
                   (org-glance-headline:deadline "DEADLINE")))
    (let ((h (org-glance-headline--from-lines
              "* foo" (format "%s: <2025-01-10 Fri>" keyword))))
      (should (string= (funcall accessor h) "<2025-01-10 Fri>")))))

(ert-deftest org-glance-test:headline-content-facts-matches-thunks ()
  "The metadata build's single-pass `--content-facts' is byte-identical to forcing
the -hash/-links/-properties/-encrypted thunks separately, across headline shapes."
  (dolist (lines '(("* TODO Plain" ":PROPERTIES:" ":ORG_GLANCE_ID: a" ":END:")
                   ("* TODO Linked [[https://x][d]]" ":PROPERTIES:" ":ORG_GLANCE_ID: b" ":END:"
                    "See [[file:y.org][y]] and [[id:z][z]].")
                   ("* TODO Propd" ":PROPERTIES:" ":ORG_GLANCE_ID: c" ":END:"
                    "author: Tolkien" "pages: 300")
                   ("* TODO Enc" ":PROPERTIES:" ":ORG_GLANCE_ID: e" ":END:"
                    "aes-encrypted V 1.3-OCB-B-4-4-Mxxxx" "morebody")
                   ("* TODO Both [[https://q][q]]" ":PROPERTIES:" ":ORG_GLANCE_ID: d" ":AUTHOR: X" ":END:"
                    "key: val")))
    (let* ((h (apply #'org-glance-headline--from-lines lines))
           (facts (org-glance-headline--content-facts h)))
      (should (equal (plist-get facts :hash)        (org-glance-headline:hash h)))
      (should (eq    (plist-get facts :linked)      (and (org-glance-headline:links h) t)))
      (should (eq    (plist-get facts :propertized) (and (org-glance-headline:properties h) t)))
      (should (eq    (plist-get facts :encrypted)   (and (org-glance-headline:encrypted? h) t))))))

(provide 'test-headline)
;;; test-headline.el ends here
