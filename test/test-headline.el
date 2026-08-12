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
    (should (string= "book-1" (org-glance-headline:node-property "ORG_GLANCE_ID" headline)))
    (should (string= "TODO READING | READ" (org-glance-headline:node-property "todo_keywords" headline)))
    (should (null (org-glance-headline:node-property "NOPE" headline)))
    (should (null (org-glance-headline:node-property "author" headline)))
    (should (string= "Tolkien" (org-glance-headline:get-user-property "author" headline)))
    (should (assoc "TODO_KEYWORDS" (org-glance-headline:node-properties headline)))))

(ert-deftest org-glance-test:headline-links ()
  (let ((headline (org-glance-headline--from-lines "* TODO Hello, world!" "[[https:duckduckgo.com][ddg]]")))
    (should (plist-get (org-glance-headline--content-facts headline) :linked))
    (should (eq 1 (org-glance-headline:with-contents headline
                    (length (org-glance--buffer-links)))))))

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
    (should (not (org-glance-headline:encrypted? decrypted)))
    (should (s-contains? "#+begin_crypt" (org-glance-headline:contents decrypted)))
    (should (s-contains? "foo bar" (org-glance-headline:contents decrypted)))
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
    (should (s-contains? "example.com" cipher))
    (should (s-contains? "public middle" cipher))
    (should-not (s-contains? "secret one" cipher))
    (should-not (s-contains? "secret two" cipher))
    (should (= 2 (s-count-matches "#\\+begin_crypt" cipher)))
    (should (org-glance-headline-metadata:encrypted? meta))
    (should (org-glance-headline-metadata:linked? meta))
    (let* ((rekeyed (org-glance-headline:encrypt
                     (org-glance-headline:decrypt fresh "pw") "new"))
           (opened (org-glance-headline:contents
                    (org-glance-headline:decrypt rekeyed "new"))))
      (should (= 2 (s-count-matches "#\\+begin_crypt"
                                    (org-glance-headline:contents rekeyed))))
      (should (s-contains? "secret one" opened))
      (should (s-contains? "secret two" opened)))
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
      (should (eq    (plist-get facts :linked)
                     (and (org-glance-headline:with-contents h (org-glance--buffer-links)) t)))
      (should (eq    (plist-get facts :propertized) (and (org-glance-headline:properties h) t)))
      (should (eq    (plist-get facts :encrypted)   (and (org-glance-headline:encrypted? h) t))))))

(ert-deftest org-glance-test:headline-hash-ignores-logbook ()
  "Clock lines and drawer notes never move the content hash: the LOGBOOK is
bookkeeping nothing derived reads, so hashing it would churn the hash-guarded
property index on every clock-in/out.  Real content still changes it."
  (let* ((bare (org-glance-test:headline "h" "* TODO Task" "body"))
         (logged (org-glance-test:headline
                  "h" "* TODO Task"
                  ":LOGBOOK:"
                  "CLOCK: [2026-07-27 Mon 10:00]--[2026-07-27 Mon 11:00] =>  1:00"
                  ":END:"
                  "body"))
         (more (org-glance-test:headline
                "h" "* TODO Task"
                ":LOGBOOK:"
                "CLOCK: [2026-07-27 Mon 10:00]--[2026-07-27 Mon 11:00] =>  1:00"
                "CLOCK: [2026-07-27 Mon 12:00]--[2026-07-27 Mon 12:30] =>  0:30"
                "- Note taken on [2026-07-27 Mon 12:31] \\\\"
                "  a state note"
                ":END:"
                "body"))
         (edited (org-glance-test:headline "h" "* TODO Task" "body edited")))
    (should (equal (org-glance-headline:hash bare) (org-glance-headline:hash logged)))
    (should (equal (org-glance-headline:hash bare) (org-glance-headline:hash more)))
    (should-not (equal (org-glance-headline:hash bare) (org-glance-headline:hash edited)))
    (let ((org-log-into-drawer "MYLOG"))
      (should (equal (org-glance-headline:hash bare)
                     (org-glance-headline:hash
                      (org-glance-test:headline "h" "* TODO Task"
                                                ":MYLOG:" "- note" ":END:" "body")))))))

(ert-deftest org-glance-test:metadata-field-table-guard-fires ()
  "The load-time guard catches the two linkages the slot-order check misses:
a keyword FROM naming no real content fact (the field would read nil forever)
and a list-valued slot with a non-vector ENCODE (which kills EVERY save, since
`--append\' calls `json-serialize\' outside the error-demoted hook)."
  (let ((slots (cdr (cl-struct-slot-info 'org-glance-headline-metadata))))
    (should (org-glance-headline-metadata--check-fields
             slots org-glance-headline-metadata:fields))
    (should-error
     (org-glance-headline-metadata--check-fields
      slots (cl-loop for (slot json from encode decode) in org-glance-headline-metadata:fields
                     collect (list slot json (if (eq slot 'hash) :no-such-fact from)
                                   encode decode)))
     :type 'error)
    (should-error
     (org-glance-headline-metadata--check-fields
      slots (cl-loop for (slot json from encode decode) in org-glance-headline-metadata:fields
                     collect (list slot json from (if (eq slot 'tags) nil encode) decode)))
     :type 'error)
    (should-error
     (org-glance-headline-metadata--check-fields
      slots (reverse org-glance-headline-metadata:fields))
     :type 'error)))

(ert-deftest org-glance-test:content-fact-keys-match-facts ()
  "`org-glance-headline--content-fact-keys\' is the real vocabulary: the guard
above trusts it, so it must equal what `--content-facts\' actually returns."
  (let* ((facts (org-glance-headline--content-facts
                 (org-glance-test:headline "f" "* TODO F" "body")))
         (keys (cl-loop for (k _v) on facts by #'cddr collect k)))
    (should (equal (sort (copy-sequence keys) #'string<)
                   (sort (copy-sequence org-glance-headline--content-fact-keys)
                         #'string<)))))

(ert-deftest org-glance-test:metadata-build-parses-once ()
  "Building metadata from a parsed headline is ONE org-mode pass.
The parse that produced the headline captures the content facts in its own
buffer; `:metadata' reads that memo instead of standing up a second buffer over
the same string.  The passes are counted."
  (let* ((contents (org-glance-test:org-with-id "* TODO Task :work:" "p1"
                                                "body [[https://example.com][X]]"))
         (passes 0))
    (cl-letf* ((orig (symbol-function 'org-glance--org-mode))
               ((symbol-function 'org-glance--org-mode)
                (lambda (&rest args) (cl-incf passes) (apply orig args))))
      (let* ((headline (org-glance-headline--from-string contents))
             (meta (org-glance-headline:metadata headline)))
        (should (org-glance-headline-metadata? meta))
        (should (= 1 passes))))))

(ert-deftest org-glance-test:content-facts-memo-is-contents-keyed ()
  "The facts memo is held against the exact contents string, so a copy that
rewrites contents recomputes rather than describing the old bytes."
  (let* ((contents (org-glance-test:org-with-id "* TODO Task" "p2" "plain body"))
         (headline (org-glance-headline--from-string contents))
         (memoized (org-glance-headline--content-facts headline))
         (fresh (org-glance-headline--content-facts
                 (org-glance-headline--copy headline :-facts nil))))
    (should (equal memoized fresh))
    (let ((rewritten (org-glance-headline--copy headline
                       :contents (org-glance-test:org-with-id
                                  "* TODO Task" "p2" "a different body"))))
      (should-not (org-glance-headline:-facts rewritten))
      (should-not (equal (plist-get memoized :hash)
                         (plist-get (org-glance-headline--content-facts rewritten) :hash))))))

(ert-deftest org-glance-test:lazy-slots-come-from-the-table ()
  "The parser fills every lazy slot the contents-derived table declares, so a
new one cannot be added to the table and forgotten in the constructor -- the
always-nil-slot class the metadata field guard exists to kill."
  (let ((headline (org-glance-headline--from-string
                   (org-glance-test:org-with-id "* TODO T" "lz" "body"))))
    (pcase-dolist (`(,slot . ,builder) org-glance-headline--contents-derived-slots)
      (when builder
        (should (cl-struct-slot-value 'org-glance-headline slot headline))))
    ;; the nil-builder slot is the parse-time memo, filled separately
    (should (assq '-facts org-glance-headline--contents-derived-slots))))

(provide 'test-headline)
;;; test-headline.el ends here
