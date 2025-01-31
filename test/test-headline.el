;;; test-headline.el --- Tests for `org-glance-headline' model  -*- lexical-binding: t -*-

(require 'test-helpers)

;; (ert-deftest org-glance-test:initial-state ()
;;   (org-glance-test:session
;;     (should (= (length (org-glance:tags)) 0))))

;; (ert-deftest org-glance-test:tag-management ()
;;   (org-glance-test:session
;;     (let ((tag 'foo))
;;       (org-glance:create-tag tag)
;;       (should (org-glance-tag:exists? tag org-glance-tags)))

;;     ;; (org-glance:remove-tag 'foo)
;;     ;; (should-not (org-glance-tag:exists? 'foo org-glance-tags))

;;     (should-error (org-glance:create-tag "bar") :type 'error)
;;     (should-error (org-glance:create-tag 'BAZ) :type 'error)))

(cl-defun org-glance-test:create-tag (tag)
  (cl-check-type tag org-glance-tag)
  (org-glance:create-tag tag)
  (should (org-glance-tag:exists? tag org-glance-tags))
  (should (= 1 (length (org-glance:tags))))
  (should (= 0 (length (org-glance:tag-headlines tag))))
  tag)

(cl-defun org-glance-test:add-headline (tag title &optional contents)
  (cl-check-type tag org-glance-tag)
  (cl-check-type title string)
  (org-glance-capture tag
    :title (if contents
               (s-concat title "\n" contents)
             title)
    :finalize t))

(cl-defun org-glance-test:headline-overview (tag id)
  (cl-check-type tag org-glance-tag)
  (cl-check-type id string)
  (save-window-excursion
    (org-glance-overview tag)
    (org-glance-headline:search-buffer-by-id id)))

(cl-defun org-glance-test:materialize-overview (tag id)
  (cl-check-type tag org-glance-tag)
  (cl-check-type id string)
  (save-window-excursion
    (org-glance-test:headline-overview tag id)
    (org-glance-overview:materialize-headline)
    (org-glance-headline:search-buffer-by-id id)))

(cl-defun org-glance-test:materialize (id)
  (cl-check-type id string)
  (let ((headline (org-glance-metadata:headline id)))
    (org-glance:materialize headline)))

(ert-deftest org-glance-test:consistency ()
  (org-glance-test:session
    (let* (;; TODO generate such entities
           (tag (org-glance-test:create-tag 'foo))
           (title "Hello, world!")
           (contents "Some contents")
           (id (org-glance-test:add-headline tag title contents))
           (metadata (org-glance-metadata:headline-metadata id))
           (overview (org-glance-test:headline-overview tag id))
           (material (org-glance-test:materialize id))
           (material-overview (org-glance-test:materialize-overview tag id)))
      (should (= 1 (length (org-glance:tag-headlines tag))))
      (should (string= (org-glance-headline:title overview) title))
      (should (org-glance-headline:equal? material overview))
      (should (org-glance-headline:equal? overview material-overview))
      (should (org-glance-headline:equal? material material-overview)))))

(ert-deftest org-glance-test:links ()
  (org-glance-test:session
    (let* (;; TODO generate such entities
           (tag (org-glance-test:create-tag 'foo))
           (title "foo")
           (temp-file-name (f-join org-glance-directory "tmp.txt"))
           (contents (format "[[file:%s][tmp.txt]]" temp-file-name))
           (id (org-glance-test:add-headline tag title contents))
           (headline (org-glance-test:materialize id)))
      (should (org-glance-headline:linked? headline))
      (org-glance:open headline)
      ;; Seems org-mode opens file-links asynchronously, so it should be enough to see that user-error has not been raised.
      ;; (should (f-equal? (buffer-file-name) temp-file-name))
      )))

(ert-deftest org-glance-test:properties ()
  (org-glance-test:session
    (let* (;; TODO generate such entities
           (tag (org-glance-test:create-tag 'foo))
           (title "Hello, world!")
           (contents "- foo: bar")
           (id (org-glance-test:add-headline tag title contents))
           (headline (org-glance-test:materialize id)))
      (should (org-glance-headline:propertized? headline))
      (should (string= "bar" (org-glance:extract headline "foo"))))))

;; (ert-deftest org-glance-test:encryption ()
;;   (org-glance-test:session
;;     (let* (;; TODO generate such entities
;;            (tag (org-glance-test:create-tag 'foo))
;;            (title "Hello, world!")
;;            (contents "- foo: bar")
;;            (id (org-glance-test:add-headline tag title contents))
;;            (headline (org-glance-test:materialize id))
;;            (initial-contents (org-glance-headline:contents headline)))
;;       (org-glance:with-headline-materialized headline
;;         (org-glance-headline:encrypt "123"))
;;       (should (string= initial-contents (org-glance-headline:contents headline))))

;;     ;; (should (org-glance-headline:propertized? headline))
;;     ;; (should (string= "bar" (org-glance:extract headline "foo")))
;;     ))

;; new headline model

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
    (should (string= (org-element-property :raw-value schedule) "<2025-01-10 Fri>"))))

(ert-deftest org-glance-test:headline-deadline ()
  (let ((deadline (-> (org-glance-headline-v2--from-lines "* foo" "DEADLINE: <2025-01-10 Fri>")
                      (org-glance-headline-v2:deadline))))
    (should (string= (org-element-property :raw-value deadline) "<2025-01-10 Fri>"))))

(ert-deftest org-glance-test:headline-hash-consistency ()
  (let* ((headline (org-glance-headline-v2--from-string "* foo"))
         (overview (org-glance-headline-v2:overview headline)))
    (should (string= (org-glance-headline-v2:hash headline)
                     (org-glance-headline-v2:hash (org-glance-headline-v2--from-string overview))))))


;; TODO Add tag, add headline, delete tag directory, add another tag, all actions should work fine

;;; test-headline.el ends here
