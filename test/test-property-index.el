;;; test-property-index.el --- Tests for the derived property index  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:property-index-keys-and-values ()
  "The index yields per-headline drawer + body properties and the key union,
dropping org-glance's own ORG_GLANCE_* bookkeeping from the candidate keys."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline--from-lines "* TODO A" ":PROPERTIES:" ":ORG_GLANCE_ID: a"
                                       ":AUTHOR: Tolkien" ":GENRE: fantasy" ":END:")
      (org-glance-headline--from-lines "* TODO B" ":PROPERTIES:" ":ORG_GLANCE_ID: b"
                                       ":AUTHOR: Le Guin" ":END:"
                                       "- rating: 5"))
    ;; union of drawer keys across both, ORG_GLANCE_ID excluded, sorted
    (should (equal '("AUTHOR" "GENRE") (org-glance-property-index:keys graph '("a" "b"))))
    (should (equal "Tolkien" (org-glance-property-index:property graph "a" "author")))
    (should (equal "Le Guin" (org-glance-property-index:property graph "b" "AUTHOR")))
    (should (null (org-glance-property-index:property graph "b" "GENRE")))
    ;; body KEY: value pairs (what extract reads)
    (should (equal "5" (alist-get "rating" (org-glance-property-index:body graph "b")
                                  nil nil #'string=)))))

(ert-deftest org-glance-test:property-index-hash-invalidation ()
  "A content change refreshes the indexed value (keyed by content hash)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline--from-lines "* TODO A" ":PROPERTIES:" ":ORG_GLANCE_ID: a"
                                       ":AUTHOR: Old" ":END:"))
    (should (equal "Old" (org-glance-property-index:property graph "a" "AUTHOR")))
    (org-glance-graph:add graph            ; new content -> new hash
      (org-glance-headline--from-lines "* TODO A" ":PROPERTIES:" ":ORG_GLANCE_ID: a"
                                       ":AUTHOR: New" ":END:"))
    (should (equal "New" (org-glance-property-index:property graph "a" "AUTHOR")))))

(ert-deftest org-glance-test:property-index-persists-and-clears ()
  "`ensure' persists to disk and survives dropping the in-session memo; `clear'
drops both."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline--from-lines "* TODO A" ":PROPERTIES:" ":ORG_GLANCE_ID: a"
                                       ":AUTHOR: X" ":END:"))
    (org-glance-property-index:ensure graph '("a"))
    (should (f-exists? (org-glance-property-index--file graph)))
    (remhash (org-glance-graph:store-path graph) org-glance-property-index--cache)  ; cold memo
    (should (equal "X" (org-glance-property-index:property graph "a" "AUTHOR")))     ; from disk
    (org-glance-property-index:clear graph)
    (should-not (f-exists? (org-glance-property-index--file graph)))))

(ert-deftest org-glance-test:table-add-column-completing ()
  "`C-u +' completing-reads a drawer property the visible headlines carry; the
column's value-fn reads that property via the index."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline--from-lines "* TODO A" ":PROPERTIES:" ":ORG_GLANCE_ID: a"
                                       ":AUTHOR: Tolkien" ":END:"))
    (org-glance-test:with-table-buffer graph buf
      (with-current-buffer buf
        (let (offered)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_p coll &rest _) (setq offered coll) (car coll))))
            (let ((col (org-glance-table--add-column-prompt)))
              (should (member "AUTHOR" offered))          ; discovered from the headline
              (should-not (member "ORG_GLANCE_ID" offered))
              (should (equal "AUTHOR" (alist-get 'key col)))
              (should (equal "Tolkien" (funcall (alist-get 'value-fn col) "a" nil))))))))))

(ert-deftest org-glance-test:table-extract-via-index ()
  "The table `e' action extracts a body KEY:value read from the property index."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline--from-lines "* TODO A" ":PROPERTIES:" ":ORG_GLANCE_ID: a" ":END:"
                                       "- author: Tolkien"))
    (let (killed)
      (cl-letf (((symbol-function 'completing-read) (lambda (_p coll &rest _) (caar coll)))
                ((symbol-function 'kill-new) (lambda (v &rest _) (setq killed v))))
        (org-glance-table--act-extract graph "a" nil))
      (should (equal "Tolkien" killed)))))

(provide 'test-property-index)
;;; test-property-index.el ends here
