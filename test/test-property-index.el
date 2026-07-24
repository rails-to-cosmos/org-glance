;;; test-property-index.el --- Tests for the derived property index  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:property-index-keys-and-values ()
  "The index yields per-headline drawer + body properties and the key union,
dropping org-glance's own ORG_GLANCE_* bookkeeping from the candidate keys."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline-props "a" "* TODO A" '(("AUTHOR" . "Tolkien") ("GENRE" . "fantasy")))
      (org-glance-test:headline-props "b" "* TODO B" '(("AUTHOR" . "Le Guin")) "- rating: 5"))
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
      (org-glance-test:headline-props "a" "* TODO A" '(("AUTHOR" . "Old"))))
    (should (equal "Old" (org-glance-property-index:property graph "a" "AUTHOR")))
    (org-glance-graph:add graph            ; new content -> new hash
      (org-glance-test:headline-props "a" "* TODO A" '(("AUTHOR" . "New"))))
    (should (equal "New" (org-glance-property-index:property graph "a" "AUTHOR")))))

(ert-deftest org-glance-test:property-index-persists-and-clears ()
  "`ensure' persists to disk and survives dropping the in-session memo; `clear'
drops both."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline-props "a" "* TODO A" '(("AUTHOR" . "X"))))
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
      (org-glance-test:headline-props "a" "* TODO A" '(("AUTHOR" . "Tolkien"))))
    (org-glance-test:with-table (graph)
        (org-glance-test:offering (offered (caar offered))
          (let ((col (org-glance-table--add-column-prompt))
                (names (mapcar #'car offered)))             ; display strings
            (should (member "AUTHOR" names))                ; discovered from the headline
            (should-not (member "ORG_GLANCE_ID" names))
            (should (equal "AUTHOR" (alist-get 'key col)))
            (should (equal "Tolkien" (funcall (alist-get 'value-fn col) "a" nil))))))))

(ert-deftest org-glance-test:table-extract-via-index ()
  "The table `e' action extracts a body KEY:value read from the property index."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* TODO A" "- author: Tolkien"))
    (let (killed)
      (org-glance-test:offering (offered (caar offered))
        (cl-letf (((symbol-function 'kill-new) (lambda (v &rest _) (setq killed v))))
          (org-glance-table--act-extract graph "a")))
      (should (equal "Tolkien" killed)))))

(ert-deftest org-glance-test:cache-dir-split ()
  "Derived sidecars live under `cache/' (git-ignored, pruned of legacy);
user config stays under `config/'."
  (org-glance-test:with-graph graph
    (should (s-contains? "/cache/" (org-glance-graph:cache-file graph "x.eld")))
    (should (s-contains? "/config/" (org-glance-graph:config-file graph "x.eld")))
    (should (s-contains? "/cache/" (org-glance-property-index--file graph)))
    ;; the store git-ignores cache/ (written once at open, if absent)
    (should (f-exists? (f-join (org-glance-graph:store-path graph) ".gitignore")))
    ;; each module prunes ITS pre-split legacy file at graph open
    (let ((legacy (org-glance-graph:config-file graph "property-index.eld")))
      (org-glance-test:write legacy "()")
      (org-glance-test:reopen graph)
      (should-not (f-exists? legacy)))
    ;; reindex drops the whole derived cache/ dir (llm cache included)
    (f-mkdir-full-path (org-glance-graph:cache-path graph))
    (f-write-text "nil" 'utf-8 (org-glance-graph:cache-file graph "llm-sessions.eld"))
    (org-glance-reindex (org-glance-graph:directory graph))
    (should-not (f-exists? (org-glance-graph:cache-path graph)))))

(provide 'test-property-index)
;;; test-property-index.el ends here
