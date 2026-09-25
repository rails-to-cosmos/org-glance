;;; test-relations.el --- Tests for @-references and the relations projection  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:relations-edges-from-links ()
  "Only material and legacy visit links yield edges, kinded or not, deduped."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "src" "* TODO Source"
        "[[org-glance-material:t1?kind=author][Ann]]"
        "roasted by [[org-glance-material:t3?kind=roasted-by][Roastery]]"
        "[[org-glance-material:t2][Base]]"
        "[[org-glance-visit:legacy1][Old note]]"
        "[[org-glance-material:t1?kind=author][Ann again]]"   ; duplicate edge
        "[[https://example.com][Web]]"))
    (should (equal '(("t1" . "author") ("t3" . "roasted-by") ("t2" . nil) ("legacy1" . nil))
                   (org-glance-test:field graph "src" relations)))))

(ert-deftest org-glance-test:relations-round-trip-deserialized ()
  "Relations survive a reopen and `:refers-to' matches deserialized structs."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* TODO A :book:"
        "[[org-glance-material:b?kind=editor][B]]")
      (org-glance-test:headline "b" "* TODO B"))
    (let* ((cold (org-glance-test:reopen graph))       ; re-read from disk
           (meta (org-glance-graph:get-headline cold "a")))
      (should (equal '(("b" . "editor"))
                     (org-glance-headline-metadata:relations meta)))
      (should (equal '("a") (org-glance-test:filter-ids cold '(:refers-to "b")))))))

(ert-deftest org-glance-test:relations-absent-field-reads-nil ()
  "A record serialized before the field existed deserializes relations = nil."
  (let ((meta (org-glance-headline-metadata:deserialize
               '(:id "old" :state "TODO" :title "Old" :tags []
                 :hash "h" :schedule nil :deadline nil :priority nil
                 :linked nil :propertized nil :encrypted nil))))
    (should (null (org-glance-headline-metadata:relations meta)))))

(ert-deftest org-glance-test:relations-id-any-filter-and-identity ()
  "`:id-any' selects the listed ids; its identity ignores their order.
Both relation keys are transient (invariant 17)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "x" "* TODO X :book:")
      (org-glance-test:headline "y" "* TODO Y")
      (org-glance-test:headline "z" "* TODO Z"))
    (should (equal '("x" "z") (org-glance-test:filter-ids graph '(:id-any ("x" "z")))))
    (should (equal '("x") (org-glance-test:filter-ids graph '(:id-any ("x" "z") :tags ("book")))))
    (should (equal (org-glance-filter:identity '(:id-any ("b" "a")))
                   (org-glance-filter:identity '(:id-any ("a" "b")))))
    (should (org-glance-filter:transient? '(:refers-to "x")))
    (should (org-glance-filter:transient? '(:id-any ("x"))))
    (should-not (org-glance-filter:transient? '(:tags ("book"))))
    (should (null (org-glance-overview:spec-key '(:refers-to "x"))))))

(ert-deftest org-glance-test:material-refer-inserts-link ()
  "`@' at a body boundary inserts a material link, kinded under a prefix arg.
Self is never a candidate."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      ;; me's own kinded edge seeds the graph's kind vocabulary ("author")
      (org-glance-test:headline "me" "* TODO Me" "[[org-glance-material:other?kind=author][x]]")
      (org-glance-test:headline "other" "* TODO Other headline"))
    (org-glance-test:with-material (buf graph "me")
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (org-glance-test:offering (offered (caar offered))
        (org-glance-material:refer)
        (let ((names (mapcar #'car offered)))
          (should (cl-some (lambda (c) (s-contains? "Other headline" c)) names))
          (should-not (cl-some (lambda (c) (s-contains? "Me" c)) names)))  ; self excluded
        (should (s-contains? "[[org-glance-material:other][Other headline]]"
                             (buffer-string))))
      (insert "\n")
      (let (kind-coll prompts)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (prompt coll &rest _)
                     (push prompt prompts)
                     (if (string-prefix-p "Reference kind" prompt)
                         (progn (setq kind-coll coll) "roasted by")
                       (caar coll)))))
          (org-glance-material:refer '(4)))
        (should (equal '("Reference kind (empty for none): " "Refer to: ")
                       (nreverse prompts)))
        (should (member "author" kind-coll)))    ; seeded above via me's own edge
      (should (s-contains?
               "roasted by [[org-glance-material:other?kind=roasted-by][Other headline]]"
               (buffer-string))))))

(ert-deftest org-glance-test:material-refer-in-title ()
  "`@' after a space in the heading title inserts a material link.
Column-0 self-insert is owned by `material-refer-self-inserts-elsewhere'."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "me" "* TODO Coffee from" "body")
      (org-glance-test:headline "other" "* TODO Roaster"))
    (org-glance-test:with-material (buf graph "me")
      (org-glance-material--goto-first-heading)
      (end-of-line)
      (insert " ")
      (org-glance-test:offering (offered (caar offered))
        (org-glance-material:refer))
      (org-glance-material--goto-first-heading)
      (should (s-contains? "Coffee from [[org-glance-material:other][Roaster]]"
                           (buffer-substring (line-beginning-position)
                                             (line-end-position)))))))

(ert-deftest org-glance-test:material-refer-uses-region-as-link-title ()
  "`@' replaces the active region with a link carrying the region as title."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "me" "* TODO Me" "Read this source today")
      (org-glance-test:headline "other" "* TODO Other headline"))
    (org-glance-test:with-material (buf graph "me")
      (goto-char (point-min))
      (search-forward "source")
      (let ((end (point))
            (transient-mark-mode t))
        (goto-char (- end (length "source")))
        (set-mark end)
        (activate-mark)
        (org-glance-test:offering (offered (caar offered))
          (org-glance-material:refer)))
      (should (s-contains?
               "Read this [[org-glance-material:other][source]] today"
               (buffer-string)))
      (should-not (s-contains? "Other headline" (buffer-string))))))

(ert-deftest org-glance-test:material-refer-duplicate-labels-injective ()
  "Same-titled candidates get a short-id suffix, and picking one targets ITS id."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "me" "* TODO Me" "body")
      (org-glance-test:headline "dup-one-xx" "* TODO Same title")
      (org-glance-test:headline "dup-two-yy" "* TODO Same title"))
    (org-glance-test:with-material (buf graph "me")
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (org-glance-test:offering
          (offered (cl-find "dup-two-" (mapcar #'car offered) :test #'s-contains?))
        (org-glance-material:refer)
        (should (= 2 (cl-count "·" (mapcar #'car offered) :test #'s-contains?)))
        (should (s-contains? "[[org-glance-material:dup-two-yy][Same title]]"
                             (buffer-string)))))))

(ert-deftest org-glance-test:material-refer-self-inserts-elsewhere ()
  "`@' mid-word and on a heading line self-inserts (no prompt)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "me" "* TODO Me" "user")
      (org-glance-test:headline "o" "* TODO O"))
    (org-glance-test:with-material (buf graph "me")
      (org-glance-test:answering ((completing-read (error "must not prompt")))
        (goto-char (point-max))
        (unless (eolp) (end-of-line))
        (insert "user")                     ; ...user|
        (let ((last-command-event ?@)) (org-glance-material:refer))
        (should (s-contains? "user@" (buffer-string)))
        (goto-char (point-min))             ; bol on "* TODO Me"
        (let ((last-command-event ?@)) (org-glance-material:refer))
        (should (s-contains? "@* TODO Me" (buffer-string)))))))

(ert-deftest org-glance-test:link-material-follow ()
  "Following a material link materializes it sans ?kind=; a dangling id errors."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "t" "* TODO Target"))
    (let ((org-glance-graph graph))
      (org-glance-test:with-shown (shown)
        (cl-letf (((symbol-function 'org-glance-ensure-init) #'ignore))
          (org-glance-link:material "t?kind=author")
          (should (buffer-live-p shown))
          (with-current-buffer shown
            (should (equal "t" org-glance-material--id)))
          (should-error (org-glance-link:material "no-such-id") :type 'user-error))))))

(ert-deftest org-glance-test:material-references-commands ()
  "`C-c @' opens ONE relation table merging both directions, anchored here.
The filter is the bare `:id-any'; a headline related to nothing errors."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* TODO A" "[[org-glance-material:b][B]]")
      (org-glance-test:headline "b" "* DONE B")        ; DONE must stay visible
      (org-glance-test:headline "c" "* TODO C"))       ; related to nothing
    (let (calls)
      (cl-letf (((symbol-function 'org-glance-table:visit)
                 (lambda (_g filter &rest args)
                   (push (cons filter (plist-get args :context)) calls)
                   nil)))
        (org-glance-test:with-material (buf graph "a")
          (org-glance-relations:references))
        (org-glance-test:with-material (buf graph "b")
          (org-glance-relations:references)))
      (setq calls (nreverse calls))
      (should (equal '(:id-any ("b")) (car (nth 0 calls))))
      (should (equal '(:anchor "a" :dir relations) (cdr (nth 0 calls))))
      (should (equal '(:id-any ("a")) (car (nth 1 calls))))
      (should (equal '(:anchor "b" :dir relations) (cdr (nth 1 calls)))))
    (org-glance-test:with-material (buf graph "c")
      (cl-letf (((symbol-function 'org-glance-table:visit) #'ignore))
        (should-error (org-glance-relations:references) :type 'user-error)))))

(ert-deftest org-glance-test:relations-table-relation-column ()
  "The `Relation' cell shows `>' from the anchor, `<' to it, both when mutual.
A kind follows its arrow; rows merge both directions."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* TODO A"
                                "roasted by [[org-glance-material:b?kind=roasted-by][B]]"
                                "[[org-glance-material:c][C]]")
      (org-glance-test:headline "b" "* TODO B" "[[org-glance-material:a][A]]")
      (org-glance-test:headline "c" "* TODO C"))
    (should (equal "> roasted by, <" (org-glance-table--relation-cell graph "a" "b")))
    (should (equal ">" (org-glance-table--relation-cell graph "a" "c")))
    (should (equal "<" (org-glance-table--relation-cell graph "c" "a")))
    (should (equal '("b" "c") (sort (copy-sequence (org-glance-table--related-ids graph "a"))
                                    #'string<)))
    (should (equal '("a") (org-glance-table--related-ids graph "b")))
    (should (equal '("a") (org-glance-table--related-ids graph "c")))
    (org-glance-test:with-table (graph '(:id-any ("b")) '(:anchor "a" :dir relations))
      (should (member "relation" (org-glance-test:table-col-keys)))
      (should (equal "> roasted by, <"
                     (alist-get 'relation (alist-get 'cells (car table-view--rows))))))
    (org-glance-test:with-table (graph)
      (should-not (member "relation" (org-glance-test:table-col-keys))))))

(ert-deftest org-glance-test:relations-crypt-sealed-excluded ()
  "Only links outside a sealed crypt block reach `relations' and `links'."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline:encrypt
       (org-glance-test:headline "s" "* TODO Secret"
         "[[org-glance-material:public-ref][Public]]"
         "[[https://public.example][P]]"
         "#+begin_crypt"
         "[[org-glance-material:secret-ref][Secret]]"
         "[[https://secret.example][S]]"
         "#+end_crypt")
       "pw"))
    (should (equal '(("public-ref" . nil))
                   (org-glance-test:field graph "s" relations)))
    (should (equal '("[[https://public.example][P]]")
                   (org-glance-test:field graph "s" links)))))

(ert-deftest org-glance-test:relations-crypt-sync-parses-sealed ()
  "Save-time sync derives relations from SEALED bytes, matching reindex.
A decrypted buffer's crypt-block link stays unindexed (invariant 27)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-headline:encrypt
       (org-glance-test:headline "cs" "* TODO Secret"
         "editme"
         "[[org-glance-material:public-ref][Public]]"
         "#+begin_crypt"
         "[[org-glance-material:secret-ref][Secret]]"
         "#+end_crypt")
       "pw")
      (org-glance-test:headline "public-ref" "* TODO P"))
    (org-glance-test:answering ((read-passwd "pw"))
      (org-glance-test:with-material (buffer graph "cs")
        (org-glance-material:decrypt)
        (should (s-contains? "secret-ref" (buffer-string)))
        (org-glance-test:sed "editme" "edited")
        (org-glance-test:save)))
    (let ((after-sync (org-glance-test:field graph "cs" relations)))
      (should (equal '(("public-ref" . nil)) after-sync))
      (org-glance-graph:reindex graph)
      (should (equal after-sync (org-glance-test:field graph "cs" relations))))))

(ert-deftest org-glance-test:snapshot-on-repeat-skips-encrypted ()
  "An encrypted headline keeps no occurrence history (would store plaintext)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "e" "* TODO E"))
    (org-glance-test:with-material (buf graph "e")
      (let ((org-glance-repeat-history-depth 3)
            (org-done-keywords '("DONE")))
        (setq-local org-glance-material--encrypted t)
        (cl-letf (((symbol-function 'org-get-todo-state) (lambda () "DONE"))
                  ((symbol-function 'org-glance-datetime-active-repeated-timestamps)
                   (lambda (&rest _) (list '(timestamp (:raw-value "<2026-06-07 Sun +1d>"))))))
          (org-glance-material:snapshot-on-repeat)
          (should (null (org-glance-graph:occurrences graph "e")))
          ;; the TRIM is gated too: no snapshot was made (invariant 14).
          (let ((before (buffer-string)))
            (org-glance-material:cleanup-after-repeat)
            (should (equal before (buffer-string))))
          (setq-local org-glance-material--encrypted nil)
          (org-glance-material:snapshot-on-repeat)
          (should (= 1 (length (org-glance-graph:occurrences graph "e")))))))))

;;; Overview links: TAG[?KEY=VALUE&...] -> filter

(ert-deftest org-glance-test:overview-link-path-parse ()
  "The overview link path grammar covers the filter table's value kinds."
  (cl-flet ((id (spec) (org-glance-filter:identity spec)))
    (should (equal (id '(:tags ("book"))) (id (org-glance-filter:from-link-path "BOOK"))))
    (should (null (org-glance-filter:from-link-path "all")))
    (should (null (org-glance-filter:from-link-path "")))
    (should (equal (id '(:tags ("book") :done nil :state "READING"))
                   (id (org-glance-filter:from-link-path "book?done=nil&state=READING"))))
    (should (equal (id '(:tags ("book" "extra")))
                   (id (org-glance-filter:from-link-path "book?tags=extra"))))  ; joins path TAG
    (should (equal (id '(:id-any ("a" "b")))
                   (id (org-glance-filter:from-link-path "?id-any=a,b"))))
    (should (equal (id '(:refers-to "x"))
                   (id (org-glance-filter:from-link-path "all?refers-to=x"))))
    (should (equal (id '(:priority ?A :linked t))
                   (id (org-glance-filter:from-link-path "?priority=A&linked=t"))))
    (should (equal (id '(:done t :done-keywords ("DONE" "GIVEN")))
                   (id (org-glance-filter:from-link-path "?done=t&done-keywords=DONE,GIVEN"))))
    ;; planning keys take the predicate's vocabulary; `t' errors.
    (should (equal (id '(:schedule :present :deadline :absent))
                   (id (org-glance-filter:from-link-path "?schedule=present&deadline=absent"))))
    (should-error (org-glance-filter:from-link-path "?schedule=t"))
    (should-not (equal (id (org-glance-filter:from-link-path "book"))
                       (id (org-glance-filter:from-link-path "book?done=nil"))))
    (should-error (org-glance-filter:from-link-path "book?done=maybe"))
    (should-error (org-glance-filter:from-link-path "book?where=f"))
    (should-error (org-glance-filter:from-link-path "book?nope=1"))
    (should-error (org-glance-filter:from-link-path "book?novalue"))))

(ert-deftest org-glance-test:overview-link-follow ()
  "A `?' path opens exactly its filter; a bare TAG merges the ambient spec.
Both land in the default view."
  (org-glance-test:with-graph graph
    (let ((org-glance-graph graph)
          (org-glance-filter-spec '(:done nil))
          seen)
      (cl-letf (((symbol-function 'org-glance-ensure-init) #'ignore)
                ((symbol-function 'org-glance-overview:visit-default)
                 (lambda (_g filter) (push filter seen) nil)))
        (org-glance-link:overview "book?state=DONE")   ; explicit: ambient must NOT leak in
        (org-glance-link:overview "book")              ; legacy: ambient merges
        (should (equal (org-glance-filter:identity '(:tags ("book") :state "DONE"))
                       (org-glance-filter:identity (cadr seen))))
        (should (equal (org-glance-filter:identity '(:done nil :tags ("book")))
                       (org-glance-filter:identity (car seen))))))))

(ert-deftest org-glance-test:table-edge-kind-column ()
  "`C-c +' offers a relation kind, pretty, as a column the per-tag schema keeps.
Cells join target titles with commas; a gone target shows its id."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "c1" "* TODO Kebena Decaf :coffee:"
        "roasted by [[org-glance-material:r1?kind=roasted-by][Manhattan]]"
        "also [[org-glance-material:gone?kind=roasted-by][Ghost]]")
      (org-glance-test:headline "c2" "* TODO Another Bean :coffee:")
      (org-glance-test:headline "r1" "* Manhattan Coffee Roasters"))
    (org-glance-test:with-table (graph 'coffee)
        (org-glance-test:offering (offered "roasted by")
          (funcall (key-binding (kbd "C-c +")))
          (should (member "roasted by" (mapcar #'car offered))))
        (should (equal "Manhattan Coffee Roasters, gone"
                       (org-glance-test:table-cell "c1" "kind:roasted-by")))
        (should (equal "" (org-glance-test:table-cell "c2" "kind:roasted-by")))
        (should (cl-find "Roasted by"
                         (alist-get 'columns table-view--spec)
                         :key (lambda (c) (alist-get 'header c)) :test #'equal)))
    (org-glance-test:with-table (graph 'coffee)
        (should (equal "Manhattan Coffee Roasters, gone"
                       (org-glance-test:table-cell "c1" "kind:roasted-by"))))))

(ert-deftest org-glance-test:table-custom-column-kind-vs-property ()
  "Case alone types a column: \"AUTHOR\" is a drawer one, \"author\" an edge."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline-props "bk" "* TODO Book" '(("AUTHOR" . "Tolkien"))
        "[[org-glance-material:t1?kind=author][Ann]]")
      (org-glance-test:headline "t1" "* Ann the Author"))
    (let ((prop-col (org-glance-table--custom-column graph "AUTHOR"))
          (edge-col (org-glance-table--custom-column graph "author")))
      (should (equal "Tolkien" (funcall (alist-get 'value-fn prop-col) "bk" nil)))
      (should (equal "Ann the Author" (funcall (alist-get 'value-fn edge-col) "bk" nil)))
      (should (equal "AUTHOR" (alist-get 'key prop-col)))
      (should (equal "kind:author" (alist-get 'key edge-col))))))

(ert-deftest org-glance-test:kind-slug-roundtrip ()
  "Kind slugs: downcase + dashes in the wire, spaces back for humans."
  (should (equal "roasted-by" (org-glance--kind-slug "Roasted By")))
  (should (equal "roasted-by" (org-glance--kind-slug "roasted-by")))   ; idempotent
  (should (equal "roasted-by" (org-glance--kind-slug "  roasted by ")))
  (should (equal "roasted by" (org-glance--kind-pretty "roasted-by")))
  (should (equal '("x" . "roasted-by")
                 (org-glance--link-edge "org-glance-material" "x?kind=roasted by"))))

(provide 'test-relations)
;;; test-relations.el ends here
