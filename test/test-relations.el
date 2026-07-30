;;; test-relations.el --- Tests for @-references and the relations projection  -*- lexical-binding: t -*-

(require 'test-helpers)

;;; Edge derivation from body links

(ert-deftest org-glance-test:relations-edges-from-links ()
  "material links (kinded + kindless), legacy visit links, and dedup; other
link types contribute no edge."
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
  "Edges survive serialize→disk→deserialize; the `:refers-to' filter matches on
DESERIALIZED structs (the inner-vector normalization trap)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* TODO A :book:"
        "[[org-glance-material:b?kind=editor][B]]")
      (org-glance-test:headline "b" "* TODO B"))
    (let* ((cold (org-glance-test:reopen graph))       ; re-read from disk
           (meta (org-glance-graph:get-headline cold "a")))
      (should (equal '(("b" . "editor"))
                     (org-glance-headline-metadata:relations meta)))
      ;; filter over the deserialized structs
      (should (equal '("a") (org-glance-test:filter-ids cold '(:refers-to "b")))))))

(ert-deftest org-glance-test:relations-absent-field-reads-nil ()
  "A record serialized before the field existed deserializes relations = nil."
  (let ((meta (org-glance-headline-metadata:deserialize
               '(:id "old" :state "TODO" :title "Old" :tags []
                 :hash "h" :schedule nil :deadline nil :priority nil
                 :linked nil :propertized nil :encrypted nil))))
    (should (null (org-glance-headline-metadata:relations meta)))))

;;; Filters

(ert-deftest org-glance-test:relations-id-any-filter-and-identity ()
  "`:id-any' selects the listed ids; its identity is order-insensitive; both
relation keys are transient (never overview-cached, no table config)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "x" "* TODO X :book:")
      (org-glance-test:headline "y" "* TODO Y")
      (org-glance-test:headline "z" "* TODO Z"))
    (should (equal '("x" "z") (org-glance-test:filter-ids graph '(:id-any ("x" "z")))))
    ;; composes with :tags
    (should (equal '("x") (org-glance-test:filter-ids graph '(:id-any ("x" "z") :tags ("book")))))
    (should (equal (org-glance-filter:identity '(:id-any ("b" "a")))
                   (org-glance-filter:identity '(:id-any ("a" "b")))))
    (should (org-glance-filter:transient? '(:refers-to "x")))
    (should (org-glance-filter:transient? '(:id-any ("x"))))
    (should-not (org-glance-filter:transient? '(:tags ("book"))))
    (should (null (org-glance-overview:spec-key '(:refers-to "x"))))))

;;; The `@' command

(ert-deftest org-glance-test:material-refer-inserts-link ()
  "`@' at a body boundary inserts a material link; the C-u variant a ?kind=;
self is excluded from the candidates."
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
      ;; C-u: kind prompt (second completing-read call) encodes ?kind= and
      ;; offers the kinds already present in the graph
      (insert "\n")
      (let (kind-coll)
        (cl-letf (((symbol-function 'completing-read)
                   (let ((n 0))
                     (lambda (_p coll &rest _)
                       (cl-incf n)
                       (if (= n 1) (caar coll)
                         (setq kind-coll coll) "roasted by")))))
          (org-glance-material:refer '(4)))
        (should (member "author" kind-coll)))    ; seeded above via me's own edge
      ;; kinded reference reads as prose (pretty), slugged in the wire
      (should (s-contains?
               "roasted by [[org-glance-material:other?kind=roasted-by][Other headline]]"
               (buffer-string))))))

(ert-deftest org-glance-test:material-refer-in-title ()
  "`@' works inside the heading title (after a space).
Column-0 self-insert is owned by `material-refer-self-inserts-elsewhere'."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "me" "* TODO Coffee from" "body")
      (org-glance-test:headline "other" "* TODO Roaster"))
    (org-glance-test:with-material (buf graph "me")
      ;; end of the title, after a space: inserts the link into the heading
      (org-glance-material--goto-first-heading)
      (end-of-line)
      (insert " ")
      (org-glance-test:offering (offered (caar offered))
        (org-glance-material:refer))
      (org-glance-material--goto-first-heading)
      (should (s-contains? "Coffee from [[org-glance-material:other][Roaster]]"
                           (buffer-substring (line-beginning-position)
                                             (line-end-position)))))))

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
          ;; pick the SECOND duplicate by its id suffix
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
        ;; mid-word (an email)
        (goto-char (point-max))
        (unless (eolp) (end-of-line))
        (insert "user")                     ; ...user|
        (let ((last-command-event ?@)) (org-glance-material:refer))
        (should (s-contains? "user@" (buffer-string)))
        ;; heading line (speed-key territory)
        (goto-char (point-min))             ; bol on "* TODO Me"
        (let ((last-command-event ?@)) (org-glance-material:refer))
        (should (s-contains? "@* TODO Me" (buffer-string)))))))

;;; Follow

(ert-deftest org-glance-test:link-material-follow ()
  "Following org-glance-material:ID materializes ID; ?kind= is stripped;
dangling id errors."
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

;;; Relations command (`C-c @')

(ert-deftest org-glance-test:material-references-commands ()
  "`C-c @' opens ONE relation table -- outgoing edges and referrers merged --
under the bare relation filter, anchored on this headline.  A headline related
to nothing in either direction errors."
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
          (org-glance-material:references))
        ;; the SAME command on the target sees the incoming edge -- no prefix arg
        (org-glance-test:with-material (buf graph "b")
          (org-glance-material:references)))
      (setq calls (nreverse calls))
      (should (equal '(:id-any ("b")) (car (nth 0 calls))))
      (should (equal '(:anchor "a" :dir relations) (cdr (nth 0 calls))))
      (should (equal '(:id-any ("a")) (car (nth 1 calls))))
      (should (equal '(:anchor "b" :dir relations) (cdr (nth 1 calls)))))
    ;; unrelated in both directions -> friendly error
    (org-glance-test:with-material (buf graph "c")
      (cl-letf (((symbol-function 'org-glance-table:visit) #'ignore))
        (should-error (org-glance-material:references) :type 'user-error)))))

(ert-deftest org-glance-test:relations-table-relation-column ()
  "The relation table's `Relation' cell names direction and kind: `>' for an
edge FROM the anchor, `<' for one TO it, both for a mutual pair, the bare arrow
for a kindless edge.  Rows merge both directions."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* TODO A"
                                "roasted by [[org-glance-material:b?kind=roasted-by][B]]"
                                "[[org-glance-material:c][C]]")
      (org-glance-test:headline "b" "* TODO B" "[[org-glance-material:a][A]]")
      (org-glance-test:headline "c" "* TODO C"))
    ;; mutual: kinded out, kindless back -- pretty (spaced) kind form
    (should (equal "> roasted by, <" (org-glance-table--relation-cell graph "a" "b")))
    (should (equal ">" (org-glance-table--relation-cell graph "a" "c")))
    (should (equal "<" (org-glance-table--relation-cell graph "c" "a")))
    ;; row population is the union of targets and referrers, distinct
    (should (equal '("b" "c") (sort (copy-sequence (org-glance-table--related-ids graph "a"))
                                    #'string<)))
    (should (equal '("a") (org-glance-table--related-ids graph "b")))
    (should (equal '("a") (org-glance-table--related-ids graph "c")))
    ;; the column reaches the VIEW: a relation table renders it, and it holds
    ;; the cell above (a plain table has no such column)
    (org-glance-test:with-table (graph '(:id-any ("b")) '(:anchor "a" :dir relations))
      (should (member "relation" (org-glance-test:table-col-keys)))
      (should (equal "> roasted by, <"
                     (alist-get 'relation (alist-get 'cells (car table-view--rows))))))
    (org-glance-test:with-table (graph)
      (should-not (member "relation" (org-glance-test:table-col-keys))))))

;;; Crypt: a sealed reference is not an edge

(ert-deftest org-glance-test:relations-crypt-sealed-excluded ()
  "A link inside a SEALED crypt block is not indexed; one outside is.
Both projections of the sealed bytes are checked: `relations' (material links)
and `links' (plain ones)."
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
  "The LIVE SAVE path (sync) derives relations from the SEALED bytes: a link
inside a crypt block is excluded although the buffer was plaintext at save
time; reindex agrees."
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
        ;; decrypted buffer shows BOTH links; save reseals before sync reads
        (should (s-contains? "secret-ref" (buffer-string)))
        (org-glance-test:sed "editme" "edited")
        (org-glance-test:save)))
    (let ((after-sync (org-glance-test:field graph "cs" relations)))
      (should (equal '(("public-ref" . nil)) after-sync))
      (org-glance-graph:reindex graph)
      (should (equal after-sync (org-glance-test:field graph "cs" relations))))))

;;; Snapshot-on-repeat: encrypted headlines keep no occurrence history

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
          ;; ...and the after-repeat TRIM is gated too: no snapshot was made, so
          ;; trimming would cut history that was never preserved (invariant 14)
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
    ;; bare tag (downcased) / all / empty
    (should (equal (id '(:tags ("book"))) (id (org-glance-filter:from-link-path "BOOK"))))
    (should (null (org-glance-filter:from-link-path "all")))
    (should (null (org-glance-filter:from-link-path "")))
    ;; booleans, strings, lists, priority letter, edges
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
    ;; planning keys use the predicate's own vocabulary (a t here would error
    ;; per headline inside the clause -- the diverged-dispatch regression)
    (should (equal (id '(:schedule :present :deadline :absent))
                   (id (org-glance-filter:from-link-path "?schedule=present&deadline=absent"))))
    (should-error (org-glance-filter:from-link-path "?schedule=t"))
    ;; `done=nil' is a clause, not an omission
    (should-not (equal (id (org-glance-filter:from-link-path "book"))
                       (id (org-glance-filter:from-link-path "book?done=nil"))))
    ;; malformed / unlinkable / unknown
    (should-error (org-glance-filter:from-link-path "book?done=maybe"))
    (should-error (org-glance-filter:from-link-path "book?where=f"))
    (should-error (org-glance-filter:from-link-path "book?nope=1"))
    (should-error (org-glance-filter:from-link-path "book?novalue"))))

(ert-deftest org-glance-test:overview-link-follow ()
  "A `?'-qualified path opens the EXACT stated filter (no ambient merge); a
bare TAG path merges the ambient spec like the command; both land in the
default view."
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
  "A relation kind is a column: `C-c +' offers it pretty, the column shows the
target TITLES (many-to-many comma-joined, gone targets fall back to the id),
and the per-tag schema round-trips it as an edge column."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "c1" "* TODO Kebena Decaf :coffee:"
        "roasted by [[org-glance-material:r1?kind=roasted-by][Manhattan]]"
        "also [[org-glance-material:gone?kind=roasted-by][Ghost]]")
      (org-glance-test:headline "c2" "* TODO Another Bean :coffee:")
      (org-glance-test:headline "r1" "* Manhattan Coffee Roasters"))
    (org-glance-test:with-table (graph 'coffee)
        ;; prompt offers the PRETTY kind
        (org-glance-test:offering (offered "roasted by")
          (funcall (key-binding (kbd "C-c +")))
          (should (member "roasted by" (mapcar #'car offered))))
        ;; column shows titles; many-to-many joins; gone target -> id
        (should (equal "Manhattan Coffee Roasters, gone"
                       (org-glance-test:table-cell "c1" "kind:roasted-by")))
        (should (equal "" (org-glance-test:table-cell "c2" "kind:roasted-by")))
        ;; header is the capitalized pretty form
        (should (cl-find "Roasted by"
                         (alist-get 'columns table-view--spec)
                         :key (lambda (c) (alist-get 'header c)) :test #'equal)))
    ;; persisted per tag: a fresh view of the SAME tag rebuilds the EDGE column
    (org-glance-test:with-table (graph 'coffee)
        (should (equal "Manhattan Coffee Roasters, gone"
                       (org-glance-test:table-cell "c1" "kind:roasted-by"))))))

(ert-deftest org-glance-test:table-custom-column-kind-vs-property ()
  "Case is the type tag: \"AUTHOR\" builds a drawer column, \"author\" an
edge column -- both usable on one graph, no live-membership flip."
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
  ;; legacy spaced kind in a stored link normalizes on parse
  (should (equal '("x" . "roasted-by")
                 (org-glance--link-edge "org-glance-material" "x?kind=roasted by"))))

(provide 'test-relations)
;;; test-relations.el ends here
