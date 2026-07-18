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
        "[[org-glance-material:t2][Base]]"
        "[[org-glance-visit:legacy1][Old note]]"
        "[[org-glance-material:t1?kind=author][Ann again]]"   ; duplicate edge
        "[[https://example.com][Web]]"))
    (should (equal '(("t1" . "author") ("t2" . nil) ("legacy1" . nil))
                   (org-glance-headline-metadata:relations
                    (org-glance-graph:get-headline graph "src"))))))

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

(ert-deftest org-glance-test:relations-transient-no-table-config ()
  "A relation view persists NO per-filter table config on layout changes."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* TODO A" "[[org-glance-material:b][B]]")
      (org-glance-test:headline "b" "* TODO B"))
    (org-glance-test:with-table-filter graph '(:refers-to "b") buf
      (with-current-buffer buf
        (let (saved)
          (cl-letf (((symbol-function 'org-glance-table--config-put)
                     (lambda (&rest _) (setq saved t))))
            (setq org-glance-table--config-snapshot nil)   ; force "layout changed"
            (org-glance-table--persist-config)
            (should-not saved)))))                          ; transient: never persisted
    ;; a plain tag view under the same forced change DOES persist
    (org-glance-test:with-table-filter graph nil buf
      (with-current-buffer buf
        (let (saved)
          (cl-letf (((symbol-function 'org-glance-table--config-put)
                     (lambda (&rest _) (setq saved t))))
            (setq org-glance-table--config-snapshot nil)
            (org-glance-table--persist-config)
            (should saved)))))))

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
      (let (offered)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p coll &rest _) (setq offered (mapcar #'car coll)) (caar coll))))
          (org-glance-material:refer))
        (should (cl-some (lambda (c) (s-contains? "Other headline" c)) offered))
        (should-not (cl-some (lambda (c) (s-contains? "Me" c)) offered))  ; self excluded
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
                         (setq kind-coll coll) "editor")))))
          (org-glance-material:refer '(4)))
        (should (member "author" kind-coll)))    ; seeded below via me's own edge
      (should (s-contains? "[[org-glance-material:other?kind=editor][Other headline]]"
                           (buffer-string))))))

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
      (let (offered)
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_p coll &rest _)
                     (setq offered (mapcar #'car coll))
                     ;; pick the SECOND duplicate by its id suffix
                     (cl-find "dup-two-" (mapcar #'car coll) :test #'s-contains?))))
          (org-glance-material:refer))
        (should (= 2 (cl-count "·" offered :test #'s-contains?)))  ; both disambiguated
        (should (s-contains? "[[org-glance-material:dup-two-yy][Same title]]"
                             (buffer-string)))))))

(ert-deftest org-glance-test:material-refer-self-inserts-elsewhere ()
  "`@' mid-word and on a heading line self-inserts (no prompt)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "me" "* TODO Me" "user")
      (org-glance-test:headline "o" "* TODO O"))
    (org-glance-test:with-material (buf graph "me")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (error "must not prompt"))))
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
    (let ((org-glance-graph graph) shown)
      (cl-letf (((symbol-function 'org-glance-ensure-init) #'ignore)
                ((symbol-function 'switch-to-buffer) (lambda (b) (setq shown b) b)))
        (org-glance-link:material "t?kind=author")
        (should (buffer-live-p shown))
        (with-current-buffer shown
          (should (equal "t" org-glance-material--id))
          (set-buffer-modified-p nil))
        (kill-buffer shown)
        (should-error (org-glance-link:material "no-such-id") :type 'user-error)))))

;;; References / back-references commands

(ert-deftest org-glance-test:material-references-commands ()
  "`C-c @' opens the outgoing-references table, `C-u C-c @' the backlinks
table; both pass the bare relation filter."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* TODO A" "[[org-glance-material:b][B]]")
      (org-glance-test:headline "b" "* DONE B"))       ; DONE must stay visible
    (org-glance-test:with-material (buf graph "a")
      (let (filters)
        (cl-letf (((symbol-function 'org-glance-table:visit)
                   (lambda (_g filter) (push filter filters) nil)))
          (org-glance-material:references)              ; outgoing
          (org-glance-material:references '(4)))        ; backlinks
        (should (equal '((:refers-to "a") (:id-any ("b"))) filters))))
    ;; no references -> friendly error
    (org-glance-test:with-material (buf graph "b")
      (cl-letf (((symbol-function 'org-glance-table:visit) #'ignore))
        (should-error (org-glance-material:references) :type 'user-error)))))

;;; Crypt: a sealed reference is not an edge

(ert-deftest org-glance-test:relations-crypt-sealed-excluded ()
  "A material link inside a SEALED crypt block is not indexed; one outside is."
  (org-glance-test:with-graph graph
    (let* ((plain (org-glance-test:headline "s" "* TODO Secret"
                    "[[org-glance-material:public-ref][Public]]"
                    "#+begin_crypt"
                    "[[org-glance-material:secret-ref][Secret]]"
                    "#+end_crypt"))
           (sealed (org-glance-headline:encrypt plain "pw")))
      (org-glance-graph:add graph sealed)
      (let ((rels (org-glance-headline-metadata:relations
                   (org-glance-graph:get-headline graph "s"))))
        (should (equal '(("public-ref" . nil)) rels))))))

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
        ;; decrypted buffer shows BOTH links; save reseals before sync reads
        (should (s-contains? "secret-ref" (buffer-string)))
        (org-glance-test:sed "editme" "edited")
        (let ((inhibit-message t)) (save-buffer))))
    (let ((after-sync (org-glance-headline-metadata:relations
                       (org-glance-graph:get-headline graph "cs"))))
      (should (equal '(("public-ref" . nil)) after-sync))
      (org-glance-graph:reindex graph)
      (should (equal after-sync (org-glance-headline-metadata:relations
                                 (org-glance-graph:get-headline graph "cs")))))))

;;; Clone-on-repeat: encrypted headlines are not cloned (would store plaintext)

(ert-deftest org-glance-test:clone-on-repeat-skips-encrypted ()
  "The repeat clone is skipped for an encrypted headline."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "e" "* TODO E"))
    (org-glance-test:with-material (buf graph "e")
      (let ((org-glance-clone-on-repeat-p t)
            (org-done-keywords '("DONE"))
            cloned)
        (setq-local org-glance-material--encrypted t)
        (cl-letf (((symbol-function 'org-get-todo-state) (lambda () "DONE"))
                  ((symbol-function 'org-glance-datetime-headline-repeated-p) (lambda () t))
                  ((symbol-function 'org-glance-material--clone-snapshot)
                   (lambda () (setq cloned t))))
          (org-glance-material:clone-on-repeat)
          (should-not cloned)
          ;; ...and the after-repeat TRIM is gated too: no clone was made, so
          ;; trimming would cut history that was never preserved (invariant 14)
          (let ((before (buffer-string)))
            (org-glance-material:cleanup-after-repeat)
            (should (equal before (buffer-string))))
          (setq-local org-glance-material--encrypted nil)
          (org-glance-material:clone-on-repeat)
          (should cloned))))))

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

(ert-deftest org-glance-test:overview-link-default-view-dispatch ()
  "`visit-default' routes to the table or the org overview per the custom."
  (org-glance-test:with-graph graph
    (let (calls)
      (cl-letf (((symbol-function 'org-glance-table:visit)
                 (lambda (&rest _) (push 'table calls) nil))
                ((symbol-function 'org-glance-overview:visit)
                 (lambda (&rest _) (push 'org calls) nil)))
        (let ((org-glance-overview-default-view 'org-glance-table))
          (org-glance-overview:visit-default graph nil))
        (let ((org-glance-overview-default-view 'org-glance-overview))
          (org-glance-overview:visit-default graph nil))
        (should (equal '(org table) calls))))))

(provide 'test-relations)
;;; test-relations.el ends here
