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
      (let ((keep? (org-glance-filter:predicate '(:refers-to "b"))))
        (should (equal '("a")
                       (mapcar #'org-glance-headline-metadata:id
                               (seq-filter keep? (org-glance-graph:headlines cold)))))))))

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
    (let ((keep? (org-glance-filter:predicate '(:id-any ("x" "z")))))
      (should (equal '("x" "z")
                     (mapcar #'org-glance-headline-metadata:id
                             (seq-filter keep? (org-glance-graph:headlines graph))))))
    ;; composes with :tags
    (let ((keep? (org-glance-filter:predicate '(:id-any ("x" "z") :tags ("book")))))
      (should (equal '("x") (org-glance-test:filter-ids graph '(:id-any ("x" "z") :tags ("book"))))
              ))
    (should (equal (org-glance-filter:identity '(:id-any ("b" "a")))
                   (org-glance-filter:identity '(:id-any ("a" "b")))))
    (should (org-glance-filter:transient? '(:refers-to "x")))
    (should (org-glance-filter:transient? '(:id-any ("x"))))
    (should-not (org-glance-filter:transient? '(:tags ("book"))))
    (should (null (org-glance-overview:spec-key '(:refers-to "x"))))
    (should (org-glance-overview:spec-key '(:tags ("book"))))))

;;; The `@' command

(ert-deftest org-glance-test:material-refer-inserts-link ()
  "`@' at a body boundary inserts a material link; the C-u variant a ?kind=;
self is excluded from the candidates."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "me" "* TODO Me" "body")
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
      ;; C-u: kind prompt (second completing-read call) encodes ?kind=
      (insert "\n")
      (cl-letf (((symbol-function 'completing-read)
                 (let ((n 0))
                   (lambda (_p coll &rest _)
                     (cl-incf n)
                     (if (= n 1) (caar coll) "editor")))))
        (org-glance-material:refer '(4)))
      (should (s-contains? "[[org-glance-material:other?kind=editor][Other headline]]"
                           (buffer-string))))))

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
          (setq-local org-glance-material--encrypted nil)
          (org-glance-material:clone-on-repeat)
          (should cloned))))))

(provide 'test-relations)
;;; test-relations.el ends here
