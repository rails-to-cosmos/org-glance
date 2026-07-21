;;; test-filter.el --- Tests for the shared filter language  -*- lexical-binding: t -*-

(require 'test-helpers)

(defun org-glance-test:meta (id state)
  "A bare metadata record carrying ID and todo STATE, for predicate tests."
  (make-org-glance-headline-metadata :id id :state state :title id))

(ert-deftest org-glance-test:filter-predicate-state ()
  "Predicate honours :done (active/done) and :state (exact); nil = all."
  (let* ((org-done-keywords '("DONE"))
         (todo (org-glance-test:meta "t" "TODO"))
         (done (org-glance-test:meta "d" "DONE"))
         (none (org-glance-test:meta "n" "")))
    (let ((p (org-glance-filter:predicate '(:done nil))))   ; active
      (should (funcall p todo)) (should (funcall p none)) (should-not (funcall p done)))
    (let ((p (org-glance-filter:predicate '(:done t))))     ; done
      (should (funcall p done)) (should-not (funcall p todo)) (should-not (funcall p none)))
    (let ((p (org-glance-filter:predicate '(:state "TODO")))) ; exact state
      (should (funcall p todo)) (should-not (funcall p done)) (should-not (funcall p none)))
    (let ((p (org-glance-filter:predicate nil)))            ; all
      (should (funcall p todo)) (should (funcall p done)) (should (funcall p none)))))

(ert-deftest org-glance-test:filter-set-state ()
  "set-state replaces the whole todo-state dimension (:state and :done)."
  (should (equal '(:done nil) (org-glance-filter:set-state nil 'active)))
  (should (equal '(:done t)   (org-glance-filter:set-state nil 'done)))
  (should (equal '(:state "TODO") (org-glance-filter:set-state nil "TODO")))
  (should (null (org-glance-filter:set-state nil 'all)))
  ;; switching dimensions: :state -> active drops :state, adds :done
  (should (equal '(:done nil) (org-glance-filter:set-state '(:state "TODO") 'active)))
  ;; active -> a concrete state drops :done, adds :state
  (should (equal '(:state "DONE") (org-glance-filter:set-state '(:done nil) "DONE")))
  ;; `all' clears both but preserves orthogonal dimensions
  (should (equal '(:tags ("work")) (org-glance-filter:set-state '(:tags ("work") :done nil) 'all))))

(ert-deftest org-glance-test:filter-set-substring ()
  "set-substring sets `:title-contains', or clears it on empty input."
  (should (equal '(:title-contains "foo") (org-glance-filter:set-substring nil "foo")))
  (should (null (org-glance-filter:set-substring '(:title-contains "foo") "")))
  (should (equal '(:done nil :title-contains "x")
                 (org-glance-filter:set-substring '(:done nil) "x"))))

(ert-deftest org-glance-test:filter-merge ()
  "merge overlays EXTRA onto BASE; EXTRA wins on a key conflict."
  (should (equal '(:done nil :tags ("work")) (org-glance-filter:merge '(:done nil) "work")))
  (should (equal '(:done nil) (org-glance-filter:merge '(:done nil) nil)))
  (should (equal '(:tags ("a")) (org-glance-filter:merge nil "a")))
  (should (equal '(:done t) (org-glance-filter:merge '(:done nil) '(:done t)))))

(ert-deftest org-glance-test:filter-describe ()
  "describe renders a compact human label for a spec."
  (should (string= "all" (org-glance-filter:describe nil)))
  (should (string= "active" (org-glance-filter:describe '(:done nil))))
  (should (string= "done" (org-glance-filter:describe '(:done t))))
  (should (string= "state=TODO" (org-glance-filter:describe '(:state "TODO"))))
  (should (s-contains? "title~foo" (org-glance-filter:describe '(:title-contains "foo"))))
  ;; relation views stay compact: truncated id / target count, never the raw list
  (should (string= "refs->abcdefgh" (org-glance-filter:describe '(:refers-to "abcdefgh-long-id"))))
  (should (string= "id-any(2)" (org-glance-filter:describe '(:id-any ("a" "b"))))))

(ert-deftest org-glance-test:filter-read-state ()
  "read-state maps the specials and concrete states; errors on empty input."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
                             (org-glance-test:headline "r1" "* TODO a")
                             (org-glance-test:headline "r2" "* DONE b"))
    (dolist (case '(("active" . active) ("done" . done)
                    ("all" . all) ("TODO" . "TODO")))
      (org-glance-test:answering ((completing-read (car case)))
        (should (equal (cdr case) (org-glance-filter:read-state graph)))))
    ;; the offered candidates are active/done/all + the graph's concrete states
    (org-glance-test:offering (offered "all")
      (org-glance-filter:read-state graph)
      (dolist (w '("active" "done" "all" "TODO" "DONE")) (should (member w offered))))
    ;; empty input -> user-error (no silent "match everything")
    (org-glance-test:answering ((completing-read ""))
      (should-error (org-glance-filter:read-state graph) :type 'user-error))))

(ert-deftest org-glance-test:filter-read-state-collision ()
  "A concrete state whose name collides with a special (active/done/all) folds
into that special: it is offered exactly once and selecting it yields the
special symbol, never the literal string.  Documents the deliberate limitation
\(org todo keywords are uppercase by convention, so this is essentially theoretical)."
  (cl-letf (((symbol-function 'org-glance-graph:states)
             (lambda (&rest _) '("active" "TODO"))))
    (org-glance-test:offering (offered "active")
      ;; selecting the colliding name returns the special symbol, not "active"
      (should (eq 'active (org-glance-filter:read-state 'dummy-graph)))
      ;; "active" is offered exactly once (the special; the concrete dup is dropped)
      (should (= 1 (cl-count "active" offered :test #'string=)))
      ;; a non-colliding concrete state is still offered as itself
      (should (member "TODO" offered)))))

(ert-deftest org-glance-test:filter-archived-commented ()
  "`:archived' / `:commented' filter on org's ARCHIVE tag and COMMENT keyword;
the metadata flags round-trip through serialize -> disk -> deserialize, and a
record from before the fields reads nil (kept by the nil filter)."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "live" "* TODO Live")
      (org-glance-test:headline "arch" "* TODO Old :ARCHIVE:")
      (org-glance-test:headline "comm" "* COMMENT Draft"))
    (let ((cold (org-glance-test:reopen graph)))       ; deserialized structs
      (should (org-glance-headline-metadata:archived?
               (org-glance-graph:get-headline cold "arch")))
      ;; the ARCHIVE marker tag never becomes a collection tag
      (should-not (org-glance-headline-metadata:tag-strings
                   (org-glance-graph:get-headline cold "arch")))
      (should-not (member "archive" (org-glance-graph:tags cold)))
      (should (org-glance-headline-metadata:commented?
               (org-glance-graph:get-headline cold "comm")))
      (should (equal '("arch") (org-glance-test:filter-ids cold '(:archived t))))
      (should (equal '("comm" "live")
                     (sort (org-glance-test:filter-ids cold '(:archived nil)) #'string<)))
      ;; the DEFAULT ambient spec hides both flags
      (should (equal '("live")
                     (org-glance-test:filter-ids
                      cold (default-value 'org-glance-filter-spec)))))
    ;; pre-field record: absent flags deserialize nil -> nil filter keeps it
    (let ((meta (org-glance-headline-metadata:deserialize
                 '(:id "old" :state "TODO" :title "Old" :tags []
                   :hash "h" :schedule nil :deadline nil :priority nil
                   :linked nil :propertized nil :encrypted nil))))
      (should-not (org-glance-headline-metadata:archived? meta))
      (should-not (org-glance-headline-metadata:commented? meta)))))

(ert-deftest org-glance-test:filter-spec-defcustom ()
  "`org-glance-filter-spec' is a defcustom whose default hides done, archived
and commented headlines; `describe' renders the flags compactly."
  (should (custom-variable-p 'org-glance-filter-spec))
  (should (equal '(:done nil :archived nil :commented nil)
                 (default-value 'org-glance-filter-spec)))
  (should (equal "active, -archived, -commented"
                 (org-glance-filter:describe
                  '(:done nil :archived nil :commented nil))))
  (should (s-contains? "archived"
                       (org-glance-filter:describe '(:archived t)))))

(provide 'test-filter)
;;; test-filter.el ends here
