;;; test-llm.el --- Tests for the LLM session command  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:llm-materializes-then-opens-menu ()
  "`org-glance-llm' materializes the headline, then opens `agnostic-llm-menu'
pinned to its data dir and title label."
  (org-glance-test:session
    (org-glance-graph:add org-glance-graph
                          (org-glance-test:headline "llmid" "* TODO Alpha :x:" "body"))
    (let ((root 'unset) (label 'unset) (mat nil))
      (cl-letf (((symbol-function 'completing-read) (lambda (_p coll &rest _) (caar coll)))
                ((symbol-function 'switch-to-buffer) (lambda (b &rest _) (setq mat b) b))
                ((symbol-function 'agnostic-llm-menu)
                 (lambda (&optional dir lbl) (setq root dir label lbl))))
        (org-glance-llm))
      (let ((expected (org-glance-graph:headline-data-path org-glance-graph "llmid")))
        (should (equal (file-truename root) (file-truename expected)))
        (should (file-directory-p root))
        (should (equal "alpha" label))
        ;; the headline was materialized (its blob buffer) before the menu
        (should (buffer-live-p mat))
        (with-current-buffer mat (should org-glance-material-mode))
        (set-buffer-modified-p nil)
        (kill-buffer mat)))))

(ert-deftest org-glance-test:llm-switches-to-live-session ()
  "When the headline's session buffer is already live (matched by its data dir),
`org-glance-llm' switches to it instead of opening the menu again."
  (org-glance-test:session
    (org-glance-graph:add org-glance-graph
                          (org-glance-test:headline "llmid" "* TODO Alpha :x:"))
    (let* ((dir (org-glance-graph:headline-data-path org-glance-graph "llmid"))
           (buf (get-buffer-create "*llm:alpha*"))
           (switched nil) (menu-called nil))
      (make-directory dir t)
      (with-current-buffer buf (setq default-directory (file-name-as-directory dir)))
      (unwind-protect
          (cl-letf (((symbol-function 'completing-read) (lambda (_p coll &rest _) (caar coll)))
                    ((symbol-function 'switch-to-buffer) (lambda (b &rest _) (setq switched b) b))
                    ((symbol-function 'agnostic-llm-menu) (lambda (&rest _) (setq menu-called t))))
            (org-glance-llm)
            (should (eq switched buf))
            (should-not menu-called))
        (kill-buffer buf)))))

(ert-deftest org-glance-test:llm-slug-and-collision ()
  "`org-glance-llm--slug' normalises a title; `--label' disambiguates only when
a session for a different headline already holds the plain slug."
  (should (equal "buy-milk-2l" (org-glance-llm--slug "Buy milk (2L)!")))
  (should (equal "hello-world" (org-glance-llm--slug "  Hello  World  ")))
  (should-not (org-glance-llm--slug "###"))
  (with-temp-directory dir
    (let* ((md (make-org-glance-headline-metadata :id "abcdef123" :title "Foo Bar"))
           (mine (f-join dir "mine")) (other (f-join dir "other")))
      (make-directory mine t) (make-directory other t)
      ;; no clash -> plain slug
      (should (equal "foo-bar" (org-glance-llm--label md mine)))
      (let ((buf (get-buffer-create "*llm:foo-bar*")))
        (unwind-protect
            (progn
              ;; a *llm:foo-bar* rooted elsewhere -> disambiguate with the dir leaf
              (with-current-buffer buf (setq default-directory (file-name-as-directory other)))
              (should (equal "foo-bar-mine" (org-glance-llm--label md mine)))
              ;; same root -> reuse the plain slug
              (with-current-buffer buf (setq default-directory (file-name-as-directory mine)))
              (should (equal "foo-bar" (org-glance-llm--label md mine))))
          (kill-buffer buf))))))

(ert-deftest org-glance-test:llm-dir-uses-project-property ()
  "`org-glance-llm--dir' returns `ORG_GLANCE_PROJECT_DIR' when set, else data dir."
  (org-glance-test:session
    (org-glance-graph:add org-glance-graph
      (org-glance-test:headline-props "a" "* TODO A" '(("ORG_GLANCE_PROJECT_DIR" . "~/x/proj")))
      (org-glance-test:headline "b" "* TODO B"))
    (should (equal (expand-file-name "~/x/proj") (org-glance-llm--dir org-glance-graph "a")))
    (should (equal (org-glance-graph:headline-data-path org-glance-graph "b")
                   (org-glance-llm--dir org-glance-graph "b")))))

(ert-deftest org-glance-test:llm-honours-project-dir ()
  "`org-glance-llm' opens the session in a headline's `ORG_GLANCE_PROJECT_DIR'."
  (org-glance-test:session
    (let ((proj (make-temp-file "og-proj" t)))
      (org-glance-graph:add org-glance-graph
        (org-glance-headline--from-lines "* TODO Proj" ":PROPERTIES:" ":ORG_GLANCE_ID: p"
                                         (format ":ORG_GLANCE_PROJECT_DIR: %s" proj) ":END:"))
      (let ((root 'unset))
        (cl-letf (((symbol-function 'completing-read) (lambda (_p coll &rest _) (caar coll)))
                  ((symbol-function 'switch-to-buffer) (lambda (b &rest _) b))
                  ((symbol-function 'agnostic-llm-menu) (lambda (&optional dir _lbl) (setq root dir))))
          (org-glance-llm))
        (should (file-equal-p root proj))))))

(provide 'test-llm)
;;; test-llm.el ends here
