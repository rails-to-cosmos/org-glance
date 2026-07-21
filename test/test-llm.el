;;; test-llm.el --- Tests for the LLM session command  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:llm-materializes-then-opens-menu ()
  "`org-glance-llm' materializes the headline, then opens `agnostic-llm-menu'
pinned to its data dir and title label."
  (org-glance-test:session
    (org-glance-graph:add org-glance-graph
                          (org-glance-test:headline "llmid" "* TODO Alpha :x:" "body"))
    (let ((root 'unset) (label 'unset))
      (org-glance-test:with-shown (mat)
        (cl-letf (((symbol-function 'completing-read) (lambda (_p coll &rest _) (caar coll)))
                  ((symbol-function 'agnostic-llm-menu)
                   (lambda (&optional dir lbl) (setq root dir label lbl))))
          (org-glance-llm))
        (let ((expected (org-glance-graph:headline-data-path org-glance-graph "llmid")))
          (should (s-suffix? "/" root))          ; directory-valued, slash-terminated
          (should (file-equal-p root expected))
          (should (file-directory-p root))
          (should (equal "alpha" label))
          ;; the headline was materialized (its blob buffer) before the menu
          (should (buffer-live-p mat))
          (with-current-buffer mat (should org-glance-material-mode)))))))

(ert-deftest org-glance-test:llm-switches-to-live-session ()
  "When the headline's session buffer is already live (matched by its data dir),
`org-glance-llm' switches to it instead of opening the menu again."
  (org-glance-test:session
    (org-glance-graph:add org-glance-graph
                          (org-glance-test:headline "llmid" "* TODO Alpha :x:"))
    (let ((dir (org-glance-graph:headline-data-path org-glance-graph "llmid"))
          (menu-called nil))
      (make-directory dir t)
      (org-glance-test:with-llm-buffer (buf "alpha" dir)
        (org-glance-test:with-shown (shown)
          (cl-letf (((symbol-function 'completing-read) (lambda (_p coll &rest _) (caar coll)))
                    ((symbol-function 'agnostic-llm-menu) (lambda (&rest _) (setq menu-called t))))
            (org-glance-llm)
            (should (eq shown buf))
            (should-not menu-called)))))))

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
      ;; a *llm:foo-bar* rooted elsewhere -> disambiguate with the dir leaf
      (org-glance-test:with-llm-buffer (buf "foo-bar" other)
        (should (equal "foo-bar-mine" (org-glance-llm--label md mine)))
        ;; same root -> reuse the plain slug
        (with-current-buffer buf (setq default-directory (file-name-as-directory mine)))
        (should (equal "foo-bar" (org-glance-llm--label md mine)))))))

(ert-deftest org-glance-test:llm-dir-uses-project-property ()
  "`org-glance-llm--dir' returns `ORG_GLANCE_PROJECT_DIR' when set, else data dir."
  (org-glance-test:session
    (org-glance-graph:add org-glance-graph
      (org-glance-test:headline-props "a" "* TODO A" '(("ORG_GLANCE_PROJECT_DIR" . "~/x/proj")))
      (org-glance-test:headline "b" "* TODO B"))
    ;; directory-valued: always trailing-slashed (feeds `default-directory')
    (should (equal (file-name-as-directory (expand-file-name "~/x/proj"))
                   (org-glance-llm--dir org-glance-graph "a")))
    (should (equal (file-name-as-directory
                    (org-glance-graph:headline-data-path org-glance-graph "b"))
                   (org-glance-llm--dir org-glance-graph "b")))))

(ert-deftest org-glance-test:llm-honours-project-dir ()
  "`org-glance-llm' opens the session in a headline's `ORG_GLANCE_PROJECT_DIR'."
  (org-glance-test:session
    (let ((proj (make-temp-file "og-proj" t)))
      (org-glance-graph:add org-glance-graph
        (org-glance-headline--from-lines "* TODO Proj" ":PROPERTIES:" ":ORG_GLANCE_ID: p"
                                         (format ":ORG_GLANCE_PROJECT_DIR: %s" proj) ":END:"))
      (let ((root 'unset))
        (org-glance-test:with-shown (_shown)
          (cl-letf (((symbol-function 'completing-read) (lambda (_p coll &rest _) (caar coll)))
                    ((symbol-function 'agnostic-llm-menu) (lambda (&optional dir _lbl) (setq root dir))))
            (org-glance-llm)))
        (should (file-equal-p root proj))))))

;;; Sessions table

(cl-defmacro org-glance-test:llm-stubs ((store) &rest body)
  "Stub the agnostic-llm seam against temp session store STORE for BODY.
Mirrors the provider layout: session dirs encode [/.] as dashes under STORE;
the newest `.jsonl' inside is the transcript; prompt history is empty."
  (declare (indent 1))
  `(with-temp-directory ,store
     (cl-letf* (((symbol-function 'agnostic-llm--provider-get)
                 (lambda (key) (when (eq key :session-dir) ,store)))
                ((symbol-function 'agnostic-llm--session-dir)
                 (lambda (dir)
                   (expand-file-name
                    (replace-regexp-in-string
                     "[/.]" "-" (directory-file-name (expand-file-name dir)))
                    ,store)))
                ((symbol-function 'agnostic-llm--session-file)
                 (lambda (dir)
                   (let ((sdir (agnostic-llm--session-dir dir)))
                     (and (file-directory-p sdir)
                          (car (directory-files sdir t "\\.jsonl\\'" t))))))
                ((symbol-function 'agnostic-llm--prompt-history-files)
                 (lambda (&optional _) nil))
                ((symbol-function 'agnostic-llm--prompt-preview) #'identity))
       ,@body)))

(cl-defun org-glance-test:llm-record-session (dir)
  "Record a fake provider transcript for session DIR."
  (let ((sdir (agnostic-llm--session-dir dir)))
    (make-directory sdir t)
    (with-temp-file (expand-file-name "s.jsonl" sdir) (insert "{}"))))

(ert-deftest org-glance-test:llm-sessions-state ()
  "State machine: no buffer -> stopped; dead buffer -> exited; live -> running."
  (should (equal "stopped" (org-glance-llm--state nil)))
  (org-glance-test:with-open buf (get-buffer-create " *llm-state-test*")
    (should (equal "exited" (org-glance-llm--state buf)))
    (let ((proc (start-process "llm-state" buf "sleep" "10")))
      (unwind-protect
          (should (equal "running" (org-glance-llm--state buf)))
        (set-process-query-on-exit-flag proc nil)
        (delete-process proc)))))

(ert-deftest org-glance-test:llm-buffer-label ()
  "`--buffer-label' unwraps `*llm:LABEL*', uniquify suffix included."
  (with-temp-buffer
    (rename-buffer "*llm:foo*" t)
    (should (equal "foo" (org-glance-llm--buffer-label (current-buffer)))))
  (with-temp-buffer
    (rename-buffer "*llm:foo*<2>" t)
    (should (equal "foo" (org-glance-llm--buffer-label (current-buffer))))))

(ert-deftest org-glance-test:llm-sessions-rows ()
  "Rows: recorded-but-dead headline sessions are `stopped'; live non-headline
buffers appear as `exited'/`running'; headlines without sessions are absent."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "a" "* TODO Alpha :x:")
      (org-glance-test:headline "b" "* TODO Beta :x:"))
    (org-glance-test:llm-stubs (store)
      (org-glance-test:llm-record-session (org-glance-llm--dir graph "a"))
      (with-temp-directory extra
        (org-glance-test:with-llm-buffer (buf "extra" extra)
          (let* ((rows (org-glance-llm--session-rows graph))
                       (arow (cl-find "a" rows :key (lambda (r) (alist-get 'headline r))
                                      :test #'equal))
                       (xrow (cl-find "*llm:extra*" rows
                                      :key (lambda (r) (alist-get 'buffer (alist-get 'cells r)))
                                      :test #'equal)))
                  (should (= 2 (length rows)))          ; b has no session
                  (should arow)
                  (let ((cells (alist-get 'cells arow)))
                    (should (equal "Alpha" (alist-get 'title cells)))
                    (should (equal "stopped" (alist-get 'state cells)))
                    (should (s-present? (alist-get 'last cells))))
                  (should xrow)
                  (should-not (alist-get 'headline xrow))
                  (let ((cells (alist-get 'cells xrow)))
                    (should (equal "extra" (alist-get 'title cells)))
                    (should (equal "exited" (alist-get 'state cells))))))))))

(ert-deftest org-glance-test:llm-sessions-open-restarts-stopped ()
  "RET on a stopped headline session starts `agnostic-llm' with the session
dir and the headline's title-slug label."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO Alpha :x:"))
    (org-glance-test:llm-stubs (store)
      (let* ((dir (org-glance-llm--dir graph "a"))
             (row `((id . ,dir) (headline . "a")))
             (started nil))
        (cl-letf (((symbol-function 'agnostic-llm)
                   (lambda (&optional root label) (setq started (cons root label)))))
          (org-glance-llm--act-open graph dir row))
        (should (equal dir (car started)))
        (should (equal "alpha" (cdr started)))))))

(ert-deftest org-glance-test:llm-sessions-visit ()
  "The sessions table renders one row per session from the cache."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO Alpha :x:"))
    (org-glance-test:llm-stubs (store)
      (org-glance-test:llm-record-session (org-glance-llm--dir graph "a"))
      (org-glance-test:with-shown (buf)
        (setq buf (org-glance-llm-sessions:visit graph))
        (with-current-buffer buf
          (should (string= "*org-glance-llm-sessions*" (buffer-name)))
          (should (= 1 (length table-view--rows)))
          (should (equal "stopped"
                         (org-glance-test:table-cell
                          (org-glance-llm--dir graph "a") "state"))))))))

(ert-deftest org-glance-test:llm-sessions-cache ()
  "`L' reads only the persisted cache: the first rows call scans and writes
it, later calls never rescan; a rescan (`g') picks up new sessions; a live
session missing from the cache still appears as a live row."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph
      (org-glance-test:headline "c1" "* TODO Coffee :coffee:")
      (org-glance-test:headline "x1" "* TODO Other :misc:"))
    (org-glance-test:llm-stubs (store)
      (org-glance-test:llm-record-session (org-glance-llm--dir graph "c1"))
      ;; first call scans + persists
      (should (= 1 (length (org-glance-llm--session-rows graph))))
      (should (f-exists? (org-glance-llm--cache-file graph)))
      ;; cache-only reads: a new recorded session stays invisible, and the
      ;; scanner must not run at all
      (org-glance-test:llm-record-session (org-glance-llm--dir graph "x1"))
      (cl-letf (((symbol-function 'org-glance-llm--scan)
                 (lambda (&rest _) (error "must not rescan"))))
        (should (= 1 (length (org-glance-llm--session-rows graph)))))
      ;; the refresh path rescans and the new session appears
      (org-glance-llm--cache-write graph (org-glance-llm--scan graph))
      (should (= 2 (length (org-glance-llm--session-rows graph))))
      ;; live overlay: a session absent from the cache shows as a live row
      (with-temp-directory extra
        (org-glance-test:with-llm-buffer (buf "orphan" extra)
          (let ((rows (org-glance-llm--session-rows graph)))
            (should (= 3 (length rows)))
            (should (cl-find "exited" rows
                             :key (lambda (r) (alist-get 'state (alist-get 'cells r)))
                             :test #'equal))))))))

(ert-deftest org-glance-test:llm-sessions-empty-cache-valid ()
  "An EMPTY cache is a valid answer: a zero-session store scans once,
then never rescans on plain re-fills."
  (org-glance-test:with-graph graph
    (org-glance-graph:add graph (org-glance-test:headline "a" "* TODO Alpha"))
    (org-glance-test:llm-stubs (store)
      (should-not (org-glance-llm--session-rows graph))      ; scans, writes []
      (should (f-exists? (org-glance-llm--cache-file graph)))
      (cl-letf (((symbol-function 'org-glance-llm--scan)
                 (lambda (&rest _) (error "must not rescan"))))
        (should-not (org-glance-llm--session-rows graph))))))

(provide 'test-llm)
;;; test-llm.el ends here
