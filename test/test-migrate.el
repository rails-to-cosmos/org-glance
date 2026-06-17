;;; test-migrate.el --- Tests for legacy v1 metadata migration  -*- lexical-binding: t -*-

(require 'test-helpers)

(cl-defun org-glance-test:write (path text)
  "Write TEXT to PATH, creating parent directories."
  (f-mkdir-full-path (f-dirname path))
  (f-write-text text 'utf-8 path))

(ert-deftest org-glance-test:migrate-detect ()
  "Legacy .metadata.el files are detected; backed-up .bak files are not."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "foo" "foo.metadata.el") "#s(hash-table)")
    (org-glance-test:write (f-join dir "bar" "bar.metadata.el.bak") "#s(hash-table)")
    (let ((found (org-glance-legacy-metadata-files dir)))
      (should (= 1 (length found)))
      (should (s-ends-with? "foo.metadata.el" (car found))))))

(ert-deftest org-glance-test:migrate ()
  "Migration ingests id-bearing headlines (id, title, tags and content
preserved) and backs up the legacy metadata non-destructively."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "foo" "foo.org")
                           "* TODO Hello :foo:\n:PROPERTIES:\n:ORG_GLANCE_ID: hello-1\n:END:\nbody text\n")
    (org-glance-test:write (f-join dir "foo" "foo.metadata.el") "#s(hash-table)")
    (should (= 1 (org-glance-migrate dir)))
    (let* ((graph (org-glance-graph dir))
           (meta (org-glance-graph:get-headline graph "hello-1")))
      (should (org-glance-headline-metadata? meta))
      (should (string= "Hello" (org-glance-headline-metadata:title meta)))
      (should (member "foo" (append (org-glance-headline-metadata:tags meta) nil)))
      (should (s-contains? "body text" (org-glance-graph:get-content graph "hello-1"))))
    ;; non-destructive backup
    (should (f-exists? (f-join dir "foo" "foo.metadata.el.bak")))
    (should-not (f-exists? (f-join dir "foo" "foo.metadata.el")))
    ;; legacy no longer detected after migration
    (should (null (org-glance-legacy-metadata-files dir)))))

(ert-deftest org-glance-test:migrate-skips-overview ()
  "Overview clones (sharing an id) must not override the canonical source."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "foo" "foo.org")
                           "* TODO Hello :foo:\n:PROPERTIES:\n:ORG_GLANCE_ID: hello-1\n:END:\n")
    (org-glance-test:write (f-join dir "foo" "overview.org")
                           "#    -*- mode: org; mode: org-glance-overview -*-\n* DONE Clone :foo:\n:PROPERTIES:\n:ORG_GLANCE_ID: hello-1\n:END:\n")
    (org-glance-migrate dir)
    (let ((meta (org-glance-graph:get-headline (org-glance-graph dir) "hello-1")))
      (should (string= "Hello" (org-glance-headline-metadata:title meta)))
      (should (string= "TODO" (org-glance-headline-metadata:state meta))))))

(ert-deftest org-glance-test:migrate-ignores-idless ()
  "Headlines without ORG_GLANCE_ID are not ingested."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "notes.org") "* just a heading\nno id here\n")
    (should (= 0 (org-glance-migrate dir)))
    (should (null (org-glance-graph:headlines (org-glance-graph dir))))))

(ert-deftest org-glance-test:migrate-skips-failing-file ()
  "One file that errors during ingest is logged and skipped; the rest still
migrate (no whole-batch abort)."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "good.org")
                           "* TODO Good\n:PROPERTIES:\n:ORG_GLANCE_ID: good\n:END:\n")
    (org-glance-test:write (f-join dir "bad.org")
                           "* TODO Bad\n:PROPERTIES:\n:ORG_GLANCE_ID: bad\n:END:\n")
    (let ((orig (symbol-function 'org-glance-graph:add)))
      (cl-letf (((symbol-function 'org-glance-graph:add)
                 (lambda (graph &rest headlines)
                   (if (cl-some (lambda (h) (string= "bad" (org-glance-headline:id h))) headlines)
                       (error "simulated ingest failure")
                     (apply orig graph headlines)))))
        ;; must not abort despite bad.org failing
        (org-glance-migrate dir)))
    (let ((graph (org-glance-graph dir)))
      (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "good")))
      (should (null (org-glance-graph:get-headline graph "bad"))))))

(ert-deftest org-glance-test:migrate-idempotent ()
  "Re-running migration ingests nothing new and appends no duplicate records:
the second run adds 0 and the store still holds one record per id."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "foo" "foo.org")
                           "* TODO Hello :foo:\n:PROPERTIES:\n:ORG_GLANCE_ID: h1\n:END:\nbody\n")
    (should (= 1 (org-glance-migrate dir)))
    (should (= 0 (org-glance-migrate dir)))     ; already migrated -> nothing new
    (should (= 1 (length (org-glance-graph:headlines (org-glance-graph dir)))))))

(ert-deftest org-glance-test:migrate-progress-survives-restart ()
  "Progress is journaled: a later run (fresh graph = an Emacs restart) skips
already-migrated sources and ingests only newly-appeared ones."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "a.org")
                           "* TODO A\n:PROPERTIES:\n:ORG_GLANCE_ID: a\n:END:\n")
    (should (= 1 (org-glance-migrate dir)))
    ;; restart, and a brand-new source file appears
    (org-glance-test:write (f-join dir "b.org")
                           "* TODO B\n:PROPERTIES:\n:ORG_GLANCE_ID: b\n:END:\n")
    (should (= 1 (org-glance-migrate dir)))     ; only b.org is ingested; a.org skipped
    (let ((graph (org-glance-graph dir)))
      (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "a")))
      (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "b"))))))

(ert-deftest org-glance-test:migrate-keeps-metadata-until-clean-pass ()
  "A skipped source keeps the legacy `.metadata.el' in place (the run is not a
clean success).  A later run -- with the failure gone -- finishes the remaining
file and only then backs the index up; the already-done file is not re-ingested."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "good.org")
                           "* TODO Good\n:PROPERTIES:\n:ORG_GLANCE_ID: good\n:END:\n")
    (org-glance-test:write (f-join dir "bad.org")
                           "* TODO Bad\n:PROPERTIES:\n:ORG_GLANCE_ID: bad\n:END:\n")
    (org-glance-test:write (f-join dir "tag.metadata.el") "#s(hash-table)")
    (let ((orig (symbol-function 'org-glance-graph:add)))
      (cl-letf (((symbol-function 'org-glance-graph:add)
                 (lambda (graph &rest headlines)
                   (if (cl-some (lambda (h) (string= "bad" (org-glance-headline:id h))) headlines)
                       (error "simulated ingest failure")
                     (apply orig graph headlines)))))
        (org-glance-migrate dir)))
    ;; legacy index kept (NOT backed up) because a file was skipped
    (should (f-exists? (f-join dir "tag.metadata.el")))
    (should-not (f-exists? (f-join dir "tag.metadata.el.bak")))
    ;; resume without the failure: finishes bad.org, leaves good.org alone, and
    ;; now the clean pass backs up the index
    (should (= 1 (org-glance-migrate dir)))     ; only bad.org ingested on resume
    (should (f-exists? (f-join dir "tag.metadata.el.bak")))
    (should-not (f-exists? (f-join dir "tag.metadata.el")))
    (let ((graph (org-glance-graph dir)))
      (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "good")))
      (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "bad"))))))

(ert-deftest org-glance-test:migrate-maybe-warns-never-migrates ()
  "When legacy metadata exists, `migrate-maybe' warns once and never prompts,
migrates, or touches the legacy store; it always returns nil."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "foo" "foo.org")
                           "* TODO Hello\n:PROPERTIES:\n:ORG_GLANCE_ID: h1\n:END:\n")
    (org-glance-test:write (f-join dir "foo" "foo.metadata.el") "#s(hash-table)")
    (let ((org-glance-migrate--warned nil)
          (warnings 0))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (error "must not prompt")))
                ((symbol-function 'display-warning)
                 (lambda (&rest _) (cl-incf warnings))))
        (should (null (org-glance-migrate-maybe dir)))
        ;; second init: already warned this session -> no second warning
        (should (null (org-glance-migrate-maybe dir)))
        (should (= 1 warnings))))
    ;; nothing was migrated or renamed; the graph stays empty
    (should (f-exists? (f-join dir "foo" "foo.metadata.el")))
    (should-not (f-exists? (f-join dir "foo" "foo.metadata.el.bak")))
    (should (null (org-glance-graph:get-headline (org-glance-graph dir) "h1")))))

(ert-deftest org-glance-test:migrate-maybe-no-legacy-noop ()
  "With no legacy metadata present, `migrate-maybe' must not warn, prompt, or
error.  Guards `org-glance-init' in the common (already-migrated) case."
  (with-temp-directory dir
    (let ((org-glance-migrate--warned nil))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (error "should not prompt without legacy metadata")))
                ((symbol-function 'display-warning)
                 (lambda (&rest _) (error "should not warn without legacy metadata"))))
        (should (null (org-glance-migrate-maybe dir)))))))

(provide 'test-migrate)
;;; test-migrate.el ends here
