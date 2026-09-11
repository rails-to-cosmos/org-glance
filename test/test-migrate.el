;;; test-migrate.el --- Tests for legacy v1 metadata migration  -*- lexical-binding: t -*-

(require 'test-helpers)

(ert-deftest org-glance-test:migrate-detect ()
  "Legacy .metadata.el files are detected; backed-up .bak files are not."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "foo" "foo.metadata.el") "#s(hash-table)")
    (org-glance-test:write (f-join dir "bar" "bar.metadata.el.bak") "#s(hash-table)")
    (let ((found (org-glance-legacy-metadata-files dir)))
      (should (= 1 (length found)))
      (should (s-ends-with? "foo.metadata.el" (car found))))))

(ert-deftest org-glance-test:migrate ()
  "Migration ingests id-bearing headlines intact and backs up legacy metadata."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "foo" "foo.org")
                           (org-glance-test:org-with-id "* TODO Hello :foo:" "hello-1" "body text"))
    (org-glance-test:write (f-join dir "foo" "foo.metadata.el") "#s(hash-table)")
    (should (= 1 (org-glance-migrate dir)))
    (let* ((graph (org-glance-graph dir))
           (meta (org-glance-graph:get-headline graph "hello-1")))
      (should (org-glance-headline-metadata? meta))
      (should (string= "Hello" (org-glance-headline-metadata:title meta)))
      (should (member "foo" (org-glance-headline-metadata:tag-strings meta)))
      (should (s-contains? "body text" (org-glance-graph:get-content graph "hello-1"))))
    (should (f-exists? (f-join dir "foo" "foo.metadata.el.bak")))
    (should-not (f-exists? (f-join dir "foo" "foo.metadata.el")))
    (should (null (org-glance-legacy-metadata-files dir)))))

(ert-deftest org-glance-test:migrate-skips-overview ()
  "Overview clones (sharing an id) must not override the canonical source."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "foo" "foo.org")
                           (org-glance-test:org-with-id "* TODO Hello :foo:" "hello-1"))
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
  "A file that errors during ingest is logged and skipped; the rest migrate."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "good.org")
                           (org-glance-test:org-with-id "* TODO Good" "good"))
    (org-glance-test:write (f-join dir "bad.org")
                           (org-glance-test:org-with-id "* TODO Bad" "bad"))
    (org-glance-test:with-failing-ingest "bad"
      (org-glance-migrate dir))
    (let ((graph (org-glance-graph dir)))
      (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "good")))
      (should (null (org-glance-graph:get-headline graph "bad"))))))

(ert-deftest org-glance-test:migrate-idempotent ()
  "A second migration run ingests 0 and appends no duplicate record."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "foo" "foo.org")
                           (org-glance-test:org-with-id "* TODO Hello :foo:" "h1" "body"))
    (should (= 1 (org-glance-migrate dir)))
    (should (= 0 (org-glance-migrate dir)))     ; already migrated -> nothing new
    (should (= 1 (length (org-glance-graph:headlines (org-glance-graph dir)))))))

(ert-deftest org-glance-test:migrate-progress-survives-restart ()
  "Journaled progress makes a post-restart run ingest only new sources."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "a.org")
                           (org-glance-test:org-with-id "* TODO A" "a"))
    (should (= 1 (org-glance-migrate dir)))
    (org-glance-test:write (f-join dir "b.org")
                           (org-glance-test:org-with-id "* TODO B" "b"))
    (should (= 1 (org-glance-migrate dir)))     ; only b.org is ingested; a.org skipped
    (let ((graph (org-glance-graph dir)))
      (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "a")))
      (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "b"))))))

(ert-deftest org-glance-test:migrate-keeps-metadata-until-clean-pass ()
  "A skipped source keeps the legacy `.metadata.el' in place.
A later clean run ingests only the remainder, then backs the index up."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "good.org")
                           (org-glance-test:org-with-id "* TODO Good" "good"))
    (org-glance-test:write (f-join dir "bad.org")
                           (org-glance-test:org-with-id "* TODO Bad" "bad"))
    (org-glance-test:write (f-join dir "tag.metadata.el") "#s(hash-table)")
    (org-glance-test:with-failing-ingest "bad"
      (org-glance-migrate dir))
    (should (f-exists? (f-join dir "tag.metadata.el")))
    (should-not (f-exists? (f-join dir "tag.metadata.el.bak")))
    (should (= 1 (org-glance-migrate dir)))     ; only bad.org ingested on resume
    (should (f-exists? (f-join dir "tag.metadata.el.bak")))
    (should-not (f-exists? (f-join dir "tag.metadata.el")))
    (let ((graph (org-glance-graph dir)))
      (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "good")))
      (should (org-glance-headline-metadata? (org-glance-graph:get-headline graph "bad"))))))

(ert-deftest org-glance-test:migrate-maybe-warns-never-migrates ()
  "With legacy metadata `migrate-maybe' warns once and returns nil.
It never prompts, migrates or touches the legacy store."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "foo" "foo.org")
                           (org-glance-test:org-with-id "* TODO Hello" "h1"))
    (org-glance-test:write (f-join dir "foo" "foo.metadata.el") "#s(hash-table)")
    (let ((org-glance-migrate--warned nil)
          (warnings 0))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (error "must not prompt")))
                ((symbol-function 'display-warning)
                 (lambda (&rest _) (cl-incf warnings))))
        (should (null (org-glance-migrate-maybe dir)))
        (should (null (org-glance-migrate-maybe dir)))
        (should (= 1 warnings))))
    (should (f-exists? (f-join dir "foo" "foo.metadata.el")))
    (should-not (f-exists? (f-join dir "foo" "foo.metadata.el.bak")))
    (should (null (org-glance-graph:get-headline (org-glance-graph dir) "h1")))))

(ert-deftest org-glance-test:migrate-maybe-no-legacy-noop ()
  "Without legacy metadata `migrate-maybe' never warns, prompts or errors."
  (with-temp-directory dir
    (let ((org-glance-migrate--warned nil))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) (error "should not prompt without legacy metadata")))
                ((symbol-function 'display-warning)
                 (lambda (&rest _) (error "should not warn without legacy metadata"))))
        (should (null (org-glance-migrate-maybe dir)))))))

(provide 'test-migrate)
;;; test-migrate.el ends here
