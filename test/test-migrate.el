;;; test-migrate.el --- Tests for legacy v1 -> v2 graph migration  -*- lexical-binding: t -*-

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
    (let* ((graph (org-glance-graph-v2 dir))
           (meta (org-glance-graph-v2:get-headline graph "hello-1")))
      (should (org-glance-headline-metadata-v2? meta))
      (should (string= "Hello" (org-glance-headline-metadata-v2:title meta)))
      (should (member "foo" (append (org-glance-headline-metadata-v2:tags meta) nil)))
      (should (s-contains? "body text" (org-glance-graph-v2:get-content graph "hello-1"))))
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
    (let ((meta (org-glance-graph-v2:get-headline (org-glance-graph-v2 dir) "hello-1")))
      (should (string= "Hello" (org-glance-headline-metadata-v2:title meta)))
      (should (string= "TODO" (org-glance-headline-metadata-v2:state meta))))))

(ert-deftest org-glance-test:migrate-ignores-idless ()
  "Headlines without ORG_GLANCE_ID are not ingested."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "notes.org") "* just a heading\nno id here\n")
    (should (= 0 (org-glance-migrate dir)))
    (should (null (org-glance-graph-v2:headlines (org-glance-graph-v2 dir))))))

(ert-deftest org-glance-test:migrate-maybe-prompts ()
  "When legacy metadata exists and the user confirms, migration runs."
  (with-temp-directory dir
    (org-glance-test:write (f-join dir "foo" "foo.org")
                           "* TODO Hello\n:PROPERTIES:\n:ORG_GLANCE_ID: h1\n:END:\n")
    (org-glance-test:write (f-join dir "foo" "foo.metadata.el") "#s(hash-table)")
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (should (org-glance-migrate-maybe dir)))
    (should (f-exists? (f-join dir "foo" "foo.metadata.el.bak")))
    (should (org-glance-headline-metadata-v2?
             (org-glance-graph-v2:get-headline (org-glance-graph-v2 dir) "h1")))))

(ert-deftest org-glance-test:migrate-maybe-no-legacy-noop ()
  "With no legacy metadata present, `migrate-maybe' must not prompt or error.
This guards `org-glance-init' from hanging in the common (already-migrated) case."
  (with-temp-directory dir
    (cl-letf (((symbol-function 'yes-or-no-p)
               (lambda (&rest _) (error "should not prompt without legacy metadata"))))
      (should (null (org-glance-migrate-maybe dir))))))

(provide 'test-migrate)
;;; test-migrate.el ends here
