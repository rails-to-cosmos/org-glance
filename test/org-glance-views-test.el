(condition-case nil  ;; for interactive usage
    (load-file "test-helper.el")
  (error nil))

(ert-deftest org-glance-test/should-visit-headlines ()
  (message "")
  (message "All views should support visiting its headlines.")
  (message "Will use %s path for visiting." org-glance-test/test-resources-path)
  (message "Creating view Country...")
  (org-glance-def-view 'Country
                       :scope (list (f-join org-glance-test/test-resources-path "countries.org")))
  (org-glance-reread-view 'Country)
  (with-simulated-input "[Country] SPC Belgium RET"
    (org-glance-action-visit)
    (should (string= (org-entry-title) "Belgium")))
  (org-glance-view-delete 'Country))

(ert-deftest org-glance-test/should-visit-headlines-with-priority ()
  (message "")
  (message "All views should support visiting its headlines and must not break if headline contains priority.")
  (message "Will use %s path for visiting." org-glance-test/test-resources-path)
  (message "Creating view Region...")
  (org-glance-def-view 'Region
                       :scope (list (f-join org-glance-test/test-resources-path "countries.org")))
  (org-glance-reread-view 'Region)
  (with-simulated-input "[Region] SPC Northwestern RET"
    (org-glance-action-visit)
    (should (string= (org-entry-title) "Northwestern")))
  (org-glance-view-delete 'Region))

(ert-deftest org-glance-test/should-visit-todos-with-priority ()
  (message "")
  (message "All views should support visiting its headlines and must not break if headline contains priority.")
  (message "Will use %s path for visiting." org-glance-test/test-resources-path)
  (message "Creating view Region...")
  (org-glance-def-view 'Task
                       :scope (list (f-join org-glance-test/test-resources-path "tasks.org")))
  (org-glance-reread-view 'Task)
  (with-simulated-input "[Task] SPC Raise SPC a SPC daughter RET"
    (org-glance-action-visit)
    (should (string= (org-entry-title) "Raise a daughter")))
  (org-glance-view-delete 'Task))
