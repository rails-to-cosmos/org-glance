(condition-case nil  ;; for interactive usage
    (load-file "test-helper.el")
  (error nil))

(ert-deftest org-glance-test/should-visit-views ()
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
  (org-glance-remove-view 'Country))
