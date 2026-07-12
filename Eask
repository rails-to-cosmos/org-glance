(package "org-glance" "0.2.1" "Org-mode mindmap.")

(website-url "https://github.com/rails-to-cosmos/org-glance")
(keywords "org-mode" "graph" "mindmap")

(package-file "org-glance.el")
(files "*.el" "src/data/*.el" "src/view/*.el")
(load-paths "./src/data" "./src/view")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")
(source "org")

(depends-on "emacs" "29.1")
(depends-on "org")
(depends-on "aes")
(depends-on "dash")
(depends-on "f")
(depends-on "s")
(depends-on "transient")
(depends-on "table-view"
            :fetcher 'github
            :repo "rails-to-cosmos/table-view"
            :commit "21af2737999bd3c8ea1f2f268c34be49113fb1a0")

(eask-defcommand test
  "Run a configured ert test suite."
  (setq ert-batch-backtrace-right-margin 1200
        ert-batch-print-length 20
        ert-batch-print-level 10)
  (load "test/test-helpers.el")
  (load "test/test-headline.el")
  (load "test/test-graph.el")
  (load "test/test-filter.el")
  (load "test/test-segments.el")
  (load "test/test-merge.el")
  (load "test/test-capture.el")
  (load "test/test-tag-config.el")
  (load "test/test-migrate.el")
  (load "test/test-material.el")
  (load "test/test-overview.el")
  (load "test/test-table.el")
  (load "test/test-tags.el")
  (load "test/test-ui.el")
  (ert-run-tests-batch-and-exit))
