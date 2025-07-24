(package "org-glance" "0.0.1" "Org-mode mindmap.")

(website-url "https://github.com/rails-to-cosmos/org-glance")
(keywords "org-mode" "graph")

(package-file "org-glance.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")
(source "org")

(depends-on "emacs" "29.1")
(depends-on "org")
(depends-on "aes")
(depends-on "dash")
(depends-on "f")
(depends-on "ht")
(depends-on "highlight")
(depends-on "transient")
(depends-on "elsa")
(depends-on "ert-runner" "20231110.1358")

(eask-defcommand test
  "Run a configured ert test suite."
  (setq ert-batch-backtrace-right-margin 1200
        ert-batch-print-length 20
        ert-batch-print-level 10)
  (load "test/test-helpers.el")
  (load "test/test-graph.el")
  (load "test/test-capture.el")
  ;; (load "test/test-headline.el")
  (ert-run-tests-batch-and-exit))
