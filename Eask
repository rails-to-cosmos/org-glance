(require 'package)
(require 'json)

(let ((desc (with-temp-buffer
              (insert-file-contents "org-glance.el")
              (package-buffer-info))))
  (package (symbol-name (package-desc-name desc))
           (package-version-join (package-desc-version desc))
           (package-desc-summary desc))
  (dolist (requirement (package-desc-reqs desc))
    (depends-on (symbol-name (car requirement))
                (package-version-join (cadr requirement)))))

(website-url "https://github.com/rails-to-cosmos/org-glance")
(keywords "org-mode" "outlines" "data" "database" "store" "projections")

(package-file "org-glance.el")
(let* ((json-object-type 'alist)
       (json-key-type 'string)
       (json-array-type 'list)
       (spec (car (json-read-file "packages.json"))))
  (apply #'files (mapcar #'car (cdr (assoc "files" spec)))))
(load-paths "./src/data" "./src/view")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")
(source "rails-to-cosmos" "https://rails-to-cosmos.github.io/elpa/")

;; Dev-only: `eask lint package' / `eask lint regexps' (make lint); never in
;; the shipped Package-Requires.  Plugins (e.g. `org-glance-llm') are
;; separate packages with their own repos -- core depends on none of them.
(development
 (depends-on "package-lint")
 (depends-on "relint"))

(eask-defcommand bench
  "Time the store's hot paths over synthetic graphs (see test/bench-graph.el)."
  (load "test/test-helpers.el")
  (load "test/bench-graph.el")
  (org-glance-bench:run))

(eask-defcommand test
  "Run a configured ert test suite."
  (setq ert-batch-backtrace-right-margin 1200
        ert-batch-print-length 20
        ert-batch-print-level 10)
  (load "test/test-helpers.el")
  (load "test/test-headline.el")
  (load "test/test-graph.el")
  (load "test/test-version.el")
  (load "test/test-cache.el")
  (load "test/test-checkpoint.el")
  (load "test/test-external.el")
  (load "test/test-filter.el")
  (load "test/test-segments.el")
  (load "test/test-merge.el")
  (load "test/test-capture.el")
  (load "test/test-tag-config.el")
  (load "test/test-config-file.el")
  (load "test/test-config.el")
  (load "test/test-property-index.el")
  (load "test/test-relations.el")
  (load "test/test-migrate.el")
  (load "test/test-material.el")
  (load "test/test-overview.el")
  (load "test/test-table.el")
  (load "test/test-table-config.el")
  (load "test/test-settings.el")
  (load "test/test-tags.el")
  (load "test/test-ui.el")
  (load "test/test-plugins.el")
  (ert-run-tests-batch-and-exit))
