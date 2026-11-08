;;; bench-runner.el --- Named performance harnesses -*- lexical-binding: t; -*-

(require 'bench-common)
(require 'bench-graph)

(defconst org-glance-bench:harnesses
  '((graph "bench-graph" org-glance-bench:run)
    (material "bench-material" org-glance-material-bench:run)
    (interactive "bench-interactive" org-glance-interactive-bench:run)
    (readers "bench-cache-readers" org-glance-reader-bench:run))
  "Benchmark names, libraries and entry points.")

(defun org-glance-bench:dispatch (&optional name directory)
  "Run named harness NAME, or all harnesses, with optional private DIRECTORY.
DIRECTORY applies to the interactive and readers harnesses."
  (let ((name (or name "all")) (inhibit-message t) (message-log-max nil))
    (dolist (entry (if (equal name "all") org-glance-bench:harnesses
                    (list (or (assq (intern name) org-glance-bench:harnesses)
                              (error "Unknown benchmark %s" name)))))
      (require (intern (cadr entry)))
      (if (memq (car entry) '(interactive readers))
          (funcall (nth 2 entry) directory)
        (funcall (nth 2 entry))))))

(provide 'bench-runner)
;;; bench-runner.el ends here
