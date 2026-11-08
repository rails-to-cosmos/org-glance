;;; test-perf.el --- Timing contracts -*- lexical-binding: t; -*-

(require 'test-helpers)
(require 'org-glance-perf)

(ert-deftest org-glance-test:perf-preserves-results-errors-and-quits ()
  "Tracing preserves command behavior and reports aborted operations."
  (let (messages)
    (cl-letf (((symbol-function 'message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages))))
      (should (eq 'value (org-glance-perf--command 'sample (lambda () 'value))))
      (should-error (org-glance-perf--command 'sample (lambda () (error "test"))))
      (should (eq 'quit
                  (condition-case nil
                      (org-glance-perf--command 'sample (lambda () (signal 'quit nil)))
                    (quit 'quit))))
      (should (= 2 (cl-count-if (lambda (text) (string-match-p "aborted:" text))
                               messages)))
      (should-not org-glance-perf--trace))))

(ert-deftest org-glance-test:perf-mode-is-reversible-and-loads-plugins-lazily ()
  "Enabling twice installs one advice; disabling removes every installed advice."
  (unwind-protect
      (let ((plugin-loaded (featurep 'org-glance-llm)))
        (org-glance-perf-mode 1)
        (org-glance-perf-mode 1)
        (should (eq plugin-loaded (featurep 'org-glance-llm)))
        (should (= 1 (cl-count 'org-glance-materialize org-glance-perf--advices
                               :key #'car)))
        (dolist (entry org-glance-perf--advices)
          (should (advice-member-p (cdr entry) (car entry))))
        (let ((installed (copy-sequence org-glance-perf--advices)))
          (org-glance-perf-mode -1)
          (dolist (entry installed)
            (should-not (advice-member-p (cdr entry) (car entry))))))
    (org-glance-perf-mode -1)))

(ert-deftest org-glance-test:perf-separates-prompt-time-and-aggregates-stages ()
  "Report nested stage counts and minibuffer interaction separately from work."
  (let ((clock 0.0) messages)
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) clock))
              ((symbol-function 'message)
               (lambda (format &rest args)
                 (push (apply #'format format args) messages))))
      (org-glance-perf--command
       'sample
       (lambda ()
         (dotimes (_ 2)
           (org-glance-perf--stage 'cache (lambda () (cl-incf clock 0.1))))
         (org-glance-perf--stage 'read-from-minibuffer
                                 (lambda () (cl-incf clock 3.0)))))
      (should (cl-some (lambda (text) (string-match-p "cache: 200.0 ms / 2 calls" text))
                       messages))
      (should (cl-some (lambda (text)
                         (string-match-p "3200.0 ms; prompt 3000.0; work 200.0" text))
                       messages)))))

(provide 'test-perf)
;;; test-perf.el ends here
