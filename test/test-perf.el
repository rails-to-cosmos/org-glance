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

(ert-deftest org-glance-test:perf-start-report-is-outside-the-measurement ()
  "Exclude start reporting time and GC from the command's measurements."
  (let ((clock 0.0) (gc-elapsed 0.0) (gcs-done 0)
        (gc-cons-threshold most-positive-fixnum) messages)
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) clock))
              ((symbol-function 'message)
               (lambda (format &rest args)
                 (let ((text (apply #'format format args)))
                   (cl-incf clock 2.0) (cl-incf gc-elapsed 1.0) (cl-incf gcs-done 5)
                   (push text messages)))))
      (org-glance-perf--command
       'sample (lambda ()
                 (org-glance-perf--stage
                  'work (lambda () (cl-incf clock 0.25)
                          (cl-incf gc-elapsed 0.1) (cl-incf gcs-done)))))
      (should (cl-some (lambda (text)
                         (and (string-search "returned: 250.0 ms" text)
                              (string-search "GC 100.0 ms / 1" text)))
                       messages)))))

(ert-deftest org-glance-test:perf-live-streams-stages-outside-the-measurement ()
  "Live mode reports nested stages as they return, excluding its output time."
  (let ((clock 0.0) (org-glance-perf-live t) messages)
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) clock))
              ((symbol-function 'message)
               (lambda (format &rest args)
                 (let ((text (apply #'format format args)))
                   (when (string-search " live " text) (cl-incf clock 1.0))
                   (push text messages)))))
      (org-glance-perf--command
       'sample
       (lambda ()
         (org-glance-perf--stage
          'outer (lambda ()
                   (cl-incf clock 0.1)
                   (org-glance-perf--stage
                    'inner (lambda () (cl-incf clock 0.2)))))))
      (let ((live (seq-filter (lambda (text) (string-search " live " text))
                              (reverse messages))))
        (should (equal '("live   inner: 200.0 ms" "live outer: 300.0 ms")
                       (mapcar (lambda (text)
                                 (substring text (string-search "live" text)))
                               live))))
      (should (cl-some (lambda (text) (string-search "returned: 300.0 ms" text))
                       messages)))))

(ert-deftest org-glance-test:perf-registration-is-idempotent-and-lazy ()
  "Trace a registered command and a stage defined by a subsequently loaded file."
  (let ((org-glance-perf-commands nil) (org-glance-perf-stages nil)
        (org-glance-perf-continuations nil) messages)
    (org-glance-perf-mode -1)
    (unwind-protect
        (with-temp-directory directory
          (let ((file (expand-file-name "late.el" directory)))
            (with-temp-file file
              (insert "(defun org-glance-perf-test-stage () 'payload)\n"))
            (cl-letf (((symbol-function 'org-glance-perf-test-command)
                       (lambda () (list (funcall 'org-glance-perf-test-stage)
                                        (funcall 'org-glance-perf-test-stage))))
                      ((symbol-function 'message)
                       (lambda (format &rest args)
                         (push (apply #'format format args) messages))))
              (dotimes (_ 2)
                (org-glance-perf:register
                 :commands '(org-glance-perf-test-command)
                 :stages '(org-glance-perf-test-stage)
                 :continuations '(org-glance-perf-test-command)))
              (should (= 1 (length org-glance-perf-commands)))
              (should (= 1 (length org-glance-perf-stages)))
              (should (= 1 (length org-glance-perf-continuations)))
              (should-not (fboundp 'org-glance-perf-test-stage))
              (org-glance-perf-mode 1)
              (load file nil t)
              (should (equal '(payload payload) (funcall 'org-glance-perf-test-command)))
              (should (cl-some (lambda (text)
                                 (and (string-search "org-glance-perf-test-stage:" text)
                                      (string-search "/ 2 calls" text)))
                               messages)))))
      (org-glance-perf-mode -1)
      (fmakunbound 'org-glance-perf-test-stage))))

(ert-deftest org-glance-test:perf-invalid-registration-is-atomic ()
  "Reject invalid symbols before appending any timing option."
  (let ((org-glance-perf-commands nil) (org-glance-perf-stages nil)
        (org-glance-perf-continuations nil))
    (should-error (org-glance-perf:register :commands '(valid) :stages '((lambda () t))))
    (should-not org-glance-perf-commands)
    (should-not org-glance-perf-stages)))

(ert-deftest org-glance-test:perf-custom-options-refresh-enabled-advice ()
  "Apply changed command options immediately while the mode is enabled."
  (let ((org-glance-perf-commands '(org-glance-perf-test-old))
        (org-glance-perf-stages nil) (org-glance-perf-continuations nil))
    (cl-letf (((symbol-function 'org-glance-perf-test-old) #'ignore)
              ((symbol-function 'org-glance-perf-test-new) #'ignore))
      (unwind-protect
          (progn
            (org-glance-perf-mode 1)
            (should (assq 'org-glance-perf-test-old org-glance-perf--advices))
            (org-glance-perf--set-functions 'org-glance-perf-commands '(org-glance-perf-test-new))
            (should-not (assq 'org-glance-perf-test-old org-glance-perf--advices))
            (should (assq 'org-glance-perf-test-new org-glance-perf--advices)))
        (org-glance-perf-mode -1)))))

(provide 'test-perf)
;;; test-perf.el ends here
