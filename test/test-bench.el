;;; test-bench.el --- Benchmark protocol contracts -*- lexical-binding: t; -*-

(require 'test-helpers)
(add-to-list 'load-path (expand-file-name "test" org-glance-test--root))
(require 'bench-runner)

(ert-deftest org-glance-test:bench-fingerprint-checks-every-round ()
  "Refuse a transient second-round result change before a matching final round."
  (let ((round 0))
    (should-error
     (org-glance-bench:measure
      "sample" "changing" (lambda () (cl-incf round)) :samples 3
      :fingerprint (lambda (value) (if (= value 2) "changed" "initial"))))
    (should (= round 2))))

(ert-deftest org-glance-test:bench-measures-only-work-and-counts-iterations ()
  "Exclude preparation and observers while preserving samples and work counts."
  (let ((clock 0.0) (calls 0) (prepared 0) (fingerprints 0) (counted 0) record)
    (cl-letf (((symbol-function 'float-time) (lambda (&rest _) clock))
              ((symbol-function 'garbage-collect) (lambda () (cl-incf clock 50)))
              ((symbol-function 'org-glance-bench:emit)
               (lambda (_harness _operation value) (setq record value))))
      (org-glance-bench:measure
       "sample" "timed" (lambda () (cl-incf calls) (cl-incf clock 0.1))
       :samples 3 :iterations 2
       :before (lambda () (cl-incf prepared) (cl-incf clock 100))
       :fingerprint (lambda (_result)
                      (cl-incf fingerprints) (cl-incf clock 200) "stable")
       :counters (lambda (_result)
                   (cl-incf counted) (cl-incf clock 300) (list :calls calls)))
      (should (= calls 6))
      (should (= prepared 3))
      (should (= fingerprints 3))
      (should (= counted 3))
      (should (= (length (plist-get record :samples-ms)) 3))
      (should (< (abs (- 100 (plist-get record :median-ms))) 0.0001))
      (should (equal [(:calls 2) (:calls 4) (:calls 6)] (plist-get record :work))))))

(ert-deftest org-glance-test:bench-rejects-empty-workloads-and-unknown-harnesses ()
  "Reject invalid sample counts, iteration counts and unregistered names."
  (should-error (org-glance-bench:measure "sample" "none" #'ignore :samples 0))
  (should-error (org-glance-bench:measure "sample" "none" #'ignore :iterations 0))
  (should-error (org-glance-bench:dispatch "unknown")))

(ert-deftest org-glance-test:bench-all-harnesses-use-disposable-stores ()
  "Run all four real harnesses and inspect their machine-readable work records."
  (let ((org-glance-bench:sizes '(3))
        (org-glance-bench:sample 2)
        (process-environment (copy-sequence process-environment)))
    (setenv "OG_BENCH_QUICK" "1")
    (setenv "OG_BENCH_SAMPLES" "3")
    (let* ((output (with-output-to-string (org-glance-bench:dispatch)))
           (records (cl-loop for line in (split-string output "\n" t)
                             when (string-prefix-p "OG_BENCH_JSON " line)
                             collect (json-parse-string (substring line 14) :object-type 'plist)))
           (names (delete-dups (mapcar (lambda (record) (plist-get record :harness)) records)))
           (interactive (cl-find "interactive" records :key (lambda (record) (plist-get record :harness))
                                 :test #'equal))
           (reader (cl-find "readers" records :key (lambda (record) (plist-get record :harness))
                            :test #'equal)))
      (should (equal '("graph" "interactive" "material" "readers") (sort names #'string<)))
      (let* ((compact (cl-find "compact superseded record" records
                               :key (lambda (record) (plist-get record :operation))
                               :test #'equal))
             (work (plist-get compact :work)))
        (mapc (lambda (sample)
                (should (= 4 (plist-get sample :input-records)))
                (should (= 3 (plist-get sample :live-records)))) work))
      (dolist (record (cl-remove-if-not
                      (lambda (record) (equal "graph" (plist-get record :harness))) records))
        (dolist (field '(:samples-ms :gc-counts :gc-ms :work))
          (should (= 3 (length (plist-get record field)))))
        (should (stringp (plist-get record :fingerprint)))
        (mapc
         (lambda (work)
           (should work)
           (pcase (plist-get record :operation)
             ("insert (batched)" (should (= 3 (plist-get work :records))))
             ("property-index cold fill"
              (should (= 3 (plist-get work :blob-reads)))
              (should (= 3 (plist-get work :indexed-headlines))))
             ("reindex"
              (should (= 3 (plist-get work :blob-reads)))
              (should (= 3 (plist-get work :headlines))))))
         (plist-get record :work)))
      (dolist (record (cl-remove-if-not
                      (lambda (record) (equal "material" (plist-get record :harness))) records))
        (should (= (if (string-prefix-p "save " (plist-get record :operation)) 6 0)
                   (plist-get (aref (plist-get record :work) 0) :family-observations))))
      (should (= 3 (plist-get (aref (plist-get interactive :work) 0) :candidates)))
      (should (= 0 (plist-get (aref (plist-get reader :work) 0) :opens)))
      (should (= 100 (plist-get (aref (plist-get reader :work) 0) :deserializations))))))

(ert-deftest org-glance-test:bench-driver-comparison-contracts ()
  "Run the Python comparison contracts from the regular ERT gate."
  (with-temp-buffer
    (should (eq 0 (call-process
                   "python3" nil t nil
                   (expand-file-name "test/test-bench-driver.py" org-glance-test--root))))))

(ert-deftest org-glance-test:bench-profile-counts-are-positive-integers ()
  "Reject malformed profile values."
  (dolist (invalid '("0" "-1" "1.5" "7suffix" ""))
    (should-error (org-glance-bench:positive-integer invalid)))
  (should (= 7 (org-glance-bench:positive-integer "7"))))

(provide 'test-bench)
;;; test-bench.el ends here
