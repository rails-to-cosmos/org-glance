;;; bench-common.el --- Shared benchmark protocol -*- lexical-binding: t; -*-

(require 'test-helpers)
(require 'benchmark)

(defvar org-glance-bench--size 0)
(defvar org-glance-bench:sizes)
(declare-function org-glance-bench:build "bench-graph" (graph n))

(cl-defun org-glance-bench:positive-integer (text &key (name "sample count"))
  "Return positive decimal TEXT as an integer, or reject parameter NAME."
  (unless (and (stringp text) (string-match-p "\\`[1-9][0-9]*\\'" text))
    (error "%s must be a positive decimal integer" name))
  (string-to-number text))

(defun org-glance-bench:rounds ()
  "Return the positive sample count selected by OG_BENCH_SAMPLES."
  (org-glance-bench:positive-integer
   (or (getenv "OG_BENCH_SAMPLES") "7") :name "OG_BENCH_SAMPLES"))

(defun org-glance-bench:fingerprint (value)
  "Return a deterministic digest of benchmark VALUE."
  (secure-hash 'sha256 (prin1-to-string value)))

(defun org-glance-bench:emit (harness operation record)
  "Print HARNESS and OPERATION's measurement RECORD as text and JSON."
  (let ((record (append (list :harness harness :operation operation
                              :size org-glance-bench--size :emacs emacs-version)
                        record)))
    (princ (format "%s / %s N=%d: %.3f ms\n" harness operation
                   org-glance-bench--size (plist-get record :median-ms)))
    (princ (concat "OG_BENCH_JSON " (json-serialize record) "\n"))
    record))

(cl-defun org-glance-bench:measure
    (harness operation thunk &key (samples (org-glance-bench:rounds))
             (iterations 1) before fingerprint counters)
  "Measure THUNK for HARNESS and OPERATION across SAMPLES of ITERATIONS.
Call BEFORE before collection, and FINGERPRINT and COUNTERS after timing.
Refuse a changed fingerprint in any round.  Return the emitted record."
  (unless (and (> samples 0) (> iterations 0))
    (error "Benchmark samples and iterations must be positive"))
  (let ((initial (make-symbol "unset")) expected times gc-counts gc-times work)
    (setq expected initial)
    (dotimes (_ samples)
      (when before (funcall before))
      (garbage-collect)
      (let ((start-gcs gcs-done) (start-gc gc-elapsed)
            (start (float-time)) result)
        (dotimes (_ iterations) (setq result (funcall thunk)))
        (push (* 1000 (/ (- (float-time) start) iterations)) times)
        (push (- gcs-done start-gcs) gc-counts)
        (push (* 1000 (- gc-elapsed start-gc)) gc-times)
        (when fingerprint
          (let ((actual (funcall fingerprint result)))
            (if (eq expected initial) (setq expected actual)
              (unless (equal expected actual)
                (error "%s/%s result changed within the run" harness operation)))))
        (when counters (push (funcall counters result) work))))
    (org-glance-bench:emit
     harness operation
     (list :median-ms (nth (/ samples 2) (sort (copy-sequence times) #'<))
           :samples-ms (vconcat (nreverse times)) :iterations iterations
           :gc-counts (vconcat (nreverse gc-counts))
           :gc-ms (vconcat (nreverse gc-times))
           :work (vconcat (nreverse work))
           :fingerprint (unless (eq expected initial) expected)))))

(defun org-glance-bench:with-store (directory function)
  "Call FUNCTION on DIRECTORY's committed cache or disposable synthetic stores."
  (require 'bench-graph)
  (if directory
      (let* ((graph (make-org-glance-graph :directory (file-truename directory)))
             (org-glance-bench--size (length (org-glance-cache:headlines graph))))
        (unwind-protect
            (progn
              (unless (org-glance-cache--read-db graph (lambda (_db) t) t)
                (error "Benchmark needs a compatible committed cache in %s" directory))
              (funcall function graph))
          (when (fboundp 'org-glance-cache:close-readers)
            (org-glance-cache:close-readers graph))))
    (dolist (size org-glance-bench:sizes)
      (org-glance-test:with-graph graph
        (let ((org-glance-bench--size size))
          (org-glance-bench:build graph size)
          (funcall function graph))))))

(provide 'bench-common)
;;; bench-common.el ends here
