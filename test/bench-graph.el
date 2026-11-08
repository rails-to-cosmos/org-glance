;;; bench-graph.el --- Store benchmarks (eask bench)  -*- lexical-binding: t -*-

;;; Commentary:
;; Measure committed UI reads, compatibility reads and store maintenance.

;;; Code:

(require 'bench-common)
(require 'benchmark)

(defconst org-glance-bench:sizes
  (if-let* ((sizes (getenv "OG_BENCH_SIZES")))
      (mapcar (lambda (value)
                (org-glance-bench:positive-integer (string-trim value)
                                                   :name "OG_BENCH_SIZES"))
              (split-string sizes ","))
    (if (getenv "OG_BENCH_QUICK") '(1000)
      (append '(1000 10000) (when (getenv "OG_BENCH_LARGE") '(100000)))))
  "Graph sizes; QUICK limits to 1k and LARGE adds an opt-in 100k tier.")

(defconst org-glance-bench:batch 500
  "Headlines per build batch; small enough that insert timings include seals.")

(defconst org-glance-bench:sample (if (getenv "OG_BENCH_QUICK") 100 1000)
  "Random-id sample size for the warm `get-headline' benchmark.")

(cl-defun org-glance-bench:headline (i)
  "Return synthetic headline I with a drawer property and a linked body line."
  (org-glance-test:headline-props
   (format "bench-%06d" i)
   (format "* TODO Benchmark headline %d :bench:" i)
   `(("KIND" . ,(format "kind-%d" (mod i 7))))
   (format "Body line for %d with a [[https://example.com/%d][link]]." i i)))

(cl-defun org-glance-bench:build (graph n)
  "Fill GRAPH with N synthetic headlines in `org-glance-bench:batch' batches.
Return the elapsed seconds."
  (car (benchmark-run 1
         (cl-loop for start from 0 below n by org-glance-bench:batch
                  do (apply #'org-glance-graph:add graph
                            (cl-loop for i from start
                                     below (min n (+ start org-glance-bench:batch))
                                     collect (org-glance-bench:headline i)))))))

(cl-defun org-glance-bench--segments (graph)
  "Return GRAPH's segment count, sealed plus the open one."
  (1+ (length (org-glance-graph--sealed-segments graph))))

(cl-defun org-glance-bench:run ()
  "Measure the graph tiers through committed UI and compatibility readers."
  (let ((inhibit-message t) (message-log-max nil))
    (dolist (n org-glance-bench:sizes)
      (org-glance-test:with-graph graph
        (let ((org-glance-bench--size n))
          (org-glance-bench:measure
           "graph" "insert (batched)" (lambda () (org-glance-bench:build graph n))
           :samples 1 :counters (lambda (_result)
                                 (list :segments (org-glance-bench--segments graph))))
          (org-glance-bench:measure
           "graph" "cold compatibility fold"
           (lambda () (org-glance-graph--invalidate-cache graph)
             (org-glance-graph:headlines graph))
           :samples 1 :fingerprint #'org-glance-bench:fingerprint)
          (let ((ids (cl-loop for index below org-glance-bench:sample
                              collect (format "bench-%06d" (mod (* index 37) n)))))
            (dolist (reader '(org-glance-graph:live-meta org-glance-graph:get-headline))
              (funcall reader graph (car ids))
              (org-glance-bench:measure
               "graph" (format "%s x%d" reader (length ids))
               (lambda () (mapcar (lambda (id) (funcall reader graph id)) ids))
               :fingerprint #'org-glance-bench:fingerprint
               :counters (lambda (_result) (list :lookups (length ids))))))
          (org-glance-bench:measure
           "graph" "property-index cold fill"
           (lambda ()
             (org-glance-property-index:clear graph)
             (org-glance-property-index:ensure
              graph (cl-loop for index below n collect (format "bench-%06d" index))))
           :samples 1)
          (org-glance-bench:measure
           "graph" "overview full refresh" (lambda () (org-glance-overview:write graph))
           :fingerprint (lambda (file) (org-glance-bench:fingerprint (f-read-text file 'utf-8))))
          (org-glance-bench:measure
           "graph" "overview one-row refresh"
           (lambda () (org-glance-overview:update graph nil '("bench-000000")))
           :fingerprint #'org-glance-bench:fingerprint)
          (org-glance-bench:measure
           "graph" "reindex" (lambda () (org-glance-graph:reindex graph)) :samples 1)
          (let (input-records)
            (org-glance-bench:measure
             "graph" "compact superseded record"
             (lambda () (org-glance-graph:compact graph)) :samples 1
             :before (lambda ()
                       (org-glance-graph:insert
                        graph (list (org-glance-graph:live-meta graph "bench-000000")))
                       (setq input-records (cdr (org-glance-graph--latest-records graph))))
             :counters (lambda (result)
                         (list :input-records input-records :live-records result
                               :segments (org-glance-bench--segments graph))))))))))

(provide 'bench-graph)
;;; bench-graph.el ends here
