;;; bench-graph.el --- Store benchmarks (eask bench)  -*- lexical-binding: t -*-

;;; Commentary:
;; Times the store's hot paths over synthetic graphs so the seal/compact
;; knobs (`org-glance-graph-segment-max-bytes',
;; `org-glance-graph-compact-segment-count') get a curve instead of a guess,
;; and every performance change gets a before/after number.  Reuses the test
;; fixtures; `benchmark' ships with Emacs -- no new dependency.
;;
;; Run with `eask bench' (or `make bench').  Default N: 1000 and 10000;
;; set OG_BENCH_LARGE=1 for the 100000 tier.  Non-assertive by design --
;; never wired into `eask test'.

;;; Code:

(require 'test-helpers)
(require 'benchmark)

(defconst org-glance-bench:sizes
  (append '(1000 10000)
          (when (getenv "OG_BENCH_LARGE") '(100000)))
  "Graph sizes to benchmark; the 100k tier is opt-in (OG_BENCH_LARGE=1).")

(defconst org-glance-bench:batch 500
  "Headlines per `org-glance-graph:add' batch while building.
Small enough that a build crosses `org-glance-graph-segment-max-bytes'
repeatedly, so insert timings include the seal path.")

(defconst org-glance-bench:sample 1000
  "Random-id sample size for the warm `get-headline' benchmark.")

(cl-defun org-glance-bench--headline (i)
  "A synthetic headline I with a drawer property and a body line."
  (org-glance-test:headline-props
   (format "bench-%06d" i)
   (format "* TODO Benchmark headline %d :bench:" i)
   `(("KIND" . ,(format "kind-%d" (mod i 7))))
   (format "Body line for %d with a [[https://example.com/%d][link]]." i i)))

(cl-defun org-glance-bench--build (graph n)
  "Fill GRAPH with N synthetic headlines in `org-glance-bench:batch' batches.
Return the elapsed seconds."
  (car (benchmark-run 1
         (cl-loop for start from 0 below n by org-glance-bench:batch
                  do (apply #'org-glance-graph:add graph
                            (cl-loop for i from start
                                     below (min n (+ start org-glance-bench:batch))
                                     collect (org-glance-bench--headline i)))))))

(cl-defun org-glance-bench--row (op seconds &optional note)
  "Print one result row: OP, SECONDS, optional NOTE."
  (princ (format "  %-24s %10.1f ms%s\n" op (* 1000 seconds)
                 (if note (format "   %s" note) ""))))

(cl-defun org-glance-bench--segments (graph)
  "GRAPH's current segment count: sealed + the open one."
  (1+ (length (org-glance-graph--sealed-segments graph))))

(cl-defun org-glance-bench:run ()
  "Run every benchmark tier and print the table to stdout.
Progress chatter (`reindex') is inhibited so the table stays readable."
  (let ((inhibit-message t))
    (dolist (n org-glance-bench:sizes)
    (org-glance-test:with-graph graph
      (princ (format "N = %d  (segment-max %dK, compact-at %d)\n" n
                     (/ org-glance-graph-segment-max-bytes 1024)
                     org-glance-graph-compact-segment-count))
      (org-glance-bench--row
       "insert (batched)" (org-glance-bench--build graph n)
       (format "%d segments" (org-glance-bench--segments graph)))
      (org-glance-bench--row
       "cold fold" (car (benchmark-run 1
                          (progn (org-glance-graph--invalidate-cache graph)
                                 (org-glance-graph:headlines graph)))))
      (let ((ids (cl-loop repeat org-glance-bench:sample
                          collect (format "bench-%06d" (random n)))))
        (org-glance-bench--row
         (format "warm get-by-id x%d" org-glance-bench:sample)
         (car (benchmark-run 1
                (dolist (id ids) (org-glance-graph:get-headline graph id))))))
      (org-glance-bench--row
       "property-index cold fill"
       (car (benchmark-run 1
              (progn (org-glance-property-index:clear graph)
                     (org-glance-property-index:ensure
                      graph (cl-loop for i below n collect (format "bench-%06d" i)))))))
      (org-glance-bench--row
       "reindex" (car (benchmark-run 1 (org-glance-graph:reindex graph))))
      (org-glance-bench--row
       "compact" (car (benchmark-run 1 (org-glance-graph:compact graph)))
       (format "%d segments after" (org-glance-bench--segments graph)))
      (princ "\n")))))

(provide 'bench-graph)
;;; bench-graph.el ends here
