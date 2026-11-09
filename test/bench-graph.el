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
  "Measure repeated graph workloads and verify their published results."
  (let ((inhibit-message t) (message-log-max nil))
    (dolist (n org-glance-bench:sizes)
      (org-glance-test:with-graph graph
        (let ((org-glance-bench--size n)
              (root (org-glance-graph:directory graph))
              (content (symbol-function 'insert-file-contents-literally))
              (blob-reads 0) inserted)
          (cl-letf (((symbol-function 'insert-file-contents-literally)
                     (lambda (path &rest args)
                       (when (equal (file-name-nondirectory path) "data.org")
                         (cl-incf blob-reads))
                       (apply content path args))))
            (cl-labels
                ((metadata-fingerprint (_result)
                   (org-glance-bench:fingerprint (org-glance-graph:headlines graph)))
                 (record-counts (_result)
                   (list :headlines (length (org-glance-graph:headlines graph))
                         :records (cdr (org-glance-graph--latest-records graph))
                         :segments (org-glance-bench--segments graph)))
                 (overview-fingerprint (_result)
                   (org-glance-bench:fingerprint
                    (f-read-text (cdr (org-glance-overview--manifest graph nil)) 'utf-8))))
              (org-glance-bench:measure
               "graph" "insert (batched)" (lambda () (org-glance-bench:build graph n))
               :before (lambda ()
                         (when inserted
                           (when (fboundp 'org-glance-cache:close-readers)
                             (org-glance-cache:close-readers graph))
                           (remhash (org-glance-graph:directory graph) org-glance-graph:list)
                           (delete-directory (org-glance-graph:directory graph) t)
                           (make-directory root t)
                           (setq graph (org-glance-graph
                                        (make-temp-file (expand-file-name "sample-" root) t))))
                         (setq inserted t))
               :fingerprint #'metadata-fingerprint :counters #'record-counts)
              (org-glance-bench:measure
               "graph" "cold compatibility fold"
               (lambda () (org-glance-graph:headlines graph))
               :before (lambda () (org-glance-graph--invalidate-cache graph))
               :fingerprint #'org-glance-bench:fingerprint
               :counters (lambda (result) (list :headlines (length result))))
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
                 (org-glance-property-index:ensure
                  graph (cl-loop for index below n collect (format "bench-%06d" index))))
               :before (lambda ()
                         (org-glance-property-index:clear graph)
                         (setq blob-reads 0))
               :fingerprint
               (lambda (_result)
                 (let ((table (org-glance-property-index--table graph)))
                   (org-glance-bench:fingerprint
                    (cl-loop for index below n
                             collect (gethash (format "bench-%06d" index) table)))))
               :counters (lambda (_result)
                           (list :indexed-headlines
                                 (hash-table-count (org-glance-property-index--table graph))
                                 :blob-reads blob-reads)))
              (org-glance-bench:measure
               "graph" "overview full refresh" (lambda () (org-glance-overview:write graph))
               :fingerprint #'overview-fingerprint :counters #'record-counts)
              (org-glance-bench:measure
               "graph" "overview one-row refresh"
               (lambda () (org-glance-overview:update graph nil '("bench-000000")))
               :fingerprint #'overview-fingerprint
               :counters (lambda (result)
                           (append (list :mode (symbol-name result)) (record-counts result))))
              (org-glance-bench:measure
               "graph" "reindex" (lambda () (org-glance-graph:reindex graph))
               :before (lambda () (org-glance-graph:compact graph) (setq blob-reads 0))
               :fingerprint #'metadata-fingerprint
               :counters (lambda (result)
                           (list :headlines result :blob-reads blob-reads)))
              (let (input-records)
                (org-glance-bench:measure
                 "graph" "compact superseded record"
                 (lambda () (org-glance-graph:compact graph))
                 :before (lambda ()
                           (org-glance-graph:insert
                            graph (list (org-glance-graph:live-meta graph "bench-000000")))
                           (setq input-records (cdr (org-glance-graph--latest-records graph))))
                 :fingerprint #'metadata-fingerprint
                 :counters (lambda (result)
                             (list :input-records input-records :live-records result
                                   :segments (org-glance-bench--segments graph))))))))))))

(provide 'bench-graph)
;;; bench-graph.el ends here
