;;; bench-cache-readers.el --- Warm reader reuse measurements -*- lexical-binding: t; -*-

(require 'org-glance)
(require 'benchmark)

(defun org-glance-reader-bench:run (directory &optional repeats)
  "Compare retained and temporary readers in DIRECTORY over REPEATS pairs.
Use identical file checks, transactions and queries; count connection opens."
  (let* ((graph (make-org-glance-graph :directory (file-truename directory)))
         (path (org-glance-cache:path graph))
         (query (lambda (db)
                  (sqlite-select db "SELECT logical_id,state FROM headline_family LIMIT 1")))
         (expected (org-glance-cache--read-db graph query))
         (open (symbol-function 'sqlite-open))
         (fresh nil) (reused nil)
         (counts (vector 0 0))
         (inhibit-message t))
    (unwind-protect
        (dotimes (round (or repeats 7))
          (dolist (variant (if (zerop (% round 2)) '(0 1) '(1 0)))
            (org-glance-cache:close-readers graph)
            (garbage-collect)
            (let ((org-glance-cache--staging-path (when (zerop variant) path))
                  (opens 0) result)
              (cl-letf (((symbol-function 'sqlite-open)
                         (lambda (&rest args) (cl-incf opens) (apply open args))))
                (let ((elapsed
                       (car (benchmark-run 100
                              (setq result (org-glance-cache--read-db graph query))))))
                  (if (zerop variant) (push (* 10 elapsed) fresh)
                    (push (* 10 elapsed) reused))))
              (unless (equal expected result) (error "Reader result changed"))
              (cl-incf (aref counts variant) opens))))
      (org-glance-cache:close-readers graph))
    (princ (format "Warm readers: %d alternating pairs of 100 reads; equal results\n"
                   (or repeats 7)))
    (princ (format "Per-read median ms: fresh %.4f; reused %.4f\n"
                   (nth (/ (length fresh) 2) (sort fresh #'<))
                   (nth (/ (length reused) 2) (sort reused #'<))))
    (princ (format "Connection opens per 100 reads: fresh %.1f; reused %.1f\n"
                   (/ (aref counts 0) (float (or repeats 7)))
                   (/ (aref counts 1) (float (or repeats 7)))))))

(provide 'bench-cache-readers)
;;; bench-cache-readers.el ends here
