;;; bench-cache-readers.el --- Warm UI point-read measurements -*- lexical-binding: t; -*-

(require 'bench-common)

(defun org-glance-reader-bench:run (&optional directory repeats)
  "Measure warm UI point reads in DIRECTORY or disposable stores over REPEATS."
  (let ((inhibit-message t) (message-log-max nil))
    (org-glance-bench:with-store
     directory
     (lambda (graph)
       (let* ((metadata (car (org-glance-cache:headlines graph)))
              (id (if metadata (org-glance-headline-metadata:id metadata) "absent"))
              (open (symbol-function 'sqlite-open))
              (deserialize (symbol-function 'org-glance-headline-metadata:deserialize))
              (opens 0) (decoded 0))
         (org-glance-graph:live-meta graph id)
         (cl-letf (((symbol-function 'sqlite-open)
                    (lambda (&rest args) (cl-incf opens) (apply open args)))
                   ((symbol-function 'org-glance-headline-metadata:deserialize)
                    (lambda (record) (cl-incf decoded) (funcall deserialize record))))
           (org-glance-bench:measure
            "readers" "org-glance-graph:live-meta"
            (lambda () (org-glance-graph:live-meta graph id))
            :samples (or repeats (org-glance-bench:rounds)) :iterations 100
            :before (lambda () (setq opens 0 decoded 0))
            :fingerprint #'org-glance-bench:fingerprint
            :counters (lambda (_result) (list :opens opens :deserializations decoded)))))))))

(provide 'bench-cache-readers)
;;; bench-cache-readers.el ends here
