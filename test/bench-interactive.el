;;; bench-interactive.el --- Warm materialize command measurements -*- lexical-binding: t; -*-

(require 'bench-common)

(defun org-glance-interactive-bench:run (&optional directory repeats)
  "Measure warm materialize commands in DIRECTORY or disposable stores.
Use REPEATS samples with automatic headline selection and a fresh editor."
  (let ((inhibit-message t) (message-log-max nil))
    (org-glance-bench:with-store
     directory
     (lambda (graph)
       (let ((org-glance-graph graph)
             (org-glance-filter-spec nil)
             (deserialize (symbol-function 'org-glance-headline-metadata:deserialize))
             (decoded 0) collection buffer)
         (org-glance-material:candidates graph)
         (unwind-protect
             (cl-letf (((symbol-function 'completing-read)
                        (lambda (_prompt candidates &rest _)
                          (setq collection candidates) (caar candidates)))
                       ((symbol-function 'org-glance-headline-metadata:deserialize)
                        (lambda (record) (cl-incf decoded) (funcall deserialize record))))
               (org-glance-bench:measure
                "interactive" "org-glance-materialize (automatic selection)"
                (lambda () (setq buffer (org-glance-materialize)))
                :samples (or repeats (org-glance-bench:rounds))
                :before (lambda ()
                          (when (buffer-live-p buffer) (org-glance--discard-buffer buffer))
                          (setq buffer nil collection nil decoded 0))
                :fingerprint (lambda (_result) (org-glance-bench:fingerprint collection))
                :counters (lambda (_result)
                            (list :candidates (length collection) :deserializations decoded))))
           (when (buffer-live-p buffer) (org-glance--discard-buffer buffer))))))))

(provide 'bench-interactive)
;;; bench-interactive.el ends here
