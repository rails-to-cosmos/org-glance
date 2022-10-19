(defvar org-glance-debug-mode t)

(defmacro org-glance-benchmark (&rest body)
  "Evaluate FN and message the time taken.
The return value is the value of the final form in FN."
  (declare (indent 0))
  (let ((value (make-symbol "value"))
        (start (make-symbol "start"))
        (gcs (make-symbol "gcs"))
        (gc (make-symbol "gc")))
    `(let ((,gc gc-elapsed)
           (,gcs gcs-done)
           (,start (current-time))
           (,value ,@body))
       (when org-glance-debug-mode
         (message "[%s] Elapsed time: %fs%s"
                  (caar (quote ,body))
                  (float-time (time-since ,start))
                  (if (> (- gcs-done ,gcs) 0)
                      (format " (%fs in %d GCs)"
                              (- gc-elapsed ,gc)
                              (- gcs-done ,gcs))
                    "")))

       ,value)))

(cl-defmacro org-glance-debug (&rest args)
  (declare (indent 1))
  (when org-glance-debug-mode
    `(let ((inhibit-message nil))
       (message ,@args))))

(provide 'org-glance-debug)
