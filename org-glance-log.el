;; (defvar org-glance-log:enabled-loggers
;;   '(benchmark
;;     cash
;;     scenario
;;     context
;;     ))

(defconst org-glance-log:enable-benchmark-report nil)
(defconst org-glance-log:enable-cash-report nil)
(defconst org-glance-log:enable-debug-report t)
(defconst org-glance-log:enable-reason-report nil)
(defconst org-glance-log:enable-context-report nil)
(defconst org-glance-log:enable-scenario-report t)

(defmacro org-glance-log:benchmark (&rest body)
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
       (when org-glance-log:enable-benchmark-report
         (message "> [benchmark] [%s] Elapsed time: %fs%s"
                  (caar (quote ,body))
                  (float-time (time-since ,start))
                  (if (> (- gcs-done ,gcs) 0)
                      (format " (%fs in %d GCs)"
                              (- gc-elapsed ,gc)
                              (- gcs-done ,gcs))
                    "")))
       ,value)))

(cl-defmacro org-glance-log:scenario (&rest args)
  (declare (indent 1))
  (when org-glance-log:enable-scenario-report
    `(let ((inhibit-message nil))
       (message (concat "> [scenario] " (upcase (format ,@args)))))))

(cl-defmacro org-glance-log:debug (&rest args)
  (declare (indent 1))
  (when org-glance-log:enable-debug-report
    `(let ((inhibit-message nil))
       (message (concat "> [debug] " (format ,@args))))))

(cl-defmacro org-glance-log:cache (&rest args)
  (declare (indent 1))
  (when org-glance-log:enable-cash-report
    `(let ((inhibit-message nil))
       (message (concat "> [cache] " (format ,@args))))))

(cl-defmacro org-glance-log:reason (&rest args)
  (declare (indent 1))
  (when org-glance-log:enable-reason-report
    `(let ((inhibit-message nil))
       (message (concat "> [reason] " (format ,@args))))))

(cl-defmacro org-glance-log:context (&rest args)
  (declare (indent 1))
  (when org-glance-log:enable-context-report
    `(let ((inhibit-message nil))
       (message (concat "> [context] " (format ,@args))))))

(provide 'org-glance-log)
