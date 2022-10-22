(require 'a)

(defvar org-glance-log:loggers
  (a-list :event nil
          :headline nil
          :cache nil
          :dimension nil
          :contents nil
          :performance nil))

(defconst org-glance-log:enable-benchmark-report nil)
(defconst org-glance-log:enable-cash-report nil)
(defconst org-glance-log:enable-debug-report nil)
(defconst org-glance-log:enable-reason-report nil)
(defconst org-glance-log:enable-context-report nil)
(defconst org-glance-log:enable-scenario-report nil)

(cl-defmacro org-glance-log (logger &rest args)
  (declare (indent 2))
  (if (a-get org-glance-log:loggers logger)
      (pcase logger
        (:performance (let ((value (make-symbol "value"))
                            (start (make-symbol "start"))
                            (gcs (make-symbol "gcs"))
                            (gc (make-symbol "gc"))
                            (inhibit-message nil))
                        `(let ((,gc gc-elapsed)
                               (,gcs gcs-done)
                               (,start (current-time))
                               (,value ,@args))
                           (message "[%s of %s] Elapsed time: %fs%s"
                                    ,logger
                                    (caar (quote ,args))
                                    (float-time (time-since ,start))
                                    (if (> (- gcs-done ,gcs) 0)
                                        (format " (%fs in %d GCs)"
                                                (- gc-elapsed ,gc)
                                                (- gcs-done ,gcs))
                                      ""))
                           ,value)))
        (_ `(let ((inhibit-message nil))
              (message (format "[%s] %s" ,logger (format ,@args))))))
    (pcase logger
      (:performance `(progn ,@args))
      (_ nil))))

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
