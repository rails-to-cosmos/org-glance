(require 'a)

(defvar org-glance-log:loggers
  (a-list :events t
          :headlines t
          :cache t
          :dimensions t
          :sql t
          :contents t
          :markers t
          :performance t
          :test t))

(cl-defmacro org-glance-log:with-logger (logger &rest forms)
  (declare (indent 2))
  `(let ((result (format ,@forms)))
     (with-current-buffer (get-buffer-create (format "*org-glance-log%s*" ,logger))
       (goto-char (point-max))
       (insert (format-time-string "%H:%M:%S") " " result "\n"))))

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
                           (org-glance-log:with-logger ,logger
                               "[%s] Elapsed time: %fs%s"
                             (caar (quote ,args))
                             (float-time (time-since ,start))
                             (if (> (- gcs-done ,gcs) 0)
                                 (format " (%fs in %d GCs)"
                                         (- gc-elapsed ,gc)
                                         (- gcs-done ,gcs))
                               ""))
                           ,value)))
        (_ `(org-glance-log:with-logger ,logger
                "[%s] %s"
              ,logger (format ,@args))))
    (pcase logger
      (:performance `(progn ,@args))
      (_ nil))))

(provide 'org-glance-log)