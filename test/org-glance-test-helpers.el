(defun org-glance-test-get-resource (resource)
  (f-join org-glance-test/test-resources-path resource))

(defmacro with-scope (scope &rest forms)
  (declare (indent 1))
  `(let* ((scope-file (make-temp-file "org-glance-test-"))
          (org-agenda-files (list scope-file))
          (default-directory org-glance-test/test-resources-path))
     (message "Entering temporary scope %s" scope-file)
     (with-temp-file scope-file
       (insert-file-contents-literally (org-glance-test-get-resource ,scope)))
     ,@forms
     (message "Exiting temporary scope %s" scope-file)
     (kill-buffer (get-file-buffer scope-file))
     (message "Remove file %s" scope-file)
     (delete-file scope-file)))

(cl-defmacro with-temp-view (view &rest forms)
  (declare (indent 1))
  `(progn
     (message "Create temp view %s" ,view)
     (org-glance-def-view ,view)
     ,@forms
     (message "Remove temp view %s" ,view)
     (org-glance-remove-view ,view)))

(cl-defmacro with-user-choice (choice &rest forms)
  (declare (indent 1))
  `(let ((user-input (format "%s RET" (eval ,choice))))
     (with-simulated-input user-input
       ,@forms)))

(cl-defmacro with-materialized-view (view &rest forms)
  (declare (indent 1))
  `(with-current-buffer
       (org-glance-action-materialize ,view t)
     (message "Visit materialized view %s buffer %s" ,view (current-buffer))
     ,@forms))

(defun org-entry-title ()
  (org-entry-get (point) "ITEM"))

(provide 'org-glance-test-helpers)
