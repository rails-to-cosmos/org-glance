(defun org-glance-test-get-resource (resource)
  (f-join org-glance-test/test-resources-path resource))

(defmacro with-scope (scope &rest forms)
  (declare (indent 1))
  `(let* ((scope-file (make-temp-file "org-glance-test-"))
          (org-agenda-files (list scope-file))
          (default-directory org-glance-test/test-resources-path))
     (message "Create scope %s from %s" scope-file ,scope)
     (with-temp-file scope-file
       (insert-file-contents-literally (org-glance-test-get-resource ,scope)))
     ,@forms
     (message "Exit scope %s" scope-file)
     (kill-buffer (get-file-buffer scope-file))
     (message "Remove file %s" scope-file)
     (delete-file scope-file)))

(cl-defmacro with-temp-view (view &rest forms)
  (declare (indent 1))
  `(progn
     (message "Create view %s" ,view)
     (org-glance-def-view ,view)
     ,@forms
     (message "Remove view %s" ,view)
     (org-glance-remove-view ,view)))

(cl-defmacro with-user-choice (choice &rest forms)
  (declare (indent 1))
  `(let ((user-input (format "%s RET" (eval ,choice))))
     (with-simulated-input user-input
       ,@forms)))

(cl-defmacro with-materialized-view (view &rest forms)
  (declare (indent 1))
  `(let ((buffer (org-glance-action-materialize ,view t)))
     (with-current-buffer buffer
       (message "Visit materialized view %s at buffer %s" ,view (current-buffer))
       ,@forms)
     (message "Kill buffer %s" buffer)
     (kill-buffer buffer)))

(cl-defmacro -og-user-story (&body forms &key choose view from act &allow-other-keys)
  (declare (indent 8))
  `(with-scope ,from
     (with-temp-view ,view
       (with-user-choice ,choose
         (when (eq ,act 'materialize)
           (with-materialized-view ,view
             ,@forms))))))

(defun org-entry-title ()
  (org-entry-get (point) "ITEM"))

(provide 'org-glance-test-helpers)
