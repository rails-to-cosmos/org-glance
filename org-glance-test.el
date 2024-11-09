(require 'org-glance)

(cl-defmacro with-temp-directory (dir &rest body)
  "Create a temporary directory, bind it to DIR, run BODY in it, and delete the directory afterward.
DIR is a symbol that will hold the path to the temporary directory within BODY."
  (declare (indent 1))
  `(let ((,dir (make-temp-file "temp-dir-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,dir t))))

(ert-deftest org-glance-test:create-tag ()
  (with-temp-directory directory
    (org-glance-init directory)
    (should (= (length (org-glance-tags:list)) 0))))

(provide 'org-glance-test)
