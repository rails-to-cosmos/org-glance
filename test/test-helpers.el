;;; test-helpers.el --- Helpers for org-glance
(require 'ert)
(require 's)
(require 'org-glance)

(cl-defmacro with-temp-directory (dir &rest body)
  "Create a temporary directory, bind it to DIR, run BODY in it, and delete the directory afterward.
DIR is a symbol that will hold the path to the temporary directory within BODY."
  (declare (indent 1))
  `(let ((,dir (make-temp-file "temp-dir-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,dir t))))

(cl-defmacro org-glance-test:session (&rest body)
  (declare (indent 0))
  `(with-temp-directory org-glance-directory
     (org-glance-init org-glance-directory)
     ,@body))

(provide 'test-helpers)
;;; test-helpers.el ends here
