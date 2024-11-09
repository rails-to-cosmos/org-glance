(require 'ert)
(require 'org-glance)

(cl-defmacro with-temp-directory (dir &rest body)
  "Create a temporary directory, bind it to DIR, run BODY in it, and delete the directory afterward.
DIR is a symbol that will hold the path to the temporary directory within BODY."
  (declare (indent 1))
  `(let ((,dir (make-temp-file "temp-dir-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,dir t))))

(cl-defmacro with-temp-glance-directory (&rest body)
  (declare (indent 0))
  `(let ((org-glance-directory (make-temp-file "temp-dir-" t)))
     (unwind-protect
         (progn (org-glance-init org-glance-directory)
                ,@body)
       (delete-directory org-glance-directory t))))

(ert-deftest initial-state ()
  (with-temp-glance-directory
    (should (= (length (org-glance-tags:list)) 0))))

(ert-deftest basic-tag-management ()
  (with-temp-glance-directory
    (org-glance:create-tag 'foo)
    (should (org-glance-tag:exists? 'foo org-glance-tags))

    ;; (org-glance:remove-tag 'foo)
    ;; (should-not (org-glance-tag:exists? 'foo org-glance-tags))

    (should-error (org-glance:create-tag "bar") :type 'error)
    (should-error (org-glance:create-tag 'BAZ) :type 'error)))

(provide 'org-glance-test)
