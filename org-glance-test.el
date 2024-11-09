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

(ert-deftest test-initial-state ()
  (with-temp-glance-directory
    (should (= (length (org-glance-tags:list)) 0))))

(ert-deftest test-tag-management ()
  (with-temp-glance-directory
    (org-glance:create-tag 'a)  ;; by default, tags are lowercased symbols
    (org-glance:create-tag "b")

    (should (and (org-glance-tag:exists? 'a org-glance-tags)
                 (org-glance-tag:exists? 'b org-glance-tags)))))

(provide 'org-glance-test)
