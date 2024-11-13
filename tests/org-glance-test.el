;; -*- lexical-binding: t -*-

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

(cl-defun org-glance-test:capture (tag title)
  (cl-letf (((symbol-function 'org-glance-tags:completing-read) (lambda (&rest _) tag)))
    (org-glance-capture))
  (insert title)
  (org-capture-finalize))

(ert-deftest org-glance-test:initial-state ()
  (with-temp-glance-directory
    (should (= (length (org-glance:tags)) 0))))

(ert-deftest org-glance-test:tag-management ()
  (with-temp-glance-directory
    (let ((tag 'foo))
      (org-glance:create-tag tag)
      (should (org-glance-tag:exists? tag org-glance-tags)))

    ;; (org-glance:remove-tag 'foo)
    ;; (should-not (org-glance-tag:exists? 'foo org-glance-tags))

    (should-error (org-glance:create-tag "bar") :type 'error)
    (should-error (org-glance:create-tag 'BAZ) :type 'error)))

(ert-deftest org-glance-test:tag-overview ()
  (let ((tag 'foo))
    (with-temp-glance-directory
      (org-glance:create-tag tag)
      (org-glance-test:capture tag "Hello, world!"))))

(provide 'org-glance-test)
