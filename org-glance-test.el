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

(cl-defmacro org-glance:with-temp-session (&rest body)
  (declare (indent 0))
  `(unwind-protect
       (with-temp-directory org-glance-directory
         (org-glance-init org-glance-directory)
         ,@body)
     (org-glance-init org-glance-directory)))

;; (ert-deftest org-glance-test:initial-state ()
;;   (org-glance:with-temp-session
;;     (should (= (length (org-glance:tags)) 0))))

;; (ert-deftest org-glance-test:tag-management ()
;;   (org-glance:with-temp-session
;;     (let ((tag 'foo))
;;       (org-glance:create-tag tag)
;;       (should (org-glance-tag:exists? tag org-glance-tags)))

;;     ;; (org-glance:remove-tag 'foo)
;;     ;; (should-not (org-glance-tag:exists? 'foo org-glance-tags))

;;     (should-error (org-glance:create-tag "bar") :type 'error)
;;     (should-error (org-glance:create-tag 'BAZ) :type 'error)))

(ert-deftest org-glance-test:headline-workflow ()
  (let ((tag 'foo)
        (headline-title "Hello, world!"))
    (org-glance:with-temp-session
      (org-glance:create-tag tag)

      (should (org-glance-tag:exists? tag org-glance-tags))
      (should (= 1 (length (org-glance:tags))))
      (should (= 0 (length (org-glance:tag-headlines tag))))

      (org-glance-capture tag
        :default headline-title
        :finalize t)

      (should (= 1 (length (org-glance:tag-headlines tag))))

      (org-glance-overview tag)

      (let ((headline (org-glance-headline:at-point)))
        (should (org-glance-headline? headline))
        (should (string= (org-glance-headline:title headline) headline-title)))

      ;; (org-glance-overview:materialize-headline)
      )))

(provide 'org-glance-test)
