;;; org-glance-test.el --- Tests for org-glance  -*- lexical-binding: t -*-

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
  `(unwind-protect
       (with-temp-directory org-glance-directory
         (org-glance-init org-glance-directory)
         ,@body)
     (org-glance-init org-glance-directory)))

;; (ert-deftest org-glance-test:initial-state ()
;;   (org-glance-test:session
;;     (should (= (length (org-glance:tags)) 0))))

;; (ert-deftest org-glance-test:tag-management ()
;;   (org-glance-test:session
;;     (let ((tag 'foo))
;;       (org-glance:create-tag tag)
;;       (should (org-glance-tag:exists? tag org-glance-tags)))

;;     ;; (org-glance:remove-tag 'foo)
;;     ;; (should-not (org-glance-tag:exists? 'foo org-glance-tags))

;;     (should-error (org-glance:create-tag "bar") :type 'error)
;;     (should-error (org-glance:create-tag 'BAZ) :type 'error)))

(cl-defun org-glance-test:create-tag (tag)
  (cl-check-type tag org-glance-tag)
  (org-glance:create-tag tag)
  (should (org-glance-tag:exists? tag org-glance-tags))
  (should (= 1 (length (org-glance:tags))))
  (should (= 0 (length (org-glance:tag-headlines tag))))
  tag)

(cl-defun org-glance-test:add-headline (tag title &optional contents)
  (cl-check-type tag org-glance-tag)
  (cl-check-type title string)
  (org-glance-capture tag
    :title (if contents
               (s-concat title "\n" contents)
             title)
    :finalize t))

(cl-defun org-glance-test:headline-overview (tag id)
  (cl-check-type tag org-glance-tag)
  (cl-check-type id string)
  (save-window-excursion
    (org-glance-overview tag)
    (org-glance-headline:search-buffer-by-id id)))

(cl-defun org-glance-test:materialize-overview (tag id)
  (cl-check-type tag org-glance-tag)
  (cl-check-type id string)
  (save-window-excursion
    (org-glance-test:headline-overview tag id)
    (org-glance-overview:materialize-headline)
    (org-glance-headline:search-buffer-by-id id)))

(cl-defun org-glance-test:materialize (id)
  (cl-check-type id string)
  (let ((headline (org-glance-metadata:headline id)))
    (org-glance:materialize headline)))

(ert-deftest org-glance-test:consistency ()
  (org-glance-test:session
    (let* (;; TODO generate such entities
           (tag (org-glance-test:create-tag 'foo))
           (title "Hello, world!")
           (contents "Some contents")
           (id (org-glance-test:add-headline tag title contents))
           (metadata (org-glance-metadata:headline-metadata id))
           (overview (org-glance-test:headline-overview tag id))
           (material (org-glance-test:materialize id))
           (material-overview (org-glance-test:materialize-overview tag id)))
      (should (= 1 (length (org-glance:tag-headlines tag))))
      (should (string= (org-glance-headline:title overview) title))
      (should (org-glance-headline:equal? material overview))
      (should (org-glance-headline:equal? overview material-overview))
      (should (org-glance-headline:equal? material material-overview)))))

(ert-deftest org-glance-test:links ()
  (org-glance-test:session
    (let* (;; TODO generate such entities
           (tag (org-glance-test:create-tag 'foo))
           (title "foo")
           (temp-file-name (f-join org-glance-directory "tmp.txt"))
           (contents (format "[[file:%s][tmp.txt]]" temp-file-name))
           (id (org-glance-test:add-headline tag title contents))
           (headline (org-glance-test:materialize id)))
      (should (org-glance-headline:linked? headline))
      (org-glance:open headline)
      ;; Seems org-mode opens file-links asynchronously, so it should be enough to see that user-error has not been raised.
      ;; (should (f-equal? (buffer-file-name) temp-file-name))
      )))

(ert-deftest org-glance-test:properties ()
  (org-glance-test:session
    (let* (;; TODO generate such entities
           (tag (org-glance-test:create-tag 'foo))
           (title "Hello, world!")
           (contents "- foo: bar")
           (id (org-glance-test:add-headline tag title contents))
           (headline (org-glance-test:materialize id)))
      (should (org-glance-headline:propertized? headline))
      (should (string= "bar" (org-glance:extract headline "foo"))))))

;; (ert-deftest org-glance-test:encryption ()
;;   (org-glance-test:session
;;     (let* (;; TODO generate such entities
;;            (tag (org-glance-test:create-tag 'foo))
;;            (title "Hello, world!")
;;            (contents "- foo: bar")
;;            (id (org-glance-test:add-headline tag title contents))
;;            (headline (org-glance-test:materialize id))
;;            (initial-contents (org-glance-headline:contents headline)))
;;       (org-glance:with-headline-materialized headline
;;         (org-glance-headline:encrypt "123"))
;;       (should (string= initial-contents (org-glance-headline:contents headline))))

;;     ;; (should (org-glance-headline:propertized? headline))
;;     ;; (should (string= "bar" (org-glance:extract headline "foo")))
;;     ))

;; new headline model

(cl-defun org-glance-test:headline1 (&rest contents)
  (with-temp-buffer
    (org-mode)
    (insert (car contents))
    (cl-dolist (line (cdr contents))
      (insert "\n" line))
    (org-glance-headline1:at-point)))

(ert-deftest org-glance-test:headline-active ()
  (let ((org-done-keywords (list "DONE")))
    (let ((headline (org-glance-test:headline1 "* TODO Hello, world!")))
      (should (org-glance-headline1:active? headline))
      (should (not (org-glance-headline1:done? headline))))))

(ert-deftest org-glance-test:headline-properties ()
  (let ((headline (org-glance-test:headline1 "* TODO Hello, world!" "- foo: bar")))
    (should (eq 1 (length (org-glance-headline1:user-properties headline))))
    (should (string= "bar" (org-glance-headline1:get-user-property "foo" headline)))))

(ert-deftest org-glance-test:headline-links ()
  (let ((headline (org-glance-test:headline1 "* TODO Hello, world!" "[[https:duckduckgo.com][ddg]]")))
    (should (eq 1 (length (org-glance-headline1:links headline))))))

(ert-deftest org-glance-test:headline-encryption ()
  (let* ((original (org-glance-test:headline1 "* TODO Hello, world!" "foo bar"))
         (password "password")
         (encrypted (org-glance-headline1:encrypt original password))
         (decrypted (org-glance-headline1:decrypt encrypted password)))
    (should (not (org-glance-headline1:encrypted? original)))
    (should (org-glance-headline1:encrypted? encrypted))
    (should (not (string= (org-glance-headline1:contents original) (org-glance-headline1:contents encrypted))))
    (should (string= (org-glance-headline1:contents decrypted) (org-glance-headline1:contents original)))))

;; TODO Add tag, add headline, delete tag directory, add another tag, all actions should work fine

(provide 'org-glance-test)

;;; org-glance-test.el ends here
