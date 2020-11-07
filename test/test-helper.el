;;; test-helper.el --- Helpers for org-glance-test.el
;;
;;; Commentary:
;; Test helpers for org-glance.
;;
;;; Code:

(and
 (require 'undercover nil t)
 (undercover "*.el"))

(require 'org-glance)

(eval-when-compile
  (require 'f)
  (require 'load-relative))

(defvar org-glance-test/test-path (->> (or load-file-name (__FILE__))
                                       file-name-directory
                                       directory-file-name)
  "Path to tests directory.")

(defvar org-glance-test/test-resources-path (f-join org-glance-test/test-path "resources")
  "Path to tests directory.")

(defvar org-glance-test/root-path (->> org-glance-test/test-path
                                       file-name-directory
                                       directory-file-name)
  "Path to base directory.")

(defun org-entry-title ()
  (org-entry-get (point) "ITEM"))

(defun org-glance-test-get-resource (resource)
  (f-join org-glance-test/test-resources-path resource))

;; (cl-defmacro in-materialized-buffer (view &rest forms)
;;   (declare (indent defun))
;;   `(let ((buffer (org-glance-action-materialize (list "--reread") ,view)))
;;      (unwind-protect
;;          (with-current-buffer buffer
;;            (message "Visit materialized view %s at buffer %s" ,view (current-buffer))
;;            (message "-org-glance-pwd: %s" -org-glance-pwd)
;;            (message "-org-glance-src: %s" -org-glance-src)
;;            (message "-org-glance-beg: %d" -org-glance-beg)
;;            (message "-org-glance-end: %d" -org-glance-end)
;;            (message "-org-glance-hash: %s" -org-glance-hash)
;;            (message "-org-glance-indent: %d" -org-glance-indent)
;;            ,@forms))))

;; (cl-defmacro follow-link-capture-output (view)
;;   (declare (indent defun))
;;   `(let ((messages-point-before-action ;; will capture action output
;;           (with-current-buffer (messages-buffer)
;;             (point-max))))
;;      (org-glance-action-open)
;;      (with-current-buffer (messages-buffer)
;;        (buffer-substring-no-properties messages-point-before-action (point-max)))))

;; (cl-defmacro org-glance-sandbox (&body forms &key input view in type act &allow-other-keys)
;;   "Run FORMS in isolated environment."
;;   (declare (indent defun))
;;   `(with-temp-view ,view ,type ,in
;;          (if (and ,act ,input)
;;              (org-glance-emulate-user-input ,input
;;                (cond ((eq ,act 'materialize) (in-materialized-buffer ,view ,@forms))
;;                      ((eq ,act 'open) (follow-link-capture-output ,view))
;;                      (t (error "Unknown action called in user story."))))
;;            ,@forms)))

;;; test-helper.el ends here
