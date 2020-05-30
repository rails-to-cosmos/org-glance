;;; test-helper.el --- Helpers for org-glance-test.el
;;
;;; Commentary:
;; Test helpers for org-glance.
;;
;;; Code:

(when (require 'undercover nil t)
  (undercover "*.el"))

(require 'org-glance)

(eval-when-compile
  (require 'f)
  (require 'load-relative))

(defvar org-glance-test/test-path
  (->> (or load-file-name
           (__FILE__))
       file-name-directory
       directory-file-name)
  "Path to tests directory.")

(defvar org-glance-test/test-resources-path
  (f-join org-glance-test/test-path "resources")
  "Path to tests directory.")

(defvar org-glance-test/root-path
  (->> org-glance-test/test-path
       file-name-directory
       directory-file-name)
  "Path to root directory.")

(defun org-entry-title ()
  (org-entry-get (point) "ITEM"))

(defun org-glance-test-get-resource (resource)
  (f-join org-glance-test/test-resources-path resource))

;; (defun -org-glance-rename-subtree (title)
;;   (interactive)
;;   (save-excursion
;;     (org-back-to-heading)
;;     (org-beginning-of-line)
;;     (org-kill-line)
;;     (insert title)
;;     (substring-no-properties (car kill-ring))))

(cl-defmacro with-temp-view (view scope &rest forms)
  (declare (indent defun))
  `(let* ((scope-file (make-temp-file "org-glance-test-"))
          (default-directory org-glance-test/test-resources-path))

     ;; prepare temporary scope file
     (with-temp-file scope-file
       (message "Create scope %s from %s" scope-file ,scope)
       (insert-file-contents-literally (org-glance-test-get-resource ,scope)))
     (message "Scope file size: %s" (->> scope-file
                                         file-attributes
                                         file-attribute-size
                                         file-size-human-readable))
     (unwind-protect
         (unwind-protect
             (progn
               (org-glance-def-view ,view
                 :scope scope-file)
               ,@forms)
           (message "Remove view %s" ,view)
           (org-glance-remove-view ,view))
       (message "Remove file %s" scope-file)
       (delete-file scope-file))))

(cl-defmacro with-user-input (user-input &rest forms)
  (declare (indent defun))
  `(let ((kbd-macro (format "%s RET" (eval ,user-input))))
     (message "Simulate user input: %s" kbd-macro)
     (with-simulated-input kbd-macro
       ,@forms)))

(cl-defmacro in-materialized-buffer (view &rest forms)
  (declare (indent defun))
  `(let ((buffer (org-glance-action-materialize ,view t)))
     (unwind-protect
         (with-current-buffer buffer
           (message "Visit materialized view %s at buffer %s" ,view (current-buffer))
           (message "-org-glance-pwd: %s" -org-glance-pwd)
           (message "-org-glance-src: %s" -org-glance-src)
           (message "-org-glance-beg: %d" -org-glance-beg)
           (message "-org-glance-end: %d" -org-glance-end)
           (message "-org-glance-hash: %s" -org-glance-hash)
           (message "-org-glance-indent: %d" -org-glance-indent)
           ,@forms))))

(cl-defmacro follow-link-capture-output (view)
  (declare (indent defun))
  `(let ((messages-point-before-action ;; will capture action output
          (with-current-buffer (messages-buffer)
            (point-max))))
     (org-glance-action-open ,view t)
     (with-current-buffer (messages-buffer)
       (buffer-substring-no-properties messages-point-before-action (point-max)))))

(cl-defmacro user-story (&body forms &key input view in act &allow-other-keys)
  (declare (indent defun))
  (cl-flet ((user-input-to-kbd-macro (str) (s-join " SPC " (s-split-words str)))
            (join-user-inputs (list) (s-join " RET " list)))
    (let ((user-input (cond ((stringp input) (user-input-to-kbd-macro input))
                            ((listp input) (join-user-inputs (mapcar #'user-input-to-kbd-macro (eval input)))))))
      `(with-temp-view ,view ,in
         (if (and ,act ,input)
             (with-user-input ,user-input
               (cond ((eq ,act 'materialize) (in-materialized-buffer ,view ,@forms))
                     ((eq ,act 'open) (follow-link-capture-output ,view))
                     (t (error "Unknown action called in user story."))))
           ,@forms)))))

;;; test-helper.el ends here
