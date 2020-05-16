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

(defmacro with-scope (scope &rest forms)
  (declare (indent 1))
  `(let* ((scope-file (make-temp-file "org-glance-test-"))
          (org-agenda-files (list scope-file))
          (default-directory org-glance-test/test-resources-path))
     (with-temp-file scope-file
       (message "Create scope %s from %s" scope-file ,scope)
       (insert-file-contents-literally (org-glance-test-get-resource ,scope)))
     (message "Scope file size: %s" (->> scope-file
                                         file-attributes
                                         file-attribute-size
                                         file-size-human-readable))
     (unwind-protect
         ,@forms
       (message "Exit scope %s" scope-file)
       ;; (kill-buffer (get-file-buffer scope-file))
       (message "Remove file %s" scope-file)
       ;; (delete-file scope-file)
       )))

(cl-defmacro with-view (view &rest forms)
  (declare (indent 1))
  `(unwind-protect
       (progn
         (message "Create view %s" ,view)
         (org-glance-def-view ,view)
         ,@forms)
     (message "Remove view %s" ,view)
     (org-glance-remove-view ,view)))

(cl-defmacro with-user-input (choice &rest forms)
  (declare (indent 1))
  `(let ((user-input (format "%s RET" (eval ,choice))))
     (with-simulated-input user-input
       ,@forms)))

(cl-defmacro in-materialized-buffer (view entry &rest forms)
  (declare (indent 2))
  `(let ((buffer (with-user-input ,entry (org-glance-action-materialize ,view t))))
     (unwind-protect
         (with-current-buffer buffer
           (message "Visit materialized view %s at buffer %s" ,view (current-buffer))
           ,@forms)
       (message "Kill buffer %s" buffer)
       ;; (kill-buffer buffer)
       )))

(cl-defmacro user-story (&body forms &key input view in act &allow-other-keys)
  (declare (indent 8))
  (let ((user-input
         (cond
          ((stringp input)
           (s-join " SPC " (s-split-words input)))
          ((listp input)
           (concat (s-join " RET "
                           (cl-loop for cmd in (eval input)
                                    collect (s-join " SPC " (s-split-words cmd))))
                   " RET")))))
    `(with-scope ,in
       (with-view ,view
         (message "Simulate user input: %s" ,user-input)
         (cond

          ((eq ,act 'materialize)
           (in-materialized-buffer ,view ,user-input
             ,@forms))

          ((eq ,act 'open)
           (with-user-input ,user-input
             (let ((messages-point-before-action ;; will capture action output
                    (with-current-buffer (messages-buffer)
                      (point-max))))
               (org-glance-action-open ,view t)
               (with-current-buffer (messages-buffer)
                 (buffer-substring-no-properties messages-point-before-action (point-max))))))

          (t (error "Unknown action in user story.")))))))

;;; test-helper.el ends here
