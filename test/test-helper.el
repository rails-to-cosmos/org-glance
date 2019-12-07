;;; test-helper.el --- Helpers for org-glance-test.el

(require 'f)

(defvar test-path
  (f-dirname (f-this-file)))

(defvar code-path
  (f-parent test-path))

(defvar sandbox-path
  (f-expand "sandbox" test-path))

(defvar resource-path
  (f-expand "resources" test-path))

(require 'org-glance (f-expand "org-glance.el" code-path))

(defmacro with-sandbox (&rest body)
  "Evaluate BODY in an empty temporary directory."
  `(let ((default-directory sandbox-path))
     (when (f-dir? sandbox-path)
       (f-delete sandbox-path :force))
     (f-mkdir sandbox-path)
     ,@body))

;;; test-helper.el ends here
