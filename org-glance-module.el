(require 'load-relative)
(require 'f)
(require 'files)

(defvar org-glance-module-root-directory (file-name-directory (f-join (__FILE__) "..")))

(cl-defmacro org-glance-module-filename (module)
  `(let* ((m (format "org-glance.%s" (quote ,module)))
          (path (concat (s-replace "." "/" m) ".el")))
     (f-join org-glance-module-root-directory path)))

(cl-defmacro org-glance:require (&rest modules)
  (declare (indent 0) (debug t))
  `(cl-loop
      for module in (quote ,modules)
      for module-name = (format "org-glance.%s" module)
      for module-path = (f-join org-glance-module-root-directory (concat (s-replace "." "/" module-name) ".el"))
      collect (condition-case nil
                  (require (intern module-name) module-path)
                (file-missing (require module)))))

(cl-defmacro org-glance:provide ()
  `(provide (intern (file-name-sans-extension
                     (s-replace "/" "." (file-relative-name (__FILE__) org-glance-module-root-directory))))))

(provide 'org-glance-module)
