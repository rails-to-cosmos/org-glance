(require 'load-relative)
(require 'f)
(require 'files)

(defvar org-glance-root-directory (file-name-directory (f-join (__FILE__) "..")))

(defmacro pythonic-import (module)
  `(let* ((m (format "org-glance.%s" (quote ,module)))
          (path (concat (s-replace "." "/" m) ".el")))
     (require (intern m) (f-join org-glance-root-directory path))))

(defmacro pythonic-module ()
  `(provide (intern (file-name-sans-extension
                     (s-replace "/" "." (file-relative-name (__FILE__) org-glance-root-directory))))))

(provide 'pythonic-import)
