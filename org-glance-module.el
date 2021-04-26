(require 'load-relative)
(require 'f)
(require 'files)

(defvar org-glance-module-root-directory (file-name-directory (f-join (__FILE__) "..")))

(defmacro org-glance-module-import (module)
  `(let* ((m (format "org-glance.%s" (quote ,module)))
          (path (concat (s-replace "." "/" m) ".el")))
     (require (intern m) (f-join org-glance-module-root-directory path))))

(defmacro org-glance-module-provide ()
  `(provide (intern (file-name-sans-extension
                     (s-replace "/" "." (file-relative-name (__FILE__) org-glance-module-root-directory))))))

(provide 'org-glance-module)
