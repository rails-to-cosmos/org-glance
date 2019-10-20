(defmacro from (relpath import &rest features)
  (declare (debug (form body))
           (indent 1))
  `(let* ((file (or load-file-name buffer-file-name))
          (path (file-name-directory file))
          (default-directory (expand-file-name ,(cadr (s-split ":" (symbol-name relpath))) path))
          (load-path (list default-directory)))

     (if (member (car features) '(:all 'all "all"))
         (loop for file in (directory-files default-directory t "^\\w.*\\.el$")
               do (let ((feature (->> file
                                      file-name-nondirectory
                                      file-name-sans-extension
                                      intern)))
                    (if (require feature file t)
                        (message "Feature %s has been loaded from file %s" feature file)
                      (message "Unable to load feature %s from file %s" feature file))))
       (loop for feature in features
             do (require feature)))))

(provide 'org-glance-helpers)
