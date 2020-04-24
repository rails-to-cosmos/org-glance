(define-error 'org-glance-cache-outdated
  "Material view cache file is outdated"
  'user-error)

(defun org-glance-cache-outdated (format &rest args)
  "Raise `org-glance-cache-outdated' exception formatted with FORMAT ARGS."
  (signal 'org-glance-cache-outdated
          (list (apply #'format-message format args))))

(cl-defun org-glance-cache--serialize (headline &key title-property)
  (prin1-to-string
   (list (when title-property
           (org-element-property title-property headline))
         (org-element-property :raw-value headline)
         (org-element-property :begin headline)
         (org-element-property :file headline))))

(cl-defun org-glance-cache--deserialize (input &key title-property)
  (cl-destructuring-bind (alias title begin file) input
    (org-element-create 'headline
                        `(,title-property ,alias
                                          :raw-value ,title
                                          :begin ,begin
                                          :file ,file))))

(provide-me)
