(eval-and-compile
  (require 'org)
  (require 'org-element)
  (require 'load-relative))

(define-error 'org-glance-cache-outdated
  "Material view cache file is outdated"
  'user-error)

;; (defun org-glance-reread (&optional view)
;;   (interactive)
;;   (setq view (-org-glance-view-completing-read view))
;;   (org-glance-cache-reread
;;    :scope (gethash (intern view) org-glance-view-scopes '(agenda))
;;    :filter (-org-glance-filter-for view)
;;    :cache-file (-org-glance-cache-for view)))

(defun org-glance-cache-outdated (format &rest args)
  "Raise `org-glance-cache-outdated' exception formatted with FORMAT ARGS."
  (signal 'org-glance-cache-outdated
          (list (apply #'format-message format args))))

(cl-defun org-glance-cache--serialize (headline)
  (prin1-to-string
   (list (org-element-property :TITLE headline)
         (org-element-property :raw-value headline)
         (org-element-property :begin headline)
         (org-element-property :file headline))))

(cl-defun org-glance-cache--deserialize (input)
  (cl-destructuring-bind (alias title begin file) input
    (org-element-create
     'headline
     `(:TITLE
       ,alias
       :raw-value ,title
       :begin ,begin
       :file ,file))))

(provide-me)
;;; org-glance-cache.el ends here
