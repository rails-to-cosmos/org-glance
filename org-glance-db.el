(eval-and-compile
  (require 'org)
  (require 'org-element)
  (require 'load-relative))

(define-error 'org-glance-db-outdated
  "Material view database is outdated"
  'user-error)

(defun org-glance-db-outdated (format &rest args)
  "Raise `org-glance-db-outdated' exception formatted with FORMAT ARGS."
  (signal 'org-glance-db-outdated
          (list (apply #'format-message format args))))

(cl-defun org-glance-db-save (file entries)
  (unless (file-exists-p (file-name-directory file))
    (make-directory (file-name-directory file) t))
  (with-temp-file file
    (insert "`(")
    (dolist (entry entries)
      (insert (org-glance-db--serialize entry) "\n"))
    (insert ")"))
  entries)

(cl-defun org-glance-db-load (file)
  (let ((entries
         (with-temp-buffer (insert-file-contents file)
                           (->> (buffer-string)
                                substring-no-properties
                                read
                                eval))))
    (cl-loop for entry in entries
             collect (org-glance-db--deserialize entry))))

(cl-defun org-glance-db-reread (&key
                                   filter
                                   cache-file
                                   scope
                                   &allow-other-keys)
  (let ((headlines (org-glance-read scope :filter filter)))
    (unless headlines
      (user-error "Nothing to glance at scope %s" (pp-to-string scope)))
    (when cache-file
      (org-glance-db-save cache-file headlines))
    headlines))

(cl-defun org-glance-db--serialize (headline)
  (prin1-to-string
   (list (org-element-property :TITLE headline)
         (org-element-property :raw-value headline)
         (org-element-property :begin headline)
         (org-element-property :file headline))))

(cl-defun org-glance-db--deserialize (input)
  (cl-destructuring-bind (alias title begin file) input
    (org-element-create
     'headline
     `(:TITLE
       ,alias
       :raw-value ,title
       :begin ,begin
       :file ,file))))

(provide-me)
;;; org-glance-db.el ends here
