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

(cl-defun org-glance-db-init (db headlines)
  (message "Init database file %s..." db)
  (unless (file-exists-p (file-name-directory db))
    (make-directory (file-name-directory db) t))
  (with-temp-file db
    (insert "`(")
    (dolist (headline headlines)
      (insert (org-glance-db--serialize headline) "\n"))
    (insert ")"))
  headlines)

(cl-defun org-glance-db-load (file)
  (let ((entries
         (with-temp-buffer (insert-file-contents file)
                           (->> (buffer-string)
                                substring-no-properties
                                read
                                eval))))
    (mapcar #'org-glance-db--deserialize entries)))

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
