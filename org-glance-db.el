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

(defun org-glance-format (headline)
  (or (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)))

(defun org-glance-choose-headline (choice headlines)
  (cl-loop for hl in headlines
           when (string= (org-glance-format hl) choice)
           do (cl-return hl)))

(defun org-glance-prompt-headlines (prompt headlines)
  (org-completing-read prompt (mapcar #'org-glance-format headlines)))

(defun org-glance-view-headlines--formatted (view)
  "List headlines as formatted strings for VIEW."
  (->> view
       org-glance-view-headlines
       (mapcar #'org-glance-format)
       (mapcar #'(lambda (hl) (format "[%s] %s" view hl)))))

(cl-defun org-glance-db-init (db headlines)
  (unless (file-exists-p (file-name-directory db))
    (make-directory (file-name-directory db) t))
  (with-temp-file db
    (insert "`(")
    (dolist (headline headlines)
      (insert (org-glance-db--serialize headline) "\n"))
    (insert ")"))
  (message "Database has been initialized: %s" db)
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
