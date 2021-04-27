(require 'org)
(require 'org-element)
(require 'load-relative)

(require 'org-glance-module)

(org-glance-module-import lib.utils.helpers)
(org-glance-module-import lib.core.scope)

(define-error 'org-glance-db-outdated "Material view database is outdated" 'user-error)

(defun org-glance-db-outdated (format &rest args)
  "Raise `org-glance-db-outdated' exception formatted with FORMAT ARGS."
  (signal 'org-glance-db-outdated
          (list (apply #'format-message format args))))

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

(defun org-glance-db-load (file)
  (-some->> file
    org-glance-read-file-headlines
    (mapcar 'org-glance-db--deserialize)))

(cl-defun org-glance-db--serialize (headline)
  (prin1-to-string
   (list (org-element-property :TITLE headline)
         (org-element-property :raw-value headline)
         (org-element-property :begin headline)
         (org-element-property :file headline)
         (or (org-element-property :ORG_GLANCE_ID headline)
             (secure-hash 'sha512 (org-element-property :raw-value headline))))))

(cl-defun org-glance-db--deserialize (input)
  (cl-destructuring-bind (alias title begin file hash) input
    (org-element-create
     'headline
     `(:TITLE ,alias
       :raw-value ,title
       :begin ,begin
       :file ,file
       :ORG_GLANCE_ID ,hash))))

(cl-defun org-glance-headlines
    (&key db
       (scope '(agenda))
       (filter #'(lambda (_) t))
       (db-init nil))
  (let* ((create-db? (or (and db db-init) (and db (not (file-exists-p db)))))
         (load-db? (and (not (null db)) (file-exists-p db)))
         (skip-db? (null db)))
    (cond (create-db? (org-glance-db-init db (org-glance-scope-headlines scope filter)))
          (load-db?   (org-glance-db-load db))
          (skip-db?   (org-glance-scope-headlines scope filter))
          (t         (user-error "Nothing to glance at (scope: %s)" scope)))))

(org-glance-module-provide)
