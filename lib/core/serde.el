(require 'org)
(require 'org-element)
(require 'org-glance-module)

(org-glance-module-import lib.utils.helpers)
(org-glance-module-import lib.core.scope)
(org-glance-module-import lib.core.exceptions)

(cl-defun org-glance-db-init (db headlines)
  (let ((table (make-hash-table :test 'equal))
        (db-tmp (make-temp-file "org-glance-view-metadata-")))

    (dolist (headline headlines)
      (puthash (org-glance-headline:id headline)
               (list (or (org-element-property :TITLE headline)
                         (org-element-property :raw-value headline))
                     (org-element-property :begin headline)
                     (org-element-property :file headline))
               table))

    (append-to-file (prin1-to-string table) nil db-tmp)

    ;; apply changes to original file
    (org-glance--make-file-directory db)
    (when (file-exists-p db)
      (delete-file db t))
    (rename-file db-tmp db)
    (message "Database has been initialized: %s" db)
    headlines))

(defun org-glance-db-load (file)
  (let ((hash-table (with-temp-buffer
                      (insert-file-contents file)
                      (read (buffer-substring-no-properties (point-min) (point-max))))))
    (cl-loop for id being the hash-keys of hash-table
       collect (cl-destructuring-bind (title begin file) (gethash id hash-table)
                 (org-element-create
                  'headline
                  `(:raw-value ,title
                               :begin ,begin
                               :file ,file
                               :ORG_GLANCE_ID ,id))))))

;; (org-glance-db-init "/home/akatovda/sync/views/article/metadata.el" (org-glance-view:headlines (org-glance-view:get-view-by-id 'Article)))
;; (org-glance-db-load "/home/akatovda/sync/views/article/metadata.el")

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
          (t          (user-error "Nothing to glance at (scope: %s)" scope)))))

(org-glance-module-provide)
