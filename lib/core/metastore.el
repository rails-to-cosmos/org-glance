(require 'org)
(require 'org-element)
(require 'org-glance-module)

(org-glance-module-import lib.utils.helpers)
(org-glance-module-import lib.core.scope)
(org-glance-module-import lib.core.exceptions)

(cl-defun org-glance-metastore:init (file headlines)
  "Create FILE and save HEADLINES metadata to it."
  (let ((table (make-hash-table :test 'equal))
        (file-tmp (make-temp-file "org-glance-view-metadata-")))

    (dolist (headline headlines)
      (puthash (org-glance-headline:id headline)
               (list (or (org-element-property :TITLE headline)
                         (org-element-property :raw-value headline))
                     (org-element-property :begin headline)
                     (org-element-property :file headline))
               table))

    (append-to-file (prin1-to-string table) nil file-tmp)

    ;; apply changes to original file
    (org-glance--make-file-directory file)
    (when (file-exists-p file)
      (delete-file file t))
    (rename-file file-tmp file)
    (message "Database has been initialized: %s" file)
    headlines))

(defun org-glance-metastore:read (file)
  "Read metastore FILE to memory."
  (with-temp-buffer
    (insert-file-contents file)
    (read (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-glance-metastore:deserialize (headline)
  "Convert metastore value HEADLINE to org-element enriched with metadata."
  (cl-destructuring-bind (title begin file) headline
    (org-element-create 'headline
                        `(:raw-value ,title
                          :begin ,begin
                          :file ,file
                          :ORG_GLANCE_ID ,id))))

(defun org-glance-metastore:load (file)
  (let ((metastore (org-glance-metastore:read file)))
    (cl-loop for id being the hash-keys of metastore
       collect (org-glance-metastore:deserialize (gethash id metastore)))))

(cl-defun org-glance-headlines
    (&key db
       (scope '(agenda))
       (filter #'(lambda (_) t))
       (db-init nil))
  "Deprecated method, refactor needed."
  (let* ((create-db? (or (and db db-init) (and db (not (file-exists-p db)))))
         (load-db? (and (not (null db)) (file-exists-p db)))
         (skip-db? (null db)))
    (cond (create-db? (org-glance-metastore:init db (org-glance-scope-headlines scope filter)))
          (load-db?   (org-glance-metastore:load db))
          (skip-db?   (org-glance-scope-headlines scope filter))
          (t          (user-error "Nothing to glance at (scope: %s)" scope)))))

(org-glance-module-provide)
