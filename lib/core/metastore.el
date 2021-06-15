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
      (puthash (org-glance-headline:id headline) (org-glance-headline:serialize headline) table))

    (append-to-file (prin1-to-string table) nil file-tmp)

    ;; apply changes to original file
    (org-glance--make-file-directory file)
    (when (file-exists-p file)
      (delete-file file t))
    (rename-file file-tmp file)
    (message "Metastore has been initialized: %s" file)
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

(cl-defun org-glance-metastore:headline (id)
  "Get org-element headline by ID."
  (let ((matched-headlines (cl-loop for vid in (org-glance-view:ids)
                              for metastore = (->> vid
                                                org-glance-view
                                                org-glance-view-metadata-location
                                                org-glance-metastore:read)
                              for headline = (gethash id metastore)
                              when headline
                              collect (org-glance-metastore:deserialize headline))))
    (unless matched-headlines
      (org-glance-headline-not-found "%s. Try to update view or make sure the headline was not deleted" id))
    (if (= (length matched-headlines) 1)
        (car matched-headlines)
      (car matched-headlines) ;; TODO Fix conflicts in DOCTOR method

      ;; (let ((conflicting-headlines (cl-loop for headline in matched-headlines
      ;;                                 collect (cons (format "%s at %d in file %s %s"
      ;;                                                       (org-glance-headline:title headline)
      ;;                                                       (org-glance-headline:begin headline)
      ;;                                                       (org-glance-headline:file headline)
      ;;                                                       headline)
      ;;                                               headline))))
      ;;   (alist-get
      ;;    (org-completing-read "ID collision detected. Please resolve it: " conflicting-headlines nil 'require-match)
      ;;    conflicting-headlines
      ;;    nil
      ;;    nil
      ;;    #'string=))
      )))

(org-glance-module-provide)
