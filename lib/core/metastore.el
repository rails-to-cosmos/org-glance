(require 'org)
(require 'org-element)
(require 'org-glance-module)

(org-glance-module-import lib.utils.helpers)
(org-glance-module-import lib.core.scope)
(org-glance-module-import lib.core.exceptions)

(cl-defun org-glance-metastore:create (file &optional headlines)
  "Create FILE and save HEADLINES metadata to it."
  (with-temp-file file
    (->>
        (cl-loop for headline in headlines
           with table = (make-hash-table :test 'equal)
           do (puthash (org-glance-headline:id headline) (org-glance-headline:serialize headline) table)
           finally (return table))
      prin1-to-string
      insert))
  (message "Metastore has been initialized: %s" file)
  headlines)

(defun org-glance-metastore:read (file)
  "Read metastore FILE to memory."
  (with-temp-buffer
    (insert-file-contents file)
    (read (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-glance-metastore:headlines (metastore)
  (cl-loop for id being the hash-keys of metastore
     collect (org-glance-headline:deserialize id (gethash id metastore))))

(cl-defun org-glance-headlines
    (&key db
       (scope '(agenda))
       (filter #'(lambda (_) t))
       (db-init nil))
  "Deprecated method, refactor needed."
  (let* ((create-db? (or (and db db-init) (and db (not (file-exists-p db)))))
         (load-db? (and (not (null db)) (file-exists-p db)))
         (skip-db? (null db)))
    (cond (create-db? (org-glance-metastore:create db (org-glance-scope-headlines scope filter)))
          (load-db?   (org-glance-metastore:headlines (org-glance-metastore:read db)))
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
                              collect (org-glance-headline:deserialize id headline))))
    (unless matched-headlines
      (org-glance-exception:headline-not-found "%s. Try to update view or make sure the headline was not deleted" id))
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
