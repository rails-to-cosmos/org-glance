(require 'org)
(require 'org-element)
(require 'org-glance-module)

(org-glance-module-import lib.core.scope)
(org-glance-module-import lib.core.exceptions)
(org-glance-module-import lib.core.headline)
(org-glance-module-import lib.utils.helpers)

(cl-defun org-glance-metastore:create (file &optional headlines)
  "Write HEADLINES to FILE."
  (let ((metastore (cl-loop for headline in headlines
                      with table = (make-hash-table :test 'equal)
                      do (puthash (org-glance-headline:id headline) (org-glance-metastore:serialize-headline headline) table)
                      finally (return table))))
    (with-temp-file file
      (insert (prin1-to-string metastore)))
    metastore))

(defun org-glance-metastore:read (file)
  "Read metastore from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defun org-glance-metastore:serialize-headline (headline)
  "Serialize HEADLINE to store it on disk."
  (list (org-glance-headline:title headline)
        (org-glance-headline:begin headline)
        (org-glance-headline:file headline)))

(defun org-glance-metastore:deserialize-headline (id headline)
  "Convert metastore value HEADLINE to org-element enriched with metadata."
  (cl-destructuring-bind (title begin file) headline
    (org-element-create
     'headline
     (list :raw-value title
           :begin begin
           :file file
           :ORG_GLANCE_ID id))))

(defun org-glance-metastore:headlines (metastore)
  (cl-loop for id being the hash-keys of metastore
     for value = (gethash id metastore)
     collect (org-glance-metastore:deserialize-headline id value)))

(cl-defun org-glance-headlines
    (&key db
       (scope '(agenda))
       (filter #'(lambda (_) t))
       (db-init nil))
  "Deprecated method, refactor needed."
  (let* ((create-db? (or (and db db-init) (and db (not (file-exists-p db)))))
         (load-db? (and (not (null db)) (file-exists-p db)))
         (skip-db? (null db)))
    (cond (create-db? (let ((headlines (org-glance-scope-headlines scope filter)))
                        (org-glance-metastore:create db headlines)
                        headlines))
          (load-db?   (org-glance-metastore:headlines (org-glance-metastore:read db)))
          (skip-db?   (org-glance-scope-headlines scope filter))
          (t          (user-error "Nothing to glance at (scope: %s)" scope)))))

(cl-defun org-glance-metastore:get-headline-by-id (id)
  "Get org-element headline by ID."
  (let ((matched-headlines
         (cl-loop for vid in (org-glance-view:ids)
            for metastore = (->> vid
                              org-glance-view
                              org-glance-view-metastore-location
                              org-glance-metastore:read)
            for headline = (gethash id metastore)
            when headline
            collect (org-glance-metastore:deserialize-headline id headline))))
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

(cl-defun org-glance-metastore:choose-headline ()
  (let* ((headlines (cl-loop for vid in (org-glance-view:ids)
                       append (cl-loop for headline in (org-glance-view:headlines vid)
                                 collect (cons ;; duplication of format*
                                          (format "[%s] %s" vid (org-glance-headline:format headline))
                                          headline))))
         (headline (alist-get (org-completing-read "Headline: " headlines) headlines nil nil #'string=)))
    (org-glance-headline:narrow headline
      (org-glance-headline:at-point))))

(org-glance-module-provide)
