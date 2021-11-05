
;; (let ((m (org-glance-metastore)))
;;   (org-glance-metastore-set m 'a 1)
;;   (org-glance-metastore-save m "/tmp/1.el")
;;   (org-glance-metastore-remove m 'a)
;;   (org-glance-metastore-load m "/tmp/1.el")
;;   m
;;   )

;; (cl-defun org-glance-metastore:write (file metastore)
;;   (declare (indent 1))
;;   (-org-glance:make-file-directory file)
;;   (with-temp-file file
;;     (insert (prin1-to-string metastore)))
;;   metastore)

;; (cl-defun org-glance-metastore:add-headline (headline metastore)
;;   (puthash (org-glance-headline:id headline)
;;            (org-glance-headline:serialize headline)
;;            metastore))

;; (cl-defun org-glance-metastore:remove-headline (headline metastore)
;;   (remhash (org-glance-headline:id headline)
;;            metastore))

;; (cl-defun org-glance-metastore:create (file &optional headlines)
;;   "Create metastore from HEADLINES and write it to FILE."
;;   (org-glance-metastore:write file
;;     (cl-loop
;;        with metastore = (make-hash-table :test 'equal)
;;        for headline in headlines
;;        do (org-glance-metastore:add-headline headline metastore)
;;        finally (return metastore))))

;; (defun org-glance-metastore:read (file)
;;   "Read metastore from FILE."
;;   (with-temp-buffer
;;     (insert-file-contents file)
;;     (read (buffer-substring-no-properties (point-min) (point-max)))))

;; (defun org-glance-metastore:headlines (metastore)
;;   (cl-loop
;;      for id being the hash-keys of metastore using (hash-value value)
;;      collect (-> value
;;                (org-glance-headline:deserialize)
;;                (org-glance-headline:enrich :ORG_GLANCE_ID id))))

;; (cl-defun org-glance-headlines
;;     (&key db
;;        (scope '(agenda))
;;        (filter #'(lambda (_) t))
;;        (db-init nil))
;;   "Deprecated method, refactor it."
;;   (let* ((create-db? (or (and db db-init) (and db (not (file-exists-p db)))))
;;          (load-db? (and (not (null db)) (file-exists-p db)))
;;          (skip-db? (null db)))
;;     (cond (create-db? (let ((headlines (org-glance-scope-headlines scope filter)))
;;                         (org-glance-metastore:create db headlines)
;;                         headlines))
;;           (load-db?   (org-glance-metastore:headlines (org-glance-metastore:read db)))
;;           (skip-db?   (org-glance-scope-headlines scope filter))
;;           (t          (user-error "Nothing to glance at (scope: %s)" scope)))))

;; (cl-defun org-glance-metastore:get-headline (id)
;;   "Get headline by ID."
;;   (cl-loop
;;      for class being the hash-keys of org-glance-class-registry
;;      for metastore = (->> class
;;                           org-glance:get-class
;;                           org-glance-view:metastore-location
;;                           org-glance-metastore:read)
;;      for headline = (gethash id metastore)
;;      when headline
;;      collect (-> headline
;;                  (org-glance-headline:deserialize)
;;                  (org-glance-headline:enrich :ORG_GLANCE_ID id))
;;      into result
;;      finally (return (car result))))

;; (cl-defun org-glance-metastore:choose-headline (&key (filter #'org-glance-headline:active?))
;;   "Main retriever, refactor needed."
;;   (let* ((headlines (cl-loop
;;                        for class being the hash-keys of org-glance-class-registry
;;                        append (cl-loop
;;                                  for headline in (org-glance-view:headlines class)
;;                                  when (funcall filter headline)
;;                                  collect (cons ;; duplication of format*
;;                                           (format "[%s] %s"
;;                                                   ;; (cl-loop
;;                                                   ;;    for icon in (list
;;                                                   ;;                 ""
;;                                                   ;;                 ;; (when (org-glance-headline:encrypted? headline) (propertize "🔒" 'face 'bold))
;;                                                   ;;                 ;; (when (org-glance-headline:contains-link-p headline) (let ((s "A"))
;;                                                   ;;                 ;;                                                (add-face-text-property 0 1 '(:underline t :weight bold) nil s)
;;                                                   ;;                 ;;                                                s))
;;                                                   ;;                 ;; (when (org-glance-headline:contains-property-p headline) "property")
;;                                                   ;;                 )
;;                                                   ;;    when icon
;;                                                   ;;    collect icon into modifiers
;;                                                   ;;    finally (return (if modifiers
;;                                                   ;;                        (concat "" (s-join ", " modifiers) " ")
;;                                                   ;;                      "")))
;;                                                   class
;;                                                   (org-glance-headline:title headline))
;;                                           headline))))
;;          (choice (org-completing-read "Headline: " headlines))
;;          (headline (alist-get choice headlines nil nil #'string=)))
;;     (unless headline
;;       (org-glance-exception:HEADLINE-NOT-FOUND choice))

;;     (org-glance-headline:narrow headline
;;       (org-glance-headline:create-from-element-at-point))))
