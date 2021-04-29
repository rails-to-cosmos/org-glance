(require 'org-glance-module)

(defun org-glance:recreate-folder-structure-in-subtree-at-point ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (loop for directory in (directory-files-recursively (org-attach-dir-get-create) ".*" t)
       if (file-directory-p directory)
       do (save-excursion
            (save-restriction
              (org-narrow-to-subtree)
              (condition-case nil
                  (save-excursion (search-forward directory))
                (error (org-insert-heading '(4))
                       (insert (file-name-nondirectory directory))
                       (org-set-property "DIR" directory)
                       (org-demote))))))))

;; (defun org-glance-view:ensure-directory (view-id)
;;   ((and (member view-id (org-get-tags nil t)) (not (org-element-property "ORG_GLANCE_ID" )))
;;    (let* ((event-dir-abs
;;            (let ((default-directory (f-join default-directory "~/sync/resources/stories")))
;;              (read-directory-name "Specify story directory: ")))
;;           (event-dir-rel (file-relative-name event-dir-abs)))
;;      (condition-case nil
;;          (make-directory event-dir-abs)
;;        (error nil))
;;      (org-set-property "DIR" event-dir-rel)
;;      (org-set-property "ARCHIVE" (f-join event-dir-rel "story.org::"))
;;      (org-set-property "COOKIE_DATA" "todo recursive"))))

(org-glance-module-provide)
