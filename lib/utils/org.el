(require 'org-archive)
(require 'org-glance-module)

(cl-defun org-glance:ensure-at-heading ()
  (unless (org-at-heading-p)
    (org-back-to-heading-or-point-min)))

;; (defun org-glance:recreate-folder-structure-in-subtree-at-point ()
;;   (interactive)
;;   (save-excursion
;;     (org-back-to-heading)
;;     (loop for directory in (directory-files-recursively (org-attach-dir-get-create) ".*" t)
;;        if (file-directory-p directory)
;;        do (save-excursion
;;             (save-restriction
;;               (org-narrow-to-subtree)
;;               (condition-case nil
;;                   (save-excursion (search-forward directory))
;;                 (error (org-insert-heading '(4))
;;                        (insert (file-name-nondirectory directory))
;;                        (org-set-property "DIR" directory)
;;                        (org-demote))))))))

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

(cl-defun org-glance-view:generate-id-for-subtree-at-point (&optional (view-id (org-glance-view:completing-read)))
  (save-excursion
    (org-glance:ensure-at-heading)
    (save-restriction
      (org-narrow-to-subtree)
      (or (org-glance-headline:id (org-element-at-point))
          (format "%s-%s-%s"
                  view-id
                  (format-time-string "%Y%m%d")
                  (secure-hash 'md5 (buffer-string)))))))

(cl-defun org-glance-view:resource-location (&optional (view-id (org-glance-view:completing-read)))
  "Path to directory where VIEW-ID resources and metadata are stored."
  (abbreviate-file-name
   (f-join org-glance-directory
           (s-downcase (format "%s" view-id))
           "resources")))

(cl-defun org-glance:generate-dir-for-subtree-at-point (&optional (view-id (org-glance-view:completing-read)))
  (save-excursion
    (org-glance:ensure-at-heading)
    (save-restriction
      (org-narrow-to-subtree)
      (f-join (org-glance-view:resource-location view-id)
              (->> (org-element-property :raw-value (org-element-at-point))
                   (s-replace-regexp "[^a-z0-9A-Z_]" "-")
                   (s-replace-regexp "\\-+" "-")
                   (s-replace-regexp "\\-+$" "")
                   (s-truncate 30)
                   (list (format-time-string "%Y-%m-%d"))
                   (s-join "_"))))))

(cl-defun org-glance:first-level-headline ()
  (cl-loop while (org-up-heading-safe)))

(cl-defun org-glance:expand-parents ()
  (save-excursion
    (org-glance:first-level-headline)))

(cl-defun org-glance:parse-links (raw-value)
  (-filter
   (lambda (it) (condition-case nil (eql (car it) 'link) (error nil)))
   (org-element-parse-secondary-string raw-value '(link))))

(cl-defun org-glance:clean-title (raw-value)
  (cl-loop
     with links = (org-glance:parse-links raw-value)
     for link in links
     for contents-begin = (org-element-property :contents-begin link)
     for contents-end = (org-element-property :contents-end link)
     if (and contents-begin contents-end)
     collect (substring raw-value (1- contents-begin) (1- contents-end))
     into titles
     else
     collect (let ((webpage-title (org-glance:title-from-url (org-element-property :raw-link link))))
               (if (string-empty-p webpage-title)
                   (read-string "New title: ")
                 webpage-title))
     into titles
     finally (return (cl-loop
                        with result = raw-value
                        for title in titles
                        do (setq result (s-replace-regexp org-link-any-re title result))
                        finally (return result)))))

(org-glance:provide)
