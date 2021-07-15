(require 'org-archive)
(require 'org-glance-module)

(cl-defun org-glance:ensure-at-heading ()
  (unless (org-at-heading-p)
    (org-back-to-heading-or-point-min)))

(cl-defun org-glance:indent-level ()
  (save-excursion
    (org-glance:ensure-at-heading)
    (beginning-of-line)
    (cl-loop while (looking-at "\\*")
       for i from 0 do (forward-char)
       finally (return i))))

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

(defun org-glance-view:ensure-directory (view-id)
  ((and (member view-id (org-get-tags nil t)) (not (org-element-property "ORG_GLANCE_ID" )))
   (let* ((event-dir-abs
           (let ((default-directory (f-join default-directory "~/sync/resources/stories")))
             (read-directory-name "Specify story directory: ")))
          (event-dir-rel (file-relative-name event-dir-abs)))
     (condition-case nil
         (make-directory event-dir-abs)
       (error nil))
     (org-set-property "DIR" event-dir-rel)
     (org-set-property "ARCHIVE" (f-join event-dir-rel "story.org::"))
     (org-set-property "COOKIE_DATA" "todo recursive"))))

(cl-defun org-glance:generate-id (&optional (view-id (org-glance-view:completing-read)))
  (format "%s-%s-%s"
          view-id
          (format-time-string "%Y%m%d")
          (secure-hash 'md5 (buffer-string))))

(cl-defun org-glance:generate-id-for-subtree-at-point (&optional (view-id (org-glance-view:completing-read)))
  (save-excursion
    (org-glance:ensure-at-heading)
    (save-restriction
      (org-narrow-to-subtree)
      (org-set-property "ORG_GLANCE_ID"
                        (or (org-element-property :ORG_GLANCE_ID (org-element-at-point))
                            (org-glance:generate-id view-id))))))

(cl-defun org-glance:generate-dir-for-subtree-at-point (&optional (view-id (org-glance-view:completing-read)))
  (save-excursion
    (org-glance:ensure-at-heading)
    (save-restriction
      (org-narrow-to-subtree)
      (let* ((dir (f-join (org-glance-view-resource-location view-id)
                          (->> (org-element-property :raw-value (org-element-at-point))
                            (s-replace-regexp "[^a-z0-9A-Z_]" "-")
                            (s-replace-regexp "\\-+" "-")
                            (s-replace-regexp "\\-+$" "")
                            (s-truncate 30)
                            (concat (format-time-string "%Y-%m-%d_"))))))

        (org-set-property "DIR"
                          (or (org-element-property :DIR (org-element-at-point))
                              dir))

        (org-set-property "ARCHIVE"
                          (or (org-element-property :ARCHIVE (org-element-at-point))
                              (f-join dir (s-downcase (format "%s.org::" view-id)))))))))

(cl-defun org-glance-capture-subtree-at-point (&optional view-id)
  (interactive)
  (save-excursion
    (org-glance:ensure-at-heading)
    (let* ((view-id (if view-id
                        (symbol-name view-id)
                      (org-completing-read "Capture subtree for view: " (seq-difference
                                                                         (org-glance-view:ids)
                                                                         (mapcar #'intern (org-get-tags))))))
           (view (org-glance-view view-id)))
      (org-glance:generate-id-for-subtree-at-point view-id)
      (org-glance:generate-dir-for-subtree-at-point view-id)
      (unless (member (downcase view-id) (org-glance--collect-tags))
        (org-toggle-tag view-id))
      (let ((archive-location (car (org-archive--compute-location
                                    (or (org-entry-get nil "ARCHIVE" 'inherit)
	                                org-archive-location)))))
        (org-archive-subtree)
        (find-file archive-location)))))

(cl-defun org-glance:first-level-headline ()
  (cl-loop while (org-up-heading-safe)))

(cl-defun org-glance:expand-parents ()
  (save-excursion
    (org-glance:first-level-headline)))

(org-glance-module-provide)
