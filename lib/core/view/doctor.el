(require 'org-glance-module)

(org-glance-module-import lib.core.view)

(defvar org-glance-view-doctor-header-template "#    -*- mode: org; mode: org-glance-summary -*-

#+CATEGORY: $category
#+STARTUP: overview

=$error_count warnings found=

")

(cl-defun org-glance-view:doctor-location (&optional (view-id (org-glance-view:completing-read)))
  "Path to file where VIEW-ID exported headlines are stored."
  (let ((view-name (s-downcase (format "%s" view-id))))
    (f-join org-glance-view-location
            view-name
            (format "%s.org_doctor" view-name))))

(cl-defun org-glance-view-doctor:check-for-corrupted-properties (file view headline)
  "Fix `org-glance' VIEW HEADLINE and return `t' if fix has been succeeded or nil otherwise."
  (not (org-glance-headline-p headline))

  ;; (when (and (not (org-glance-headline-p headline)) ;; current headline can't be identified as org-glance-headline
  ;;            (org-glance-headline:matches-filter? (org-glance-view-filter view) headline) ;; but it has required tags
  ;;            )
  ;;   (goto-char (org-glance-headline:begin headline))
  ;;   (save-restriction
  ;;     (org-narrow-to-subtree)
  ;;     (hlt-highlight-region (point-min) (point-max))
  ;;     (when (y-or-n-p "Some properties are corrupted. Recapture the headline?")
  ;;       (org-glance-capture-subtree-at-point view-id)
  ;;       (message "Patch headline %s in file %s" headline file)
  ;;       t)))
  )

(cl-defun org-glance-view-doctor:check-for-links-in-title (file view headline)
  (s-matches? org-link-any-re (org-glance-headline:title headline))

  ;; (when
  ;;     (goto-char (org-glance-headline:begin headline))
  ;;   (when (y-or-n-p "Headline contains org-link in title. Replace it with the raw-value?")))
  )

(cl-defun org-glance-view:doctor (&optional (vid (org-glance-view:completing-read)))
  (interactive)
  (let* ((view (org-glance-view:get-view-by-id vid))
         (db (org-glance-view-metadata-location view))
         (scope (or (org-glance-view-scope view) org-glance-default-scope))
         (report-buffer (format "*org-glance-doctor:%s*" vid))
         (err-count 0))

    (if (get-buffer report-buffer)
        (with-current-buffer report-buffer
          (delete-region (point-min) (point-max)))
      (get-buffer-create report-buffer))

    (cl-loop for file in (org-glance-scope scope)
       do (save-window-excursion
            (message "Working with %s" file)
            (redisplay)
            (find-file file)
            (org-element-map (org-element-parse-buffer 'headline) 'headline
              (lambda (headline)
                ;; - [ ] check if there is an org-link in title
                ;; - [ ] check if visited file is not headline archive file
                ;; - [ ] check for view data structure: no empty directories etc
                ;; - [ ] check for view data structure: proper partitioning
                ;; - [ ] check for nested views and ask to flatten them

                (when (org-glance-headline:matches-filter? (org-glance-view-filter view) headline)

                  (when (org-glance-view-doctor:check-for-corrupted-properties file view headline)
                    (with-current-buffer report-buffer
                      (goto-char (point-max))
                      ;; (insert (org-glance-headline:contents headline) "\n")
                      ;; (insert (format "* Uncaptured headline\n%s\n" (org-glance-headline:title headline)))
                      )
                    (incf err-count))

                  (when (org-glance-view-doctor:check-for-links-in-title file view headline)
                    (with-current-buffer report-buffer
                      (goto-char (point-max))
                      ;; (insert (org-glance-headline:contents headline) "\n")
                      ;; (insert (format "* Link in title\n%s\n" (org-glance-headline:title headline)))
                      )
                    (incf err-count)))))))

    (with-current-buffer report-buffer
      (org-mode)
      (goto-char (point-min))
      (insert (org-glance:f org-glance-view-doctor-header-template
                            :category vid
                            :error_count err-count))
      (write-file (org-glance-view:doctor-location vid)))

    ;; (switch-to-buffer report-buffer)
    ))

(org-glance-module-provide)
