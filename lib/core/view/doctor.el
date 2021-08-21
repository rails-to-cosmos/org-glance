(require 'org-glance-module)

(org-glance-module-import lib.core.view)

(defconst org-glance-view-doctor-header-template "#    -*- mode: org; mode: org-glance-overview -*-

#+CATEGORY: ${vid}
#+STARTUP: overview

=${error-count} warnings found=

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
  ;;       (org-glance-view:capture-headline-at-point view-id)
  ;;       (message "Patch headline %s in file %s" headline file)
  ;;       t)))
  )

(cl-defun org-glance-view-doctor:check-for-links-in-title (file view headline)
  (s-matches? org-link-any-re (org-glance-headline:title headline))

  ;; (when
  ;;     (goto-char (org-glance-headline:begin headline))
  ;;   (when (y-or-n-p "Headline contains org-link in title. Replace it with the raw-value?")))
  )

(cl-defun org-glance-headline:contents* (headline)
  (let ((title (org-glance-headline:title headline))
        (id (org-glance-headline:id headline)))
    (org-glance:format
     "* ${title}
      |:PROPERTIES:
      |:ORG_GLANCE_ID: ${id}
      |:END:")))

;; (cl-defun org-glance-view:doctor (&optional (vid (org-glance-view:completing-read)))
;;   (interactive)
;;   (let* ((view (org-glance-view:get-view-by-id vid))
;;          (db (org-glance-view-metastore-location view))
;;          (scope (or (org-glance-view-scope view) org-glance-default-scope))
;;          (report-buffer (format "*org-glance-doctor:%s*" vid))
;;          (error-count 0))

;;     (if (get-buffer report-buffer)
;;         (with-current-buffer report-buffer
;;           (delete-region (point-min) (point-max)))
;;       (get-buffer-create report-buffer))

;;     (cl-loop for file in (org-glance-scope scope)
;;        do (save-window-excursion
;;             (message "Staring at %s" file)
;;             (redisplay)
;;             (find-file file)
;;             (org-element-map (org-element-parse-buffer 'headline) 'headline
;;               (lambda (headline)
;;                 ;; maybe enrich headline with :file property?
;;                 (when (org-glance-headline:matches-filter? (org-glance-view-filter view) headline)
;;                   (loop for check in '(org-glance-view-doctor:check-for-corrupted-properties
;;                                        org-glance-view-doctor:check-for-links-in-title
;;                                        ;; - [ ] check if visited file is not headline archive file
;;                                        ;; - [ ] check for view data structure: no empty directories etc
;;                                        ;; - [ ] check for view data structure: proper partitioning
;;                                        ;; - [ ] check for nested views and ask to flatten them
;;                                        ;; - [ ] check if original headline is stored in archive
;;                                        ;; - [ ] check for PROPERTIES drawer indentation
;;                                        )
;;                      for failed? = (apply check (list file view headline))
;;                      when failed? collect check into failed-checks
;;                      count failed? into failed-counts
;;                      finally (unless (null failed-checks)
;;                                (incf error-count failed-counts)
;;                                (with-current-buffer report-buffer
;;                                  (goto-char (point-max))
;;                                  (insert (org-glance-headline:contents* headline))
;;                                  (loop for check in failed-checks
;;                                     do (insert "- " (symbol-name check) "\n"))))))))))

;;     (with-temp-file (org-glance-view:doctor-location vid)
;;       (insert (org-glance:format org-glance-view-doctor-header-template)
;;               (with-current-buffer report-buffer (buffer-string))))
;;     (kill-buffer report-buffer)
;;     (find-file (org-glance-view:doctor-location vid))))

(org-glance-module-provide)
