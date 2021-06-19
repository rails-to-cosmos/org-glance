(require 'org-glance-module)

(org-glance-module-import lib.core.view)

(defvar org-glance-view-summary-header-template "#    -*- mode: org; mode: org-glance-summary -*-

#+CATEGORY: {:category}
#+STARTUP: overview
#+LATEST_CHANGE: {:latest_change}

")

(cl-defun org-glance-view:summary-location (&optional (view-id (org-glance-view:completing-read)))
  "Path to file where VIEW-ID exported headlines are stored."
  (let ((view-name (s-downcase (format "%s" view-id))))
    (f-join org-glance-view-location
            view-name
            (format "%s.org_summary" view-name))))

(cl-defun org-glance-view:summary-locations ()
  (cl-loop for view in (org-glance-view:ids)
     collect (org-glance-view:summary-location view)))

(cl-defun org-glance-view-visit
    (&optional
       (view-id (org-glance-view:completing-read)))
  (interactive)
  (find-file (org-glance-view:summary-location view-id)))

(cl-defun org-glance-view:summary (&optional (view-id (org-glance-view:completing-read)))
  (interactive)
  (let ((summary-file (org-glance-view:summary-location view-id))
        (headlines (->> view-id
                     org-glance-view:update
                     org-glance-view:headlines)))
    (org-glance--make-file-directory summary-file)
    (with-temp-file summary-file
      (insert (org-glance-expand-template org-glance-view-summary-header-template :category view-id))
      (insert "\n")
      (cl-loop for headline in headlines
         do (insert (org-glance-headline:contents headline))
           (insert "\n"))
      (org-mode)
      (read-only-mode -1)
      (goto-char (point-min))
      (set-mark (point-max))
      (condition-case nil
          (org-sort-entries nil ?o)
        (error 'nil))
      (org-glance:sort-buffer-headlines)
      (org-align-tags t))
    (find-file summary-file)))

(org-glance-module-provide)
