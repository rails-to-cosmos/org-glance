(require 'org-glance-module)

(org-glance-module-import lib.core.view)

(defvar org-glance-view-summary-header-template "#    -*- mode: org; mode: org-glance-overview -*-

#+CATEGORY: $category
#+STARTUP: overview
#+LATEST_CHANGE: $latest_change

")

(cl-defun org-glance-view:summary-location (&optional (vid (org-glance-view:completing-read)))
  "Path to file where VIEW-ID exported headlines are stored."
  (let ((view-name (s-downcase (format "%s" vid))))
    (f-join org-glance-view-location
            view-name
            (format "%s.org_summary" view-name))))

(cl-defun org-glance-view:summary-locations ()
  (cl-loop for view in (org-glance-view:ids)
     collect (org-glance-view:summary-location view)))

(cl-defun org-glance-view:summary (&optional (view-id (org-glance-view:completing-read)))
  (interactive)
  (org-glance-view:if-all? view-id
      (mapc #'org-glance-view:summary (org-glance-view:ids))
    (let ((filename (org-glance-view:summary-location view-id))
          (header (org-glance:f org-glance-view-summary-header-template :category view-id))
          (headlines (->> view-id org-glance-view:update org-glance-view:headlines))
          (inhibit-read-only t))
      (--org-glance:make-file-directory filename)
      (with-temp-file filename
        (insert header)
        (cl-loop for headline in headlines
           do (insert (org-glance-headline:contents headline) "\n"))
        (org-mode)
        (goto-char (point-min))
        (set-mark (point-max))
        (condition-case nil
            (org-sort-entries nil ?o)
          (error 'nil))
        (org-glance:sort-buffer-headlines)
        (org-align-tags t))
      (find-file filename))))

(cl-defun org-glance-view:visit (&optional (view-id (org-glance-view:completing-read)))
  (interactive)
  (let ((location (org-glance-view:summary-location view-id)))
    (if (file-exists-p location)
        (find-file location)
      (org-glance-view:summary view-id))))

(org-glance-module-provide)
