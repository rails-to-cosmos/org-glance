(require 'org-glance-module)

(org-glance-module-import lib.core.view)

(defvar org-glance-view-summary-header-template "#    -*- mode: org; mode: org-glance-summary -*-

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

(cl-defun org-glance-view-visit
    (&optional
       (vid (org-glance-view:completing-read)))
  (interactive)
  (find-file (org-glance-view:summary-location vid)))

(cl-defun org-glance-view:summary (&optional (vid (org-glance-view:completing-read)))
  (interactive)
  (org-glance-view:if-all? vid
      (mapc #'org-glance-view:summary (org-glance-view:ids))
    (let ((filename (org-glance-view:summary-location vid))
          (header (org-glance:f org-glance-view-summary-header-template :category vid))
          (headlines (->> vid org-glance-view:update org-glance-view:headlines)))
      (--org-glance:make-file-directory filename)
      (with-temp-file filename
        (insert header)
        (cl-loop for headline in headlines
           do (insert (org-glance-headline:contents headline) "\n"))
        (org-mode)
        (read-only-mode -1)
        (goto-char (point-min))
        (set-mark (point-max))
        (condition-case nil
            (org-sort-entries nil ?o)
          (error 'nil))
        ;; (org-glance:sort-buffer-headlines)
        (org-align-tags t))
      (find-file filename))))

(org-glance-module-provide)
