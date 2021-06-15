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
  (let ((summary-orig-file (org-glance-view:summary-location view-id))
        (summary-temp-file (make-temp-file "org-glance-view-summary-"))
        (file-offsets (make-hash-table :test 'equal))
        (headlines (->> view-id
                     org-glance-view:update
                     org-glance-view:headlines)))

    (append-to-file (org-glance-expand-template
                     org-glance-view-summary-header-template
                     `(:category ,view-id))
                    nil summary-temp-file)

    (cl-loop for headline in headlines
       do (append-to-file (format "%s\n" (org-glance-headline:contents headline)) nil summary-temp-file))

    (progn ;; sort headlines by TODO order
      (find-file summary-temp-file)
      (read-only-mode -1)
      (goto-char (point-min))
      (set-mark (point-max))
      (condition-case nil
          (org-sort-entries nil ?o)
        (error 'nil))
      (org-glance:sort-buffer-headlines)
      (org-overview)
      (org-align-tags t)
      (save-buffer)
      (kill-buffer))

    ;; apply changes to original file
    (org-glance--make-file-directory summary-orig-file)
    (when (file-exists-p summary-orig-file)
      (delete-file summary-orig-file t))
    (rename-file summary-temp-file summary-orig-file)

    (find-file summary-orig-file)))

(org-glance-module-provide)
