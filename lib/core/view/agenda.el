(require 'org-glance-module)

(org-glance-module-import lib.core.view.summary)

(cl-defun org-glance-view:agenda (&optional (vid (org-glance-view:completing-read)))
  (interactive)
  (let ((org-agenda-files (org-glance-view:if-all? vid
                              (org-glance-view:summary-locations)
                            (list (org-glance-view:summary-location vid)))))
    (org-agenda-list)
    (org-agenda-day-view)))

(org-glance-module-provide)
