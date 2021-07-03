(require 'org-glance-module)

(org-glance-module-import lib.core.view.summary)

(cl-defun org-glance-view:agenda ()
  (interactive)
  (let ((org-agenda-files (org-glance-view:summary-locations)))
    (org-agenda-list)
    (org-agenda-day-view)))

(org-glance-module-provide)
