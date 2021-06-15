(require 'org-glance-module)

(org-glance-module-import lib.core.view.summary)

(cl-defun org-glance-view:agenda (&optional (vid org-glance-form:view))
  (interactive)
  (let ((org-agenda-files (org-glance-view:if-all? vid
                              (org-glance-view:summary-locations)
                            (list (org-glance-view:summary-location org-glance-form:view)))))
    (org-agenda-list)))

(org-glance-module-provide)
