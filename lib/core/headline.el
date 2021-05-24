(require 'org-glance-module)

(org-glance-module-import lib.core.metastore)

(cl-defun org-glance-headline:by-id (id)
  "Get org-element headline by ID."
  (cl-loop for view-id in (org-glance-view:list-view-ids)
     for metastore = (->> view-id
                       org-glance-view
                       org-glance-view-metadata-location
                       org-glance-metastore:read)
     for headline = (gethash id metastore)
     when headline
     do (return (org-glance-metastore:deserialize headline))))

(org-glance-module-provide)
