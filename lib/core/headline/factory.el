(require 'org-glance-module)

(org-glance-module-import lib.core.metastore)
(org-glance-module-import lib.core.headline.def)
(org-glance-module-import lib.utils.helpers)

(cl-defgeneric org-glance-headline (obj)
  "Headline factory.")

(cl-defmethod org-glance-headline ((obj string))
  (org-glance-metastore:headline obj))

(cl-defmethod org-glance-headline ((obj symbol))
  (org-glance-metastore:headline (symbol-name obj)))

(cl-defmethod org-glance-headline ((obj list))
  obj)

(cl-defmethod org-glance-headline ((obj null))
  "Extract headline from point."
  (save-excursion
    (org-glance-headline:goto-beginning-of-nearest-headline)
    (org-glance-headline (org-glance-headline:id))))

(org-glance-module-provide)
