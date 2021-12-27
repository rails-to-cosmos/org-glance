(require 'org-glance-module)

(org-glance:require org)

(defcustom org-glance-directory org-directory
  "Directory with Org files."
  :group 'org-glance
  :type 'directory)

(defcustom org-glance-clone-on-repeat-p nil
  "Clone repeated headlines instead of repeating it."
  :group 'org-glance
  :type 'boolean)

(org-glance:provide)
