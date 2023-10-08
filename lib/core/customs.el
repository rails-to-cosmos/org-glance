(require 'org-glance-module)

(org-glance:require org f)

(defcustom org-glance-directory org-directory
  "Directory containing org-mode files and metadata."
  :group 'org-glance
  :type 'directory)

(defcustom org-glance-resource-directory (f-join org-directory "resources")
  "Directory containing various non-org resources like attachments, media, binary files etc"
  :group 'org-glance
  :type 'directory)

(defcustom org-glance-clone-on-repeat-p nil
  "Clone repeated headlines instead of repeating it."
  :group 'org-glance
  :type 'boolean)

(org-glance:provide)
