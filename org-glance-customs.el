;; -*- lexical-binding: t -*-

(require 'org)

(defcustom org-glance-directory org-directory
  "Main location for all Org mode content managed by `org-glance`."
  :group 'org-glance
  :type 'directory)

(defcustom org-glance-resource-directory (f-join org-directory "resources")
  "Directory for non-Org resources associated with `org-glance`."
  :group 'org-glance
  :type 'directory)

(defcustom org-glance-clone-on-repeat-p nil
  "Create a new headline copy when repeating rather than modifying in place."
  :group 'org-glance
  :type 'boolean)

(provide 'org-glance-customs)
