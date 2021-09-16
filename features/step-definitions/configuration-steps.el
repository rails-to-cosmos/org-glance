;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(require 'org-glance)
(require 'with-simulated-input)
(require 'subr-x)

(Given "^`org-glance-directory' = view location$"
       (lambda ()
         (setq org-glance-directory org-glance-test:view-location)
         (message "Set `org-glance-directory' to \"%s\"" org-glance-test:view-location)))
