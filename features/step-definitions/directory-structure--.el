;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(require 'org-glance)
(require 'with-simulated-input)
(require 'subr-x)

(cl-defun verbose-cd (location)
  (message "Default directory has been changed to \"%s\"" location)
  (cd location))

(Given "^I'm in a root directory$" (lambda () (verbose-cd org-glance-test:root-location)))
(Given "^I'm in a user directory$" (lambda () (verbose-cd org-glance-test:user-location)))
(Given "^I'm in a view directory$" (lambda () (verbose-cd org-glance-test:view-location)))

(And "^I change directory to a root location$" (lambda () (verbose-cd org-glance-test:root-location)))
(And "^I change directory to a user location$" (lambda () (verbose-cd org-glance-test:user-location)))
(And "^I change directory to a view location$" (lambda () (verbose-cd org-glance-test:view-location)))

(Then "^I create directory \"\\(.+\\)\"$" (lambda (dir) (mkdir dir)))
(Then "^I change directory to \"\\(.+\\)\"$" (lambda (dir) (cd dir)))
(Then "^I change directory to parent directory$" (lambda () (verbose-cd "../")))

(And "^I create file \"\\(.+\\)\"$"
     (lambda (filename text)
       (find-file filename)
       (insert text)
       (message "Create file %s" filename)
       (save-buffer)))
