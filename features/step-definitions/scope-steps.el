;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(require 'org-glance)
(require 'with-simulated-input)
(require 'subr-x)

(When "^I add file \"\\(.+\\)\" to default scope"
  (lambda (relative-file-name) (cl-pushnew (f-join default-directory relative-file-name) org-glance-default-scope)))

(Then "^I should have \\([[:digit:]]+\\) files? in default scope$"
      (lambda (n) (should
              (= (length org-glance-default-scope)
                 (string-to-number n)))))

(And "^Scope should contain file \"\\(.+\\)\"$"
     (lambda (relative-file-name)
       (should
        (member (f-join default-directory relative-file-name) org-glance-default-scope))))
