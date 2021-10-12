;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(require 'org-glance)
(require 'with-simulated-input)

(When "I enter the forest"
  (lambda () (org-glance:forest)))

(And "^I should have \\([[:digit:]]+\\) grounds? owned$"
     (lambda (cardinality)
       (should (string= cardinality "0"))))

(And "^I should have \\([[:digit:]]+\\) trees? planted$"
     (lambda (cardinality)
       (should (string= cardinality "0"))))

(Then "^I should see$"
      (lambda (text)
        (should (s-contains-p text (buffer-substring-no-properties (point-min) (point-max))))))

(Given "^I am in the buffer \"\\([^\"]+\\)\"$"
       (lambda (arg) (switch-to-buffer (get-buffer-create arg))))
