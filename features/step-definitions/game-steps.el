;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(require 'org-glance)
(require 'with-simulated-input)

(When "I enter the forest"
  (lambda () (org-glance:forest)))

(And "^I should have \\([[:digit:]]+\\) grounds? owned$"
     (lambda (cardinality)
       (should (= cardinality 0))))

(And "^I should have \\([[:digit:]]+\\) trees? planted$"
     (lambda (cardinality)
       (should (= cardinality 0))))
