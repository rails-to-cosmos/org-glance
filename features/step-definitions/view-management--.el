;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(require 'org-glance)
(require 'with-simulated-input)
(require 'subr-x)

(Then "^I define view \"\\(.+\\)\"$"
      (lambda (view-id) (org-glance-def-view :id (intern view-id))))

(And "^I should have \\([[:digit:]]+\\) headlines in view \"\\(.+\\)\"$"
     (lambda (cardinality view-id)
       (let ((actual-cardinality (->> org-glance:classes
                                   (gethash (intern view-id))
                                   (org-glance-view:headlines)
                                   (length)))
             (expected-cardinality (->> cardinality
                                     (string-to-number))))
         (should (= expected-cardinality actual-cardinality)))))

(And "^I should have \\([[:digit:]]+\\) views? registered$"
     (lambda (cardinality)
       (let ((actual-cardinality (->> org-glance:classes
                                   (hash-table-keys)
                                   (length)))
             (expected-cardinality (->> cardinality
                                     (string-to-number))))
         (should (= expected-cardinality actual-cardinality)))))
