;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(require 'org-glance)
(require 'with-simulated-input)
(require 'subr-x)

(Then "^I define view \"\\(.+\\)\" in default scope$"
      (lambda (view-id) (org-glance-def-view (intern view-id))))

(Then "^I compile view \"\\(.+\\)\"$"
      (lambda (view-id) (org-glance-view:update (intern view-id))))

(And "^I should have \\([[:digit:]]+\\) headlines in view \"\\(.+\\)\"$"
     (lambda (cardinality view-id)
       (let ((actual-cardinality (->> org-glance-views
                                   (gethash (intern view-id))
                                   (org-glance-view:headlines)
                                   (length)))
             (expected-cardinality (->> cardinality
                                     (string-to-number))))
         (should (= expected-cardinality actual-cardinality)))))

(And "^I should have \\([[:digit:]]+\\) views? registered$"
     (lambda (cardinality)
       (let ((actual-cardinality (->> org-glance-views
                                   (hash-table-keys)
                                   (length)))
             (expected-cardinality (->> cardinality
                                     (string-to-number))))
         (should (= expected-cardinality actual-cardinality)))))
