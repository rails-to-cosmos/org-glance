(require 'org-glance)

(When "^I define class \"\\([^\"]+\\)\"$"
  (lambda (class)
    (org-glance-register-class (intern class))))

(Then "^I should have \\([[:digit:]]+\\) active classe?s? in class registry$"
      (lambda (count)
        (should (= (hash-table-count org-glance-class-registry) (string-to-number count)))))

(When "^I create directory \"\\([^\"]+\\)\" in org glance directory$"
  (lambda (directory)
    (org-glance--ensure-directory (f-join org-glance-directory directory))))

(And "^I update class registry$"
  (lambda ()
    (org-glance-class-registry:update org-glance-class-registry org-glance-directory)))
