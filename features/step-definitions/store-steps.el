(require 'ecukes)
(require 'ert)

(require 'org-glance)
(require 'org-glance-headline)
(require 'org-glance-store)
(require 'org-glance-scope)

(Given "^store \"\\([^\"]+\\)\"$"
       (lambda (store-name)
         (puthash store-name (org-glance-store-create) org-glance-test-stores)))

(When "^I import headlines from directory \"\\([^\"]+\\)\" to store \"\\([^\"]+\\)\"$"
  (lambda (directory store-name)
    (let* ((scope (org-glance-scope (F directory)))
           (store (org-glance-import scope)))
      (puthash store-name store org-glance-test-stores))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) tasks?$"
      (lambda (store-name numtasks)
        (should (= (string-to-number numtasks)
                   (org-glance-cardinality (S store-name))))))

(When "^I materialize headlines? \"\\([^\"]+\\)\" to file \"\\([^\"]+\\)\"$"
  (lambda (headlines file)
    (org-glance-materialize (org-glance-store-create :headlines (HS headlines)) (F file))))
