(require 'ecukes)
(require 'ert)

(require 'org-glance)
(require 'org-glance-headline)
(require 'org-glance-store)
(require 'org-glance-scope)

(Given "^store \"\\([^\"]+\\)\"$"
       (lambda (store-name)
         (puthash store-name (org-glance-store) org-glance-test-stores)))

(When "^I import store \"\\([^\"]+\\)\" from directory \"\\([^\"]+\\)\"$"
  (lambda (store-name directory)
    (let* ((scope (org-glance-scope (F directory)))
           (store (org-glance-import scope)))
      (puthash store-name store org-glance-test-stores))))

(When "^I import store \"\\([^\"]+\\)\" from file \"\\([^\"]+\\)\"$"
  (lambda (store-name file)
    (let* ((scope (org-glance-scope (F file)))
           (store (org-glance-import scope)))
      (puthash store-name store org-glance-test-stores))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) headlines?$"
      (lambda (store-name cardinality)
        ;; (-all? #'org-glance-headline-p (org-glance-headlines store))
        (let ((store (S store-name)))
          (should (= (string-to-number cardinality)
                     (org-glance-cardinality store))))))

(When "^I materialize store \"\\([^\"]+\\)\" to file \"\\([^\"]+\\)\"$"
  (lambda (store-name file-name)
    (let ((file (F file-name))
          (store (S store-name)))
      (org-glance-materialize store file))))

(And "^I print store \"\\([^\"]+\\)\"$"
  (lambda (store-name)
    (let ((store (S store-name)))
      (pp store))))
