(require 'env)
(require 'ert)
(require 'ecukes)
(require 'org-glance-registry)

(Given "^registry \"\\([^\"]+\\)\"$"
       (lambda (alias)
         (puthash alias (org-glance-registry :id alias) ecukes--registries)))

(When "^I add \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$"
  (lambda (headline registry)
    (org-glance-registry-put (H headline) (R registry))))

(Then "^registry \"\\([^\"]+\\)\" should contain \\([0-9]+\\) headline$"
      (lambda (registry count)
        (let ((count (string-to-number count)))
          (should (= (hash-table-count
                      (org-glance-headlines (R registry)))
                     count)))))

(Then "^\"\\([^\"]+\\)\" should be registered in \"\\([^\"]+\\)\"$"
      (lambda (headline registry)
        (should-not (null (org-glance-headline-property-get (H headline) (org-glance-registry:id-key (R registry)))))))

(Then "^\"\\([^\"]+\\)\" should not be registered in \"\\([^\"]+\\)\"$"
      (lambda (headline registry)
        (should-error (Then "\"%s\" should be registered in \"%s\"" headline registry))))

(When "^I add \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\" as \"\\([^\"]+\\)\"$"
  (lambda (headline registry alias)

    ))
