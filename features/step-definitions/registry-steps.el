(require 'org-glance-registry)

(Given "^registry \"\\([^\"]+\\)\"$"
       (lambda (name)
         (puthash name (org-glance-registry :id name) ecukes--registries)))

(When "^I add \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$"
  (lambda (headline registry)
    (let ((headline (gethash headline ecukes--headlines))
          (registry (gethash registry ecukes--registries)))
      (org-glance-registry-put headline registry))))

(Then "^registry \"\\([^\"]+\\)\" should contain \\([0-9]+\\) headline$"
      (lambda (registry count)
        (let ((registry (gethash registry ecukes--registries))
              (count (string-to-number count)))
          (should (= (hash-table-count (org-glance-headlines registry)) count)))))

(Then "^\"\\([^\"]+\\)\" should be registered in \"\\([^\"]+\\)\"$"
     (lambda (headline registry)
       (let ((headline (gethash headline ecukes--headlines))
             (registry (gethash registry ecukes--registries)))
         (should-not (null (org-glance-headline-property-get headline (org-glance-registry:id-key registry)))))))

(Then "^\"\\([^\"]+\\)\" should not be registered in \"\\([^\"]+\\)\"$"
      (lambda (headline registry)
        (should-error (Then "\"%s\" should be registered in \"%s\"" headline registry))))
