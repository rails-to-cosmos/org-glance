(require 'org-glance-registry)

(Given "^registry \"\\([^\"]+\\)\"$"
       (lambda (name)
         (puthash name (org-glance-registry :id name) ecukes--registries)))

(When "^I put headline \"\\([^\"]+\\)\" into registry \"\\([^\"]+\\)\"$"
  (lambda (headline registry)
    (let ((headline (gethash headline ecukes--headlines))
          (registry (gethash registry ecukes--registries)))
      (org-glance-registry-put headline registry))))

(Then "^registry \"\\([^\"]+\\)\" should contain \\([0-9]+\\) headline$"
      (lambda (registry count)
        (let ((registry (gethash registry ecukes--registries))
              (count (string-to-number count)))
          (should (= (hash-table-count (org-glance-headlines registry)) count)))))

(And "^headline \"\\([^\"]+\\)\" should have an ID in registry \"\\([^\"]+\\)\"$"
     (lambda (headline registry)
       (let ((headline (gethash headline ecukes--headlines))
             (registry (gethash registry ecukes--registries)))
         (should (not (null (org-glance-headline-property-get headline (org-glance-registry:id-key registry))))))))

(Then "^headline \"\\([^\"]+\\)\" should not have an ID in registry \"\\([^\"]+\\)\"$"
      (lambda (headline registry)
        (let ((headline (gethash headline ecukes--headlines))
              (registry (gethash registry ecukes--registries)))
          (should (null (org-glance-headline-property-get headline (org-glance-registry:id-key registry)))))))
