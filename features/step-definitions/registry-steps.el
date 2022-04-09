(Given "^registry \"\\([^\"]+\\)\"$"
  (lambda (name)
    (puthash name (org-glance-registry :id name) org-glance-test--registries)))

(When "^I put headline \"\\([^\"]+\\)\" into registry \"\\([^\"]+\\)\"$"
  (lambda (name registry)
    (org-glance-registry-put
     (gethash name org-glance-test--headlines)
     (gethash registry org-glance-test--registries))))

(Then "^registry \"\\([^\"]+\\)\" should contain \\([0-9]+\\) headline$"
  (lambda (r c)
    (let ((registry (gethash r org-glance-test--registries))
          (count (string-to-number c)))
      (should (= (hash-table-count (org-glance-headlines registry)) count)))))
