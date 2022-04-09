(let ((reg (org-glance-registry :id "headlines"))
      (headline (with-temp-buffer
                  (insert-file-contents "headline.org")
                  (org-glance-headline-at-point))))
  (org-glance-registry-put headline reg)
  ;; (org-glance-headline:contents headline)
  reg)
