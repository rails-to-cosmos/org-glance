(When "^I materialize store \"\\([^\"]+\\)\" to file \"\\([^\"]+\\)\"$"
  (lambda (store-name file-name)
    (let ((file (FILE file-name))
          (store (STORE store-name)))
      (org-glance-materialize store file))))

(When "^I commit changes to store \"\\([^\"]+\\)\"$"
  (lambda (store-name)
    (org-glance-material-redisplay)
    (puthash store-name (org-glance-material-commit) org-glance-test-stores)))
