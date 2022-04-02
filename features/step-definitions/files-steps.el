(When "^I create an org file with contents:$"
  (lambda (content)
    (find-file (make-temp-file "org-glance-" nil ".org" content))))
