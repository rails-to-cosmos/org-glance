(When "^I create an org file with content:$"
  (lambda (content)
    (find-file (make-temp-file "org-glance-" nil ".org" content))))

(And "^I goto the end of the buffer$"
     (lambda ()
       (goto-char (point-max))))
