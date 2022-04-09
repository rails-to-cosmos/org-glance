(Given "^file \"\\([^\"]+\\)\"$"
       (lambda (filename contents)
         (puthash filename
                  (make-temp-file filename nil ".org" contents)
                  org-glance-test--files)))

(Then "^I find file \"\\([^\"]+\\)\"$"
      (lambda (filename)
        (find-file (gethash filename org-glance-test--files))))
