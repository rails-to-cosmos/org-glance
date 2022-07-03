(Given "^file \"\\([^\"]+\\)\"$"
       (lambda (filename contents)
         (let ((full-path (f-join org-glance-test:temp-location filename)))
           (f-write contents 'utf-8 full-path)
           (puthash filename full-path org-glance-test:files))))

(Then "^I find file \"\\([^\"]+\\)\"$"
      (lambda (filename)
        (find-file (gethash filename org-glance-test:files))))
