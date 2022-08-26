(Given "^file \"\\([^\"]+\\)\"$"
       (lambda (filename contents)
         (let ((full-path (f-join org-glance-test:location filename)))
           (mkdir (file-name-directory full-path) t)
           (f-write contents 'utf-8 full-path)
           (puthash filename full-path org-glance-test-files))))

(Given "^empty file \"\\([^\"]+\\)\"$"
       (lambda (file)
         (Given "file \"%s\"" file "")))

(When "^I? ?find file \"\\([^\"]+\\)\"$"
      (lambda (filename)
        (find-file (gethash filename org-glance-test-files))))

(When "^I? ?append to file \"\\([^\"]+\\)\"$"
  (lambda (filename new-contents)
    (let ((file (org-glance-test:get-file filename)))
      (append-to-file new-contents nil file))))
