(Given "^empty file \"\\([^\"]+\\)\"$"
  (lambda (file)
    (let ((full-path (f-join org-glance-test-location file)))
      (f-touch full-path)
      (puthash file full-path org-glance-test-files))))

(Given "^file \"\\([^\"]+\\)\"$"
       (lambda (filename contents)
         (let ((full-path (f-join org-glance-test-location filename)))
           (f-write contents 'utf-8 full-path)
           (puthash filename full-path org-glance-test-files))))

(Then "^I find file \"\\([^\"]+\\)\"$"
      (lambda (filename)
        (find-file (gethash filename org-glance-test-files))))

(Given "^file \"\\([^\"]+\\)\" in directory \"\\([^\"]+\\)\"$"
       (lambda (file directory contents)
         (f-mkdir-full-path (f-join org-glance-test-location directory))
         (let ((full-path (f-join org-glance-test-location directory file)))
           (f-write contents 'utf-8 full-path)
           (puthash file full-path org-glance-test-files))))
