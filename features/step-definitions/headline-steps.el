(require 'org-glance)

(Then "^element transforms into headline$"
      (lambda ()
        (Then "element transforms into 1 headline")))

(Then "^element transforms into \\([[:digit:]]+\\) headlines?$"
      (lambda (count)
        (should (= (string-to-number count) (length (org-glance-headline:create-headlines-from-element-at-point))))))

(Then "^headline title should be \"\\([^\"]+\\)\"$"
      (lambda (expected-title)
        (Then (format "1st headline title should be \"%s\"" expected-title))))

(Then "^\\([[:digit:]]\\)\\(st\\|nd\\|rd\\) headline title should be \"\\([^\"]+\\)\"$"
      (lambda (index _ expected-title)
        (let* ((index (1- (string-to-number index)))
               (headlines (org-glance-headline:create-headlines-from-element-at-point))
               (title (oref (nth index headlines) :title)))
          (should (string= expected-title title)))))

(Then "^headline should be an? \\([^\"]+\\)$"
      (lambda (expected-class)
        (Then (format "1st headline should be a %s" expected-class))))

(Then "^\\([[:digit:]]\\)\\(st\\|nd\\|rd\\) headline should be an? \\([^\"]+\\)$"
      (lambda (index _ expected-class)
        (let* ((index (1- (string-to-number index)))
               (expected-class (intern expected-class))
               (headlines (org-glance-headline:create-headlines-from-element-at-point))
               (class (oref (nth index headlines) :class))
               (id (oref class :id)))
          (should (eql expected-class id)))))

(Then "^headline contents? should be:$"
      (lambda (expected-contents)
        (Then "%s%s headline contents should be:\n%s" "1" "st" expected-contents)))

(Then "^\\([[:digit:]]\\)\\(st\\|nd\\|rd\\) headline contents? should be:$"
      (lambda (index _ expected-contents)
        (let* ((index (1- (string-to-number index)))
               (headlines (org-glance-headline:create-headlines-from-element-at-point))
               (contents (oref (nth index headlines) :contents)))
          (should (string= (with-temp-buffer
                             (insert expected-contents)
                             (org-align-tags 'all)
                             (buffer-substring-no-properties (point-min) (point-max)))
                           (with-temp-buffer
                             (insert contents)
                             (org-align-tags 'all)
                             (buffer-substring-no-properties (point-min) (point-max))))))))
