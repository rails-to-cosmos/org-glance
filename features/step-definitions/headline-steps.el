(require 'org-glance)

(Then "^headline title should be \"\\([^\"]+\\)\"$"
      (lambda (expected-title)
        (let ((headline org-glance-test:current-headline))
          (should (string= expected-title (org-glance-headline-title headline))))))

(Then "^headline should be an? \\([^\"]+\\)$"
      (lambda (expected-class)
        (let* ((expected-class (intern expected-class))
               (headline org-glance-test:current-headline)
               (tags (org-glance-headline-tags headline)))
          (should (memq expected-class tags)))))

(Then "^headline contents? should be:$"
      (lambda (expected-contents)
        (let* ((headline org-glance-test:current-headline)
               (contents (org-glance-headline-contents headline)))
          (should (string= (with-temp-buffer
                             (insert expected-contents)
                             (org-align-tags 'all)
                             (buffer-substring-no-properties (point-min) (point-max)))
                           (with-temp-buffer
                             (insert contents)
                             (org-align-tags 'all)
                             (buffer-substring-no-properties (point-min) (point-max))))))))

(And "^I create headline from element at point$"
  (lambda ()
    (setq org-glance-test:current-headline (org-glance-headline-create))))

(And "^I save headline to file \"\\([^\"]+\\)\"$"
  (lambda (file)
    (org-glance-headline-save org-glance-test:current-headline
                              (f-join org-glance-test:root-location file))))

(Then "^I load headline from file \"\\([^\"]+\\)\"$"
  (lambda (file)
    (setq org-glance-test:current-headline
          (org-glance-headline-load (f-join org-glance-test:root-location file)))))
