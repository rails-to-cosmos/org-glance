(require 'org-glance)

(Then "^headline title should be \"\\([^\"]+\\)\"$"
      (lambda (expected-title)
        (let ((headline (org-glance-headline-create)))
          (should (string= expected-title (org-glance-headline-title headline))))))

(Then "^headline should be an? \\([^\"]+\\)$"
      (lambda (expected-class)
        (let* ((expected-class (intern expected-class))
               (headline (org-glance-headline-create))
               (tags (org-glance-headline-tags headline)))
          (should (memq expected-class tags)))))

(Then "^headline contents? should be:$"
      (lambda (expected-contents)
        (let* ((headline (org-glance-headline-create))
               (contents (org-glance-headline-contents headline)))
          (should (string= (with-temp-buffer
                             (insert expected-contents)
                             (org-align-tags 'all)
                             (buffer-substring-no-properties (point-min) (point-max)))
                           (with-temp-buffer
                             (insert contents)
                             (org-align-tags 'all)
                             (buffer-substring-no-properties (point-min) (point-max))))))))
