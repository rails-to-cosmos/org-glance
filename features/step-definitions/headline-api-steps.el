(require 'org-glance)

(Then "^headline title should be \"\\([^\"]+\\)\"$"
      (lambda (expected-title)
        (should (string= expected-title (oref (org-glance-headline-at-point) :title)))))

(Then "^headline contents should be:$"
      (lambda (expected-contents)
        (let ((headline (org-glance-headline-at-point)))
          (should (string= (with-temp-buffer
                             (insert expected-contents)
                             (org-align-tags 'all)
                             (buffer-substring-no-properties (point-min) (point-max)))
                           (with-temp-buffer
                             (insert (oref headline :contents))
                             (org-align-tags 'all)
                             (buffer-substring-no-properties (point-min) (point-max))))))))

(And "^headline classes should be \"\\([^\"]+\\)\"$"
  (lambda (expected-classes)
    (let ((headline (org-glance-headline-at-point)))
      (should (string= (s-join ", " (mapcar #'symbol-name (oref headline :classes)))
                       expected-classes)))))
