(require 'org-glance)

(Then "^headline \"\\([^\"]+\\)\" title should be \"\\([^\"]+\\)\"$"
      (lambda (name expected-title)
        (let ((headline (gethash name org-glance-test--headlines)))
          (should (string= expected-title (org-glance-headline:title headline))))))

(Then "^headline \"\\([^\"]+\\)\" should be an? \\([^\"]+\\)$"
      (lambda (name expected-class)
        (let* ((expected-class (intern expected-class))
               (headline (gethash name org-glance-test--headlines))
               (class (org-glance-headline:class headline)))
          (should (memq expected-class class)))))

(Then "^headline \"\\([^\"]+\\)\" contents? should be:$"
      (lambda (name expected-contents)
        (let* ((headline (gethash name org-glance-test--headlines))
               (contents (org-glance-headline:contents headline)))
          (should (string= (with-temp-buffer
                             (insert expected-contents)
                             (org-align-tags 'all)
                             (buffer-substring-no-properties (point-min) (point-max)))
                           (with-temp-buffer
                             (insert contents)
                             (org-align-tags 'all)
                             (buffer-substring-no-properties (point-min) (point-max))))))))

(And "^I create headline \"\\([^\"]+\\)\" from element at point$"
     (lambda (name)
       (puthash name (org-glance-headline-at-point) org-glance-test--headlines)))

(And "^I save headline \"\\([^\"]+\\)\" to file \"\\([^\"]+\\)\"$"
     (lambda (name file)
       (let ((headline (gethash name org-glance-test--headlines)))
         (org-glance-headline-save headline (f-join org-glance-test--root-location file)))))

(Then "^I load headline \"\\([^\"]+\\)\" from file \"\\([^\"]+\\)\"$"
      (lambda (name file)
        (puthash name
                 (org-glance-headline-load (f-join org-glance-test--root-location file))
                 org-glance-test--headlines)))

(Then "^headline \"\\([^\"]+\\)\" should contain links?$"
      (lambda (name)
        (let ((headline (gethash name org-glance-test--headlines)))
          (should (org-glance-headline:linked-p headline)))))

(Then "^headline \"\\([^\"]+\\)\" should not contain links?$"
      (lambda (name)
        (let ((headline (gethash name org-glance-test--headlines)))
          (should (not (org-glance-headline:linked-p headline))))))

(Then "^headline \"\\([^\"]+\\)\" should be encrypted$"
      (lambda (name)
        (let ((headline (gethash name org-glance-test--headlines)))
          (should (org-glance-headline:encrypted-p headline)))))

(Then "^headline \"\\([^\"]+\\)\" should not be encrypted$"
      (lambda (name)
        (let ((headline (gethash name org-glance-test--headlines)))
          (should (not (org-glance-headline:encrypted-p headline))))))

(Then "^headline \"\\([^\"]+\\)\" should be propertized$"
      (lambda (name)
        (let ((headline (gethash name org-glance-test--headlines)))
          (should (org-glance-headline:propertized-p headline)))))

(Then "^headline \"\\([^\"]+\\)\" should not be propertized$"
      (lambda (name)
        (let ((headline (gethash name org-glance-test--headlines)))
          (should (not (org-glance-headline:propertized-p headline))))))

(Then "^headline \"\\([^\"]+\\)\" should be archived$"
      (lambda (name)
        (let ((headline (gethash name org-glance-test--headlines)))
          (should (org-glance-headline:archived-p headline)))))

(Then "^headline \"\\([^\"]+\\)\" should be commented$"
      (lambda (name)
        (let ((headline (gethash name org-glance-test--headlines)))
          (should (org-glance-headline:commented-p headline)))))

(Then "^headline \"\\([^\"]+\\)\" should be closed$"
      (lambda (name)
        (let ((headline (gethash name org-glance-test--headlines)))
          (should (org-glance-headline:closed-p headline)))))

(Given "^headline \"\\([^\"]+\\)\"$"
       (lambda (name contents)
         (let ((capture-file-name "org-glance--capture.org"))
           (Given "file \"%s\"" capture-file-name contents)
           (And "I find file \"%s\"" capture-file-name)
           (And "I create headline \"%s\" from element at point" name))))

(Then "^the title of headline \"\\([^\"]+\\)\" should be \"\\([^\"]+\\)\"$"
      (lambda (name title)
        (should
         (string=
          (org-glance-headline:title (gethash name org-glance-test--headlines))
          title))))

(And "^the contents of headline \"\\([^\"]+\\)\" should be:$"
     (lambda (name contents)
       (let ((actual-headline (gethash name org-glance-test--headlines))
             (expected-headline (with-temp-buffer
                                  (insert contents)
                                  (org-glance-headline-at-point))))
         (should (org-glance-headline-equal-p actual-headline expected-headline)))))

(And "^headline \"\\([^\"]+\\)\" should be equal to headline \"\\([^\"]+\\)\"$"
     (lambda (a b)
       (should (org-glance-headline-equal-p
                (gethash a org-glance-test--headlines)
                (gethash b org-glance-test--headlines)))))
