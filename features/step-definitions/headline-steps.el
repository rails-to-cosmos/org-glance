(require 'ecukes)
(require 'ert)
(require 'org-glance)

(Given "^headline \"\\([^\"]+\\)\"$"
       (lambda (headline contents)
         (let ((file "org-glance--capture.org"))
           (Given "file \"%s\"" file contents)
           (And "I find file \"%s\"" file)
           (And "I create headline \"%s\" from element at point" headline)
           (And "I kill buffer"))))

(Then "^headline \"\\([^\"]+\\)\" should be an? \\([^\"]+\\)$"
      (lambda (name expected-class)
        (let* ((expected-class (intern expected-class))
               (headline (gethash name org-glance-test:headlines))
               (class (org-glance-headline:class headline)))
          (should (memq expected-class class)))))

(Then "^headline \"\\([^\"]+\\)\" contents? should be:$"
      (lambda (name expected-contents)
        (let* ((headline (gethash name org-glance-test:headlines))
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
     (lambda (headline)
       (puthash headline (org-glance-headline-at-point) org-glance-test:headlines)))

(And "^I save headline \"\\([^\"]+\\)\" to file \"\\([^\"]+\\)\"$"
     (lambda (headline file)
       (let ((headline (gethash headline org-glance-test:headlines)))
         (org-glance-headline-save headline (f-join org-glance-test:root-location file)))))

(Then "^I load headline \"\\([^\"]+\\)\" from file \"\\([^\"]+\\)\"$"
      (lambda (headline file)
        (puthash headline (org-glance-headline-load (f-join org-glance-test:root-location file)) org-glance-test:headlines)))

(Then "^headline \"\\([^\"]+\\)\" should contain links?$"
      (lambda (headline)
        (let ((headline (gethash headline org-glance-test:headlines)))
          (should (org-glance-headline:linked-p headline)))))

(Then "^headline \"\\([^\"]+\\)\" should not contain links?$"
      (lambda (headline)
        (should-error (Then "headline \"%s\" should contain links" headline))))

(Then "^headline \"\\([^\"]+\\)\" should be encrypted$"
      (lambda (name)
        (let ((headline (gethash name org-glance-test:headlines)))
          (should (org-glance-headline:encrypted-p headline)))))

(Then "^headline \"\\([^\"]+\\)\" should not be encrypted$"
      (lambda (headline)
        (should-error (Then "headline \"%s\" should be encrypted" headline))))

(Then "^headline \"\\([^\"]+\\)\" should be propertized$"
      (lambda (headline)
        (let ((headline (gethash headline org-glance-test:headlines)))
          (should (org-glance-headline:propertized-p headline)))))

(Then "^headline \"\\([^\"]+\\)\" should not be propertized$"
      (lambda (headline)
        (should-error (Then "headline \"%s\" should be propertized" headline))))

(Then "^headline \"\\([^\"]+\\)\" should be archived$"
      (lambda (headline)
        (let ((headline (gethash headline org-glance-test:headlines)))
          (should (org-glance-headline:archived-p headline)))))

(Then "^headline \"\\([^\"]+\\)\" should be commented$"
      (lambda (headline)
        (let ((headline (gethash headline org-glance-test:headlines)))
          (should (org-glance-headline:commented-p headline)))))

(Then "^headline \"\\([^\"]+\\)\" should be closed$"
      (lambda (name)
        (let ((headline (gethash name org-glance-test:headlines)))
          (should (org-glance-headline:closed-p headline)))))

(Then "^the title of headline \"\\([^\"]+\\)\" should be \"\\([^\"]+\\)\"$"
      (lambda (name title)
        (should
         (string=
          (org-glance-headline:title (gethash name org-glance-test:headlines))
          title))))

(And "^the contents of headline \"\\([^\"]+\\)\" should be:$"
     (lambda (name contents)
       (let ((actual-headline (gethash name org-glance-test:headlines))
             (expected-headline (with-temp-buffer
                                  (insert contents)
                                  (org-glance-headline-at-point))))
         (should (org-glance-headline-equal-p actual-headline expected-headline)))))

(And "^headline \"\\([^\"]+\\)\" should be equal to headline \"\\([^\"]+\\)\"$"
     (lambda (a b)
       (should (org-glance-headline-equal-p
                (H a)
                (H b)))))

(Then "^I set title of the headline at point to \"\\([^\"]+\\)\"$" #'org-edit-headline)

(When "^I materialize headlines? \"\\([^\"]+\\)\" to file \"\\([^\"]+\\)\"$"
  (lambda (headlines file)
    (org-glance-materialize (F file) (HS headlines))))
