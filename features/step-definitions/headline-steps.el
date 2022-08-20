(require 'ecukes)
(require 'ert)
(require 'org-glance)
(require 'org-glance-headline)

(Given "^headline \"\\([^\"]+\\)\"$"
       (lambda (headline contents)
         (let ((file "org-glance--capture.org"))
           (Given "file \"%s\"" file contents)
           (And "I find file \"%s\"" file)
           (And "I create headline \"%s\" from element at point" headline)
           (And "I kill buffer"))))

(When "^I select first headline$"
  (lambda ()
    (goto-char (point-min))
    (unless (org-at-heading-p)
      (outline-next-heading))))

(When "^I go to headline with title \"\\([^\"]+\\)\"$"
  (lambda (title)
    (goto-char (point-min))
    (search-forward (format "* %s" title))))

(Then "^headline \"\\([^\"]+\\)\" should be an? \\([^\"]+\\)$"
      (lambda (name expected-class)
        (let* ((headline (gethash name org-glance-test-headlines))
               (class (org-glance-headline-class headline)))
          (should (member expected-class class)))))

(Then "^headline \"\\([^\"]+\\)\" contents? should be:$"
      (lambda (name expected-contents)
        (let* ((headline (gethash name org-glance-test-headlines))
               (contents (org-glance-headline-contents headline)))
          (should (string= (with-temp-buffer
                             (insert expected-contents)
                             (org-align-tags 'all)
                             (buffer-substring-no-properties (point-min) (point-max)))
                           (with-temp-buffer
                             (insert contents)
                             (org-align-tags 'all)
                             (buffer-substring-no-properties (point-min) (point-max))))))))

(When "^I create headline \"\\([^\"]+\\)\" from element at point$"
  (lambda (headline)
    (puthash headline (org-glance-headline-at-point) org-glance-test-headlines)))

(When "^I save headline \"\\([^\"]+\\)\" to file \"\\([^\"]+\\)\"$"
  (lambda (headline file)
    (let ((headline (gethash headline org-glance-test-headlines)))
      (org-glance-headline-save headline (f-join org-glance-test:location file)))))

(When "^I load headline \"\\([^\"]+\\)\" from file \"\\([^\"]+\\)\"$"
      (lambda (headline file)
        (puthash headline (org-glance-headline-load (f-join org-glance-test:location file))
                 org-glance-test-headlines)))

(When "^headline \"\\([^\"]+\\)\" should contain links?$"
      (lambda (headline)
        (let ((headline (gethash headline org-glance-test-headlines)))
          (should (org-glance-headline-linked-p headline)))))

(Then "^headline \"\\([^\"]+\\)\" should not contain links?$"
      (lambda (headline)
        (should-error (Then "headline \"%s\" should contain links" headline))))

(Then "^headline \"\\([^\"]+\\)\" should be encrypted$"
      (lambda (name)
        (let ((headline (gethash name org-glance-test-headlines)))
          (should (org-glance-headline-encrypted-p headline)))))

(Then "^headline \"\\([^\"]+\\)\" should not be encrypted$"
      (lambda (headline)
        (should-error (Then "headline \"%s\" should be encrypted" headline))))

(Then "^headline \"\\([^\"]+\\)\" should be propertized$"
      (lambda (headline)
        (let ((headline (gethash headline org-glance-test-headlines)))
          (should (org-glance-headline-propertized-p headline)))))

(Then "^headline \"\\([^\"]+\\)\" should not be propertized$"
      (lambda (headline)
        (should-error (Then "headline \"%s\" should be propertized" headline))))

(Then "^headline \"\\([^\"]+\\)\" should be archived$"
      (lambda (headline)
        (let ((headline (gethash headline org-glance-test-headlines)))
          (should (org-glance-headline-archived-p headline)))))

(Then "^headline \"\\([^\"]+\\)\" should be commented$"
      (lambda (headline)
        (let ((headline (gethash headline org-glance-test-headlines)))
          (should (org-glance-headline-commented-p headline)))))

(Then "^headline \"\\([^\"]+\\)\" should be closed$"
      (lambda (name)
        (let ((headline (gethash name org-glance-test-headlines)))
          (should (org-glance-headline-closed-p headline)))))

(Then "^the title of headline \"\\([^\"]+\\)\" should be \"\\([^\"]+\\)\"$"
      (lambda (name title)
        (should
         (string=
          (org-glance-headline-title (gethash name org-glance-test-headlines))
          title))))

(Then "^the contents of headline \"\\([^\"]+\\)\" should be:$"
     (lambda (name contents)
       (let ((actual-headline (gethash name org-glance-test-headlines))
             (expected-headline (with-temp-buffer
                                  (insert contents)
                                  (org-glance-headline-at-point))))
         (should (org-glance-headline-equal-p actual-headline expected-headline)))))

(Then "^headline \"\\([^\"]+\\)\" should be equal to headline \"\\([^\"]+\\)\"$"
     (lambda (a b)
       (should (org-glance-headline-equal-p (org-glance-test:get-headline a) (org-glance-test:get-headline b)))))

(When "^I set title of the headline at point to \"\\([^\"]+\\)\"$"
      (lambda (title)
        (with-mutex org-glance-material-overlay-manager--mutex
          (org-edit-headline title))))
