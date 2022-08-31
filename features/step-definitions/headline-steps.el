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

(When "^I? ?select first headline$"
  (lambda ()
    (goto-char (point-min))
    (unless (org-at-heading-p)
      (outline-next-heading))))

(When "^I? ?go to headline with title \"\\([^\"]+\\)\"$"
  (lambda (title)
    (goto-char (point-min))
    (re-search-forward (format "^*\\([A-Z ]+\\)?%s" title))))

(Then "^headline \"\\([^\"]+\\)\" should be an? \\([^\"]+\\)$"
      (lambda (name expected-class)
        (let* ((headline (gethash name org-glance-test-headlines))
               (class (org-glance-> headline :class)))
          (should (member expected-class class)))))

(Then "^headline \"\\([^\"]+\\)\" contents? should be:$"
      (lambda (name expected-contents)
        (let* ((headline (gethash name org-glance-test-headlines))
               (contents (org-glance-> headline :contents)))
          (should (string= (with-temp-buffer
                             (insert expected-contents)
                             (org-align-tags 'all)
                             (buffer-substring-no-properties (point-min) (point-max)))
                           (with-temp-buffer
                             (insert contents)
                             (org-align-tags 'all)
                             (buffer-substring-no-properties (point-min) (point-max))))))))

(When "^I? ?create headline \"\\([^\"]+\\)\" from element at point$"
  (lambda (headline)
    (puthash headline (org-glance-headline-at-point) org-glance-test-headlines)))

(When "^I? ?save headline \"\\([^\"]+\\)\" to file \"\\([^\"]+\\)\"$"
  (lambda (headline file)
    (let ((headline (gethash headline org-glance-test-headlines)))
      (org-glance-headline-save headline (f-join org-glance-test:location file)))))

(When "^I? ?load headline \"\\([^\"]+\\)\" from file \"\\([^\"]+\\)\"$"
      (lambda (headline file)
        (puthash headline (org-glance-headline:read (f-join org-glance-test:location file))
                 org-glance-test-headlines)))

(When "^headline \"\\([^\"]+\\)\" should contain links?$"
      (lambda (headline)
        (let ((headline (gethash headline org-glance-test-headlines)))
          (should (org-glance-> headline :linked-p)))))

(Then "^headline \"\\([^\"]+\\)\" should not contain links?$"
      (lambda (headline)
        (should-error (Then "headline \"%s\" should contain links" headline))))

(Then "^headline \"\\([^\"]+\\)\" should be encrypted$"
      (lambda (name)
        (let ((headline (gethash name org-glance-test-headlines)))
          (should (org-glance-> headline :encrypted-p)))))

(Then "^headline \"\\([^\"]+\\)\" should not be encrypted$"
      (lambda (headline)
        (should-error (Then "headline \"%s\" should be encrypted" headline))))

(Then "^headline \"\\([^\"]+\\)\" should be propertized$"
      (lambda (headline)
        (let ((headline (gethash headline org-glance-test-headlines)))
          (should (org-glance-> headline :propertized-p)))))

(Then "^headline \"\\([^\"]+\\)\" should not be propertized$"
      (lambda (headline)
        (should-error (Then "headline \"%s\" should be propertized" headline))))

(Then "^headline \"\\([^\"]+\\)\" should be archived$"
      (lambda (headline)
        (let ((headline (gethash headline org-glance-test-headlines)))
          (should (org-glance-> headline :archived-p)))))

(Then "^headline \"\\([^\"]+\\)\" should be commented$"
      (lambda (headline)
        (let ((headline (gethash headline org-glance-test-headlines)))
          (should (org-glance-> headline :commented-p)))))

(Then "^headline \"\\([^\"]+\\)\" should be closed$"
      (lambda (name)
        (let ((headline (gethash name org-glance-test-headlines)))
          (should (org-glance-> headline :closed-p)))))

(Then "^the title of headline \"\\([^\"]+\\)\" should be \"\\([^\"]+\\)\"$"
      (lambda (name title)
        (should
         (string=
          (org-glance-> (gethash name org-glance-test-headlines) :title)
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

(When "^I? ?set title of the headline at point to \"\\([^\"]+\\)\"$"
  (lambda (title)
    (org-edit-headline title)
    (org-glance-material-mode:update)))

(Then "^current buffer should contain \\([[:digit:]]+\\) headlines?$"
  (lambda (expected-count)
    (let ((actual-count 0))
      (org-glance-headline:map-buffer (headline)
        (cl-incf actual-count))
      (should (= actual-count (string-to-number expected-count))))))

(And "^I? ?set headline todo state to \"\\([^\"]+\\)\"$"
     (lambda (state)
       (let ((inhibit-message t))
         (org-todo state))))

(And "^I? ?set headline tags? to \"\\([^\"]+\\)\"$"
     (lambda (tags)
       (let ((inhibit-message t))
         (org-set-tags tags))))

(Then "^headline with title \"\\([^\"]+\\)\" should be in current buffer$"
      (lambda (title)
        (goto-char (point-min))
        (should (condition-case nil
                    (progn
                      (re-search-forward (format "^*\\([A-Z ]+\\)?%s" title))
                      (string= (org-glance-> (org-glance-headline-at-point) :title)
                               title))
                  (search-failed nil)))))
