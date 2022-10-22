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

(When "^I? ?go to headline \"\\([^\"]+\\)\"$"
  (lambda (title)
    (let ((title-prefix (format "^*\\([A-Z ]+\\)?%s" title)))
      (goto-char (point-min))
      (condition-case nil
          (while (or (null (org-glance-headline-at-point))
                     (not (string= (org-glance- (org-glance-headline-at-point) :title) title)))
            (re-search-forward title-prefix))
        (search-failed (error "Headline not found: %s" title))))))

(Then "^headline \"\\([^\"]+\\)\" contents? should be$"
      (lambda (name expected-contents)
        (let* ((headline (gethash name org-glance-test-headlines))
               (contents (org-glance- headline :contents)))
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
      (org-glance-headline:save headline (f-join org-glance-test:location file)))))

(When "^I? ?load headline \"\\([^\"]+\\)\" from file \"\\([^\"]+\\)\"$"
      (lambda (headline file)
        (puthash headline (org-glance-headline:read (f-join org-glance-test:location file))
                 org-glance-test-headlines)))

(Then "^headline at point should contain links?$"
      (lambda ()
        (should (org-glance- (org-glance-headline-at-point) :linked?))))

(Then "^headline \"\\([^\"]+\\)\" should contain links?$"
      (lambda (headline-title)
        (When "I go to headline \"%s\"" headline-title)
        (Then "headline at point should contain link")))

(Then "^headline \"\\([^\"]+\\)\" should not contain links?$"
      (lambda (headline)
        (should-error (Then "headline \"%s\" should contain links" headline))))

(Then "^headline at point should be encrypted$"
      (lambda ()
        (should (org-glance- (org-glance-headline-at-point) :encrypted?))))

(Then "^headline \"\\([^\"]+\\)\" should be encrypted$"
      (lambda (headline-title)
        (When "I go to headline \"%s\"" headline-title)
        (Then "headline at point should be encrypted")))

(Then "^headline at point should not be encrypted$"
      (lambda ()
        (should (not (org-glance- (org-glance-headline-at-point) :encrypted?)))))

(Then "^headline \"\\([^\"]+\\)\" should not be encrypted$"
      (lambda (headline-title)
        (When "I go to headline \"%s\"" headline-title)
        (Then "headline at point should not be encrypted")))

(Then "^headline at point should be propertized$"
      (lambda ()
        (should (org-glance- (org-glance-headline-at-point) :store?))))

(Then "^headline at point should not be propertized$"
      (lambda ()
        (should (org-glance- (org-glance-headline-at-point) :store?))))

(Then "^headline \"\\([^\"]+\\)\" should be propertized$"
      (lambda (headline-title)
        (When "I go to headline \"%s\"" headline-title)
        (Then "headline at point should be propertized")))

(Then "^headline \"\\([^\"]+\\)\" should not be propertized$"
      (lambda (headline-title)
        (When "I go to headline \"%s\"" headline-title)
        (Then "headline at point should not be propertized")))

(Then "^headline at point should be archived$"
      (lambda ()
        (should (org-glance- (org-glance-headline-at-point) :archived?))))

(Then "^headline at point should not be archived$"
      (lambda ()
        (should (org-glance- (org-glance-headline-at-point) :archived?))))

(Then "^headline \"\\([^\"]+\\)\" should be archived$"
      (lambda (headline-title)
        (When "I go to headline \"%s\"" headline-title)
        (Then "headline at point should be archived")))

(Then "^headline \"\\([^\"]+\\)\" should not be archived$"
      (lambda (headline-title)
        (When "I go to headline \"%s\"" headline-title)
        (Then "headline at point should not be archived")))

(Then "^headline at point should be commented$"
      (lambda ()
        (should (org-glance- (org-glance-headline-at-point) :commented?))))

(Then "^headline at point should not be commented$"
      (lambda ()
        (should (org-glance- (org-glance-headline-at-point) :commented?))))

(Then "^headline \"\\([^\"]+\\)\" should be commented$"
      (lambda (headline-title)
        (When "I go to headline \"%s\"" headline-title)
        (Then "headline at point should be commented")))

(Then "^headline \"\\([^\"]+\\)\" should not be commented$"
      (lambda (headline-title)
        (When "I go to headline \"%s\"" headline-title)
        (Then "headline at point should not be commented")))

(Then "^headline at point should be closed$"
      (lambda ()
        (should (org-glance- (org-glance-headline-at-point) :closed?))))

(Then "^headline at point should not be closed$"
      (lambda ()
        (should (org-glance- (org-glance-headline-at-point) :closed?))))

(Then "^headline \"\\([^\"]+\\)\" should be closed$"
      (lambda (headline-title)
        (When "I go to headline \"%s\"" headline-title)
        (Then "headline at point should be closed")))

(Then "^headline \"\\([^\"]+\\)\" should not be closed$"
      (lambda (headline-title)
        (When "I go to headline \"%s\"" headline-title)
        (Then "headline at point should not be closed")))

(Then "^the title of headline at point should be \"\\([^\"]+\\)\"$"
  (lambda (expected-title)
    (should
     (string=
      (org-glance- (org-glance-headline-at-point) :title)
      expected-title))))

(Then "^the title of headline \"\\([^\"]+\\)\" should be \"\\([^\"]+\\)\"$"
      (lambda (headline-title expected-title)
        (When "I go to headline \"%s\"" headline-title)
        (Then "the title of headline at point should be \"%s\"" expected-title)))

(Then "^headline at point contents should be$"
      (lambda (expected-contents)
        (let ((contents (org-glance-headline:with-headline-at-point
                          (goto-char (point-min))
                          (forward-line)
                          (s-trim (buffer-substring-no-properties (point) (point-max))))))
          (should (string= (with-temp-buffer
                             (insert expected-contents)
                             (org-align-tags 'all)
                             (buffer-substring-no-properties (point-min) (point-max)))
                           (with-temp-buffer
                             (insert contents)
                             (org-align-tags 'all)
                             (buffer-substring-no-properties (point-min) (point-max))))))))

(Then "^headline at point should be an? \\([^\"]+\\)$"
      (lambda (expected-class)
        (let* ((headline (org-glance-headline-at-point))
               (class (org-glance- headline :tags)))
          (should (member expected-class class)))))

(Then "^the contents of headline \"\\([^\"]+\\)\" should be$"
      (lambda (headline-title expected-contents)
        (When "I go to headline \"%s\"" headline-title)
        (Then "headline at point contents should be\n\"\"%s\"\"\"" expected-contents)))

(Then "^headline \"\\([^\"]+\\)\" should be an? \\([^\"]+\\)$"
      (lambda (headline-title expected-class)
        (When "I go to headline \"%s\"" headline-title)
        (Then "headline at point should be a %s" expected-class)))

(When "^I? ?set title of headline at point to \"\\([^\"]+\\)\"$"
  (lambda (title)
    (org-edit-headline title)))

(And "^I? ?set headline \"\\([^\"]+\\)\" contents to$"
     (lambda (title contents)
       (When "I go to headline \"%s\"" title)
       (org-glance-headline:with-headline-at-point
         (goto-char (point-min))
         (if (= 0 (forward-line))
             (delete-region (point) (point-max))
           (goto-char (point-max)))
         (insert contents "\n"))))

(Then "^current buffer should contain \\([[:digit:]]+\\) headlines?$"
      (lambda (expected-count)
        (let ((actual-count 0))
          (org-glance-headline:map (headline)
            (cl-incf actual-count))
          (should (= actual-count (string-to-number expected-count))))))

(And "^I? ?set headline todo state to \"\\([^\"]+\\)\"$"
     (lambda (state)
       (org-glance-log:scenario "Update headline set state = \"%s\" where state = \"%s\""
         state
         (org-get-todo-state))
       (let ((inhibit-message t))
         (org-todo state))))

(And "^I? ?set headline tags? to \"\\([^\"]+\\)\"$"
     (lambda (tags)
       (let ((inhibit-message t))
         (org-set-tags tags))))

(Then "^headline \"\\([^\"]+\\)\" should be in current buffer$"
      (lambda (title)
        (should (--any (eq it t)
                       (org-glance-headline:map (headline)
                         (org-glance-log:debug "Compare \"%s\" vs \"%s\"" title (org-glance- headline :title))
                         (string= (org-glance- headline :title) title))))))

(Then "^headline \"\\([^\"]+\\)\" should not be in current buffer$"
      (lambda (title)
        (should (not (--any (eq it t)
                            (org-glance-headline:map (headline)
                              (string= (org-glance- headline :title) title)))))))

(When "^I? ?rename headline \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$"
  (lambda (from-title to-title)
    (When "I go to headline \"%s\"" from-title)
    (And "set title of headline at point to \"%s\"" to-title)))

(Then "^headline \"\\([^\"]+\\)\" should be changed$"
  (lambda (headline)
    (When "I go to headline \"%s\"" headline)
    (Then "marker at point should be changed")))
