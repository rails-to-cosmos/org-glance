;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(require 'org-glance)
(require 'with-simulated-input)
(require 'subr-x)

(Given "^temporary directory with read and write permission$"
       (lambda ()
         (let ((dir (make-temp-file "org-glance-test-" t)))
           (find-file dir))))

(Then "^I create directory \"\\(.+\\)\"$"
      (lambda (dir) (mkdir dir)))

(Then "^I change directory to \"\\(.+\\)\"$"
      (lambda (dir) (cd dir)))

(Then "^I'm in a root directory$"
      (lambda () (cd org-glance-test:location)))

(Then "^I change directory to parent directory$"
      (lambda () (cd "../")))

(Given "^empty default scope"
       (lambda ()
         (setq org-glance-default-scope (list))))

(Given "^I set view location to current directory$"
       (lambda ()
         (setq org-glance-view-location default-directory)))

(When "^I add the file to default scope"
  (lambda () (cl-pushnew (buffer-file-name) org-glance-default-scope)))

(And "^I create org-mode file \"\\(.+\\)\"$"
     (lambda (filename text)
       (find-file filename)
       (insert text)
       (message "Create file %s" filename)
       (save-buffer)))

(Then "^I should have \\([[:digit:]]+\\) files? in default scope$"
      (lambda (n) (should
              (= (length org-glance-default-scope)
                 (string-to-number n)))))

(Then "^I define view \"\\(.+\\)\" with default scope$"
      (lambda (view-id) (org-glance-def-view (intern view-id))))

(Then "^I should see$"
      (lambda (text)
        (should (s-contains-p text (s-trim (thing-at-point 'line))))))

(Then "^I should see \"\\(.+\\)\"$"
      (lambda (text)
        (Then "I should see" text)))

(And "^I should see message$"
     (lambda (text)
       (with-current-buffer "*Messages*"
         (goto-char (point-max))
         (Then "I should see" text))))

(And "^I should have \\([[:digit:]]+\\) headlines in view \"\\(.+\\)\"$"
     (lambda (cardinality view-id)
       (let ((actual-cardinality (->> org-glance-views
                                   (gethash (intern view-id))
                                   (org-glance-view-headlines)
                                   (length)))
             (expected-cardinality (->> cardinality
                                     (string-to-number))))
         (should (= expected-cardinality actual-cardinality)))))

(And "^I should have \\([[:digit:]]+\\) views? registered$"
     (lambda (cardinality)
       (let ((actual-cardinality (->> org-glance-views
                                   (hash-table-keys)
                                   (length)))
             (expected-cardinality (->> cardinality
                                     (string-to-number))))
         (should (= expected-cardinality actual-cardinality)))))

(And "^I should have a choice of \\([[:digit:]]+\\) headlines? for \"\\([^\"]+\\)\" action$"
     (lambda (cardinality action)
       (let ((actual-cardinality (->> (org-glance-action-headlines 'visit)
                                   (length)))
             (expected-cardinality (->> cardinality
                                     (string-to-number))))
         (should (= expected-cardinality actual-cardinality)))))

(When "^I run action \"\\(.+\\)\" for headlines and type \"\\(.+\\)\"$"
  (lambda (action user-input)
    (with-simulated-input user-input
      (cond ((string-equal action "visit") (org-glance-action-visit))))))

;; (with-simulated-input "Gosuslugi RET"
;;   (org-glance-action-visit))

(But "^I should not have \"\\(.+\\)\"$"
     (lambda (something)
       ;; ...
       ))

(Given "^I am in the buffer \"\\([^\"]+\\)\"$"
  (lambda (arg)
    (switch-to-buffer (get-buffer-create arg))
    ))

(And "^I insert$"
  (lambda (arg)
    (insert arg)))
