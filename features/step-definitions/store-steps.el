;; -*- lexical-binding: t; -*-

(require 'ecukes)
(require 'ert)
(require 'f)

(require 'org-glance)
(require 'org-glance-headline)
(require 'org-glance-store)
(require 'org-glance-scope)

(Given "^store \"\\([^\"]+\\)\" in directory \"\\([^\"]+\\)\"$"
       (lambda (store-name relative-location)
         (let ((location (org-glance-test:get-file relative-location)))
           (f-mkdir-full-path location)
           (org-glance-test:put-store store-name (org-glance-store :location location)))))

(Given "^store \"\\([^\"]+\\)\" in directory \"\\([^\"]+\\)\" with headlines$"
       (lambda (store-name relative-location headlines)
         (let ((location (org-glance-test:get-file relative-location)))
           (let ((store (apply #'org-glance-store:from-scratch
                               location
                               (->> headlines
                                    (s-split "* ")
                                    (-map #'s-trim)
                                    (--filter (not (string-empty-p it)))
                                    (--map (concat "* " it))))))
             (org-glance-test:put-store store-name store)))))

(When "^I import headlines to store \"\\([^\"]+\\)\" from directory \"\\([^\"]+\\)\"$"
  (lambda (store-name location)
    (let ((store (org-glance-test:get-store store-name)))
      (org-glance-store:import store (org-glance-test:get-file location))
      store)))

(Then "^\\([[:digit:]]+\\) staged changes should be in store \"\\([^\"]+\\)\"$"
      (lambda (expected-change-count store-name)
        (let ((store (org-glance-test:get-store store-name)))
          (should (= (string-to-number expected-change-count)
                     (org-glance-changelog:length (org-glance-store:changelog* store)))))))

(Then "^\\([[:digit:]]+\\) committed changes should be in store \"\\([^\"]+\\)\"$"
  (lambda (expected-change-count store-name)
    (let ((store (org-glance-test:get-store store-name)))
      (should (= (string-to-number expected-change-count)
                 (org-glance-changelog:length (org-glance-store:changelog store)))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) headlines?$"
      (lambda (store-name expected-count)
        (let ((store (org-glance-test:get-store store-name)))
          (should (= (string-to-number expected-count)
                     (length (org-glance-store:headlines store)))))))

(Then "^store \"\\([^\"]+\\)\" should contain headline with title \"\\([^\"]+\\)\" in staging layer$"
      (lambda (store-name title)
        (let* ((store (org-glance-test:get-store store-name))
               (event (org-glance-changelog:last
                       (org-glance-changelog:filter
                        (org-glance-store:changelog* store)
                        (lambda (event) (string= (org-glance-headline-title (org-glance-event-state event))
                                            title))))))
          (should (and event (org-glance-event:PUT-p event))))))

(Then "^store \"\\([^\"]+\\)\" should contain headline with title \"\\([^\"]+\\)\" in committed layer$"
      (lambda (store-name title)
        (let* ((store (org-glance-test:get-store store-name))
               (event (org-glance-changelog:last
                       (org-glance-changelog:filter
                        (org-glance-store:changelog store)
                        (lambda (event) (string= (org-glance-headline-title (org-glance-event-state event))
                                            title))))))
          (should (and event (org-glance-event:PUT-p event))))))

(Then "^store \"\\([^\"]+\\)\" should not contain headline with title \"\\([^\"]+\\)\" in staging layer$"
      (lambda (store-name title)
        (let* ((store (org-glance-test:get-store store-name))
               (event (org-glance-changelog:last
                       (org-glance-changelog:filter
                        (org-glance-store:changelog* store)
                        (lambda (event) (string= (org-glance-headline-title (org-glance-event-state event))
                                            title))))))
          (should (or (null event) (org-glance-event:RM-p event))))))

(Then "^store \"\\([^\"]+\\)\" should not contain headline with title \"\\([^\"]+\\)\" in committed layer$"
      (lambda (store-name title)
        (let* ((store (org-glance-test:get-store store-name))
               (event (org-glance-changelog:last
                       (org-glance-changelog:filter
                        (org-glance-store:changelog store)
                        (lambda (event) (string= (org-glance-headline-title (org-glance-event-state event))
                                            title))))))
          (should (or (null event) (org-glance-event:RM-p event))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) \"\\([^\"]+\\)\" headlines?$"
      (lambda (store-name expected-count expected-state)
        (cl-loop
           with store = (org-glance-test:get-store store-name)
           with events = (org-glance-store:events store)
           for event in events
           when (and (org-glance-event:PUT-p event)
                     (string= (org-glance-headline-state (org-glance-event-state event)) expected-state))
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) commented headlines?$"
      (lambda (store-name expected-count)
        (cl-loop
           with store = (org-glance-test:get-store store-name)
           for headline in (org-glance-store:headlines store)
           for commented-p = (org-glance-headline-commented-p headline)
           unless (null commented-p)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) archived headlines?$"
      (lambda (store-name expected-count)
        (cl-loop
           with store = (org-glance-test:get-store store-name)
           for headline in (org-glance-store:headlines store)
           for archived-p = (org-glance-headline-archived-p headline)
           unless (null archived-p)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) closed headlines?$"
      (lambda (store-name expected-count)
        (cl-loop
           with store = (org-glance-test:get-store store-name)
           for headline in (org-glance-store:headlines store)
           for closed-p = (org-glance-headline-closed-p headline)
           unless (null closed-p)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) linked headlines?$"
      (lambda (store-name expected-count)
        (cl-loop
           with store = (org-glance-test:get-store store-name)
           for headline in (org-glance-store:headlines store)
           for linked-p = (org-glance-headline-linked-p headline)
           unless (null linked-p)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) propertized headlines?$"
      (lambda (store-name expected-count)
        (cl-loop
           with store = (org-glance-test:get-store store-name)
           for headline in (org-glance-store:headlines store)
           for propertized-p = (org-glance-headline-propertized-p headline)
           unless (null propertized-p)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) encrypted headlines?$"
      (lambda (store-name expected-count)
        (cl-loop
           with store = (org-glance-test:get-store store-name)
           for headline in (org-glance-store:headlines store)
           for encrypted-p = (org-glance-headline-encrypted-p headline)
           unless (null encrypted-p)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) headlines? of class \"\\([^\"]+\\)\"$"
      (lambda (store-name expected-count expected-class)
        (cl-loop
           with store = (org-glance-test:get-store store-name)
           for event in (org-glance-store:events store)
           for headline = (org-glance-event-state event)
           for class = (org-glance-headline-class headline)
           when (member (downcase expected-class) class)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(When "^I create view \"\\([^\"]+\\)\" from \"\\([^\"]+\\)\" \"\\([^\"]+\\)\"$"
  (lambda (view-name class-name store-name)
    (let* ((store (org-glance-test:get-store store-name))
           (view (org-glance-store:view
                  store
                  (lambda (headline)
                    (member (downcase class-name) (org-glance-headline-class headline))))))
      (org-glance-test:put-view view-name view))))

(When "^I materialize view \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$"
  (lambda (view-name file-name)
    (let ((view (org-glance-test:get-view view-name)))
      (org-glance-view:materialize view (org-glance-test:get-file file-name)))))

;; (Given "^\"\\([^\"]+\\)\" \"\\([^\"]+\\)\" as \"\\([^\"]+\\)\"$"
;;        (lambda (filter-expr store-name src-store-name)
;;          (let ((store (org-glance-test:get-store src-store-name)))
;;            (org-glance-test:put-store dst-store-name
;;                                       (org-glance-store:filter store (org-glance-store:filter-expr filter-expr))))))

(When "^I flush store \"\\([^\"]+\\)\"$"
  (lambda (store-name)
    (let ((store (org-glance-test:get-store store-name)))
      (org-glance-store:flush store))))
