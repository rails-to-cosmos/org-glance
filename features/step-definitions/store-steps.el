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
           (org-glance-test:store-put store-name (org-glance-store :location location)))))

(Given "^store \"\\([^\"]+\\)\" in directory \"\\([^\"]+\\)\" with headlines$"
       (lambda (store-name relative-location headlines)
         (let ((location (org-glance-test:get-file relative-location)))
           (let ((store (apply #'org-glance-store:create
                               location
                               (->> headlines
                                    (s-split "* ")
                                    (-map #'s-trim)
                                    reverse
                                    (--filter (not (string-empty-p it)))
                                    (--map (concat "* " it))))))
             (org-glance-test:store-put store-name store)))))

(When "^I? ?import headlines to store \"\\([^\"]+\\)\" from directory \"\\([^\"]+\\)\"$"
  (lambda (store-name location)
    (let ((store (org-glance-test:store-get store-name)))
      (org-glance-store:import store (org-glance-test:get-file location))
      store)))

(Then "^\\([[:digit:]]+\\) staged changes should be in store \"\\([^\"]+\\)\"$"
      (lambda (expected-change-count store-name)
        (let ((store (org-glance-test:store-get store-name)))
          (should (= (string-to-number expected-change-count)
                     (org-glance-changelog:length (org-glance-> store :changelog*)))))))

(Then "^\\([[:digit:]]+\\) committed changes should be in store \"\\([^\"]+\\)\"$"
  (lambda (expected-change-count store-name)
    (let ((store (org-glance-test:store-get store-name)))
      (should (= (string-to-number expected-change-count)
                 (org-glance-changelog:length (org-glance-> store :changelog)))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) headlines?$"
      (lambda (store-name expected-count)
        (let ((store (org-glance-test:store-get store-name)))
          (should (= (string-to-number expected-count)
                     (length (org-glance-store:headlines store)))))))

(Then "^store \"\\([^\"]+\\)\" should contain headline \"\\([^\"]+\\)\" in staging layer$"
      (lambda (store-name title)
        (let ((store (org-glance-test:store-get store-name)))
          (should (org-glance-test:changelog-contains-headline-with-title title (org-glance-> store :changelog*))))))

(Then "^store \"\\([^\"]+\\)\" should contain headline \"\\([^\"]+\\)\" in committed layer$"
      (lambda (store-name title)
        (let ((store (org-glance-test:store-get store-name)))
          (should (org-glance-test:changelog-contains-headline-with-title title (org-glance-> store :changelog))))))

(Then "^store \"\\([^\"]+\\)\" should not contain headline \"\\([^\"]+\\)\" in staging layer$"
      (lambda (store-name title)
        (let ((store (org-glance-test:store-get store-name)))
          (should (not (org-glance-test:changelog-contains-headline-with-title title (org-glance-> store :changelog*)))))))

(Then "^store \"\\([^\"]+\\)\" should not contain headline \"\\([^\"]+\\)\" in committed layer$"
      (lambda (store-name title)
        (let ((store (org-glance-test:store-get store-name)))
          (should (not (org-glance-test:changelog-contains-headline-with-title title (org-glance-> store :changelog)))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) \"\\([^\"]+\\)\" headlines?$"
      (lambda (store-name expected-count expected-state)
        (cl-loop
           with store = (org-glance-test:store-get store-name)
           with events = (org-glance-store:events store)
           for event in events
           when (cl-typecase event
                  ((or org-glance-event:PUT org-glance-event:UPDATE) (string= (org-glance-> event :headline :state) expected-state))
                  (org-glance-event:RM nil))
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) commented headlines?$"
      (lambda (store-name expected-count)
        (cl-loop
           with store = (org-glance-test:store-get store-name)
           for headline in (org-glance-store:headlines store)
           for commented-p = (org-glance-> headline :commented-p)
           unless (null commented-p)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) archived headlines?$"
      (lambda (store-name expected-count)
        (cl-loop
           with store = (org-glance-test:store-get store-name)
           for headline in (org-glance-store:headlines store)
           for archived-p = (org-glance-> headline :archived-p)
           unless (null archived-p)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) closed headlines?$"
      (lambda (store-name expected-count)
        (cl-loop
           with store = (org-glance-test:store-get store-name)
           for headline in (org-glance-store:headlines store)
           for closed-p = (org-glance-> headline :closed-p)
           unless (null closed-p)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) linked headlines?$"
      (lambda (store-name expected-count)
        (cl-loop
           with store = (org-glance-test:store-get store-name)
           for headline in (org-glance-store:headlines store)
           for linked-p = (org-glance-> headline :linked-p)
           unless (null linked-p)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) propertized headlines?$"
      (lambda (store-name expected-count)
        (cl-loop
           with store = (org-glance-test:store-get store-name)
           for headline in (org-glance-store:headlines store)
           for propertized-p = (org-glance-> headline :propertized-p)
           unless (null propertized-p)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) encrypted headlines?$"
      (lambda (store-name expected-count)
        (cl-loop
           with store = (org-glance-test:store-get store-name)
           for headline in (org-glance-store:headlines store)
           for encrypted-p = (org-glance-> headline :encrypted-p)
           unless (null encrypted-p)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) headlines? of class \"\\([^\"]+\\)\"$"
      (lambda (store-name expected-count expected-class)
        (cl-loop
           with store = (org-glance-test:store-get store-name)
           for headline in (org-glance-store:headlines store)
           for class = (org-glance-> headline :class)
           when (member (downcase expected-class) class)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(When "^I? ?flush store \"\\([^\"]+\\)\"$"
  (lambda (store-name)
    (let ((store (org-glance-test:store-get store-name)))
      (org-glance-store:flush store))))

(Then "^store \"\\([^\"]+\\)\" should be equal to buffer store$"
      (lambda (store-name)
        (let ((store (org-glance-test:store-get store-name)))
          (should (eq store (org-glance-view:get-buffer-store))))))
