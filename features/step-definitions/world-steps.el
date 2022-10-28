;; -*- lexical-binding: t; -*-

(require 'ecukes)
(require 'ert)
(require 'f)
(require 'dash)

(require 'org-glance)
(require 'org-glance-headline)
(require 'org-glance-world)
(require 'org-glance-scope)

(Given "^world \"\\([^\"]+\\)\"$"
       (lambda (world-name)
         (Given "world \"%s\" in directory \"%s\"" world-name (downcase world-name))))

(Given "^world \"\\([^\"]+\\)\" in directory \"\\([^\"]+\\)\"$"
       (lambda (world-name relative-location)
         (let* ((location (org-glance-test:get-file relative-location))
                (world (org-glance-world:get-or-create location)))
           (org-glance-test:world-put world-name world))))

(Given "^world \"\\([^\"]+\\)\" in directory \"\\([^\"]+\\)\" with headlines$"
       (lambda (world-name relative-location headlines)
         (Given "world \"%s\" in directory \"%s\"" world-name relative-location)
         (When "I add headlines to world \"%s\"" world-name headlines)))

(When "^I? ?add headlines to world \"\\([^\"]+\\)\"$"
  (lambda (world-name headlines)
    (let ((world (org-glance-test:get-world world-name))
          (strings (->> headlines
                        (s-split "* ")
                        (-map #'s-trim)
                        reverse
                        (--filter (not (string-empty-p it)))
                        (--map (concat "* " it)))))
      (cl-loop
         for string in strings
         for headline = (org-glance-headline-from-string string)
         do (org-glance-world:add-headline world headline))

      (org-glance-world:persist world)
      (org-glance-test:world-put world-name world))))

(When "^I? ?import headlines to world \"\\([^\"]+\\)\" from directory \"\\([^\"]+\\)\"$"
  (lambda (world-name location)
    (let ((world (org-glance-test:get-world world-name)))
      (org-glance-world:import-headlines world (org-glance-test:get-file location))
      world)))

(Then "^\\([[:digit:]]+\\) staged changes should be in world \"\\([^\"]+\\)\"$"
      (lambda (expected-change-count world-name)
        (let ((world (org-glance-test:get-world world-name)))
          (should (= (string-to-number expected-change-count)
                     (org-glance-changelog:length (org-glance- world :changelog*)))))))

(Then "^\\([[:digit:]]+\\) committed changes should be in world \"\\([^\"]+\\)\"$"
      (lambda (expected-change-count world-name)
        (let ((world (org-glance-test:get-world world-name)))
          (should (= (string-to-number expected-change-count)
                     (org-glance-changelog:length (org-glance- world :changelog)))))))

(Then "^world \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) headlines?$"
      (lambda (world-name expected-count)
        (let ((world (org-glance-test:get-world world-name)))
          (should (= (string-to-number expected-count)
                     (length (org-glance-world:get-headlines world)))))))

(Then "^world \"\\([^\"]+\\)\" should contain headline \"\\([^\"]+\\)\" in staging layer$"
      (lambda (world-name title)
        (let ((world (org-glance-test:get-world world-name)))
          (should (org-glance-test:changelog-contains-headline-with-title title (org-glance- world :changelog*))))))

(Then "^world \"\\([^\"]+\\)\" should contain headline \"\\([^\"]+\\)\" in committed layer$"
      (lambda (world-name title)
        (let ((world (org-glance-test:get-world world-name)))
          (should (org-glance-test:changelog-contains-headline-with-title title (org-glance- world :changelog))))))

(Then "^world \"\\([^\"]+\\)\" should not contain headline \"\\([^\"]+\\)\" in staging layer$"
      (lambda (world-name title)
        (let ((world (org-glance-test:get-world world-name)))
          (should (not (org-glance-test:changelog-contains-headline-with-title title (org-glance- world :changelog*)))))))

(Then "^world \"\\([^\"]+\\)\" should not contain headline \"\\([^\"]+\\)\" in committed layer$"
      (lambda (world-name title)
        (let ((world (org-glance-test:get-world world-name)))
          (should (not (org-glance-test:changelog-contains-headline-with-title title (org-glance- world :changelog)))))))

(Then "^world \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) \"\\([^\"]+\\)\" headlines?$"
      (lambda (world-name expected-count expected-state)
        (cl-loop
           with world = (org-glance-test:get-world world-name)
           with events = (org-glance-world:events world)
           for event in events
           when (cl-typecase event
                  ((or org-glance-event:PUT org-glance-event:UPDATE) (string= (org-glance- event :headline :state) expected-state))
                  (org-glance-event:RM nil))
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^world \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) commented headlines?$"
      (lambda (world-name expected-count)
        (cl-loop
           with world = (org-glance-test:get-world world-name)
           for headline in (org-glance-world:get-headlines world)
           for commented? = (org-glance- headline :commented?)
           unless (null commented?)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^world \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) archived headlines?$"
      (lambda (world-name expected-count)
        (cl-loop
           with world = (org-glance-test:get-world world-name)
           for headline in (org-glance-world:get-headlines world)
           for archived? = (org-glance- headline :archived?)
           unless (null archived?)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^world \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) closed headlines?$"
      (lambda (world-name expected-count)
        (cl-loop
           with world = (org-glance-test:get-world world-name)
           for headline in (org-glance-world:get-headlines world)
           for closed? = (org-glance- headline :closed?)
           unless (null closed?)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^world \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) linked headlines?$"
      (lambda (world-name expected-count)
        (cl-loop
           with world = (org-glance-test:get-world world-name)
           for headline in (org-glance-world:get-headlines world)
           for linked? = (org-glance- headline :linked?)
           unless (null linked?)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^world \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) propertized headlines?$"
      (lambda (world-name expected-count)
        (cl-loop
           with world = (org-glance-test:get-world world-name)
           for headline in (org-glance-world:get-headlines world)
           for store? = (org-glance- headline :store?)
           unless (null store?)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^world \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) encrypted headlines?$"
      (lambda (world-name expected-count)
        (cl-loop
           with world = (org-glance-test:get-world world-name)
           for headline in (org-glance-world:get-headlines world)
           for encrypted? = (org-glance- headline :encrypted?)
           unless (null encrypted?)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^world \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) headlines? of class \"\\([^\"]+\\)\"$"
      (lambda (world-name expected-count expected-class)
        (cl-loop
           with world = (org-glance-test:get-world world-name)
           for headline in (org-glance-world:get-headlines world)
           for tags = (org-glance- headline :tags)
           when (member (downcase expected-class) tags)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(When "^I? ?persist world \"\\([^\"]+\\)\"$"
  (lambda (world-name)
    (let ((world (org-glance-test:get-world world-name)))
      (org-glance-world:persist world))))

(Then "^world \"\\([^\"]+\\)\" should be equal to buffer world$"
      (lambda (world-name)
        (let ((world (org-glance-test:get-world world-name)))
          (should (eq world (org-glance-view:get-buffer-world))))))
