;; -*- lexical-binding: t; -*-

(require 'ecukes)
(require 'ert)
(require 'f)

(require 'org-glance)
(require 'org-glance-headline)
(require 'org-glance-world)
(require 'org-glance-scope)

(Given "^world \"\\([^\"]+\\)\" in directory \"\\([^\"]+\\)\"$"
       (lambda (world-name relative-location)
         (let ((location (org-glance-test:get-file relative-location)))
           (f-mkdir-full-path location)
           (org-glance-test:world-put world-name (org-glance-world :location location)))))

(Given "^world \"\\([^\"]+\\)\" in directory \"\\([^\"]+\\)\" with headlines$"
       (lambda (world-name relative-location headlines)
         (cl-loop
            with location = (org-glance-test:get-file relative-location)
            with world = (org-glance-world:create location
                           (list (a-list :name "State"
                                         :partitions '(list state)
                                         :location '(format "%s.org" partition)
                                         :predicate `(eq state partition)
                                         :capture '(concat "* " partition " %?")))
                           ;; (list (a-list :location "by-state/%s.org"
                           ;;               :type "(eq state '%s)"
                           ;;               :filter '(lambda (headline)
                           ;;                         (list (downcase (org-glance- headline :state)))))
                           ;;       ("by-tag/%s.org"     -> (lambda (headline)
                           ;;                                 (org-glance- headline :tags)))

                           ;;       ("by-feature/%s.org" -> (lambda (headline)
                           ;;                                 (list
                           ;;                                  (when (org-glance- headline :commented?)
                           ;;                                    "commented")
                           ;;                                  (when (org-glance- headline :archived?)
                           ;;                                    "archived")
                           ;;                                  (when (org-glance- headline :closed?)
                           ;;                                    "closed")
                           ;;                                  (when (org-glance- headline :encrypted?)
                           ;;                                    "encrypted")
                           ;;                                  (when (org-glance- headline :linked?)
                           ;;                                    "linked")
                           ;;                                  (when (org-glance- headline :propertized?)
                           ;;                                    "propertized"))))
                           ;;       )
                           )
            with strings = (->> headlines
                                (s-split "* ")
                                (-map #'s-trim)
                                reverse
                                (--filter (not (string-empty-p it)))
                                (--map (concat "* " it)))
            for string in strings
            for headline = (org-glance-headline-from-string string)
            do (org-glance-world:add world headline)
            finally do
              (org-glance-world:persist world)
              (org-glance-test:world-put world-name world))))

(When "^I? ?import headlines to world \"\\([^\"]+\\)\" from directory \"\\([^\"]+\\)\"$"
  (lambda (world-name location)
    (let ((world (org-glance-test:world-get world-name)))
      (org-glance-world:import-headlines world (org-glance-test:get-file location))
      world)))

(Then "^\\([[:digit:]]+\\) staged changes should be in world \"\\([^\"]+\\)\"$"
      (lambda (expected-change-count world-name)
        (let ((world (org-glance-test:world-get world-name)))
          (should (= (string-to-number expected-change-count)
                     (org-glance-changelog:length (org-glance- world :changelog*)))))))

(Then "^\\([[:digit:]]+\\) committed changes should be in world \"\\([^\"]+\\)\"$"
      (lambda (expected-change-count world-name)
        (let ((world (org-glance-test:world-get world-name)))
          (should (= (string-to-number expected-change-count)
                     (org-glance-changelog:length (org-glance- world :changelog)))))))

(Then "^world \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) headlines?$"
      (lambda (world-name expected-count)
        (let ((world (org-glance-test:world-get world-name)))
          (should (= (string-to-number expected-count)
                     (length (org-glance-world:headlines world)))))))

(Then "^world \"\\([^\"]+\\)\" should contain headline \"\\([^\"]+\\)\" in staging layer$"
      (lambda (world-name title)
        (let ((world (org-glance-test:world-get world-name)))
          (should (org-glance-test:changelog-contains-headline-with-title title (org-glance- world :changelog*))))))

(Then "^world \"\\([^\"]+\\)\" should contain headline \"\\([^\"]+\\)\" in committed layer$"
      (lambda (world-name title)
        (let ((world (org-glance-test:world-get world-name)))
          (should (org-glance-test:changelog-contains-headline-with-title title (org-glance- world :changelog))))))

(Then "^world \"\\([^\"]+\\)\" should not contain headline \"\\([^\"]+\\)\" in staging layer$"
      (lambda (world-name title)
        (let ((world (org-glance-test:world-get world-name)))
          (should (not (org-glance-test:changelog-contains-headline-with-title title (org-glance- world :changelog*)))))))

(Then "^world \"\\([^\"]+\\)\" should not contain headline \"\\([^\"]+\\)\" in committed layer$"
      (lambda (world-name title)
        (let ((world (org-glance-test:world-get world-name)))
          (should (not (org-glance-test:changelog-contains-headline-with-title title (org-glance- world :changelog)))))))

(Then "^world \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) \"\\([^\"]+\\)\" headlines?$"
      (lambda (world-name expected-count expected-state)
        (cl-loop
           with world = (org-glance-test:world-get world-name)
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
           with world = (org-glance-test:world-get world-name)
           for headline in (org-glance-world:headlines world)
           for commented? = (org-glance- headline :commented?)
           unless (null commented?)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^world \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) archived headlines?$"
      (lambda (world-name expected-count)
        (cl-loop
           with world = (org-glance-test:world-get world-name)
           for headline in (org-glance-world:headlines world)
           for archived? = (org-glance- headline :archived?)
           unless (null archived?)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^world \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) closed headlines?$"
      (lambda (world-name expected-count)
        (cl-loop
           with world = (org-glance-test:world-get world-name)
           for headline in (org-glance-world:headlines world)
           for closed? = (org-glance- headline :closed?)
           unless (null closed?)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^world \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) linked headlines?$"
      (lambda (world-name expected-count)
        (cl-loop
           with world = (org-glance-test:world-get world-name)
           for headline in (org-glance-world:headlines world)
           for linked? = (org-glance- headline :linked?)
           unless (null linked?)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^world \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) propertized headlines?$"
      (lambda (world-name expected-count)
        (cl-loop
           with world = (org-glance-test:world-get world-name)
           for headline in (org-glance-world:headlines world)
           for propertized? = (org-glance- headline :propertized?)
           unless (null propertized?)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^world \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) encrypted headlines?$"
      (lambda (world-name expected-count)
        (cl-loop
           with world = (org-glance-test:world-get world-name)
           for headline in (org-glance-world:headlines world)
           for encrypted? = (org-glance- headline :encrypted?)
           unless (null encrypted?)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(Then "^world \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) headlines? of class \"\\([^\"]+\\)\"$"
      (lambda (world-name expected-count expected-class)
        (cl-loop
           with world = (org-glance-test:world-get world-name)
           for headline in (org-glance-world:headlines world)
           for tags = (org-glance- headline :tags)
           when (member (downcase expected-class) tags)
           count 1 into count
           finally (should (= count (string-to-number expected-count))))))

(When "^I? ?flush world \"\\([^\"]+\\)\"$"
  (lambda (world-name)
    (let ((world (org-glance-test:world-get world-name)))
      (org-glance-world:persist world))))

(Then "^world \"\\([^\"]+\\)\" should be equal to buffer world$"
      (lambda (world-name)
        (let ((world (org-glance-test:world-get world-name)))
          (should (eq world (org-glance-view:get-buffer-world))))))
