(require 'ecukes)
(require 'ert)

(require 'org-glance)
(require 'org-glance-headline)
(require 'org-glance-store)
(require 'org-glance-scope)

(Given "^store \"\\([^\"]+\\)\" in directory \"\\([^\"]+\\)\"$"
       (lambda (store-name location)
         (puthash store-name
                  (org-glance-store (FILE location))
                  org-glance-test-stores)))

(Given "^store \"\\([^\"]+\\)\" in directory \"\\([^\"]+\\)\" with headlines$"
       (lambda (store-name location headlines)
         (puthash store-name
                  (apply #'org-glance-store-from-scratch
                         (FILE location)
                         (->> headlines
                              (s-split "* ")
                              (-map #'s-trim)
                              (--filter (not (string-empty-p it)))
                              (--map (concat "* " it))))
                  org-glance-test-stores)))

(When "^I import headlines to store \"\\([^\"]+\\)\" from directory \"\\([^\"]+\\)\"$"
  (lambda (store-name location)
    (STORE>> store-name (org-glance-store-import (STORE store-name) (FILE location)))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) headlines?$"
      (lambda (store-name cardinality)
        (let ((store (STORE store-name)))
          (should (= (string-to-number cardinality)
                     (length (org-glance-store-hashes store)))))))

(Then "^store \"\\([^\"]+\\)\" should be equal to \"\\([^\"]+\\)\"$"
      (lambda (store-1 store-2)
        (should (org-glance-store-equal-p (STORE store-1) (STORE store-2)))))

(Then "^store \"\\([^\"]+\\)\" should contain headline with title \"\\([^\"]+\\)\" in memory store$"
      (lambda (store-name title)
        (let ((store (STORE store-name)))
          (should (org-glance-store-get-headline-by-title store title 'memory)))))

(Then "^store \"\\([^\"]+\\)\" should contain headline with title \"\\([^\"]+\\)\" in persistent store$"
      (lambda (store-name title)
        (let ((store (STORE store-name)))
          (should (org-glance-store-get-headline-by-title store title 'disk)))))

(Then "^store \"\\([^\"]+\\)\" should not contain headline with title \"\\([^\"]+\\)\" in memory store$"
     (lambda (store-name title)
       (let ((store (STORE store-name)))
         (should (not (org-glance-store-get-headline-by-title store title 'memory))))))

(Then "^store \"\\([^\"]+\\)\" should not contain headline with title \"\\([^\"]+\\)\" in persistent store$"
      (lambda (store-name title)
        (let ((store (STORE store-name)))
          (should (not (org-glance-store-get-headline-by-title store title 'disk))))))

(And "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) \"\\([^\"]+\\)\" headlines?$"
     (lambda (store-name expected-count expected-state)
       (cl-loop
          with store = (STORE store-name)
          for (state . hash) in (org-glance-store--state->hash store)
          when (string= state expected-state)
          count 1 into count
          finally (should (= count (string-to-number expected-count))))))

(And "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) commented headlines?$"
     (lambda (store-name expected-count)
       (cl-loop
          with store = (STORE store-name)
          for (commented-p . hash) in (org-glance-store--commented->hash store)
          unless (null commented-p)
          count 1 into count
          finally (should (= count (string-to-number expected-count))))))

(And "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) archived headlines?$"
     (lambda (store-name expected-count)
       (cl-loop
          with store = (STORE store-name)
          for (archived-p . hash) in (org-glance-store--archived->hash store)
          unless (null archived-p)
          count 1 into count
          finally (should (= count (string-to-number expected-count))))))

(And "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) closed headlines?$"
     (lambda (store-name expected-count)
       (cl-loop
          with store = (STORE store-name)
          for (closed-p . hash) in (org-glance-store--closed->hash store)
          unless (null closed-p)
          count 1 into count
          finally (should (= count (string-to-number expected-count))))))

(And "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) linked headlines?$"
     (lambda (store-name expected-count)
       (cl-loop
          with store = (STORE store-name)
          for (linked-p . hash) in (org-glance-store--linked->hash store)
          unless (null linked-p)
          count 1 into count
          finally (should (= count (string-to-number expected-count))))))

(And "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) propertized headlines?$"
     (lambda (store-name expected-count)
       (cl-loop
          with store = (STORE store-name)
          for (propertized-p . hash) in (org-glance-store--propertized->hash store)
          unless (null propertized-p)
          count 1 into count
          finally (should (= count (string-to-number expected-count))))))

(And "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) encrypted headlines?$"
     (lambda (store-name expected-count)
       (cl-loop
          with store = (STORE store-name)
          for (encrypted-p . hash) in (org-glance-store--encrypted->hash store)
          unless (null encrypted-p)
          count 1 into count
          finally (should (= count (string-to-number expected-count))))))

(And "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) headlines? of class \"\\([^\"]+\\)\"$"
     (lambda (store-name expected-count expected-class)
       (cl-loop
          with store = (STORE store-name)
          for (class . hash) in (org-glance-store--class->hash store)
          when (string= class (downcase expected-class))
          count 1 into count
          finally (should (= count (string-to-number expected-count))))))
