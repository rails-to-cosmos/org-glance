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
                              (--map (concat "* " it)))
                         )
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
       (let ((store (STORE store-name)))
         (cl-loop for (state . hash) in (org-glance-store--state->hash store)
            when (string= state expected-state)
            count 1 into count
            finally (should (= count (string-to-number expected-count)))))))

(And "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) commented headlines?$"
     (lambda (store cardinality)
       (should nil)
       ))

(And "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) closed headlines?$"
  (lambda (store cardinality)
    (should nil)
    ))

(And "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) archived headlines?$"
  (lambda (store cardinality)
    (should nil)
    ))

(And "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) linked headlines?$"
  (lambda (store cardinality)
    (should nil)
    ))

(And "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) propertized headlines?$"
  (lambda (store cardinality)
    (should nil)
    ))

(And "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) headlines? of class \"\\([^\"]+\\)\"$"
  (lambda (store cardinality class)
    (should nil)
    ))
