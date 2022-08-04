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
          (should (org-glance-store-get-headline-by-title store title)))))

(Then "^store \"\\([^\"]+\\)\" should contain headline with title \"\\([^\"]+\\)\" in persistent store$"
      (lambda (store-name title)
        (should (cl-loop with store = (STORE store-name)
                   for (_ instruction headline) in (reverse (org-glance-store-wal store))
                   for hash = (org-glance-headline-hash headline)
                   with seen = (make-hash-table :test #'equal)
                   if (and (string= title (org-glance-headline-title headline))
                           (f-exists-p (org-glance-store-headline-location store hash)))
                   return t
                   else if (not (gethash hash seen))
                   do (puthash hash t seen)
                   finally return nil))))

(Then "^store \"\\([^\"]+\\)\" should not contain headline with title \"\\([^\"]+\\)\" in memory store$"
     (lambda (store-name title)
       (let ((store (STORE store-name)))
         (should (not (org-glance-store-get-headline-by-title store title))))))

(Then "^store \"\\([^\"]+\\)\" should not contain headline with title \"\\([^\"]+\\)\" in persistent store$"
     (lambda (store-name title)
       (should (not (cl-loop with store = (STORE store-name)
                       for (_ instruction headline) in (reverse (org-glance-store-wal store))
                       for hash = (org-glance-headline-hash headline)
                       with seen = (make-hash-table :test #'equal)
                       if (and (string= title (org-glance-headline-title headline))
                               (f-exists-p (org-glance-store-headline-location store hash)))
                       return t
                       else if (not (gethash hash seen))
                       do (puthash hash t seen)
                       finally return nil)))))

(And "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) \"\\([^\"]+\\)\" headlines?$"
  (lambda (store cardinality state)
    (should nil)
    ))

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
