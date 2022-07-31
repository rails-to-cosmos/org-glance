(require 'ecukes)
(require 'ert)

(require 'org-glance)
(require 'org-glance-headline)
(require 'org-glance-store)
(require 'org-glance-scope)

(Given "^store \"\\([^\"]+\\)\" in directory \"\\([^\"]+\\)\"$"
       (lambda (store-name location)
         (puthash store-name (org-glance-store (FILE location)) org-glance-test-stores)))

(When "^I import headlines to store \"\\([^\"]+\\)\" from directory \"\\([^\"]+\\)\"$"
  (lambda (store-name location)
    (STORE>> store-name (org-glance-store-import (STORE store-name) (FILE location)))))

;; (When "^I import store \"\\([^\"]+\\)\" from directory \"\\([^\"]+\\)\"$"
;;   (lambda (store-name directory)
;;     (let* ((scope (org-glance-scope (FILE directory)))
;;            (store (org-glance-import scope)))
;;       (puthash store-name store org-glance-test-stores))))

;; (When "^I import store \"\\([^\"]+\\)\" from file \"\\([^\"]+\\)\"$"
;;   (lambda (store-name file)
;;     (let* ((scope (org-glance-scope (FILE file)))
;;            (store (org-glance-import scope)))
;;       (puthash store-name store org-glance-test-stores))))

;; (When "^I export store \"\\([^\"]+\\)\" to directory \"\\([^\"]+\\)\"$"
;;   (lambda (store-name dir-name)
;;     (let ((store (STORE store-name)))
;;       (org-glance-export store (f-join org-glance-test-location dir-name)))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) headlines?$"
      (lambda (store-name cardinality)
        (let ((store (STORE store-name)))
          (should (= (string-to-number cardinality)
                     (org-glance-store-cardinality store))))))

(Then "^store \"\\([^\"]+\\)\" should be equal to \"\\([^\"]+\\)\"$"
      (lambda (store-1 store-2)
        (should (org-glance-store-equal-p (STORE store-1) (STORE store-2)))))

(Then "^store \"\\([^\"]+\\)\" should contain headline with title \"\\([^\"]+\\)\"$"
      (lambda (store-name headline-title)
        (let ((store (STORE store-name)))
          (should (org-glance-store-get-headline-by-title store headline-title)))))

(And "^store \"\\([^\"]+\\)\" should not contain headline with title \"\\([^\"]+\\)\"$"
     (lambda (store-name headline-title)
       (let ((store (STORE store-name)))
         (should (not (org-glance-store-get-headline-by-title store headline-title))))))
