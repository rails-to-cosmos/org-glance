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
                     (org-glance-cardinality store))))))

(When "^I materialize store \"\\([^\"]+\\)\" to file \"\\([^\"]+\\)\"$"
  (lambda (store-name file-name)
    (let ((file (FILE file-name))
          (store (STORE store-name)))
      (org-glance-materialize store file))))

(Then "^store \"\\([^\"]+\\)\" should be equal to \"\\([^\"]+\\)\"$"
      (lambda (store-1 store-2)
        (should (org-glance-equal-p (STORE store-1) (STORE store-2)))))

(Then "^store \"\\([^\"]+\\)\" should contain headline \"\\([^\"]+\\)\"$"
      (lambda (store-name headline-title)
        (let ((store (STORE store-name)))
          (should (a-get (org-glance-store-i-title store)
                         headline-title)))))

(And "^store \"\\([^\"]+\\)\" should not contain headline \"\\([^\"]+\\)\"$"
     (lambda (store-name headline-title)
       (let ((store (STORE store-name)))
         (should (not (a-get (org-glance-store-i-title store)
                             headline-title))))))

(And "^I commit changes to store \"\\([^\"]+\\)\"$"
     (lambda (store-name)
       (puthash store-name (org-glance-material-commit) org-glance-test-stores)))
