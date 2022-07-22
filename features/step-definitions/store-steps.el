(require 'ecukes)
(require 'ert)

(require 'org-glance)
(require 'org-glance-headline)
(require 'org-glance-store)
(require 'org-glance-scope)

(Given "^store \"\\([^\"]+\\)\" in directory \"\\([^\"]+\\)\"$"
       (lambda (store-name directory)
         (puthash store-name (org-glance-store (F directory)) org-glance-test-stores)))

;; (When "^I import store \"\\([^\"]+\\)\" from directory \"\\([^\"]+\\)\"$"
;;   (lambda (store-name directory)
;;     (let* ((scope (org-glance-scope (F directory)))
;;            (store (org-glance-import scope)))
;;       (puthash store-name store org-glance-test-stores))))

;; (When "^I import store \"\\([^\"]+\\)\" from file \"\\([^\"]+\\)\"$"
;;   (lambda (store-name file)
;;     (let* ((scope (org-glance-scope (F file)))
;;            (store (org-glance-import scope)))
;;       (puthash store-name store org-glance-test-stores))))

;; (When "^I export store \"\\([^\"]+\\)\" to directory \"\\([^\"]+\\)\"$"
;;   (lambda (store-name dir-name)
;;     (let ((store (S store-name)))
;;       (org-glance-export store (f-join org-glance-test-location dir-name)))))

(Then "^store \"\\([^\"]+\\)\" should contain \\([[:digit:]]+\\) headlines?$"
      (lambda (store-name cardinality)
        (let ((store (S store-name)))
          (should (= (string-to-number cardinality)
                     (org-glance-cardinality store))))))

(When "^I materialize store \"\\([^\"]+\\)\" to file \"\\([^\"]+\\)\"$"
  (lambda (store-name file-name)
    (let ((file (F file-name))
          (store (S store-name)))
      (org-glance-materialize store file))))

(Then "^store \"\\([^\"]+\\)\" should be equal to \"\\([^\"]+\\)\"$"
      (lambda (store-1 store-2)
        (should (org-glance-equal-p (S store-1) (S store-2)))))
