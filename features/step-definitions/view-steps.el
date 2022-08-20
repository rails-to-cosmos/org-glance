;; -*- lexical-binding: t; -*-

(require 'ecukes)
(require 'ert)
(require 'f)

(require 'org-glance)
(require 'org-glance-scope)

;; Models
(require 'org-glance-headline)
(require 'org-glance-store)
(require 'org-glance-view)

(When "^I create view \"\\([^\"]+\\)\" from \"\\([^\"]+\\)\" \"\\([^\"]+\\)\"$"
  (lambda (view-name class-name store-name)

    ;; TODO make proper query parser here
    (defun org-glance-test-class-filter (headline)
      (member (downcase class-name) (org-glance-headline-class headline)))

    (let* ((store (org-glance-test:get-store store-name))
           (view (org-glance-store:view store #'org-glance-test-class-filter)))
      (org-glance-test:put-view view-name view))))

(When "^I materialize view \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$"
  (lambda (view-name file-name)
    (let ((view (org-glance-test:get-view view-name)))
      (org-glance-view:materialize view (org-glance-test:get-file file-name)))))

(Then "^view \"\\([^\"]+\\)\" should be equal to buffer view$"
      (lambda (view-name)
        (let ((view (org-glance-test:get-view view-name)))
          (should (eq view (org-glance-materialization:get-buffer-view))))))
