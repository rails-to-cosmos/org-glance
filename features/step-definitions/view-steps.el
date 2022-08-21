;; -*- lexical-binding: t; -*-

(require 'ecukes)
(require 'ert)
(require 'f)

(require 'org-glance)
(require 'org-glance-scope)

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
    (Given "empty file \"%s\"" file-name)
    (let* ((file (org-glance-test:get-file file-name))
           (view (org-glance-test:get-view view-name)))
      (org-glance-view:materialize view file))))

(Then "^view \"\\([^\"]+\\)\" should be equal to buffer view$"
      (lambda (view-name)
        (let ((view (org-glance-test:get-view view-name)))
          (should (eq view (org-glance-materialization:get-buffer-view))))))

(Then "^\\([[:digit:]]+\\) markers? should be changed$"
      (lambda (changed-markers-count)
        (should (= (string-to-number changed-markers-count)
                   (length (org-glance-> (org-glance-buffer-materialization)
                             :changes))))))

(When "^I commit changes$"
  (lambda ()
    (org-glance-commit)))

(Then "^marker at point should be changed$"
      (lambda ()
        (when-let (marker (org-glance-marker:at-point))
          (org-glance-materialization:update (org-glance-buffer-materialization))
          (should (member marker (org-glance-> (org-glance-buffer-materialization)
                                   :changes))))
        (should (eq t (org-glance-> (org-glance-marker:at-point) :state :changed)))))

(Then "^marker at point should not be changed$"
      (lambda ()
        (when-let (marker (org-glance-marker:at-point))
          (org-glance-materialization:update (org-glance-buffer-materialization)))
        (should (not (eq t (org-glance-> (org-glance-marker:at-point) :state :changed))))))

(Then "^marker at point should not be outdated$"
      (lambda ()
        (should (not (eq t (org-glance-> (org-glance-marker:at-point) :state :outdated))))))

(Then "^marker at point should be committed$"
      (lambda ()
        (message "Actual %s" (org-glance-marker:prin1-to-string (org-glance-marker:at-point)))
        ;; (should (eq t (org-glance-> (org-glance-marker:at-point) :state :committed)))
        ))

(Then "^marker at point should not be committed$"
      (lambda ()
        (should (not (eq t (org-glance-> (org-glance-marker:at-point) :state :committed))))))
