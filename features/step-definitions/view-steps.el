;; -*- lexical-binding: t; -*-

(require 'ecukes)
(require 'ert)
(require 'f)

(require 'org-glance)
(require 'org-glance-scope)

(require 'org-glance-headline)
(require 'org-glance-store)
(require 'org-glance-view)

(When "^I? ?create view \"\\([^\"]+\\)\" from \"\\([^\"]+\\)\" \"\\([^\"]+\\)\"$"
  (lambda (view-name type store-name)
    (let ((view (thread-first (org-glance-test:get-store store-name)
                  (org-glance-store:view type))))
      (org-glance-test:put-view view-name view))))

(When "^I? ?materialize view \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$"
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

(When "^I? ?commit changes$"
  (lambda ()
    (org-glance-material-mode:commit)))

(Then "^marker at point should be changed$"
      (lambda ()
        (when-let (marker (org-glance-marker:at-point))
          (should (member marker (org-glance-> (org-glance-buffer-materialization)
                                   :changes))))
        (should (eq t (org-glance-> (org-glance-marker:at-point) :state :changed)))))

(Then "^marker at point should not be changed$"
      (lambda ()
        (should (not (eq t (org-glance-> (org-glance-marker:at-point) :state :changed))))))

(Then "^marker at point should not be outdated$"
      (lambda ()
        (should (not (eq t (org-glance-> (org-glance-marker:at-point) :state :outdated))))))

(Then "^marker at point should be committed$"
      (lambda ()
        (should (eq t (org-glance-> (org-glance-marker:at-point) :state :committed)))))

(Then "^marker at point should not be committed$"
      (lambda ()
        (should (not (eq t (org-glance-> (org-glance-marker:at-point) :state :committed))))))

(Then "^marker at point should be corrupted$"
      (lambda ()
        (should (eq t (org-glance-> (org-glance-marker:at-point) :state :corrupted)))))

(Then "^marker at point should not be corrupted$"
      (lambda ()
        (should (not (eq t (org-glance-> (org-glance-marker:at-point) :state :corrupted))))))

(Then "^current buffer offset should be latest$"
      (lambda ()
        (let* ((materialization (org-glance-buffer-materialization))
               (store (org-glance-> materialization :view :store)))
          (should (= (read (org-glance-materialization:get-property "OFFSET"))
                     (org-glance-event-offset (org-glance-changelog:last (org-glance-> store :changelog))))))))

(Then "^current materialization offset should be latest$"
      (lambda ()
        (let* ((materialization (org-glance-buffer-materialization))
               (store (org-glance-> materialization :view :store)))
          (should (= (org-glance-> materialization :offset)
                     (org-glance-event-offset (org-glance-changelog:last (org-glance-> store :changelog))))))))

(Then "^current buffer offset should not be latest$"
      (lambda ()
        (let* ((materialization (org-glance-buffer-materialization))
               (store (org-glance-> materialization :view :store)))
          (should (< (read (org-glance-materialization:get-property "OFFSET"))
                     (org-glance-event-offset (org-glance-changelog:last (org-glance-> store :changelog))))))))

(Then "^current materialization offset should not be latest$"
      (lambda ()
        (let* ((materialization (org-glance-buffer-materialization))
               (store (org-glance-> materialization :view :store)))
          (should (< (org-glance-> materialization :offset)
                     (org-glance-event-offset (org-glance-changelog:last (org-glance-> store :changelog))))))))
