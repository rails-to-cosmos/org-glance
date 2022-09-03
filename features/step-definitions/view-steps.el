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
          (should (eq view (org-glance-mew:get-buffer-view))))))

(Then "^\\([[:digit:]]+\\) markers? should be changed$"
      (lambda (changed-markers-count)
        (should (= (string-to-number changed-markers-count)
                   (length (org-glance-> (org-glance-buffer:mew)
                             :changes))))))

(When "^I? ?commit changes$"
  (lambda ()
    (org-glance-mew:commit)))

(Then "^marker at point should be changed$"
      (lambda ()
        (when-let (marker (org-glance-marker:at-point))
          (should (member marker (org-glance-> (org-glance-buffer:mew)
                                   :changes))))
        (should (eq t (org-glance-> (org-glance-marker:at-point) :state :changed)))))

(Then "^marker at point should not be changed$"
      (lambda ()
        (should (not (eq t (org-glance-> (org-glance-marker:at-point) :state :changed))))))

(Then "^marker at point should not be outdated$"
      (lambda ()
        (should (not (eq t (org-glance-> (org-glance-marker:at-point) :state :outdated))))))

(Then "^marker at point should be outdated$"
      (lambda ()
        (should (eq t (org-glance-> (org-glance-marker:at-point) :state :outdated)))))

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

(Then "^marker at point should be up to date$"
      (lambda ()
        (And "marker at point should not be changed")
        (And "marker at point should not be corrupted")
        (And "marker at point should not be committed")
        (And "marker at point should not be outdated")))

(Then "^current buffer offset should be latest$"
      (lambda ()
        (let* ((mew (org-glance-buffer:mew))
               (store (org-glance-> mew :view :store)))
          (should (time-equal-p (read (org-glance-mew:get-property "OFFSET"))
                                      (org-glance-> (org-glance-changelog:last (org-glance-> store :changelog)) :offset))))))

(Then "^current mew offset should be latest$"
      (lambda ()
        (let* ((mew (org-glance-buffer:mew))
               (store (org-glance-> mew :view :store)))
          (should (time-equal-p (org-glance-> mew :offset)
                                      (org-glance-> (org-glance-changelog:last (org-glance-> store :changelog)) :offset))))))

(Then "^current buffer offset should not be latest$"
      (lambda ()
        (let* ((mew (org-glance-buffer:mew))
               (store (org-glance-> mew :view :store)))
          (should (time-less-p (read (org-glance-mew:get-property "OFFSET"))
                                      (org-glance-> (org-glance-changelog:last (org-glance-> store :changelog)) :offset))))))

(Then "^current mew offset should not be latest$"
      (lambda ()
        (let* ((mew (org-glance-buffer:mew))
               (store (org-glance-> mew :view :store)))
          (should (time-less-p (org-glance-> mew :offset)
                                      (org-glance-> (org-glance-changelog:last (org-glance-> store :changelog)) :offset))))))

(And "^current buffer should be up to date$"
  (lambda ()
    (And "current buffer offset should be latest")
    (And "current mew offset should be latest")
    (And "0 markers should be changed")))

(Then "^I shouldn't be able to commit$"
      (lambda ()
        (should (eq nil (condition-case nil
                            (progn
                              (org-glance-mew:commit (org-glance-buffer:mew))
                              t)
                          (error nil))))))

(When "^I? ?fetch store changes$"
  (lambda ()
    (org-glance-mew:fetch)))

(And "^markers should be consistent$"
     (lambda ()
       (let ((hc (org-glance-headline:map (headline)
                   (let ((marker (org-glance-marker:at-point)))
                     ;; (unless (and (= (org-glance-> marker :beg) (point-min))
                     ;;              (= (org-glance-> marker :end) (point-max)))
                     ;;   (message "Marker %d %d" (org-glance-> marker :beg) (org-glance-> marker :end))
                     ;;   (message "Headline %d %d" (point-min) (point-max))
                     ;;   (message "Contents: %s" (prin1-to-string (buffer-string)))
                     ;;   (message "Diff: \"%s\"" (buffer-substring-no-properties
                     ;;                            (org-glance-> marker :end)
                     ;;                            (point-max))))
                     (and marker (= (org-glance-> marker :beg) (point-min)) (= (org-glance-> marker :end) (point-max)))))))
         (should (--all-p (eq it t) hc)))))
