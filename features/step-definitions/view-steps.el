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
                   (bool-vector-count-population
                    (org-glance-> (org-glance-mew:current) :changed-markers--0.0.2))))))

(When "^I? ?commit changes$"
  (lambda ()
    (org-glance-mew:commit)))

(Then "^marker at point should be changed$"
      (lambda ()
        (let ((mew (org-glance-mew:current))
              (midx (org-glance-mew:marker-at-point)))
          (should (org-glance-mew:marker-changed-p mew midx)))))

(Then "^marker at point should not be changed$"
      (lambda ()
        (let ((mew (org-glance-mew:current))
              (midx (org-glance-mew:marker-at-point)))
          (should (not (org-glance-mew:marker-changed-p mew midx))))))

(Then "^marker at point should be committed$"
      (lambda ()
        (let ((mew (org-glance-mew:current))
              (midx (org-glance-mew:marker-at-point)))
          (should (org-glance-mew:marker-committed-p mew midx)))))

(Then "^marker at point should not be committed$"
      (lambda ()
        (let ((mew (org-glance-mew:current))
              (midx (org-glance-mew:marker-at-point)))
          (should (not (org-glance-mew:marker-committed-p mew midx))))))

(Then "^marker at point should be corrupted$"
      (lambda ()
        (let ((mew (org-glance-mew:current))
              (midx (org-glance-mew:marker-at-point)))
          (should (org-glance-mew:marker-corrupted-p mew midx)))))

(Then "^marker at point should not be corrupted$"
      (lambda ()
        (let ((mew (org-glance-mew:current))
              (midx (org-glance-mew:marker-at-point)))
          (should (not (org-glance-mew:marker-corrupted-p mew midx))))))

(Then "^marker at point should be up to date$"
      (lambda ()
        (And "marker at point should not be changed")
        (And "marker at point should not be corrupted")
        (And "marker at point should not be committed")))

(Then "^current buffer offset should be latest$"
      (lambda ()
        (let* ((mew (org-glance-mew:current))
               (store (org-glance-> mew :view :store)))
          (should (time-equal-p (read (org-glance-mew:get-property "OFFSET"))
                                      (org-glance-> (org-glance-changelog:last (org-glance-> store :changelog)) :offset))))))

(Then "^current mew offset should be latest$"
      (lambda ()
        (let* ((mew (org-glance-mew:current))
               (store (org-glance-> mew :view :store)))
          (should (time-equal-p (org-glance-> mew :offset)
                                      (org-glance-> (org-glance-changelog:last (org-glance-> store :changelog)) :offset))))))

(Then "^current buffer offset should not be latest$"
      (lambda ()
        (let* ((mew (org-glance-mew:current))
               (store (org-glance-> mew :view :store)))
          (should (time-less-p (read (org-glance-mew:get-property "OFFSET"))
                                      (org-glance-> (org-glance-changelog:last (org-glance-> store :changelog)) :offset))))))

(Then "^current mew offset should not be latest$"
      (lambda ()
        (let* ((mew (org-glance-mew:current))
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
                              (org-glance-mew:commit (org-glance-mew:current))
                              t)
                          (error nil))))))

(When "^I? ?fetch store changes$"
  (lambda ()
    (org-glance-mew:fetch)))

(And "^markers positions should be consistent$"
     (lambda ()
       (let ((hc (org-glance-headline:map (headline)
                   (thunk-let ((mew (org-glance-mew:current))
                               (midx (org-glance-mew:marker-at-point)))
                     ;; (unless (and (= (org-glance-> marker :beg) (point-min))
                     ;;              (= (org-glance-> marker :end) (point-max)))
                     ;;   (message "Marker %d %d" (org-glance-> marker :beg) (org-glance-> marker :end))
                     ;;   (message "Headline %d %d" (point-min) (point-max))
                     ;;   (message "Contents: %s" (prin1-to-string (buffer-string)))
                     ;;   (message "Diff: \"%s\"" (buffer-substring-no-properties
                     ;;                            (org-glance-> marker :end)
                     ;;                            (point-max))))
                     (and (> midx -1)
                          (= (aref (org-glance-> mew :marker-positions) midx) (point-min)))))))
         (should (--all-p (eq it t) hc)))))

(And "^markers positions and hashes should be consistent$"
     (lambda ()
       (let ((hc (org-glance-headline:map (headline)
                   (thunk-let* ((mew (org-glance-mew:current))
                                (midx (org-glance-mew:marker-at-point mew (point-min))))

                     (unless (= (org-glance-mew:get-marker-position mew midx) (point-min))
                       (message "---")
                       (message "Marker positions: %s" (org-glance-> mew :marker-positions))
                       (message "Marker: %d" (org-glance-mew:get-marker-position mew midx))
                       (message "Headline: %d %d" (point-min) (point-max))
                       (message "--- Headline contents ---\n%s" (buffer-substring-no-properties (point-min) (point-max)))
                       (save-restriction
                         (widen)
                         (message "--- Marker contents ---\n%s" (buffer-substring-no-properties (org-glance-mew:get-marker-position mew midx)
                                                                                         (point-max))))


                       ;; (message "Diff: \"%s\"" (buffer-substring-no-properties
                       ;;                          (org-glance-> marker :end)
                       ;;                          (point-max)))
                       )

                     (and (> midx -1)
                          (string= (org-glance-mew:get-marker-hash mew midx) (org-glance-> headline :hash))
                          (= (org-glance-mew:get-marker-position mew midx) (point-min)))))))
         (should (--all-p (eq it t) hc)))))
