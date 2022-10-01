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
    (let ((view (thread-first (org-glance-test:store-get store-name)
                  (org-glance-store:view type))))
      (org-glance-test:view-put view-name view))))

(When "^I? ?materialize view \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$"
  (lambda (view-name file-name)
    (Given "empty file \"%s\"" file-name)
    (let* ((file (org-glance-test:get-file file-name))
           (view (org-glance-test:view-get view-name)))
      (org-glance-view:materialize view file))))

(Then "^view \"\\([^\"]+\\)\" should be equal to buffer view$"
      (lambda (view-name)
        (let ((view (org-glance-test:view-get view-name)))
          (should (eq view (org-glance-mew:get-buffer-view))))))

(Then "^\\([[:digit:]]+\\) markers? should be changed$"
      (lambda (changed-markers-count)
        (should (= (string-to-number changed-markers-count)
                   (bool-vector-count-population
                    (org-glance-> (org-glance-mew:current) :changed-markers))))))

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
        (And "marker at point should not be corrupted")
        (And "marker at point should not be committed")))

(Then "^current buffer offset should be latest$"
      (lambda ()
        (let* ((mew (org-glance-mew:current))
               (store (org-glance-> mew :view :store)))
          (should (org-glance-offset:equal-p
                   (org-glance-mew:offset mew)
                   (org-glance-store:offset store))))))

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

(And "^markers? positions should be consistent$"
     (lambda ()
       (let ((hc (org-glance-headline:map (headline)
                   (thunk-let ((mew (org-glance-mew:current))
                               (midx (org-glance-mew:marker-at-point)))
                     (unless (= (org-glance-mew:get-marker-position mew midx) (point-min))
                       (org-glance-debug "---")
                       (org-glance-debug "Marker positions: %s" (org-glance-> mew :marker-positions))
                       (org-glance-debug "Marker: %d" (org-glance-mew:get-marker-position mew midx))
                       (org-glance-debug "Headline: %d %d" (point-min) (point-max))
                       (org-glance-debug "--- Headline contents ---\n%s" (buffer-substring-no-properties (point-min) (point-max)))
                       (save-restriction
                         (widen)
                         (org-glance-debug "--- Marker contents ---\n%s" (buffer-substring-no-properties (org-glance-mew:get-marker-position mew midx)
                                                                                                           (point-max))))


                       ;; (org-glance-debug "Diff: \"%s\"" (buffer-substring-no-properties
                       ;;                          (org-glance-> marker :end)
                       ;;                          (point-max)))
                       )

                     (and (> midx -1)
                          (= (aref (org-glance-> mew :marker-positions) midx) (point-min)))))))
         (should (--all-p (eq it t) hc)))))

(And "^marker positions and hashes should be consistent$"
     (lambda ()
       (let ((hc (org-glance-headline:map (headline)
                   (let* ((mew (org-glance-mew:current))
                          (midx (org-glance-mew:marker-at-point mew (point-min))))

                     (org-glance-debug "---")
                     (org-glance-debug "Looking for midx: %d" midx)

                     (unless (= (org-glance-mew:get-marker-position mew midx) (point-min))
                       (org-glance-debug "Marker positions: %s" (org-glance-> mew :marker-positions))
                       (org-glance-debug "Marker position: %d" (org-glance-mew:get-marker-position mew midx))
                       (org-glance-debug "Headline: %d %d" (point-min) (point-max))
                       (org-glance-debug "Headline contents:\n\"%s\"" (buffer-substring-no-properties (point-min) (point-max)))
                       (save-restriction
                         (widen)
                         (org-glance-debug "Marker contents:\n\"%s\""
                                             (buffer-substring-no-properties
                                              (org-glance-mew:get-marker-position mew midx)
                                              (org-glance-mew:get-marker-position mew (1+ midx)))))


                       ;; (org-glance-debug "Diff: \"%s\"" (buffer-substring-no-properties
                       ;;                          (org-glance-> marker :end)
                       ;;                          (point-max)))
                       )

                     (org-glance-debug "Marker hash: %s" (org-glance-mew:get-marker-hash mew midx))
                     (org-glance-debug "Headline hash: %s" (org-glance-> headline :hash))
                     (org-glance-debug "Marker position: %d" (org-glance-mew:get-marker-position mew midx))
                     (org-glance-debug "Headline position: %d" (point-min))

                     (and (> midx -1)
                          (string= (org-glance-mew:get-marker-hash mew midx) (org-glance-> headline :hash))
                          (= (org-glance-mew:get-marker-position mew midx) (point-min)))))))
         (org-glance-debug "HC: %s" hc)
         (should (--all-p (eq it t) hc)))))

(When "^I? ?create view \"\\([^\"]+\\)\" from \"\\([^\"]+\\)\" \"\\([^\"]+\\)\" in file \"\\([^\"]+\\)\" as \"\\([^\"]+\\)\"$"
  (lambda (view class store file buffer)
    (When "I create view \"%s\" from \"%s\" \"%s\"" view class store)
    (And "I materialize view \"%s\" to \"%s\"" view file)
    (And "I find file \"%s\" as \"%s\"" file buffer)))

(When "^I? ?create view \"\\([^\"]+\\)\" from \"\\([^\"]+\\)\" \"\\([^\"]+\\)\" in file \"\\([^\"]+\\)\"$"
  (lambda (view class store file)
    (When "I create view \"%s\" from \"%s\" \"%s\"" view class store)
    (And "I materialize view \"%s\" to \"%s\"" view file)
    (And "I find file \"%s\"" file)))
