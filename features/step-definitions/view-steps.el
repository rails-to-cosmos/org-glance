;; -*- lexical-binding: t; -*-

(require 'ecukes)
(require 'ert)
(require 'f)

(require 'org-glance)

(When "^I? ?materialize view \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$"
  (lambda (view-name file-name)
    (Given "empty file \"%s\"" file-name)
    (let* ((file (org-glance-test:get-file file-name))
           (view (org-glance-test:get-view view-name)))
      (org-glance-view:materialize view file))))

(Then "^view \"\\([^\"]+\\)\" should be equal to buffer view$"
      (lambda (view-name)
        (let ((view (org-glance-test:get-view view-name)))
          (should (eq view (org-glance-view:get-buffer-view))))))

(Then "^\\([[:digit:]]+\\) markers? should be changed$"
      (lambda (changed-markers-count)
        (let ((view (org-glance-view:get-buffer-view)))
          (should (= (string-to-number changed-markers-count)
                     (cl-loop with markers = (lance-get (org-glance-view:get-buffer-view) :markers)
                        for midx below (org-glance-vector:size markers)
                        for marker = (org-glance-vector:get markers midx)
                        when (lance-get marker :changed?)
                        sum 1))))))

(When "^I? ?commit changes$"
  (lambda ()
    (let* ((view (org-glance-view:get-buffer-view))
           (world (lance-get view :world))
           (markers (lance-get view :markers)))
      (org-glance-log :markers "Before commit markers:\n\"%s\"\n" (pp-to-string markers))
      (org-glance-log :markers "Before commit changelog*:\n\"%s\"\n" (pp-to-string (lance-get world :changelog*)))
      (org-glance-log :markers "Before commit changelog:\n\"%s\"\n" (pp-to-string (lance-get world :changelog)))
      (save-buffer)
      (org-glance-log :markers "After commit markers:\n\"%s\"\n" (pp-to-string markers))
      (org-glance-log :changelog "After commit changelog*:")
      (cl-loop for event in (reverse (lance-get world :changelog* :events))
         for i from 1
         do (org-glance-log :changelog "%d) %s" i event))
      (org-glance-log :changelog "After commit changelog:")
      (cl-loop for event in (reverse (lance-get world :changelog :events))
         for i from 1
         do (org-glance-log :changelog "%d) %s" i event)))))

(When "^I? ?apply changes$"
  (lambda ()
    (When "I commit changes")
    (And "I kill buffer")))

(Then "^marker at point should be changed$"
      (lambda ()
        (let ((view (org-glance-view:get-buffer-view))
              (midx (org-glance-view:marker-at-point)))
          (should (org-glance-view:marker-changed? view midx)))))

(Then "^marker at point should not be changed$"
      (lambda ()
        (let ((view (org-glance-view:get-buffer-view))
              (midx (org-glance-view:marker-at-point)))
          (should (not (org-glance-view:marker-changed? view midx))))))

(Then "^buffer offset should be latest$"
      (lambda ()
        (let* ((view (org-glance-view:get-buffer-view))
               (world (lance-get view :world)))
          (should (org-glance-offset:equal-p
                   (org-glance-view:get-offset view)
                   (lance-run WorldOffset world))))))

(Then "^I shouldn't be able to commit$"
      (lambda ()
        (should (eq nil (condition-case nil
                            (progn
                              (org-glance-view:commit (org-glance-view:get-buffer-view))
                              t)
                          (error nil))))))

(And "^markers? positions should be consistent$"
     (lambda ()
       (let* ((view (org-glance-view:get-buffer-view))
              (hc (org-glance-headline:map (headline)
                    (let ((midx (org-glance-view:marker-at-point)))
                      (and (> midx -1)
                           (= (lance-get (org-glance-vector:get (lance-get view :markers) midx) :position)
                              (point-min)))))))
         (should (--all-p (eq it t) hc)))))

(And "^marker positions and hashes should be consistent$"
     (lambda ()
       (let* ((view (org-glance-view:get-buffer-view))
              (hc (org-glance-headline:map (headline)
                    (let ((midx (org-glance-view:marker-at-point view (point-min))))

                      (org-glance-log :markers "Looking for midx: %d" midx)

                      (unless (= (org-glance-view:get-marker-position view midx) (point-min))
                        (org-glance-log :markers "Markers: %s" (lance-get view :markers))
                        (org-glance-log :markers "Marker position: %d" (org-glance-view:get-marker-position view midx))
                        (org-glance-log :markers "Headline: %d %d" (point-min) (point-max))
                        (org-glance-log :markers "Headline contents:\n\"%s\"" (buffer-substring-no-properties (point-min) (point-max)))
                        (save-restriction
                          (widen)
                          (org-glance-log :markers "Marker contents:\n\"%s\""
                            (buffer-substring-no-properties
                             (org-glance-view:get-marker-position view midx)
                             (org-glance-view:get-marker-position view (1+ midx)))))


                        (org-glance-log :markers "Diff: \"%s\"" (buffer-substring-no-properties
                                                                 (lance-get marker :end)
                                                                 (point-max))))

                      (org-glance-log :markers "Marker hash: %s" (org-glance-view:get-marker-hash view midx))
                      (org-glance-log :markers "Headline hash: %s" (lance-get headline :hash))
                      (org-glance-log :markers "Marker position: %d" (org-glance-view:get-marker-position view midx))
                      (org-glance-log :markers "Headline position: %d" (point-min))

                      (and (> midx -1)
                           (string= (org-glance-view:get-marker-hash view midx) (lance-get headline :hash))
                           (= (org-glance-view:get-marker-position view midx) (point-min)))))))
         (org-glance-log :markers "HC: %s" hc)
         (should (--all-p (eq it t) hc)))))

(When "^I? ?visit view \"\\([^\"]+\\)\" derived from dimension \"\\([^\"]+\\)\" in world \"\\([^\"]+\\)\"$"
  (lambda (view-name dimension-name world-name)
    (let ((world (org-glance-test:get-world world-name))
          (partition (org-glance-partition:from-key-value dimension-name view-name)))
      (org-glance-world:materialize world partition))))

(Then "^world \"\\([^\"]+\\)\" should contain view \"\\([^\"]+\\)\" derived from dimension \"\\([^\"]+\\)\"$"
      (lambda (world-name view-name dimension-name)
        (let ((world (org-glance-test:get-world world-name))
              (partition (org-glance-partition:from-key-value dimension-name view-name)))
          (should (f-exists? (lance-run LocatePartition world partition))))))
