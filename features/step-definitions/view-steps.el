;; -*- lexical-binding: t; -*-

(require 'ecukes)
(require 'ert)
(require 'f)

(require 'org-glance)
(require 'org-glance-scope)

(require 'org-glance-headline)
(require 'org-glance-world)
(require 'org-glance-world-model)
(require 'org-glance-view)

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
        (should (= (string-to-number changed-markers-count)
                   (cl-loop for marker across-ref (org-glance- (org-glance-view:get-buffer-view) :markers)
                      when (org-glance- marker :changed?)
                      sum 1)))))

(When "^I? ?commit changes$"
  (lambda ()
    (org-glance-view:commit)
    (save-buffer)))

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
               (world (org-glance- view :world)))
          (should (org-glance-offset:equal-p
                   (org-glance-view:get-offset view)
                   (org-glance-world-model:offset world))))))

(Then "^I shouldn't be able to commit$"
      (lambda ()
        (should (eq nil (condition-case nil
                            (progn
                              (org-glance-view:commit (org-glance-view:get-buffer-view))
                              t)
                          (error nil))))))

(And "^markers? positions should be consistent$"
     (lambda ()
       (let ((hc (org-glance-headline:map (headline)
                   (thunk-let ((view (org-glance-view:get-buffer-view))
                               (midx (org-glance-view:marker-at-point)))
                     (unless (= (org-glance-view:get-marker-position view midx) (point-min))
                       (org-glance-log :markers "Markers: %s" (org-glance- view :markers))
                       (org-glance-log :markers "Marker: %d" (org-glance-view:get-marker-position view midx))
                       (org-glance-log :markers "Headline: %d %d" (point-min) (point-max))
                       (org-glance-log :markers "Headline contents:\n%s" (buffer-substring-no-properties (point-min) (point-max)))
                       (save-restriction
                         (widen)
                         (org-glance-log :markers "Marker contents:\n%s" (buffer-substring-no-properties (org-glance-view:get-marker-position view midx)
                                                                                                         (point-max))))

                       (org-glance-log :markers "Diff: \"%s\"" (buffer-substring-no-properties
                                                                (org-glance- marker :end)
                                                                (point-max))))

                     (and (> midx -1)
                          (= (org-glance- (aref (org-glance- view :markers) midx) :position) (point-min)))))))
         (should (--all-p (eq it t) hc)))))

(And "^marker positions and hashes should be consistent$"
     (lambda ()
       (let ((hc (org-glance-headline:map (headline)
                   (let* ((view (org-glance-view:get-buffer-view))
                          (midx (org-glance-view:marker-at-point view (point-min))))

                     (org-glance-log :markers "Looking for midx: %d" midx)

                     (unless (= (org-glance-view:get-marker-position view midx) (point-min))
                       (org-glance-log :markers "Markers: %s" (org-glance- view :markers))
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
                                                             (org-glance- marker :end)
                                                             (point-max))))

                     (org-glance-log :markers "Marker hash: %s" (org-glance-view:get-marker-hash view midx))
                     (org-glance-log :markers "Headline hash: %s" (org-glance- headline :hash))
                     (org-glance-log :markers "Marker position: %d" (org-glance-view:get-marker-position view midx))
                     (org-glance-log :markers "Headline position: %d" (point-min))

                     (and (> midx -1)
                          (string= (org-glance-view:get-marker-hash view midx) (org-glance- headline :hash))
                          (= (org-glance-view:get-marker-position view midx) (point-min)))))))
         (org-glance-log :markers "HC: %s" hc)
         (should (--all-p (eq it t) hc)))))

(When "^I? ?visit view \"\\([^\"]+\\)\" derived from dimension \"\\([^\"]+\\)\" in world \"\\([^\"]+\\)\"$"
  (lambda (view-name dimension-name world-name)
    (let ((world (org-glance-test:get-world world-name)))
      (org-glance-world:browse world (format "%s=%s" dimension-name view-name)))))

(Then "^world \"\\([^\"]+\\)\" should contain view \"\\([^\"]+\\)\" derived from dimension \"\\([^\"]+\\)\"$"
      (lambda (world-name view-name dimension-name)
        (let ((world (org-glance-test:get-world world-name)))
          (should (f-exists? (org-glance-world:locate-dimension world (format "%s=%s" dimension-name view-name)))))))
