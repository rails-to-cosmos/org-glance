(When "^I materialize view \"\\([^\"]+\\)\" to file \"\\([^\"]+\\)\"$"
  (lambda (view-name file-name)
    (Given "empty file \"%s\"" file-name)
    (let* ((file (org-glance-test:get-file file-name))
           (view (org-glance-test:get-view view-name)))
      (org-glance-view:materialize view file))))

(When "^I commit changes to store \"\\([^\"]+\\)\"$"
  (lambda (store-name)
    (org-glance-material-overlay-manager-redisplay)
    (org-glance-commit)))

(Then "^marker at point should be changed$"
      (lambda ()
        (org-glance-material-overlay-manager-redisplay)

        (when-let (marker (org-glance-marker:at-point))
          (org-glance-materialization:update (org-glance-buffer-materialization)))

        (should (eq t (org-glance-> (org-glance-marker:at-point) :state :changed)))))

(Then "^marker at point should not be outdated$"
      (lambda ()
        (when-let (marker (org-glance-marker:at-point))
          (org-glance-marker:print marker)
          (should (not (org-glance-marker:outdated-p marker))))))
