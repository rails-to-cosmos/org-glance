(When "^I materialize view \"\\([^\"]+\\)\" to file \"\\([^\"]+\\)\"$"
  (lambda (view-name file-name)
    (Given "empty file \"%s\"" file-name)
    (let* ((file (org-glance-test:get-file file-name))
           (view (org-glance-test:get-view view-name)))
      (org-glance-view:materialize view file))))

(When "^I commit changes to store \"\\([^\"]+\\)\"$"
  (lambda (store-name)
    (org-glance-material-overlay-manager-redisplay) ;; refactor, unlink "overlay" term
    (org-glance-commit)
    (org-glance-test:put-store store-name (org-glance-material-store))))

(Then "^headline materialization at point should be changed$"
      (lambda ()
        (let ((marker (get-text-property (point) :marker)))
          (org-glance-material-marker-print marker)
          (should (eq t (org-glance-material-marker-changed-p marker))))))

(Then "^headline materialization at point should not be outdated$"
      (lambda ()
        (let ((marker (get-text-property (point) :marker)))
          (should (not (org-glance-material-marker-outdated-p marker))))))
