(When "^I materialize store \"\\([^\"]+\\)\" to file \"\\([^\"]+\\)\"$"
  (lambda (store-name file-name)
    (Given "empty file \"%s\"" file-name)
    (let ((file (FILE file-name))
          (store (STORE store-name)))
      (org-glance-materialize store file)
      (find-file file))))

(When "^I commit changes to store \"\\([^\"]+\\)\"$"
  (lambda (store-name)
    (org-glance-material-overlay-manager-redisplay) ;; refactor, unlink "overlay" term
    (org-glance-commit)
    (STORE>> store-name (org-glance-material-store))))

(Then "^headline materialization at point should be changed$"
      (lambda ()
        (let ((marker (get-text-property (point) :marker)))
          (org-glance-material-marker-print marker)
          (should (eq t (org-glance-material-marker-changed-p marker))))))

(And "^headline materialization at point should not be outdated$"
     (lambda ()
       (let ((marker (get-text-property (point) :marker)))
         (should (not (org-glance-material-marker-outdated-p marker))))))
