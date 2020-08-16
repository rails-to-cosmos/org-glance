(require 'org-glance-views)

(ert-deftest org-glance-test/def-view ()
  (let ((org-glance-default-scope '("/tmp")))
    (org-glance-let -registered-view :as registered-view
      (should (equal (org-glance-view-scope registered-view) org-glance-default-scope))
      (should (string= (org-glance-view-db registered-view) (concat user-emacs-directory "org-glance/org-glance--registered-view.el"))))))
