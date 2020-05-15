;;; org-glance-test.el --- Tests for org-glance

;;; Commentary:
;; This package allows you to manage your org-mode entries as materialized views.

(require 'org)
(require 'org-element)
(require 'load-relative)
(require 'f)
(require 'org-glance)
(require 'org-glance-views)
(require 'with-simulated-input)
(require 'subr-x)

(ert-deftest org-glance-test/provide-feature ()
  (should (featurep 'org-glance)))

(ert-deftest org-glance-test/define-view ()
  "View should be properly registered."
  (let ((view 'Country))
    (with-view (symbol-name view)
      (should (member view org-glance-views))
      (should (member view (hash-table-keys org-glance-view-scopes)))
      (should (member view (hash-table-keys org-glance-view-types))))
    (should-not (member view org-glance-views))
    (should-not (member view (hash-table-keys org-glance-view-scopes)))
    (should-not (member view (hash-table-keys org-glance-view-types)))))

(ert-deftest org-glance-test/materialize-view ()
  "View should be able to materialize in a separate buffer."
  (-og-user-story :choose "Netherlands" :view "Country" :from "countries.org" :act 'materialize
    (should (eq -org-glance-beg 1))
    (should (eq -org-glance-end 90))
    (should (eq -org-glance-indent 0))
    (should (eq -org-glance-pwd nil))
    (should (string= -org-glance-hash "c27f2eb34de678e0a7ef3312b239c1b2f6f61885"))
    (should (string= "Netherlands" (org-entry-title)))
    (should (member "Country" (org-get-tags)))))

(ert-deftest org-glance-test/sync-view-not-modified ()
  "Sync should raise `org-glance-view-not-modified' if materialized view is not modified."
  (-og-user-story :act 'materialize :choose "Ukraine" :view "Country" :from "countries.org"
    (should-error (org-glance-view-sync-subtree) :type 'org-glance-view-not-modified)))

(ert-deftest org-glance-test/sync-view-modified ()
  "Sync should modify original file when materialized view is modified."
  (-og-user-story :act 'materialize :choose "Russia" :view "Country" :from "countries.org"
    (let* ((old-title "Russia")
           (new-title "Russian Federation")
           (mvpos (save-excursion
                    (search-forward old-title)
                    (point)))
           (titlediff (- (length new-title) (length old-title)))
           (mvbeg -org-glance-beg))
      (goto-char (point-min))
      (replace-string old-title new-title)
      (with-user-choice "y" (org-glance-view-sync-subtree))
      (message "Visit source file: %s" -org-glance-src)
      (message "Searching for changes...")
      (with-user-choice "yes" (find-file -org-glance-src))
      (goto-char (point-min))
      (search-forward "Russian Federation")
      (should (eq (point) (+ mvbeg mvpos titlediff -1))))))

;;; org-glance-test.el ends here
