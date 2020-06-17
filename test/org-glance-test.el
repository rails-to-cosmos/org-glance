;;; org-glance-test.el --- Tests for org-glance

;;; Commentary:
;; This package allows you to manage your org-mode entries as materialized views.

;;; Code:

(require 'org)
(require 'org-element)
(require 'load-relative)
(require 'f)
(require 'org-glance)
(require 'org-glance-views)
(require 'with-simulated-input)
(require 'subr-x)

(ert-deftest org-glance-test/provide-feature ()
  "Dummy test."
  (should (featurep 'org-glance)))

(ert-deftest org-glance-test/define-view ()
  "View should be properly registered."
  (org-glance-sandbox :view "Country" :in "countries.org"
    (should (member 'Country org-glance-views))
    ;; (should (member 'Country (hash-table-keys org-glance-view-scopes)))
    ;; (should (member 'Country (hash-table-keys org-glance-view-types)))
    ))

(ert-deftest org-glance-test/materialize-view ()
  "View should be able to materialize in a separate buffer."
  (org-glance-sandbox :view "Country" :in "countries.org" :input "Netherlands" :act 'materialize
    (should (eq -org-glance-beg 1))
    (should (eq -org-glance-end 90))
    (should (eq -org-glance-indent 0))
    (should (eq -org-glance-pwd nil))
    (should (string= -org-glance-hash "c27f2eb34de678e0a7ef3312b239c1b2f6f61885"))
    (should (string= "Netherlands" (org-entry-title)))
    (should (member "Country" (org-get-tags)))))

(ert-deftest org-glance-test/sync-view-not-modified ()
  "Sync should raise `org-glance-view-not-modified' if materialized view is not modified."
  (org-glance-sandbox :view "Country" :in "countries.org" :input "Ukraine" :act 'materialize
    (should-error (org-glance-view-sync-subtree) :type 'org-glance-view-not-modified)))

(ert-deftest org-glance-test/sync-view-not-modified-indented ()
  "Sync should raise `org-glance-view-not-modified' if materialized view is not modified (subtree indented)."
  (org-glance-sandbox :view "Region" :in "countries.org" :input "Northwestern" :act 'materialize
    (should-error (org-glance-view-sync-subtree) :type 'org-glance-view-not-modified)))

(ert-deftest org-glance-test/sync-view-modified ()
  "Sync should modify original file when materialized view is modified."
  (org-glance-sandbox :view "Country" :in "countries.org" :input "Russia" :act 'materialize
    (let* ((old-title "Russia")
           (new-title "Russian Federation")
           (mvpos (save-excursion
                    (search-forward old-title)
                    (point)))
           (titlediff (- (length new-title) (length old-title)))
           (mvbeg -org-glance-beg))
      (goto-char (point-min))
      (replace-string old-title new-title)
      (org-glance-emulate-user-input "y" (org-glance-view-sync-subtree))
      (message "Visit source file: %s" -org-glance-src)
      (message "Searching for changes...")
      (org-glance-emulate-user-input "yes" (find-file -org-glance-src))
      (goto-char (point-min))
      (search-forward "Russian Federation")
      (should (eq (point) (+ mvbeg mvpos titlediff -1))))))

(ert-deftest org-glance-test/link-view-open ()
  "Link view open feature spec."
  (cl-flet ((link-opened-p (title output) (s-ends-with-p (format "(pp \"%s\") => \"%s\"\n" title title) output)))
    (let ((org-confirm-elisp-link-function nil))

      ;; (should (->> ;; completing read on multiple links in subtree
      ;;          (org-glance-sandbox
      ;;            (org-glance-def-view "Service"
      ;;              :file "services.org"
      ;;              :type '(link))

      ;;            (org-glance-emulate-user-input ["Service with CI" "Coverage"]
      ;;              (org-glance-action-open nil "Bookmark")))
      ;;          (link-opened-p "Coverage")))

      (should (->> ; without completing read if there is only one link
               (org-glance-sandbox :view "Service" :in "services.org" :type '(link) :input "Simple service bookmark" :act 'open)
               (link-opened-p "Bookmark")))

      (should (->>   ; methods from child subtrees should be available
               (org-glance-sandbox :view "Service" :in "services.org" :type '(link) :input '("Service with CI" "Child method") :act 'open)
               (link-opened-p "Child method"))))))

;;; org-glance-test.el ends here
