;;; org-glance-test.el --- Tests for org-glance

;;; Commentary:
;; This package allows you to manage your org-mode entries as materialized views.

(eval-when-compile
  (require 'org)
  (require 'org-element)
  (require 'org-glance)
  (require 'with-simulated-input)
  (require 'load-relative)
  (require 'f))

(require 'org-glance-test-init)
(require 'org-glance-test-helpers)

(ert-deftest org-glance-test/provide-feature ()
  (should (featurep 'org-glance)))

(ert-deftest org-glance-test/define-view ()
  "View should be properly registered."
  (let ((view 'Country))
    (with-temp-view (symbol-name view)
      (should (member view org-glance-views))
      (should (member view (hash-table-keys org-glance-view-scopes)))
      (should (member view (hash-table-keys org-glance-view-types))))
    (should-not (member view org-glance-views))
    (should-not (member view (hash-table-keys org-glance-view-scopes)))
    (should-not (member view (hash-table-keys org-glance-view-types)))))

(ert-deftest org-glance-test/materialize-view ()
  "View should be able to materialize in a separate buffer."
  (with-scope "countries.org"
    (with-temp-view "Country"
      (with-user-choice "Netherlands"
        (with-materialized-view "Country"
          (should (eq -org-glance-beg 1))
          (should (eq -org-glance-end 90))
          (should (eq -org-glance-indent 0))
          (should (eq -org-glance-pwd nil))
          (should (string= -org-glance-hash "c27f2eb34de678e0a7ef3312b239c1b2f6f61885"))
          ;; (should (eq -org-glance-src 90))
          (should (string= "Netherlands" (org-entry-title)))
          (should (member "Country" (org-get-tags))))))))

(ert-deftest org-glance-test/sync-view ()
  "Materialized view should be able to sync with original file."
  (with-scope "countries.org"
    (with-temp-view "Country"
      (with-user-choice "Ukraine"
        (with-materialized-view "Country"
          ;; (replace-string "Ukraine" "Belarus")
          (should-error
           (org-glance-view-sync-subtree)

           :type 'org-glance-view-not-modified))))))

;; (with-current-buffer
;;     (with-simulated-input
;;         '("first RET")
;;       (org-glance-action-materialize "testview" t))
;;   ;; w
;;   ;; (with-simulated-input '("y RET")
;;   ;;   (org-glance-mv--sync-subtree))
;;   )

;; (let ((org-agenda-files (list (org-glance-test-get-resource "1.org")))
;;       (view "testview"))


;;   (with-simulated-input
;;       '("third RET yes RET")
;;     (org-glance-action-materialize view t)))

;; Local Variables:
;; org-literate-test-selector: "^org-glance-test--*"
;; org-literate-test-buffer: "*org-glance-tests*"
;; End:

;;; org-glance-test.el ends here
