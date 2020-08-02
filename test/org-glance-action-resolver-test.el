(require 'org-glance-views)

(ert-deftest org-glance-test/action-resolver ()
  (with-org-glance-view ordinary-view :as view
    (should (equal (org-glance-view-action-resolve view 'materialize) org-glance-view-default-type))
    (should (equal (org-glance-view-action-resolve view 'visit) org-glance-view-default-type))
    (should (equal (org-glance-view-action-resolve view 'open) nil))
    (should (equal (org-glance-view-action-resolve view 'extract-property) nil)))

  (with-org-glance-view key-value-store :as view :type '(kvs)
    (should (equal (org-glance-view-action-resolve view 'materialize) org-glance-view-default-type))
    (should (equal (org-glance-view-action-resolve view 'visit) org-glance-view-default-type))
    (should (equal (org-glance-view-action-resolve view 'open) nil))
    (should (equal (org-glance-view-action-resolve view 'extract-property) '(kvs))))

  (with-org-glance-view encrypted-key-value-store :as view :type '(kvs crypt)
    (should (equal (org-glance-view-action-resolve view 'materialize) '(crypt)))
    (should (equal (org-glance-view-action-resolve view 'visit) org-glance-view-default-type))
    (should (equal (org-glance-view-action-resolve view 'open) nil))
    (should (equal (org-glance-view-action-resolve view 'extract-property) '(kvs crypt))))

  (with-org-glance-view encrypted-key-value-store :as view :type '(link)
    (should (equal (org-glance-view-action-resolve view 'materialize) org-glance-view-default-type))
    (should (equal (org-glance-view-action-resolve view 'visit) org-glance-view-default-type))
    (should (equal (org-glance-view-action-resolve view 'open) '(link)))
    (should (equal (org-glance-view-action-resolve view 'extract-property) nil))))
