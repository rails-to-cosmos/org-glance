(require 'org-glance-views)

(ert-deftest org-glance-test/resolve-ordinary-view ()
  (org-glance-let ordinary-view :as view
    (should (equal (org-glance-view-action-resolve view 'materialize) org-glance-view-default-type))
    (should (equal (org-glance-view-action-resolve view 'visit) org-glance-view-default-type))
    (should (equal (org-glance-view-action-resolve view 'open) nil))
    (should (equal (org-glance-view-action-resolve view 'extract-property) nil))))

(ert-deftest org-glance-test/resolve-key-value-store ()
  (org-glance-let key-value-store :as view :type '(kvs)
    (should (equal (org-glance-view-action-resolve view 'materialize) org-glance-view-default-type))
    (should (equal (org-glance-view-action-resolve view 'visit) org-glance-view-default-type))
    (should (equal (org-glance-view-action-resolve view 'open) nil))
    (should (equal (org-glance-view-action-resolve view 'extract-property) '(kvs)))))

(ert-deftest org-glance-test/resolve-encrypted-key-value-store ()
  (org-glance-let encrypted-key-value-store :as view :type '(kvs crypt)
    (should (equal (org-glance-view-action-resolve view 'materialize) '(crypt)))
    (should (equal (org-glance-view-action-resolve view 'visit) org-glance-view-default-type))
    (should (equal (org-glance-view-action-resolve view 'open) nil))
    (should (equal (org-glance-view-action-resolve view 'extract-property) '(kvs crypt)))))

(ert-deftest org-glance-test/resolve-link ()
  (org-glance-let link-store :as view :type '(link)
    (should (equal (org-glance-view-action-resolve view 'materialize) org-glance-view-default-type))
    (should (equal (org-glance-view-action-resolve view 'visit) org-glance-view-default-type))
    (should (equal (org-glance-view-action-resolve view 'open) '(link)))
    (should (equal (org-glance-view-action-resolve view 'extract-property) nil))))
