(require 'f)
(require 'org-glance)

(defvar org-glance-support-path
  (f-dirname load-file-name))

(defvar org-glance-features-path
  (f-parent org-glance-support-path))

(defvar org-glance-root-path
  (f-parent org-glance-features-path))

(add-to-list 'load-path org-glance-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'org-glance)
  (require 'espuds)
  (require 'ert))

(Setup  ;; Before anything has run

 ;; Implement directory structure for user and system data
 (defvar org-glance-test--root-location)
 (defvar org-glance-test--view-location)
 (defvar org-glance-test--user-location)
 (defvar org-glance-test--files)
 (defvar org-glance-test--headlines)
 (defvar org-glance-test--registries))

(Before
 ;; Before each scenario is run

 (desktop-clear)
 (setq org-glance-test--root-location (make-temp-file "org-glance-" 'directory)
       org-glance-test--view-location (f-join org-glance-test--root-location "org-glance")
       org-glance-test--user-location (f-join org-glance-test--root-location "user-data")
       org-glance-directory org-glance-test--view-location
       org-glance:log-level -2305843009213693952
       org-glance-classes (make-hash-table)
       org-glance-test--files (make-hash-table :test #'equal)
       org-glance-test--headlines (make-hash-table :test #'equal)
       org-glance-test--registries (make-hash-table :test #'equal))

 (mkdir org-glance-test--view-location)
 (mkdir org-glance-test--user-location))

(After
 (delete-directory org-glance-test--root-location t))

(Teardown
 (message "** Teardown"))
