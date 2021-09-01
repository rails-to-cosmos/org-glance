(require 'f)

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
 (defvar org-glance-test:root-location (make-temp-file "org-glance-test-" t))
 (defvar org-glance-test:view-location (f-join org-glance-test:root-location "org-glance-views"))
 (defvar org-glance-test:user-location (f-join org-glance-test:root-location "user-data"))

 (mkdir org-glance-test:view-location)
 (mkdir org-glance-test:user-location)

 (message "Root directory initialized: %s" org-glance-test:root-location))

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 (delete-directory org-glance-test:root-location t)
 (message "Root directory removed on teardown"))
