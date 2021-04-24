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

(Setup
 ;; Before anything has run
 (defvar org-glance-test:location)
 (setq org-glance-test:location (make-temp-file "org-glance-test-" t))
 (message "Root directory initialized: %s" org-glance-test:location))

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 (delete-directory org-glance-test:location t)
 (message "Root directory removed on teardown"))
