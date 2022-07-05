(require 'f)
(require 'dash)
(require 'org-glance)

(defvar org-glance-support-path
  (f-dirname load-file-name))

(defvar org-glance-features-path
  (f-parent org-glance-support-path))

(defvar org-glance-root-path
  (f-parent org-glance-features-path))

(add-to-list 'load-path org-glance-root-path)

(defvar org-glance-test:root-location)
(defvar org-glance-test:temp-location)
(defvar org-glance-test:view-location)
(defvar org-glance-test:user-location)
(defvar org-glance-test:files)
(defvar org-glance-test:headlines)

(defun H (alias) (gethash alias org-glance-test:headlines))
(defun HS (aliases) (-map #'H (s-split ", " aliases)))
(defun F (alias) (gethash alias org-glance-test:files))
(defun org-glance-test:normalize-string (s)
  (s-trim (s-replace-regexp "[[:space:]]+" " " s)))

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'org-glance)
  (require 'espuds)
  (require 'ert))

(Setup  ;; Before anything has run
 (setq inhibit-message t))

(Before
 (desktop-clear)

 (setq org-glance-test:root-location (make-temp-file "org-glance-" 'directory)
       org-glance-test:temp-location (f-join org-glance-test:root-location "tmp")
       org-glance-test:view-location (f-join org-glance-test:root-location "org-glance")
       org-glance-test:user-location (f-join org-glance-test:root-location "user-data")
       org-glance-directory org-glance-test:view-location
       org-glance-test:files (make-hash-table :test #'equal)
       org-glance-test:headlines (make-hash-table :test #'equal))

 (f-mkdir-full-path org-glance-test:temp-location)
 (f-mkdir-full-path org-glance-test:view-location)
 (f-mkdir-full-path org-glance-test:user-location)
 ;; (message "Root location: %s" org-glance-test:root-location)
 )

(After
 (delete-directory org-glance-test:root-location t))

(Teardown
 (setq default-directory org-glance-root-path))
