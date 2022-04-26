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
 (defvar ecukes--root-location)
 (defvar ecukes--view-location)
 (defvar ecukes--user-location)
 (defvar ecukes--files)
 (defvar ecukes--headlines)
 (defvar ecukes--registries)

 (defun H (alias) (gethash alias ecukes--headlines))
 (defun R (alias) (gethash alias ecukes--registries)))

(Before
 ;; Before each scenario is run

 (desktop-clear)
 (setq ecukes--root-location (make-temp-file "org-glance-" 'directory)
       ecukes--view-location (f-join ecukes--root-location "org-glance")
       ecukes--user-location (f-join ecukes--root-location "user-data")
       org-glance-directory ecukes--view-location
       org-glance:log-level -2305843009213693952
       org-glance-classes (make-hash-table)
       ecukes--files (make-hash-table :test #'equal)
       ecukes--headlines (make-hash-table :test #'equal)
       ecukes--registries (make-hash-table :test #'equal))

 (mkdir ecukes--view-location)
 (mkdir ecukes--user-location))

(After
 (delete-directory ecukes--root-location t))

(Teardown
 )
