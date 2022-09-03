;; -*- lexical-binding: t; -*-

(require 'f)
(require 'dash)
(require 'org-glance)
(require 'org-glance-helpers)

(defvar org-glance-support-path
  (f-dirname load-file-name))

(defvar org-glance-features-path
  (f-parent org-glance-support-path))

(defvar org-glance-root-path
  (f-parent org-glance-features-path))

(add-to-list 'load-path org-glance-root-path)

(defvar org-glance-test:location)
(defvar org-glance-test-files)
(defvar org-glance-test-stores)
(defvar org-glance-test-views)
(defvar org-glance-test-headlines)

(defun org-glance-test:get-headline (alias)
  "Get headline from test storage by ALIAS."
  (gethash alias org-glance-test-headlines))

(defun org-glance-test:get-headlines (aliases)
  "Get headlines from test storage by ALIAS."
  (-map #'H (s-split ", " aliases)))

(defun org-glance-test:get-file (alias)
  "Get file from test storage by ALIAS."
  (or (gethash alias org-glance-test-files)
      (f-join org-glance-test:location alias)))

(defun org-glance-test:put-store (key val)
  "Update store KEY with value VAL."
  (puthash key val org-glance-test-stores))

(defun org-glance-test:get-store (key)
  "Get storage from test storage by ALIAS."
  (or (gethash key org-glance-test-stores)
      (error "Store \"%s\" is not registered in the system. Available stores: \"%s\""
             key
             (s-join "\", \"" (hash-table-keys org-glance-test-stores)))))

(defun org-glance-test:put-view (key val)
  "Update store KEY with value VAL."
  (puthash key val org-glance-test-views))

(defun org-glance-test:get-view (key)
  "Get storage from test storage by ALIAS."
  (or (gethash key org-glance-test-views)
      (error "Store \"%s\" is not registered in the system. Available views: \"%s\""
             key
             (s-join "\", \"" (hash-table-keys org-glance-test-views)))))

(defun org-glance-test:normalize-string (s)
  (s-trim (s-replace-regexp "[[:space:]]+" " " s)))

(cl-defun org-glance-test:changelog-contains-headline-with-title (title changelog)
  (let ((filter (lambda (event)
                  (cl-typecase event
                    (org-glance-event:RM nil)
                    ((or org-glance-event:PUT org-glance-event:UPDATE) (string= title (org-glance-> event :headline :title)))))))
    (not (null (org-glance-changelog:last (org-glance-changelog:filter changelog filter))))))

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'org-glance)
  (require 'espuds)
  (require 'ert))

(Setup  ;; Before anything has run

 )

(Before
 (desktop-clear)
 (setq org-glance-test:location (make-temp-file "org-glance-" 'directory)
       org-glance-test-files (make-hash-table :test #'equal)
       org-glance-test-stores (make-hash-table :test #'equal)
       org-glance-test-views (make-hash-table :test #'equal)
       org-glance-test-headlines (make-hash-table :test #'equal))
 (f-mkdir-full-path org-glance-test:location)
 (f-touch (f-join org-glance-test:location "ecukes.lock")))

(After
 (when (f-exists-p (f-join org-glance-test:location "ecukes.lock"))
   (delete-directory org-glance-test:location t)))

(Fail
 (message "Scenario has failed. Check out test directory for details: %s" org-glance-test:location)
 (f-delete (f-join org-glance-test:location "ecukes.lock")))

(Teardown
 (setq default-directory org-glance-root-path))
