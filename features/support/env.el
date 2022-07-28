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

(defvar org-glance-test-location)
(defvar org-glance-test-files)
(defvar org-glance-test-stores)
(defvar org-glance-test-headlines)

(defun HEADLINE (alias)
  "Get headline from test storage by ALIAS."
  (gethash alias org-glance-test-headlines))

(defun HEADLINES (aliases)
  "Get headlines from test storage by ALIAS."
  (-map #'H (s-split ", " aliases)))

(defun FILE (alias)
  "Get file from test storage by ALIAS."
  (or (gethash alias org-glance-test-files)
      (f-join org-glance-test-location alias)))

(defun STORE (key)
  "Get storage from test storage by ALIAS."
  (gethash key org-glance-test-stores))

(defun STORE>> (key val)
  "Update store KEY with value VAL."
  (puthash key val org-glance-test-stores))

(defun org-glance-test:normalize-string (s)
  (s-trim (s-replace-regexp "[[:space:]]+" " " s)))

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'org-glance)
  (require 'espuds)
  (require 'ert))

(Setup  ;; Before anything has run
 ;; (setq inhibit-message t)
 )

(Before
 (desktop-clear)
 (setq org-glance-test-location (make-temp-file "org-glance-" 'directory)
       org-glance-test-files (make-hash-table :test #'equal)
       org-glance-test-stores (make-hash-table :test #'equal)
       org-glance-test-headlines (make-hash-table :test #'equal))
 (f-mkdir-full-path org-glance-test-location))

(After
 (pp org-glance-test-location)
 ;; (delete-directory org-glance-test-location t)
 )

(Teardown
 (setq default-directory org-glance-root-path))
