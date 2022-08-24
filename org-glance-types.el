;; -*- lexical-binding: t; -*-

(require 'f)
(require 'cl-macs)

(cl-deftype org-glance-file () '(satisfies org-glance-file-p))
(cl-deftype org-glance-directory () '(satisfies org-glance-directory-p))

(cl-defun org-glance-file-p (location)
  "Determine if LOCATION is a readable org-mode file."
  (or (not (f-exists-p location))
      (and (f-readable-p location)
           (f-ext-p location "org"))))

(cl-defun org-glance-directory-p (location)
  "Determine if LOCATION is a readable directory."
  (and (f-dir-p location)
       (f-readable-p location)))

(provide 'org-glance-types)
