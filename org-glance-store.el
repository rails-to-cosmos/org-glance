;; -*- lexical-binding: t; -*-

(require 'cl-macs)
(require 'org-glance-helpers)
(require 'org-glance-headline)

(cl-defstruct (org-glance-store (:constructor org-glance-store)
                                (:copier org-glance-store-copy))
  "Stores headlines."
  (headlines nil :type list))

(cl-defmethod org-glance-cardinality ((store org-glance-store))
  "Return number of headlines in STORE."
  (let ((headlines (org-glance-headlines store)))
    (cond ((null headlines) 0)
          (t (cl-loop for (_ . h) in headlines
                sum (length h))))))

(cl-defmethod org-glance-materialize ((store org-glance-store) (file string))
  "Insert STORE into the FILE and provide ability to push changes to its origins by calling `org-glance-commit'."
  (org-glance-with-file file
    (insert "#    -*- mode: org; mode: org-glance-material -*-\n\n")
    (cl-loop for (origin . headlines) in (org-glance-headlines store)
       do (if origin
              (cl-loop for headline in headlines
                 do
                   (org-glance-headline:set-org-property* headline "Hash" (org-glance-hash headline))
                   (org-glance-headline:set-org-property* headline "Origin" origin)
                   (org-glance-headline-insert headline))
            (warn "Unable to materialize headline without file origin")))))

(cl-defmethod org-glance-headlines ((store org-glance-store))
  "Retrieve headlines from STORE."
  (org-glance-store-headlines store))

(cl-defmethod org-glance-import (scope)
  "Read SCOPE recursively, extract `org-glance-headline' from
  each visited file then return new instance of `org-glance-store'."
  (cl-loop for file in scope
     collect (cons file (org-glance-loop-file file <headline>))
     into headlines
     finally return (org-glance-store :headlines headlines)))

(cl-defgeneric org-glance-store-export-headlines (store directory)
  "Save STORE headlines to DIRECTORY.
Under the hood it could apply partitioning schemas and write metadata to optimize load.

-- example
Storage partitioning schema: class/created-date/headline-id/headline.el
Archive partitioning schema: class/closed-date/headline-id/headline.el")

(provide 'org-glance-store)
