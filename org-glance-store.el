;; -*- lexical-binding: t; -*-

(require 'cl-macs)
(require 'org-glance-helpers)
(require 'org-glance-headline)

(cl-defstruct (org-glance-store (:constructor org-glance-store-create)
                                (:copier org-glance-store-copy))
  "Stores headlines."
  (headlines nil :type list))

(cl-defmethod org-glance-cardinality ((store org-glance-store))
  "Return number of headlines in STORE."
  (let ((headlines (org-glance-store-headlines store)))
    (cond ((null headlines) 0)
          (t (length headlines)))))

(cl-defmethod org-glance-materialize ((store org-glance-store) (file string))
  "Insert STORE's headlines into the FILE and provide ability to push changes to its origins by calling `org-glance-commit'."
  (org-glance-with-file file
    (insert "#    -*- mode: org; mode: org-glance-material -*-\n\n")
    (--map (if-let (file (org-glance-headline-origin it))
               (progn
                 (org-glance-headline:set-org-property* it "Hash" (org-glance-headline-hash it))
                 (org-glance-headline:set-org-property* it "Origin" (org-glance-headline-origin it))
                 (org-glance-headline-insert it))
             (warn "Unable to materialize headline without file origin"))
           (org-glance-store-headlines store))))

(cl-defmethod org-glance-headlines ((store org-glance-store))
  "Retrieve headlines from STORE."
  (org-glance-store-headlines store))

(cl-defmethod org-glance-import (scope)
  "Read SCOPE recursively, extract `org-glance-headline' from
  each visited file then return new instance of `org-glance-store'."
  (cl-loop for file in scope
     append (org-glance-loop-file file <headline>)
     into headlines
     finally return (org-glance-store-create :headlines headlines)))

(cl-defgeneric org-glance-store-export-headlines (store directory)
  "Save STORE headlines to DIRECTORY.
Under the hood it could apply partitioning schemas and write metadata to optimize load.

-- example
Storage partitioning schema: class/created-date/headline-id/headline.el
Archive partitioning schema: class/closed-date/headline-id/headline.el")

(provide 'org-glance-store)
