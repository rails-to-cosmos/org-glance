;; -*- lexical-binding: t; -*-

(require 'cl-macs)
(require 'org-glance-helpers)
(require 'org-glance-headline)

(cl-defstruct (org-glance-store (:constructor org-glance-store)
                                (:copier org-glance-store-copy))
  "Stores headlines."
  (headlines nil :type list))

(cl-defmethod org-glance-equal-p ((a org-glance-store) (b org-glance-store))
  "Return t if A contains same headlines as store B."
  (let ((sorted-a (cl-loop for (_ . headlines) in (org-glance-headlines a)
                     append headlines into result
                     finally return (--sort (string< (org-glance-hash it) (org-glance-hash other)) result)))
        (sorted-b (cl-loop for (_ . headlines) in (org-glance-headlines b)
                     append headlines into result
                     finally return (--sort (string< (org-glance-hash it) (org-glance-hash other)) result))))
    (and
     (not (null sorted-a))
     (not (null sorted-b))
     (--all? (and (consp it)
                  (org-glance-equal-p (car it) (cdr it)))
             (-zip sorted-a sorted-b)))))

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
                 do (org-glance-headline:set-org-property* headline "Hash" (org-glance-hash headline))
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

(cl-defmethod org-glance-export ((store org-glance-store) dest)
  "Export STORE to DEST.

-- Example
Storage partitioning schema: class/created-date/headline-id/headline.el
Archive partitioning schema: class/closed-date/headline-id/headline.el"
  (cond ((and (f-exists? dest) (not (f-empty? dest))) (user-error "Destination exists and not empty."))
        ((and (f-exists? dest) (not (f-readable? dest))) (user-error "Destination exists and not readable.")))
  (cl-loop for (_ . headlines) in (org-glance-headlines store)
     do (cl-loop for headline in headlines
           do (let* ((hash (org-glance-hash headline))
                     (prefix (substring hash 0 2))
                     (dir (f-join dest prefix)))
                (mkdir dir t)
                (org-glance-export headline (f-join dir (format "%s.org" (substring hash 2 (length hash)))))))))

(provide 'org-glance-store)
