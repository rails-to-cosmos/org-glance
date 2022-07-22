;; -*- lexical-binding: t; -*-

(require 'cl-macs)

(require 'org-glance-helpers)
(require 'org-glance-scope)
(require 'org-glance-headline)

(cl-defstruct (org-glance-store (:constructor org-glance-store--create)
                                (:copier org-glance-store--copy))
  "Persistent store of headlines.
Implements indexes to optimize reads.
Builds and preserves indexes in actualized state."
  (location nil :type string :read-only t :documentation "Directory where store persists.")
  (headlines nil :type list :documentation "List of headlines.")
  (origins (make-hash-table) :read-only t :documentation "Headline to origin map."))

(cl-defun org-glance-store (location)
  "Create persistent store from directory LOCATION."
  ;; TODO: restore sparse indexes if exist
  (let ((origins (make-hash-table :test 'equal)))
    (cl-loop for file in (org-glance-scope location)
       append (cl-loop for headline in (org-glance-file-headlines file)
                 do (puthash (org-glance-hash headline) file origins)
                 collect headline)
       into headlines
       finally return (org-glance-store--create
                       :location location
                       :headlines headlines
                       :origins origins))))

(cl-defun org-glance-store-headline-origin (store headline)
  (gethash (org-glance-hash headline) (org-glance-store-origins store)))

(cl-defmethod org-glance-headlines ((store org-glance-store))
  "Retrieve headlines from STORE."
  (org-glance-store-headlines store))

(cl-defmethod org-glance-equal-p ((a org-glance-store) (b org-glance-store))
  "Return t if A contains same headlines as B."
  (let ((sorted-a (cl-loop for headline in (org-glance-headlines a)
                     collect headline into result
                     finally return (--sort (string< (org-glance-hash it) (org-glance-hash other)) result)))
        (sorted-b (cl-loop for headline in (org-glance-headlines b)
                     collect headline into result
                     finally return (--sort (string< (org-glance-hash it) (org-glance-hash other)) result))))
    (and (not (null sorted-a))
         (not (null sorted-b))
         (--all? (and (consp it)
                      (org-glance-equal-p (car it) (cdr it)))
                 (-zip sorted-a sorted-b)))))

(cl-defmethod org-glance-cardinality ((store org-glance-store))
  "Return number of headlines in STORE."
  (length (org-glance-headlines store)))

(cl-defmethod org-glance-materialize ((store org-glance-store) (file string))
  "Insert STORE into the FILE and provide ability to push changes
to its origins by calling `org-glance-commit'."
  (org-glance-with-file file
    (insert "#    -*- mode: org; mode: org-glance-material -*-\n\n")
    (cl-loop for headline in (org-glance-headlines store)
       do (let ((origin (org-glance-store-headline-origin store headline)))
            (org-glance-headline-insert
             (-> headline
                 (org-glance-headline-set-org-properties "Hash" (org-glance-hash headline))
                 (org-glance-headline-set-org-properties "Origin" origin)))))))

(cl-defmethod org-glance-save ((store org-glance-store) dest)
  "Save STORE to DEST.

Storage partitioning schema: class/created-date/headline-id/headline.el
Archive partitioning schema: class/closed-date/headline-id/headline.el"
  (when (f-exists? dest)
    (cond ((not (f-empty? dest)) (user-error "Destination exists and is not empty."))
          ((not (f-readable? dest)) (user-error "Destination exists and is not readable."))))

  (cl-loop for headline in (org-glance-headlines store)
     do (let* ((hash (org-glance-hash headline))
               (prefix (substring hash 0 2))
               (dir (f-join dest prefix)))
          (mkdir dir t)
          (org-glance-export headline (f-join dir (format "%s.org" (substring hash 2 (length hash))))))))

(provide 'org-glance-store)
