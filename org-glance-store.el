;; -*- lexical-binding: t; -*-

(require 'cl-macs)

(require 'org-glance-helpers)
(require 'org-glance-headline)
(require 'org-glance-index)
(require 'org-glance-scope)

(cl-defstruct (org-glance-store (:constructor org-glance-store--create)
                                (:copier nil))
  "Persistent store of headlines.
Implements indexes to optimize reads.
Builds and preserves indexes in actualized state."
  (location nil  :type string :read-only t :documentation "Directory where we store all data..")
  (headlines nil :type list   :read-only t :documentation "List of headlines.")
  (i-title nil   :type list   :read-only t :documentation "Inversed index title->hash."))

(cl-defgeneric org-glance-store-headline-location (haystack needle)
  "Return location of NEEDLE in HAYSTACK.")

(cl-defmethod org-glance-store-headline-location ((store org-glance-store) (headline org-glance-headline))
  "Return HEADLINE location from STORE."
  (org-glance-store-headline-location store (org-glance-hash headline)))

(cl-defmethod org-glance-store-headline-location ((store org-glance-store) (headline org-glance-headline*))
  "Return HEADLINE location from STORE."
  (org-glance-store-headline-location store (org-glance-hash headline)))

(cl-defmethod org-glance-store-headline-location ((store org-glance-store) (hash string))
  "Return HASH location from STORE."
  (let ((prefix (substring hash 0 2))
        (postfix (substring hash 2 (length hash))))
    (f-join (org-glance-store-location store) "data" prefix postfix)))

(cl-defgeneric org-glance-store-headline-full (store headline)
  "Return fully qualified `org-glance-headline' from STORE using HEADLINE.")

(cl-defmethod org-glance-store-headline-full ((_ org-glance-store) (headline org-glance-headline))
  headline)

(cl-defmethod org-glance-store-headline-full ((store org-glance-store) (headline org-glance-headline*))
  (org-glance-headline-load (org-glance-store-headline-location store headline)))

(cl-defun org-glance-store (location)
  "Create persistent store from directory LOCATION."
  (cond ((and (f-exists-p location) (f-readable-p location))
         (org-glance-store-read location))
        ((and (f-exists-p location) (not (f-readable-p location)))
         (user-error "Store initialization failed. Location is not readable: %s" location))
        (t (mkdir location t)
           (org-glance-store--create :location location))))

(cl-defun org-glance-store-read (location)
  (unless (f-exists-p (f-join location "index" "title" "0"))
    (user-error "Unable to read %s: index not found. Maybe you want to import store from it?" location))

  (unless (f-readable-p (f-join location "index" "title" "0"))
    (user-error "Unable to read %s: index not readable. Maybe you want to import store from it?" location))

  (let ((i-title (org-glance-index-read (f-join location "index" "title" "0"))))
    (org-glance-store--create
     :i-title (org-glance-index-uniq (org-glance-index-inversed i-title))
     :location location
     :headlines (cl-loop for (hash . title) in i-title
             collect (org-glance-headline* :title title :hash hash)))))

(cl-defun org-glance-store-put (store &rest headlines)
  "Return new `org-glance-store' instance by copying STORE with HEADLINES registered in it."
  (dolist (headline headlines)
    (let ((location (org-glance-store-headline-location store headline)))
      (unless (f-exists-p location) ;; TODO or read full headline if current is dummy?
        (org-glance-save headline location))))

  (let ((i-title-old (org-glance-store-i-title store))
        (i-title-new (org-glance-index headlines :map #'org-glance-title)))
    (org-glance-index-append i-title-new (f-join (org-glance-store-location store) "index" "title" "0"))
    (org-glance-store--create
     :i-title (org-glance-index-merge
               i-title-old
               (org-glance-index-uniq (org-glance-index-inversed i-title-new)))
     :location (org-glance-store-location store)
     :headlines (append headlines (org-glance-headlines store)))))

(cl-defun org-glance-store-get (store hash)
  (car (--filter (string= (org-glance-hash it) hash) (org-glance-store-headlines store))))

(cl-defun org-glance-store-import (store loc)
  "Add headlines from location LOC to STORE."
  (let ((headlines (-flatten (-map #'org-glance-file-headlines (org-glance-scope loc)))))
    (apply #'org-glance-store-put store headlines)))

;; (cl-defun org-glance-store-choose (store)
;;   (let* ((index (org-glance-store-title-index store))
;;          (hash (gethash (completing-read "Headline: " index nil t) index)))
;;     (org-glance-headline-load (org-glance-store-headline-location store hash))))

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
  "Insert STORE headlines into the FILE and provide ability to push changes
to its origins by calling `org-glance-commit'."
  (org-glance--with-temp-file file
    (insert (format "#    -*- mode: org; mode: org-glance-material; store: \"%s\" -*-\n\n"
                    (org-glance-store-location store)))
    (cl-loop for headline in (org-glance-headlines store)
       do
         (let* ((headline (org-glance-store-headline-full store headline)))
           (-> headline
               ;; (org-glance-headline-set-org-properties "Hash" (org-glance-hash headline))
               (org-glance-headline-insert))))))

(provide 'org-glance-store)
