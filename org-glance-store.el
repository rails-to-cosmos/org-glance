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
  (org-glance-store-headline-location store (org-glance-headline-hash headline)))

(cl-defmethod org-glance-store-headline-location ((store org-glance-store) (headline org-glance-headline*))
  "Return HEADLINE location from STORE."
  (org-glance-store-headline-location store (org-glance-headline-hash headline)))

(cl-defmethod org-glance-store-headline-location ((store org-glance-store) (hash string))
  "Return HASH location from STORE."
  (let ((prefix (substring hash 0 2))
        (postfix (substring hash 2 (length hash))))
    (f-join (org-glance-store-location store) "data" prefix postfix)))

(cl-defgeneric org-glance-store-headline (store headline)
  "Return fully qualified `org-glance-headline' from STORE using HEADLINE.")

(cl-defmethod org-glance-store-headline ((_ org-glance-store) (headline org-glance-headline))
  headline)

(cl-defmethod org-glance-store-headline ((store org-glance-store) (headline org-glance-headline*))
  (org-glance-headline-load (org-glance-store-headline-location store headline)))

(cl-defmethod org-glance-store-headline ((store org-glance-store) (hash string))
  (org-glance-headline-load (org-glance-store-headline-location store hash)))

(cl-defun org-glance-store (location)
  "Create persistent store from directory LOCATION."
  (cond ((and (f-exists-p location) (f-readable-p location))
         (org-glance-store-read location))
        ((and (f-exists-p location) (not (f-readable-p location)))
         (user-error "Store initialization failed. Location is not readable: %s" location))
        (t (mkdir location t)
           (org-glance-store--create :location location))))

(cl-defun org-glance-store-read (location)
  (let ((i-title (when (and (f-exists-p (f-join location "index" "title" "0"))
                            (f-readable-p (f-join location "index" "title" "0")))
                   (org-glance-index-read (f-join location "index" "title" "0")))))
    (org-glance-store--create
     :i-title (org-glance-index-uniq (org-glance-index-inversed i-title))
     :location location
     :headlines (cl-loop for (hash . title) in i-title
                   collect (org-glance-headline* :-title title
                                                 :-hash hash)))))

(cl-defun org-glance-store-put (store &rest headlines)
  "Return new `org-glance-store' instance by copying STORE with HEADLINES registered in it."
  (dolist (headline headlines)
    (let ((location (org-glance-store-headline-location store headline)))
      (unless (f-exists-p location) ;; TODO or read full headline if current is dummy?
        (org-glance-headline-save headline location))))

  (let ((i-title-old (org-glance-store-i-title store))
        (i-title-new (org-glance-index headlines :map #'org-glance-headline-title)))
    (org-glance-index-append i-title-new (f-join (org-glance-store-location store) "index" "title" "0"))
    (org-glance-store--create
     :i-title (org-glance-index-merge
               i-title-old
               (org-glance-index-uniq (org-glance-index-inversed i-title-new)))
     :location (org-glance-store-location store)
     :headlines (append headlines (org-glance-store-headlines store)))))

(cl-defun org-glance-store-get (store hash)
  "TODO currently is O(n), could be optimized with hash/tree ds."
  (car (--filter (string= (org-glance-headline-hash it) hash) (org-glance-store-headlines store))))

(cl-defun org-glance-store-rem (store hash)
  "TODO currently is O(n), could be optimized with hash/tree ds."
  (message "Remove \"%s\"" (org-glance-store-headline-location store hash))
  store)

(cl-defun org-glance-store-import (store loc)
  "Add headlines from location LOC to STORE."
  (let ((headlines (-flatten (-map #'org-glance-file-headlines (org-glance-scope loc)))))
    (apply #'org-glance-store-put store headlines)))

(cl-defun org-glance-store-choose (store)
  (let* ((titles (org-glance-store-i-title store))
         (hash (alist-get (completing-read "Headline: " titles nil t) titles nil nil #'string=)))
    (org-glance-store-headline store hash)))

(cl-defun org-glance-store-equal-p (a b)
  "Return t if A contains same headlines as B."
  (let ((sorted-a (cl-loop for headline in (org-glance-store-headlines a)
                     collect headline into result
                     finally return (--sort (string< (org-glance-headline-hash it) (org-glance-headline-hash other)) result)))
        (sorted-b (cl-loop for headline in (org-glance-store-headlines b)
                     collect headline into result
                     finally return (--sort (string< (org-glance-headline-hash it) (org-glance-headline-hash other)) result))))
    (and (not (null sorted-a))
         (not (null sorted-b))
         (--all? (and (consp it)
                      (org-glance-headline-equal-p (car it) (cdr it)))
                 (-zip sorted-a sorted-b)))))

(cl-defun org-glance-store-cardinality (store)
  "Return number of headlines in STORE."
  (length (org-glance-store-headlines store)))

(provide 'org-glance-store)
