;; -*- lexical-binding: t; -*-

(require 'ts)
(require 'cl-macs)

(require 'org-glance-helpers)
(require 'org-glance-headline)
(require 'org-glance-scope)

(cl-defstruct (org-glance-store (:constructor org-glance-store--create)
                                (:copier nil))
  "Persistent store of headlines."
  (location nil :type string :read-only t :documentation "Directory where we store all the data.")
  (offset nil :type list :read-only t :documentation "Last committed offset.")
  (memtbl nil :type list :read-only t))

(cl-defun org-glance-store-read (location)
  "Read `org-glance-store' from LOCATION."
  (let ((wal (org-glance-store-log-read (f-join location "wa.log"))))
    (org-glance-store--create
     :location location
     :offset (cond ((f-exists-p (f-join location "offset.el"))
                    (with-temp-buffer
                      (insert-file-contents-literally (f-join location "offset.el"))
                      (read (buffer-substring (point-min) (point-max)))))
                   ((null wal) (ts-now))
                   (t (cl-destructuring-bind (ts _ _) (car (last wal))
                        ts)))
     :memtbl wal)))

(cl-defun org-glance-store-commit (store)
  "Persist STORE changes."
  (cl-loop
     with wal = (reverse (org-glance-store-memtbl store))
     for (ts instruction headline) in wal
     with last-committed-offset = (org-glance-store-offset store)
     with seen = (make-hash-table :test #'equal)
     while (ts> ts last-committed-offset)
     for hash = (org-glance-headline-hash headline)
     when (and (not (gethash hash seen)) (eq instruction 'RM))
     do (f-delete (org-glance-store-headline-location store headline))
     finally do (cl-destructuring-bind (ts _ _) (car wal)
                  (with-temp-file (f-join (org-glance-store-location store) "offset.el")
                    (insert (prin1-to-string ts))))
     finally return store))

(cl-defun org-glance-store-put-headlines (store &rest headlines)
  "Return new `org-glance-store' instance by copying STORE with HEADLINES registered in it.

Append PUT event to WAL and insert headlines to persistent storage."
  (org-glance-store--create
   :location (org-glance-store-location store)
   :offset (org-glance-store-offset store)
   :memtbl (cl-loop for headline in headlines
              for location = (org-glance-store-headline-location store headline)
              unless (f-exists-p location)
              ;; could be made in separate thread
              do (org-glance-headline-save headline location)
              ;; no need to write fully qualified headlines, write only headers
              collect (list (ts-now) 'PUT (org-glance-headline-dummy headline))
              into wal
              finally do (org-glance-store-log-append wal (f-join (org-glance-store-location store) "wa.log"))
              finally return (append (org-glance-store-memtbl store) wal))))

(cl-defun org-glance-store-remove-headlines (store &rest headlines)
  "Return `org-glance-store' with HEADLINES removed from STORE.

Append RM event to WAL, but not remove HEADLINES from persistent storage.
Actual deletion is handled in a separate thread of `org-glance-material-mode'."
  (org-glance-store--create
   :location (org-glance-store-location store)
   :offset (org-glance-store-offset store)
   :memtbl (cl-loop for headline in headlines
              collect (list (ts-now) 'RM headline)
              into wal
              finally do (org-glance-store-log-append wal (f-join (org-glance-store-location store) "wa.log"))
              finally return (append (org-glance-store-memtbl store) wal))))

(cl-defun org-glance-store-remove-headline-by-hash (store hash)
  (org-glance-store-remove-headlines store (org-glance-headline-header :-hash hash)))

(cl-defun org-glance-store-get-headline-by-hash (store hash)
  "Return `org-glance-headline-header' from STORE searched by HASH."
  (cl-loop for (_ instruction headline) in (reverse (org-glance-store-memtbl store))
     when (string= hash (org-glance-headline-hash headline))
     return (cl-case instruction
              ('PUT headline)
              ('RM nil))))

(cl-defun org-glance-store-get-headline-by-title (store title)
  "Return `org-glance-headline-header' from STORE searched by TITLE."
  (cl-loop for (_ instruction headline) in (reverse (org-glance-store-memtbl store))
     for hash = (org-glance-headline-hash headline)
     with seen = (make-hash-table :test #'equal)
     if (and (string= title (org-glance-headline-title headline))
             (not (gethash hash seen))
             (eq instruction 'PUT))
     return headline
     else if (and (not (gethash hash seen))
                  (eq instruction 'RM))
     do (puthash hash t seen)))

(cl-defun org-glance-store-log-read (location)
  (when (and (f-exists-p location) (f-readable-p location))
    (with-temp-buffer
      (insert-file-contents-literally location)
      (goto-char (point-min))
      (cl-loop while (not (eobp))
         collect (read (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         do (forward-line)))))

(cl-defun org-glance-store-log-append (log location)
  (cl-loop for entry in log
     collect (prin1-to-string entry)
     into contents
     finally do (cond ((and (f-exists-p location) (f-empty-p location))
                       (append-to-file (s-join "\n" contents) nil location))
                      ((f-exists-p location)
                       (append-to-file (concat "\n" (s-join "\n" contents)) nil location))
                      (t (org-glance--with-temp-file location
                           (insert (s-join "\n" contents)))))))

(cl-defun org-glance-store-headlines (store)
  "Return all headlines from STORE."
  (cl-loop for (_ instruction headline) in (reverse (org-glance-store-memtbl store))
     for hash = (org-glance-headline-hash headline)
     with seen = (make-hash-table :test #'equal)
     when (and (eq instruction 'PUT) (not (gethash hash seen)))
     collect (puthash hash hash seen)
     when (and (eq instruction 'RM) (not (gethash hash seen)))
     do (puthash hash hash seen)))

(cl-defgeneric org-glance-store-headline-location (store headline)
  "Return location of HEADLINE in STORE.")

(cl-defmethod org-glance-store-headline-location ((store org-glance-store) (headline org-glance-headline))
  "Return HEADLINE location from STORE."
  (org-glance-store-headline-location store (org-glance-headline-hash headline)))

(cl-defmethod org-glance-store-headline-location ((store org-glance-store) (headline org-glance-headline-header))
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

(cl-defmethod org-glance-store-headline ((store org-glance-store) (headline org-glance-headline-header))
  (org-glance-headline-load (org-glance-store-headline-location store headline)))

(cl-defmethod org-glance-store-headline ((store org-glance-store) (hash string))
  (org-glance-headline-load (org-glance-store-headline-location store hash)))

(cl-defun org-glance-store-from-scratch (location &rest headlines-as-a-strings)
  (declare (indent 1))
  (let ((store (org-glance-store location)))
    (cl-loop for headline-string in headlines-as-a-strings
       collect (org-glance-headline-from-string headline-string)
       into headlines
       finally return (apply #'org-glance-store-put-headlines store headlines))))

(cl-defun org-glance-store (location)
  "Create persistent store from directory LOCATION."
  (cond ((and (f-exists-p location) (f-readable-p location))
         (org-glance-store-read location))
        ((and (f-exists-p location) (not (f-readable-p location)))
         (user-error "Store initialization failed. Location is not readable: %s" location))
        (t (mkdir location t)
           (org-glance-store--create :location location))))

;; (cl-defun org-glance-store-completing-read (store)
;;   "Read headlines from STORE with completion."
;;   (let* ((titles (org-glance-store-i-title* store))
;;          (hash (alist-get (completing-read "Headline: " titles nil t) titles nil nil #'string=)))
;;     (org-glance-store-headline store hash)))

(cl-defun org-glance-store-import (store loc)
  "Add headlines from location LOC to STORE."
  (let ((headlines (-flatten (-map #'org-glance-file-headlines (org-glance-scope loc)))))
    (apply #'org-glance-store-put-headlines store headlines)))

(cl-defun org-glance-store-equal-p (a b)
  "Return t if A contains same headlines as B.

TODO should be optimized to not read all headlines from store."
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
  "Return number of headlines in STORE.

TODO should be optimized to not read all headlines from store."
  (length (org-glance-store-headlines store)))

(provide 'org-glance-store)
