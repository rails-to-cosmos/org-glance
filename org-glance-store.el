;; -*- lexical-binding: t; -*-

(require 'a)

(require 'f)
(declare-function f-mkdir-full-path 'f)

(require 'ts)
(require 'cl-macs)
(require 'cl-lib)
(require 'eieio)

;; Experimental
(require 'thunk)

(require 'org-glance-helpers)
(require 'org-glance-types)
(require 'org-glance-headline)
(require 'org-glance-scope)
(require 'org-glance-changelog)
(require 'org-glance-event)
(require 'org-glance-view)

(defconst org-glance-store:log-location "WAL")

(cl-deftype org-glance-store-log ()
  '(satisfies org-glance-store-log-p))

(cl-defun org-glance-store-log-p (object)
  (and (org-glance-changelog-p object)
       (cl-every #'org-glance-event-p (org-glance-> object :events))))

(org-glance-class org-glance-store nil
  ((location
    :type org-glance-directory
    :initarg :location
    :documentation "Directory containing all the data.")
   (changelog*
    :type org-glance-store-log
    :initarg :changelog*
    :initform (org-glance-changelog)
    :documentation "In-memory changelog.")
   (changelog
    :type org-glance-store-log
    :initarg :changelog
    :initform (org-glance-changelog)
    :documentation "Persistent changelog.")
   (cache
    :type hash-table
    :initarg :cache
    :initform (make-hash-table :test #'equal)
    :documentation "LRU cache with headlines.")
   (views
    :type hash-table
    :initarg :views
    :initform (make-hash-table :test #'equal)
    :documentation "Views associated with store by type."
    :reader org-glance-store:views)))

(defvar org-glance-stores (make-hash-table :test #'equal)
  "List of stores registered in system.")

(cl-defun org-glance-store:view (store type)
  (or (gethash type (org-glance-> store :views))
      (let ((view (org-glance-view :store store :type type)))
        (puthash type view (org-glance-> store :views))
        view)))

(cl-defun org-glance-store:from-scratch (location &rest strings)
  "Creates store from LOCATION, puts headlines in it and flushes it on disk.

It destructs store in `org-glance-stores' hashtable.

All changes are stored in memory before you call `org-glance-store:flush' explicitly."
  (declare (indent 1))
  (f-mkdir-full-path location)
  (let ((store (org-glance-store :location location)))
    (dolist (string strings)
      (org-glance-store:put store (org-glance-headline-from-string string)))
    (org-glance-store:flush store)
    (puthash location store org-glance-stores)
    store))

(cl-defun org-glance-store:read (location)
  "Read `org-glance-store' from LOCATION."
  (or (gethash location org-glance-stores)
      (let ((changelog (org-glance-changelog:read (f-join location org-glance-store:log-location))))
        (puthash location (org-glance-store :location location :changelog changelog) org-glance-stores))))

(cl-defun org-glance-store:offset (store)
  (if-let (event (org-glance-changelog:last (org-glance-> store :changelog)))
      (org-glance-> event :offset)
    0))

(cl-defun org-glance-store:/ (store location)
  "Resolve relative LOCATION to full path in context of STORE."
  (apply #'f-join (org-glance-> store :location) (s-split "/" location)))

(cl-defun org-glance-store:flush (store)
  "Persist STORE changes.

- Persist event log.
- Apply PUT operations.
- TODO Apply RM operations.
- ...

This should be the only point to destructively change underlying
persistent storage.

In all other places `org-glance-store' should act like pure
functional data structure.

Return last committed offset."
  (dolist (event (reverse (org-glance-> store :changelog* :events)))
    (cl-typecase event
      (org-glance-event:RM
       (org-glance-changelog:push (org-glance-> store :changelog) event)
       ;; TODO think about when to delete headlines
       ;; (f-delete (org-glance-store:locate store headline))
       )
      (org-glance-event:PUT
       (let* ((headline (org-glance-store:get store (org-glance-event:id event)))
              (location (org-glance-store:locate store headline)))
         (unless (f-exists-p location)
           (org-glance-headline-save headline location)
           (org-glance-changelog:push (org-glance-> store :changelog) event))))))

  (org-glance-changelog:write (org-glance-> store :changelog)
                              (org-glance-store:/ store org-glance-store:log-location))
  (setf (org-glance-> store :changelog*)
        (org-glance-changelog))
  (org-glance-> (org-glance-changelog:last (org-glance-> store :changelog)) :offset))

;; TODO `org-glance-changelog' filter now filter events instead of headlines

(cl-defun org-glance-store:put (store headline)
  "Put HEADLINE to STORE.
TODO: Transaction."
  (let ((event (org-glance-event:PUT :headline (org-glance-headline-header:from-headline headline))))
    (org-glance-changelog:push (org-glance-> store :changelog*) event)
    (puthash (org-glance-> headline :hash) headline (org-glance-> store :cache))
    (org-glance-> event :offset)))

(cl-defun org-glance-store:remove (store hash)
  "Return `org-glance-store' with HEADLINES removed from STORE.

Append RM event to WAL, but do not remove HEADLINES from the
persistent storage.

Actual deletion should be handled in a separate thread and
achieved by calling `org-glance-store:flush' method."
  (let ((event (org-glance-event:RM :hash hash)))
    (org-glance-changelog:push (org-glance-> store :changelog*) event)
    (remhash hash (org-glance-> store :cache))))

(cl-defun org-glance-store:get (store hash)
  "Return fully qualified `org-glance-headline' by hash.

1. (TODO) Search in LRU cache.
2. Search in staged changes.
3. Search in persistent storage."
  (or (gethash hash (org-glance-> store :cache))

      (let ((changelog (org-glance-> store :changelog*)))
        (cl-loop for event in (org-glance-changelog:flatten changelog)
           when (and (org-glance-event:RM-p event) (string= hash (org-glance-> event :hash)))
           return nil  ;; headline deleted
           when (and (org-glance-event:PUT-p event) (string= hash (org-glance-> event :headline :hash)))
           return (org-glance-> event :headline)))

      (org-glance:with-temp-buffer
       (message "Locating %s..." (org-glance-store:locate store hash))
       (insert-file-contents (org-glance-store:locate store hash))
       (goto-char (point-min))
       (unless (org-at-heading-p)
         (outline-next-heading))
       (let ((headline (org-glance-headline-at-point)))
         (puthash (org-glance-> headline :hash)
                  headline
                  (org-glance-> store :cache))))))

(cl-defun org-glance-store:in (store hash)
  "Return t if HASH is in STORE, nil otherwise."
  (or (not (null (gethash hash (org-glance-> store :cache) nil)))

      (let ((changelog (org-glance-> store :changelog*)))
        (cl-loop for event in (org-glance-changelog:flatten changelog)
           when (and (org-glance-event:RM-p event) (string= hash (org-glance-> event :hash)))
           return nil ;; headline deleted
           when (and (org-glance-event:PUT-p event) (string= hash (org-glance-> event :headline :hash)))
           return t))

      (and (f-exists-p (org-glance-store:locate store hash))
           (f-readable-p (org-glance-store:locate store hash)))))

(cl-defun org-glance-store:headlines (store)
  "Return actual headline hashes from STORE."
  (cl-loop
     with seen = (make-hash-table :test #'equal)
     for event in (org-glance-store:events store)
     when (and (org-glance-event:PUT-p event) (not (gethash (org-glance-> event :headline :hash) seen)))
     collect (org-glance-> event :headline)
     when (org-glance-event:RM-p event)
     do (puthash (org-glance-> event :hash) t seen)))

(cl-defgeneric org-glance-store:locate (store headline-or-hash)
  "Return location of HEADLINE in STORE.")

(cl-defmethod org-glance-store:locate ((store org-glance-store) (hash string))
  "Return HASH location from STORE."
  (let ((prefix (substring hash 0 2))
        (postfix (substring hash 2 (length hash))))
    (f-join (org-glance-> store :location) "data" prefix postfix)))

(cl-defmethod org-glance-store:locate ((store org-glance-store) (headline org-glance-headline))
  "Return HEADLINE location from STORE."
  (org-glance-store:locate store (org-glance-> headline :hash)))

(cl-defmethod org-glance-store:locate ((store org-glance-store) (headline org-glance-headline-header))
  "Return HEADLINE location from STORE."
  (org-glance-store:locate store (org-glance-> headline :hash)))

;; (cl-defun org-glance-store-completing-read (store)
;;   "Read headlines from STORE with completion."
;;   (let* ((title->hash (cl-loop for event in (org-glance-store:events store)
;;                          collect (cons (org-glance-> event :headline :title)
;;                                        (org-glance-headline:hash (org-glance-event-state event)))))
;;          (hash (alist-get (completing-read "Headline: " title->hash nil t) title->hash nil nil #'string=)))
;;     (org-glance-store:retrieve store hash)))

(cl-defun org-glance-store:events (store)
  (org-glance-changelog:flatten
   (org-glance-changelog:merge
    (org-glance-> store :changelog*)
    (org-glance-> store :changelog))))

(cl-defun org-glance-store:import (store location)
  "Add headlines from LOCATION to STORE."
  (dolist (file (org-glance-scope location))
    (org-glance:with-temp-buffer
     (insert-file-contents file)
     (org-glance-headline:map-buffer (headline)
       (org-glance-store:put store headline)))))

(provide 'org-glance-store)
