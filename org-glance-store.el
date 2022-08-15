;; -*- lexical-binding: t; -*-

(require 'a)
(require 'f)
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

(defconst org-glance-store-log-location "wa.log")
(defconst org-glance-store-materializations-filename "materializations.el")

(cl-deftype org-glance-store-event ()
  '(satisfies org-glance-store-event-p))

(cl-deftype org-glance-store-log ()
  '(satisfies org-glance-store-log-p))

(cl-defun org-glance-store-event-p (event)
  (and (org-glance-event-p event)
       (org-glance-headline-header-p (org-glance-event-state event))))

(cl-defun org-glance-store-log-p (log)
  (and (org-glance-changelog-p log)
       (cl-every #'org-glance-store-event-p (org-glance-changelog-events log))
       (eq #'org-glance-store:event-id (org-glance-changelog-id-determinator log))))

(cl-defun org-glance-store:event-id (event)
  (org-glance-headline:hash (org-glance-event-state event)))

(defmacro eieio-declare-slot (name)
  "Declares slot to be available at runtime."
  `(eval-when-compile (cl-pushnew ,name eieio--known-slot-names)))

(defclass org-glance-store nil
  ((location
    :type org-glance-directory
    :initarg :location
    :documentation "Directory containing all the data."
    :reader org-glance-store:location)
   (changelog*
    :type org-glance-store-log
    :initarg :changelog*
    :initform (org-glance-changelog:create nil #'org-glance-store:event-id)
    :documentation "In-memory changelog."
    :reader org-glance-store:changelog*)
   (changelog
    :type org-glance-store-log
    :initarg :changelog
    :initform (org-glance-changelog:create nil #'org-glance-store:event-id)
    :documentation "Persisted changelog."
    :reader org-glance-store:changelog)
   (cache
    :type hash-table
    :initarg :cache
    :initform (make-hash-table :test #'equal)
    :documentation "Changelog persisted on disk."
    :reader org-glance-store:cache)
   (views
    :type list
    :initarg :views
    :initform nil
    :documentation "List of views associated with store."
    :reader org-glance-store:views)))

;; Avoid warnings
(eieio-declare-slots :location)
(eieio-declare-slots :materializations)
(eieio-declare-slots :store)
(eieio-declare-slots :view)
(eieio-declare-slots :views)

(declare-function f-mkdir-full-path 'f)

(cl-deftype org-glance-materialization-list ()
  '(satisfies org-glance-materialization-list-p))

(cl-defun org-glance-materialization-list-p (list)
  (cl-every #'org-glance-materialization-p list))

(defclass org-glance-view nil
  ((store
    :type org-glance-store
    :initarg :store
    :reader org-glance-view:store
    :documentation "Original `org-glance-store' instance.")
   (predicate
    :type function
    :initarg :predicate
    :reader org-glance-view:predicate
    :documentation "Predicate that takes one argument of type
    `org-glance-headline' and is guaranteed to be `t' for each
    headline per `org-glance-view' instance.")
   (materializations
    :type org-glance-materialization-list
    :initarg :materializations
    :initform nil
    :documentation "Keep track of materializations."
    :reader org-glance-view:materializations)))

(defclass org-glance-materialization nil
  ((view
    :type org-glance-view
    :initarg :view
    :reader org-glance-materialization:view
    :documentation "View that is represented by the materialization.")
   (location
    :type org-glance-file
    :initarg :location
    :reader org-glance-materialization:location
    :documentation "Location where materialization persists.")))

(defmacro org-glance-> (object &rest slots)
  (cl-reduce (lambda (acc slot) `(slot-value ,acc (quote ,slot)))
             slots
             :initial-value object))

(cl-defmethod org-glance-materialization:header ((materialization org-glance-materialization))
  "Generate header for MATERIALIZATION."
  (format
   "#  -*- mode: org; mode: org-glance-material -*-

#+ORIGIN: %s:0.0

"
   (org-glance-> materialization :view :store :location)))

(cl-defmethod org-glance-view:materialize ((view org-glance-view) (location string))
  (f-mkdir-full-path (file-name-directory location))
  (let ((materialization (org-glance-materialization :view view :location location)))
    (org-glance--with-temp-file location
      (insert (org-glance-materialization:header materialization))
      (cl-dolist (headline (org-glance-store:headlines (org-glance-view:store view)))
        (when (funcall (org-glance-view:predicate view) headline)
          (org-glance-headline-insert
           (org-glance-store:get
            (org-glance-view:store view)
            (org-glance-headline:hash headline))))))
    (cl-pushnew materialization (slot-value view :materializations))))

(cl-defun org-glance-store:view (store predicate)
  (let ((view (org-glance-view :store store :predicate predicate)))
    (cl-pushnew view (slot-value store :views))
    view))

(cl-defun org-glance-store:from-scratch (location &rest strings)
  "Creates store from LOCATION, puts headlines in it and flushes it on disk.

All changes are stored in memory before you call `org-glance-store:flush' explicitly."
  (declare (indent 1))
  (f-mkdir-full-path location)
  (let ((store (org-glance-store :location location)))
    (dolist (string strings)
      (org-glance-store:put store (org-glance-headline-from-string string)))
    (org-glance-store:flush store)
    store))

(cl-defun org-glance-store:read (location)
  "Read `org-glance-store' from LOCATION."
  (org-glance-store
   :location location
   :changelog (org-glance-changelog:read
               (f-join location org-glance-store-log-location)
               #'org-glance-store:event-id)))

(cl-defun org-glance-store:/ (store location)
  "Resolve relative LOCATION to full path in context of STORE."
  (apply #'f-join (org-glance-store:location store) (s-split "/" location)))

(cl-defun org-glance-store:flush (store)
  "Persist STORE changes.

- Persist event log.
- Apply PUT operations.
- TODO Apply RM operations.
- ...

This should be the only point to destructively change underlying
persistent storage.

In all other places `org-glance-store' should act like pure
functional data structure."
  (org-glance-changelog:write (org-glance-store:changelog* store)
                        (org-glance-store:/ store org-glance-store-log-location))
  (while (not (org-glance-changelog:empty-p (org-glance-store:changelog* store)))
    (let ((event (org-glance-changelog:pop (org-glance-store:changelog* store))))
      (cl-typecase event
        (org-glance-event:RM
         ;; if (org-glance-event:RM-p event)
         ;; do (puthash seen hash)
         ;; TODO think about when to delete headlines
         ;; (f-delete (org-glance-store:locate store headline))
         nil)
        (org-glance-event:PUT
         (let* ((headline (org-glance-store:get store (org-glance-store:event-id event)))
                (location (org-glance-store:locate store headline)))
           (unless (f-exists-p location)
             (org-glance-headline-save headline location)
             (org-glance-changelog:push (org-glance-store:changelog store) event))))))))

;; TODO `org-glance-changelog' filter now filter events instead of headlines

(cl-defun org-glance-store:filter (store func)
  "Return new `org-glance-store' instance by copying STORE with HEADLINES filtered by FUNC."
  (let ((filter (lambda (event)
                  (and
                   (org-glance-event:PUT-p event)
                   (funcall func (org-glance-event-state event))))))
    (org-glance-store
     :location (org-glance-store:location store)
     :changelog* (org-glance-changelog:filter (org-glance-store:changelog* store) filter)
     :changelog (org-glance-changelog:filter (org-glance-store:changelog store) filter))))

(cl-defun org-glance-store:put (store headline)
  "Put HEADLINE to STORE.
TODO: Transaction."
  (org-glance-changelog:push (org-glance-store:changelog* store)
                             (org-glance-event:PUT (org-glance-headline-header headline)))
  (puthash (org-glance-headline:hash headline) headline (org-glance-store:cache store)))

(cl-defun org-glance-store:remove (store headline)
  "Return `org-glance-store' with HEADLINES removed from STORE.

Append RM event to WAL, but do not remove HEADLINES from the
persistent storage.

Actual deletion should be handled in a separate thread and
achieved by calling `org-glance-store:flush' method."
  (org-glance-changelog:push (org-glance-store:changelog* store) (org-glance-event:RM headline))
  (remhash (org-glance-headline:hash headline) (org-glance-store:cache store)))

(cl-defun org-glance-store:get (store hash)
  "Return fully qualified `org-glance-headline' by hash.

1. (TODO) Search in LRU cache.
2. Search in staged changes.
3. Search in persistent storage."
  (or
   (gethash hash (org-glance-store:cache store))
   (cl-loop for event in (org-glance-changelog:flatten (org-glance-store:changelog* store))
      for headline = (org-glance-event-state event)
      when (and (org-glance-event:PUT-p event)
                (org-glance-headline-p headline)
                (string= (org-glance-headline:hash headline) hash))
      return headline)
   (org-glance--with-temp-buffer
    (insert-file-contents (org-glance-store:locate store hash))
    (goto-char (point-min))
    (unless (org-at-heading-p)
      (outline-next-heading))
    (let ((headline (org-glance-headline-at-point)))
      (puthash (org-glance-headline:hash headline)
               headline
               (org-glance-store:cache store))))))

(cl-defun org-glance-store:headlines (store)
  "Return actual headline hashes from STORE."
  (cl-loop
     for event in (org-glance-store:events store)
     when (org-glance-event:PUT-p event)
     collect (org-glance-event-state event)))

(cl-defgeneric org-glance-store:locate (store headline)
  "Return location of HEADLINE in STORE.")

(cl-defmethod org-glance-store:locate ((store org-glance-store) (headline org-glance-headline))
  "Return HEADLINE location from STORE."
  (org-glance-store:locate store (org-glance-headline:hash headline)))

(cl-defmethod org-glance-store:locate ((store org-glance-store) (headline org-glance-headline-header))
  "Return HEADLINE location from STORE."
  (org-glance-store:locate store (org-glance-headline:hash headline)))

(cl-defmethod org-glance-store:locate ((store org-glance-store) (hash string))
  "Return HASH location from STORE."
  (let ((prefix (substring hash 0 2))
        (postfix (substring hash 2 (length hash))))
    (f-join (org-glance-store:location store) "data" prefix postfix)))

;; (cl-defun org-glance-store-completing-read (store)
;;   "Read headlines from STORE with completion."
;;   (let* ((title->hash (cl-loop for event in (org-glance-store:events store)
;;                          collect (cons (org-glance-headline-title (org-glance-event-state event))
;;                                        (org-glance-headline:hash (org-glance-event-state event)))))
;;          (hash (alist-get (completing-read "Headline: " title->hash nil t) title->hash nil nil #'string=)))
;;     (org-glance-store:retrieve store hash)))

(cl-defun org-glance-store:events (store)
  (org-glance-changelog:flatten
   (org-glance-changelog:merge
    (org-glance-store:changelog* store)
    (org-glance-store:changelog store))))

(cl-defun org-glance-store:import (store location)
  "Add headlines from LOCATION to STORE."
  (dolist (file (org-glance-scope location))
    (org-glance--with-temp-buffer
     (insert-file-contents file)
     (org-glance-map (headline)
       (org-glance-store:put store headline)))))

(provide 'org-glance-store)
