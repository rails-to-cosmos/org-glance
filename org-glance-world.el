;; -*- lexical-binding: t; -*-

(require 'a)

(require 'f)
(declare-function f-mkdir-full-path 'f)

(require 'ts)
(require 'cl-macs)
(require 'cl-lib)
(require 'eieio)

(require 'org-glance-changelog)
(require 'org-glance-debug)
(require 'org-glance-event)
(require 'org-glance-headline)
(require 'org-glance-helpers)
(require 'org-glance-scope)
(require 'org-glance-types)

(defvar org-glance-worlds (make-hash-table :test #'equal) "List of worlds registered in system.")
(defconst org-glance-world:log-location "WAL")

(org-glance-class org-glance-world nil
    ((location
      :type org-glance-directory
      :initarg :location
      :documentation "Directory containing all the data.")
     (changelog*
      :type org-glance-changelog
      :initarg :changelog*
      :initform (org-glance-changelog)
      :documentation "In-memory changelog.")
     (changelog
      :type org-glance-changelog
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
      :documentation "Views associated with world by type.")))

(cl-defun org-glance-world:create (location)
  "Create world located in directory LOCATION."
  (thunk-let ((cached-world (gethash location org-glance-worlds))
              (persist-world (org-glance-world:read location))
              (location-exists (and (f-exists? location)
                                    (f-directory? location)
                                    (f-readable? location))))
    (cond ((and location-exists cached-world) cached-world)
          (location-exists (puthash location persist-world org-glance-worlds))
          (t (f-mkdir-full-path location)
             (puthash location (org-glance-world :location location) org-glance-worlds)))))

(cl-defun org-glance-world:extend (world strings)
  (cl-loop for string in strings
     for headline = (org-glance-headline-from-string string)
     do (org-glance-world:put world headline)
     finally do (org-glance-world:flush world)
     finally return world))

(cl-defun org-glance-world:read (location)
  "Read `org-glance-world' from LOCATION."
  (or (gethash location org-glance-worlds)
      (let* ((changelog (org-glance-changelog:read (f-join location org-glance-world:log-location)))
             (world (org-glance-world :location location :changelog changelog)))
        (puthash location world org-glance-worlds)
        world)))

(cl-defun org-glance-world:offset (world)
  (if-let (event (org-glance-changelog:last (org-glance- world :changelog)))
      (org-glance- event :offset)
    (org-glance-offset:current)))

(cl-defun org-glance-world:flush (world)
  "Persist WORLD changes.

- Persist event log.
- Apply PUT operations.
- TODO Apply RM operations.
- ...

This should be the only point to destructively change underlying
persistent storage.

In all other places `org-glance-world' should act like pure
functional data structure.

Return last committed offset."
  (let ((changelog (org-glance- world :changelog))
        (changelog* (org-glance- world :changelog*))
        (world-changelog-location (f-join (org-glance- world :location) org-glance-world:log-location)))

    (dolist (event (reverse (org-glance- changelog* :events)))
      (thunk-let* ((headline (org-glance-world:get world (org-glance- event :headline :hash)))
                   (location (org-glance-world:locate world headline)))
        (cl-typecase event
          (org-glance-event:RM
           (user-error "RM operation not implemented")
           ;; TODO think about when to delete headlines
           ;; (f-delete (org-glance-world:locate world headline))
           )

          (org-glance-event:PUT
           (unless (f-exists-p location)
             (org-glance-headline-save headline location)))

          (org-glance-event:UPDATE
           (unless (f-exists-p location)
             (org-glance-headline-save headline location))
           ;; TODO think about when to delete headlines
           ;; (f-delete (org-glance-world:locate world (org-glance- event :hash))
           )))

      (org-glance-changelog:push changelog event))

    (org-glance-changelog:write changelog world-changelog-location)
    (setf (org-glance- world :changelog*) (org-glance-changelog))
    (if (org-glance-changelog:last changelog)
        (org-glance- (org-glance-changelog:last changelog) :offset)
      (org-glance-offset:current))))

;; TODO `org-glance-changelog' filter now filter events instead of headlines

(cl-defun org-glance-world:put (world headline)
  "Put HEADLINE to WORLD.
TODO: Transaction."
  (let ((event (org-glance-event:PUT :headline (org-glance-headline-header:from-headline headline))))
    (org-glance-changelog:push (org-glance- world :changelog*) event)
    (puthash (org-glance- headline :hash) headline (org-glance- world :cache))
    (org-glance- event :offset)))

(cl-defun org-glance-world:remove (world hash)
  "Return `org-glance-world' with HEADLINES removed from WORLD.

Append RM event to WAL, but do not remove HEADLINES from the
persistent storage.

Actual deletion should be handled in a separate thread and
achieved by calling `org-glance-world:flush' method."
  (let ((event (org-glance-event:RM :hash hash)))
    (org-glance-changelog:push (org-glance- world :changelog*) event)
    (remhash hash (org-glance- world :cache))))

(cl-defun org-glance-world:update (world hash headline)
  "Update HEADLINE with HASH to WORLD.
TODO: Transaction."
  (let ((event (org-glance-event:UPDATE
                :hash hash
                :headline (org-glance-headline-header:from-headline headline))))
    (org-glance-changelog:push (org-glance- world :changelog*) event)
    (puthash (org-glance- headline :hash) headline (org-glance- world :cache))
    (remhash hash (org-glance- world :cache))
    (org-glance- event :offset)))

(cl-defun org-glance-world:get (world hash)
  "Return fully qualified `org-glance-headline' by hash.

1. (TODO) Search in LRU cache.
2. Search in staged changes.
3. Search in persistent storage."
  (or (gethash hash (org-glance- world :cache))

      (cl-loop for event in (org-glance-changelog:flatten (org-glance- world :changelog*))
         do (cl-typecase event
              (org-glance-event:RM
               (pcase hash
                 ((pred (string= (org-glance- event :hash))) (cl-return nil))))
              (org-glance-event:UPDATE
               (pcase hash
                 ((pred (string= (org-glance- event :hash))) (cl-return nil))
                 ((pred (string= (org-glance- event :headline :hash))) (cl-return (org-glance- event :headline)))))
              (org-glance-event:PUT
               (pcase hash
                 ((pred (string= (org-glance- event :headline :hash))) (cl-return (org-glance- event :headline)))))))

      (org-glance:with-temp-buffer
       (insert-file-contents (org-glance-world:locate world hash))
       (goto-char (point-min))
       (unless (org-at-heading-p)
         (outline-next-heading))
       (let ((headline (org-glance-headline-at-point)))
         (puthash (org-glance- headline :hash)
                  headline
                  (org-glance- world :cache))))))

(cl-defun org-glance-world:in (world hash)
  "Return t if HASH is in WORLD, nil otherwise."
  (or (not (null (gethash hash (org-glance- world :cache) nil)))
      (cl-loop for event in (org-glance-changelog:flatten (org-glance- world :changelog*))
         do (cl-typecase event
              (org-glance-event:RM (pcase hash
                                     ((pred (string= (org-glance- event :hash))) (cl-return nil))))
              (org-glance-event:UPDATE
               (pcase hash
                 ((pred (string= (org-glance- event :hash))) (cl-return nil))
                 ((pred (string= (org-glance- event :headline :hash))) (cl-return t))))
              (org-glance-event:PUT
               (pcase hash
                 ((pred (string= (org-glance- event :headline :hash))) (cl-return t))))))
      (and (f-exists-p (org-glance-world:locate world hash))
           (f-readable-p (org-glance-world:locate world hash)))))

(cl-defun org-glance-world:headlines (world)
  "Return actual headline hashes from WORLD."
  (cl-loop
     with removed = (make-hash-table :test #'equal)
     for event in (org-glance-world:events world)
     when (cl-typecase event
            ((or org-glance-event:PUT org-glance-event:UPDATE)
             (not (gethash (org-glance- event :headline :hash) removed))))
     collect (org-glance- event :headline)
     when (cl-typecase event
            ((or org-glance-event:RM org-glance-event:UPDATE) t))
     do (puthash (org-glance- event :hash) t removed)))

;; (cl-defun org-glance-world-completing-read (world)
;;   "Read headlines from WORLD with completion."
;;   (let* ((title->hash (cl-loop for event in (org-glance-world:events world)
;;                          collect (cons (org-glance- event :headline :title)
;;                                        (org-glance-headline:hash (org-glance-event-state event)))))
;;          (hash (alist-get (completing-read "Headline: " title->hash nil t) title->hash nil nil #'string=)))
;;     (org-glance-world:retrieve world hash)))

(cl-defun org-glance-world:events (world)
  (org-glance-changelog:flatten
   (org-glance-changelog:merge
    (org-glance- world :changelog*)
    (org-glance- world :changelog))))

(cl-defun org-glance-world:import (world location)
  "Add headlines from LOCATION to WORLD."
  (dolist (file (org-glance-scope location))
    (org-glance:with-temp-buffer
     (insert-file-contents file)
     (org-glance-headline:map (headline)
       (org-glance-world:put world headline)))))

(cl-defgeneric org-glance-world:locate (world headline-or-hash)
  "Return location of HEADLINE in WORLD.")

(cl-defmethod org-glance-world:locate ((world org-glance-world) (hash string))
  "Return HASH location from WORLD."
  (let ((prefix (substring hash 0 2))
        (postfix (substring hash 2 (length hash))))
    (f-join (org-glance- world :location) "data" prefix postfix)))

(cl-defmethod org-glance-world:locate ((world org-glance-world) (headline org-glance-headline))
  "Return HEADLINE location from WORLD."
  (org-glance-world:locate world (org-glance- headline :hash)))

(cl-defmethod org-glance-world:locate ((world org-glance-world) (headline org-glance-headline-header))
  "Return HEADLINE location from WORLD."
  (org-glance-world:locate world (org-glance- headline :hash)))

(provide 'org-glance-world)
