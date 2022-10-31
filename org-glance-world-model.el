;; -*- lexical-binding: t; -*-

(require 'a)
(require 'f)
(require 's)
(require 'ol)
(require 'org)
(require 'org-capture)

(declare-function s-replace-regexp 's)
(declare-function f-mkdir-full-path 'f)

(require 'ts)
(require 'cl-macs)
(require 'cl-lib)
(require 'eieio)

(require 'org-glance-changelog)
(require 'org-glance-log)
(require 'org-glance-event)
(require 'org-glance-headline)
(require 'org-glance-helpers)
(require 'org-glance-scope)
(require 'org-glance-types)

(cl-deftype org-glance-world:location ()
  '(satisfies (lambda (location)
                (and (f-absolute? location)
                     (f-exists? location)
                     (f-directory? location)
                     (f-readable? location)
                     (f-exists? (f-join location "org-glance-world.md"))))))

(org-glance-class org-glance-world nil
    ((location
      :type org-glance-world:location
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
     (dimensions
      :type list
      :initarg :dimensions
      :initform '((tag       . (mapcar (lambda (tag) (intern (downcase tag))) (org-glance- headline :tags)))
                  (state     . (intern (downcase (org-glance- headline :state))))
                  (title     . (org-glance- headline :title))
                  (linked    . (org-glance- headline :linked?))
                  (store     . (org-glance- headline :store?))
                  (encrypted . (org-glance- headline :encrypted?))))
     (views
      :type hash-table
      :initarg :views
      :initform (make-hash-table :test #'equal)
      :documentation "Views associated with world by type.")))

(cl-defun org-glance-world-model:create (location)
  "Create world located in directory LOCATION."
  (declare (indent 1))
  (f-mkdir-full-path location)
  (f-touch (f-join location "org-glance-world.md"))
  (org-glance-world :location location))

(cl-defun org-glance-world-model:read (location)
  (cl-typecase location
    (org-glance-world:location (let ((world (org-glance-world-model:create location)))
                                 (setf (org-glance- world :changelog) (org-glance-changelog:read (f-join location "log" "event.log")))
                                 world))
    (otherwise nil)))

(cl-defun org-glance-world-model:offset (world)
  (if-let (event (org-glance-changelog:last (org-glance- world :changelog)))
      (org-glance- event :offset)
    (org-glance-offset:current)))

(cl-defun org-glance-world-model:persist (world)
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
  (let* ((changelog (org-glance- world :changelog))
         (changelog* (org-glance- world :changelog*))
         (changelog-location (f-join (org-glance- world :location) "log" "event.log")))
    (org-glance-log :world "Persist changes")
    (dolist (event (reverse (org-glance- changelog* :events)))
      (org-glance-log :world "Process %s" event)
      (thunk-let ((headline (org-glance-world-model:get-headline world (org-glance- event :headline :hash))))
        (cl-typecase event
          (org-glance-event:RM
           (user-error "RM operation has not been implemented yet")
           ;; TODO think about when to delete headlines
           ;; (f-delete (org-glance-world-model:locate-headline world headline))
           )

          (org-glance-event:PUT
           (org-glance-log :world "Generate headline id: %s" (org-glance-world-model:generate-headline-id world headline))
           (org-glance-log :world "Save headline: %s" headline)
           (org-glance-world-model:save-headline world headline))

          (org-glance-event:UPDATE
           (org-glance-log :world "Save headline: %s" headline)
           (org-glance-world-model:save-headline world headline)
           ;; TODO think about when to delete headlines
           ;; (f-delete (org-glance-world-model:locate-headline world (org-glance- event :hash))
           )))

      (org-glance-log :world "Push event to changelog: %s" event)
      (org-glance-changelog:push changelog event))

    (org-glance-log :world "Write changelog to %s: \n%s" changelog-location changelog)
    (org-glance-changelog:write changelog changelog-location)
    (setf (org-glance- world :changelog*) (org-glance-changelog))

    (let ((offset (if (org-glance-changelog:last changelog)
                      (org-glance- (org-glance-changelog:last changelog) :offset)
                    (org-glance-offset:current))))
      (org-glance-log :world "Persist world offset: %s" offset)
      (org-glance-log :world "Actual world offset: %s" (org-glance-world-model:offset world))
      offset)))

(cl-defun org-glance-world-model:add-headline (world headline)
  "Put HEADLINE to WORLD."
  (org-glance-log :world "Put headline \"%s\" to world \"%s\" " (org-glance- headline :title) world)
  (let ((event (org-glance-event:PUT :headline (org-glance-headline-header:from-headline headline))))
    (org-glance-changelog:push (org-glance- world :changelog*) event)
    (org-glance-log :cache "[org-glance-headline] cache put: \"%s\"" (org-glance- headline :title))
    (puthash (org-glance- headline :hash) headline (org-glance- world :cache))
    world))

(cl-defun org-glance-world-model:remove-headline (world hash)
  "Return `org-glance-world' with HEADLINES removed from WORLD.

Append RM event to WAL, but do not remove HEADLINES from the
persistent storage.

Actual deletion should be handled in a separate thread and
achieved by calling `org-glance-world-model:persist' method."
  (let ((event (org-glance-event:RM :hash hash)))
    (org-glance-changelog:push (org-glance- world :changelog*) event)
    (when-let (headline (gethash hash (org-glance- world :cache)))
      (org-glance-log :cache "Remove headline \"%s\" from the world cache" (org-glance- headline :title))
      (remhash hash (org-glance- world :cache)))))

(cl-defun org-glance-world-model:update-headline (world old-hash headline)
  "Update HEADLINE with HASH in WORLD."
  (let* ((new-hash (org-glance- headline :hash))
         (header (org-glance-headline-header:from-headline headline))
         (changelog (org-glance- world :changelog*))
         (cache (org-glance- world :cache))
         (event (org-glance-event:UPDATE :hash old-hash
                                         :headline header))
         (offset (org-glance- event :offset)))
    (org-glance-changelog:push changelog event)
    (org-glance-log :cache "Update headline \"%s\" in the world cache" (org-glance- header :title))
    (puthash new-hash headline cache)
    (remhash old-hash cache)
    offset))

(cl-defun org-glance-world-model:get-headline (world hash)
  "Return fully qualified `org-glance-headline' by its hash."
  (or

   ;; Search LRU cache
   (when-let (result (gethash hash (org-glance- world :cache)))
     (org-glance-log :cache "[org-glance-headline] cache hit (hashmap): \"%s\"" (org-glance- result :title))
     result)

   ;; Search staged changes
   (when-let (result (cl-loop for event in (org-glance-changelog:flatten (org-glance- world :changelog*))
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
                                ((pred (string= (org-glance- event :headline :hash))) (cl-return (org-glance- event :headline))))))))
     (org-glance-log :cache "[org-glance-headline] cache hit (changelog*): \"%s\"" result)
     result)

   ;; Search persistent storage
   (org-glance:with-temp-buffer
    (org-glance-log :cache "[org-glance-headline] cache miss: \"%s\"" hash)
    (insert-file-contents (org-glance-world-model:locate-headline world hash))
    (goto-char (point-min))
    (unless (org-at-heading-p)
      (outline-next-heading))
    (when-let (headline (org-glance-headline-at-point))
      (org-glance-log :cache "[org-glance-headline] cache put: \"%s\"" (org-glance- headline :title))
      (puthash (org-glance- headline :hash) headline (org-glance- world :cache))))))

(cl-defun org-glance-world-model:events (world)
  (org-glance-changelog:flatten
   (org-glance-changelog:merge
    (org-glance- world :changelog*)
    (org-glance- world :changelog))))

(cl-defun org-glance-world-model:get-headlines (world)
  "Return actual headline hashes from WORLD."
  (cl-loop
     with removed = (make-hash-table :test #'equal)
     for event in (org-glance-log :performance (org-glance-world-model:events world))
     when (cl-typecase event
            ((or org-glance-event:PUT org-glance-event:UPDATE)
             (not (gethash (org-glance- event :headline :hash) removed))))
     collect (org-glance- event :headline)
     when (cl-typecase event
            ((or org-glance-event:RM org-glance-event:UPDATE) t))
     do (puthash (org-glance- event :hash) t removed)))

(cl-defun org-glance-world-model:save-headline (world headline)
  (let ((location (org-glance-world-model:locate-headline world headline)))
    (unless (f-exists-p location)
      (org-glance-headline:save headline location)
      (org-glance-world-model:apply-dimensions world headline))
    location))

(cl-defun org-glance-world-model:generate-headline-id (world headline)
  (cl-labels ((unique-id (id path &optional (tryout 0))
                (let ((dest (if (> tryout 0)
                                (f-join path (format "%s_%d" id tryout))
                              (f-join path (format "%s" id)))))
                  (cond ((f-exists? dest) (unique-id id path (1+ tryout)))
                        (t dest)))))
    (unique-id (s-join "_" (list (format-time-string "%Y-%m-%d")
                                 (thread-last (org-glance- headline :title)
                                   (replace-regexp-in-string "[^a-z0-9A-Z_]" "_")
                                   (replace-regexp-in-string "\\-+" "-")
                                   (replace-regexp-in-string "\\-+$" "")
                                   (s-truncate 30))))
               (f-join (org-glance- world :location) "resources"))))

(cl-defun org-glance-world-model:locate-headline (world headline)
  "Return location of HEADLINE in WORLD."
  (cl-typecase headline
    (string (let ((prefix (substring headline 0 2))
                  (postfix (substring headline 2 (length headline))))
              (f-join (org-glance- world :location) "data" prefix postfix)))
    ((or org-glance-headline org-glance-headline-header)
     (org-glance-world-model:locate-headline world (org-glance- headline :hash)))))

(cl-defun org-glance-world-model:locate-dimension (world dimension)
  (f-join (org-glance- world :location)
          "dimensions"
          (format "%s.org" (downcase dimension))))

(cl-defun org-glance-world-model:evaluate-dimensions (world headline)
  (cl-loop for (dimension . partition-by) in (org-glance- world :dimensions)
     collect (cons dimension (let ((result (eval partition-by (a-list 'headline headline))))
                               (cond ((atom result) (list result))
                                     (t result))))))

(cl-defun org-glance-world-model:apply-dimensions (world headline)
  (cl-loop
     for (dimension . partitions) in (org-glance-world-model:evaluate-dimensions world headline)
     do (dolist (partition partitions)
          (when (not (string-empty-p (format "%s" partition)))
            (let ((predicate (cl-typecase partition
                               (symbol `(member (quote ,partition) ,dimension))
                               (t `(member ,partition ,dimension))))
                  (location (f-join (org-glance- world :location)
                                    "dimensions"
                                    (downcase (format "%s=%s.org" dimension partition)))))
              (org-glance-log :dimensions "Create derived view \"%s -> %s\"" dimension partition)
              (org-glance-view:get-or-create world predicate location (org-glance-offset:zero)))))))

(cl-defun org-glance-world-model:list-dimensions (world)
  (--map (file-name-sans-extension it)
         (--filter (member (file-name-extension it) org-glance-scope-extensions)
                   (directory-files (f-join (org-glance- world :location) "dimensions")))))

(cl-defun org-glance-world-model:update-dimension (world dim)
  (let* ((view-location (org-glance-world-model:locate-dimension world dim))
         (view-header (thread-first view-location
                        (org-glance-view:get-header-location-by-view-location)
                        (org-glance-view:read-header)))
         (view (org-glance-view :world world
                                :type (a-get view-header :type)
                                :location view-location
                                :offset (a-get view-header :offset)))
         (world-offset (org-glance-world-model:offset world)))

    (org-glance-log :world "Fetch view %s" dim)
    (org-glance-log :offsets "[%s] View offset = %s" (org-glance- view :type) (org-glance- view :offset))
    (org-glance-log :offsets "[%s] World offset = %s" (org-glance- view :type) (org-glance-world-model:offset world))
    (org-glance-log :world "[%s] World log:\n%s" (org-glance- view :type) (org-glance-changelog:contents (org-glance- world :changelog)))
    (org-glance-log :buffers "[%s] Buffer before update: \n%s" (org-glance- view :type) (buffer-string))

    (when (org-glance-offset:less? (org-glance- view :offset) world-offset)
      (with-temp-file view-location
        (org-mode)
        (insert-file-contents view-location)
        (org-glance-log :performance
            (org-glance-view:mark-buffer view))
        (org-glance-log :performance
            (org-glance-view:fetch view))
        (org-glance-log :performance
            (org-glance-view:write-header view))
        (org-glance-log :buffers "[%s] Buffer after update:\n%s" (org-glance- view :type) (buffer-string))))

    view-location))

(cl-defun org-glance-world-model:filter-headlines (world &optional predicate)
  "TODO cache headlines by predicate."
  (declare (indent 1))
  (cl-loop for headline in (org-glance-log :performance
                               (org-glance-world-model:get-headlines world))
     when (or (null predicate) (funcall predicate headline))
     collect (cons (org-glance- headline :title) (org-glance- headline :hash))))

(cl-defun org-glance-world:cashew-get (world key)
  "Get `org-glance-view' from WORLD's cache by KEY."
  (when (and (f-exists? (org-glance- key :location))
             (f-file? (org-glance- key :location)))
    (when-let (view (gethash key (org-glance- world :views)))
      (prog1 view
        (org-glance-log :cache "[org-glance-view] cache hit: %s" key)))))

(cl-defun org-glance-world:cashew-set (key view world)
  "Set `org-glance-view' to the WORLD's cache using KEY."
  (puthash key view (org-glance- world :views)))

(cl-defun org-glance-world-model:root (location)
  (cl-typecase location
    (org-glance-world:location location)
    (otherwise (org-glance-world-model:root (f-parent location)))))

(provide 'org-glance-world-model)
