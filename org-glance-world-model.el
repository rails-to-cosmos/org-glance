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

(require 'org-glance-dimension)
(require 'org-glance-changelog)
(require 'org-glance-log)
(require 'org-glance-headline)
(require 'org-glance-helpers)
(require 'org-glance-scope)
(require 'org-glance-types)

(org-glance-class org-glance-world nil
    ((location
      :type org-glance-type:world-location
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
      :documentation "World dimensions.")))

(cl-defun org-glance-world:create (location)
  "Create world located in directory LOCATION."
  (declare (indent 1))
  (f-mkdir-full-path location)
  (f-touch (f-join location "world.md"))
  (org-glance-world :location location))

(cl-defun org-glance-world:read (location)
  (cl-typecase location
    (org-glance-type:world-location (let ((world (org-glance-world:create location)))
                                      (setf (org-glance- world :changelog) (org-glance-changelog:read (f-join location "log" "event.log")))
                                      world))
    (otherwise nil)))

(cl-defun org-glance-world:offset (world)
  (if-let (event (org-glance-changelog:last (org-glance- world :changelog)))
      (org-glance- event :offset)
    (org-glance-offset:current)))

(cl-defmacro org-glance-world:with-locked-derivation (world derivation &rest forms)
  (declare (indent 2))
  `(progn
     (cl-check-type ,world org-glance-world)
     (cl-check-type ,derivation org-glance-derivation)

     (when (--> ,world
                (org-glance-world:locate-derivation it ,derivation)
                (get-file-buffer it)
                (cond ((null it) t)
                      ((buffer-live-p it) (kill-buffer it))))
       ,@forms)))

(cl-defun org-glance-world:locate-derivation (world derivation)
  (cl-check-type world org-glance-world)
  (cl-check-type derivation org-glance-derivation)

  (f-join (org-glance- world :location) "views" (org-glance-derivation:filename derivation)))

(cl-defun org-glance-world:make-derivations (world headline)
  (cl-check-type world org-glance-world)
  (cl-check-type headline org-glance-headline)

  (let ((dimensions (org-glance- world :dimensions)))
    (dolist-with-progress-reporter (dimension dimensions)
        "Make derivations"
      (let ((partitions (org-glance-dimension:partitions dimension headline))
            (predicates (org-glance-dimension:predicates dimension headline)))
        (cl-loop for (partition . predicate) in (-zip partitions predicates)
           for derivation = (org-glance-derivation
                             :dimension (format "%s" (org-glance- dimension :name))
                             :value (format "%s" (org-glance-dimension:validate predicate headline dimensions)))
           for location = (org-glance-world:locate-derivation world derivation)
           do (org-glance-log :dimensions "Create derived view \"%s -> %s\" in %s" partition derivation location)
           do (org-glance-view:get-or-create world predicate location (org-glance-offset:zero)))))))

(cl-defun org-glance-world:persist (world)
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
  (cl-check-type world org-glance-world)

  (let* ((changelog (org-glance- world :changelog))
         (changelog* (org-glance- world :changelog*))
         (changelog-location (f-join (org-glance- world :location) "log" "event.log")))
    (org-glance-log :world "Persist changes")
    (dolist (event (reverse (org-glance- changelog* :events)))
      (org-glance-log :world "Process %s" event)
      (thunk-let ((headline (org-glance-world:get-headline world (org-glance- event :headline :hash))))
        (cl-typecase event
          (org-glance-event:RM
           (user-error "RM operation has not been implemented yet")
           ;; TODO think about when to delete headlines
           ;; (f-delete (org-glance-world:locate-headline world headline))
           (org-glance-changelog:push changelog event))

          (org-glance-event:PUT
           (let* ((id (org-glance-world:generate-headline-id world headline))
                  (headline (org-glance-headline:with-properties headline
                              `(("GLANCE_ID" ,id)
                                ("DIR" ,(concat "../resources/" id))))))
             (org-glance-world:save-headline world headline)
             (org-glance-world:make-derivations world headline)
             (org-glance-changelog:push changelog (org-glance-event:PUT :headline (org-glance-headline-header:from-headline headline)))))

          (org-glance-event:UPDATE
           (org-glance-world:save-headline world headline)
           (org-glance-world:make-derivations world headline)
           (org-glance-changelog:push changelog event)
           ;; TODO think about when to delete headlines
           ;; (f-delete (org-glance-world:locate-headline world (org-glance- event :hash))
           ))))

    (org-glance-changelog:write changelog changelog-location)
    (setf (org-glance- world :changelog*) (org-glance-changelog))
    (cl-the org-glance-type:offset (if (org-glance-changelog:last changelog)
                                       (org-glance- (org-glance-changelog:last changelog) :offset)
                                     (org-glance-offset:current)))))

(cl-defun org-glance-world:save-headline (world headline)
  (cl-check-type world org-glance-world)
  (cl-check-type headline org-glance-headline)

  (let ((location (org-glance-world:locate-headline world headline)))
    (unless (f-exists-p location)
      (org-glance-headline:save headline location))
    location))

(cl-defun org-glance-world:add-headline (world headline)
  "Put HEADLINE to WORLD."
  (declare (indent 1))
  (org-glance-log :world "Put headline \"%s\" to world \"%s\" " (org-glance- headline :title) world)
  (let ((event (org-glance-event:PUT :headline (org-glance-headline-header:from-headline headline))))
    (org-glance-changelog:push (org-glance- world :changelog*) event)
    (org-glance-log :cache "[org-glance-headline] cache put: \"%s\"" (org-glance- headline :title))
    (puthash (org-glance- headline :hash) headline (org-glance- world :cache))
    world))

(cl-defun org-glance-world:remove-headline (world hash)
  "Return `org-glance-world' with HEADLINES removed from WORLD.

Append RM event to WAL, but do not remove HEADLINES from the
persistent storage.

Actual deletion should be handled in a separate thread and
achieved by calling `org-glance-world:persist' method."
  (let ((event (org-glance-event:RM :hash hash)))
    (org-glance-changelog:push (org-glance- world :changelog*) event)
    (when-let (headline (gethash hash (org-glance- world :cache)))
      (org-glance-log :cache "Remove headline \"%s\" from the world cache" (org-glance- headline :title))
      (remhash hash (org-glance- world :cache)))))

(cl-defun org-glance-world:update-headline (world old-hash headline)
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

(cl-defun org-glance-world:get-headline (world hash)
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
    (insert-file-contents (org-glance-world:locate-headline world hash))
    (goto-char (point-min))
    (unless (org-at-heading-p)
      (outline-next-heading))
    (when-let (headline (org-glance-headline-at-point))
      (org-glance-log :cache "[org-glance-headline] cache put: \"%s\"" (org-glance- headline :title))
      (puthash (org-glance- headline :hash) headline (org-glance- world :cache))))))

(cl-defun org-glance-world:events (world)
  (org-glance-changelog:flatten
   (org-glance-changelog:merge
    (org-glance- world :changelog*)
    (org-glance- world :changelog))))

(cl-defun org-glance-world:headlines (world)
  "Return actual headline hashes from WORLD."
  (cl-check-type world org-glance-world)

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

(cl-defun org-glance-world:generate-headline-id (world headline)
  (cl-check-type world org-glance-world)
  (cl-check-type headline (or org-glance-headline org-glance-headline-header))

  (cl-labels ((unique-location (id path &optional (tryout 0))
                (let ((dest (if (> tryout 0)
                                (f-join path (format "%s_%d" id tryout))
                              (f-join path (format "%s" id)))))
                  (cond ((f-exists? dest) (unique-location id path (1+ tryout)))
                        (t dest)))))
    (let ((location (unique-location (s-join "_" (list (format-time-string "%Y-%m-%d")
                                                       (thread-last (org-glance- headline :title)
                                                         (replace-regexp-in-string "[^a-z0-9A-Z_]" "_")
                                                         (replace-regexp-in-string "\\-+" "-")
                                                         (replace-regexp-in-string "\\-+$" "")
                                                         (s-truncate 30))))
                                     (f-join (org-glance- world :location) "resources"))))
      (f-mkdir-full-path location)
      (file-name-nondirectory location))))

(cl-defun org-glance-world:locate-headline (world headline)
  "Return location of HEADLINE in WORLD."
  (cl-check-type world org-glance-world)

  (cl-typecase headline
    ((or org-glance-headline org-glance-headline-header)
     (org-glance-world:locate-headline world (org-glance- headline :hash)))
    (string (let ((prefix (substring headline 0 2))
                  (postfix (substring headline 2 (length headline))))
              (f-join (org-glance- world :location) "data" prefix postfix)))))

(cl-defun org-glance-world:filter-headlines (world predicate)
  "TODO cache headlines by predicate."
  (declare (indent 1))
  (cl-check-type world org-glance-world)
  (cl-check-type predicate function)

  (cl-loop for headline in (org-glance-world:headlines world)
     when (funcall predicate headline)
     collect (cons (org-glance- headline :title) (org-glance- headline :hash))))

(cl-defun org-glance-world:root (location)
  (cl-typecase location
    (org-glance-type:world-location location)
    (otherwise (org-glance-world:root (f-parent location)))))

(provide 'org-glance-world-model)
