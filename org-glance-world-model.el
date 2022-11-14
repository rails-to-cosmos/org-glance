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
     (dimensions
      :type (org-glance-type:list-of org-glance-dimension)
      :initarg :dimensions)

     ;; in-memory caches
     (partitions
      :type (org-glance-type:list-of org-glance-partition)
      :initarg :partitions
      :initform nil)
     (headline-by-hash
      :type hash-table
      :initarg :headline-by-hash
      :initform (make-hash-table :test #'equal)
      :documentation "HASH -> HEADLINE")
     (relations
      :type hash-table
      :initarg :relations
      :initform (make-hash-table :test #'equal)
      :documentation "id -> headlines")))

(cl-defun org-glance-world:create (location)
  "Create world located in directory LOCATION."
  (declare (indent 1))
  (cl-the org-glance-world
    (progn
      (f-mkdir-full-path location)
      (f-touch (f-join location "world.md"))
      (org-glance-world :location location))))

(cl-defun org-glance-world:read (location)
  (cl-the (org-glance-type:optional org-glance-world)
    (cl-typecase location
      (org-glance-type:world-location (let ((world (org-glance-world:create location)))
                                        (org-glance-world:read-changelog world)
                                        (org-glance-world:read-relations! world)
                                        world))
      (otherwise nil))))

(cl-defun org-glance-world:offset (world)
  (cl-check-type world org-glance-world)

  (if-let (event (org-glance-changelog:last (org-glance? world :changelog)))
      (org-glance? event :offset)
    (org-glance-offset:current)))

(cl-defmacro org-glance-world:with-locked-location (location &rest forms)
  (declare (indent 1))
  `(when (--> ,location
              (get-file-buffer it)
              (cond ((null it) t)
                    ((buffer-live-p it) (kill-buffer it))))
     (org-glance:with-file-overwrite ,location
       ,@forms)))

(cl-defun org-glance-world:locate-partition (world partition)
  (cl-check-type world org-glance-world)
  (cl-check-type partition org-glance-partition)

  (f-join (org-glance? world :location) "views"
          (org-glance-partition:path partition)
          (format "%s.org" (org-glance-partition:representation partition))))

(cl-defun org-glance-world:partitions (world)
  (cl-check-type world org-glance-world)

  (or (org-glance? world :partitions)
      (org-glance! world :partitions := (--map (--> it
                                                    (file-name-sans-extension it)
                                                    (list (file-name-nondirectory (f-parent (f-parent it)))
                                                          (file-name-nondirectory (f-parent it)))
                                                    (-zip-lists '(:dimension :value) it)
                                                    (-flatten it)
                                                    (apply #'org-glance-partition it))
                                               (directory-files-recursively (f-join (org-glance? world :location) "views") ".*\\.org$")))))

(cl-defun org-glance-world:read-partition (world partition)
  (-> (org-glance-world:locate-partition world partition)
      (org-glance-view:locate-header)
      (org-glance-view:read-header)))

(cl-defun org-glance-world:make-partitions (world headline)
  (cl-check-type world org-glance-world)
  (cl-check-type headline org-glance-headline)

  (cl-loop with dimensions = (org-glance? world :dimensions)
     for dimension in dimensions
     for predicates = (org-glance-dimension:predicates dimension headline)
     append (cl-loop for predicate in predicates
               for value = (org-glance-dimension:validate predicate headline dimensions)
               when value
               collect (let* ((partition (org-glance-partition
                                          :dimension (format "%s" (org-glance? dimension :name))
                                          :value value))
                              (location (org-glance-world:locate-partition world partition)))
                         (unless (f-exists? location)
                           (org-glance-log :dimensions "Create derived view %s in %s" partition location)
                           (push partition (org-glance? world :partitions))
                           (org-glance-view:get-or-create world partition location (org-glance-offset:zero)))))))

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

  (cl-the org-glance-type:offset
    (let* ((changelog (org-glance? world :changelog))
           (changelog* (org-glance? world :changelog*)))
      (dolist-with-progress-reporter (event (reverse (org-glance? changelog* :events)))
          (format "Persist world %s" (org-glance? world :location))
        (thunk-let* ((source-hash (org-glance? event :hash))
                     (target-hash (org-glance? event :headline :hash))
                     (headline-exists? (org-glance-world:headline-exists? world target-hash))
                     (headline (org-glance-world:get-headline world target-hash)))
          (cl-typecase event
            (org-glance-event:RM (org-glance-world:delete-headline world source-hash))
            (org-glance-event:PUT (org-glance-world:write-headline world headline)
                                  (org-glance-world:make-partitions world headline))
            (org-glance-event:UPDATE (org-glance-world:write-headline world headline)
                                     (org-glance-world:make-partitions world headline)
                                     (org-glance-world:delete-headline world source-hash)))
          (org-glance-changelog:push changelog event)))

      (org-glance-world:write-changelog! world)
      (org-glance-world:write-relations! world)

      (if (org-glance-changelog:last changelog)
          (org-glance? (org-glance-changelog:last changelog) :offset)
        (org-glance-offset:current)))))

(org-glance-fun org-glance-world:write-headline ((world :: org-glance-world) (headline :: org-glance-headline)) -> org-glance-type:readable-file
  (let ((location (org-glance-world:locate-headline world headline)))
    (unless (f-exists-p location)
      (org-glance-headline:save headline location))
    location))

;; (org-glance-fun write-headline! ((world :: World) (headline :: Headline)) -> ReadableFile
;;   )

(cl-defun org-glance-world:locate-changelog (world)
  (cl-check-type world org-glance-world)

  (f-join (org-glance? world :location) "log" "event.log"))

(cl-defun org-glance-world:write-changelog! (world)
  (cl-check-type world org-glance-world)

  (let ((changelog (org-glance? world :changelog))
        (location (org-glance-world:locate-changelog world)))
    (org-glance-changelog:write changelog location)
    (org-glance! world :changelog* := (org-glance-changelog))))

(cl-defun org-glance-world:read-changelog (world)
  (cl-check-type world org-glance-world)

  (org-glance! world :changelog := (org-glance-changelog:read (org-glance-world:locate-changelog world))))

(cl-defun org-glance-world:locate-relations (world)
  (cl-check-type world org-glance-world)

  (f-join (org-glance? world :location) "relations.el"))

(cl-defun org-glance-world:write-relations! (world)
  (cl-check-type world org-glance-world)

  (with-temp-file (org-glance-world:locate-relations world)
    (insert (pp-to-string (org-glance? world :relations)))))

(cl-defun org-glance-world:read-relations! (world)
  (cl-check-type world org-glance-world)

  (pcase (org-glance-world:locate-relations world)
    ((and (cl-struct org-glance-type:readable-file) location) (with-temp-buffer
                                                                (insert-file-contents-literally location)
                                                                (org-glance! world :relations := (read (buffer-string)))))))

(cl-defun org-glance-world:delete-headline (world headline)
  (cl-check-type world org-glance-world)
  (cl-check-type headline (or org-glance-type:hash org-glance-headline org-glance-headline-header))

  (condition-case nil
      (f-delete (org-glance-world:locate-headline world headline))
    (error nil)))

(cl-defun org-glance-world:add-headline! (world headline)
  "Put HEADLINE to WORLD."
  (cl-check-type world org-glance-world)
  (cl-check-type headline org-glance-headline)

  (let* ((id (org-glance-world:headline-id world headline))
         (headline (org-glance-headline:with-properties headline
                     `(("GLANCE_ID" ,id)
                       ("DIR" ,(concat "../../../resources/" id))))))
    (org-glance-world:add-headline-to-cache world id headline)
    (org-glance-changelog:push (org-glance? world :changelog*)
      (org-glance-event:PUT :headline (org-glance-headline-header:from-headline headline))))

  world)

(cl-defun org-glance-world:add-headline-to-cache (world id headline)
  (cl-check-type world org-glance-world)
  (cl-check-type headline org-glance-headline)
  (cl-check-type id string)

  (org-glance-log :cache "[org-glance-headline] cache put: \"%s\"" (org-glance? headline :title))
  (puthash id t (org-glance? world :relations))
  (puthash (org-glance? headline :hash) headline (org-glance? world :headline-by-hash)))

(cl-defun org-glance-world:remove-headline-from-cache (world hash)
  (cl-check-type world org-glance-world)
  (cl-check-type hash string)

  (when-let (headline (gethash hash (org-glance? world :headline-by-hash)))
    (org-glance-log :cache "Remove headline \"%s\" from the world cache" (org-glance? headline :title))
    (remhash hash (org-glance? world :headline-by-hash))))

(cl-defun org-glance-world:remove-headline (world hash)
  "Return `org-glance-world' with HEADLINES removed from WORLD.

Append RM event to WAL, but do not remove HEADLINES from the
persistent storage.

Actual deletion should be handled in a separate thread and
achieved by calling `org-glance-world:persist' method."
  (cl-check-type world org-glance-world)
  (cl-check-type hash string)

  (let ((event (org-glance-event:RM :hash hash)))
    (org-glance-changelog:push (org-glance? world :changelog*) event))
  (org-glance-world:remove-headline-from-cache world hash))

(cl-defun org-glance-world:update-headline (world old-hash headline)
  "Update HEADLINE with HASH in WORLD."
  (cl-check-type world org-glance-world)
  (cl-check-type headline org-glance-headline)

  (let* ((new-hash (org-glance? headline :hash))
         (header (org-glance-headline-header:from-headline headline))
         (changelog (org-glance? world :changelog*))
         (cache (org-glance? world :headline-by-hash))
         (event (org-glance-event:UPDATE :hash old-hash
                                         :headline header))
         (offset (org-glance? event :offset)))
    (org-glance-changelog:push changelog event)
    (org-glance-log :cache "Update headline \"%s\" in the world cache" (org-glance? header :title))
    (puthash new-hash headline cache)
    (remhash old-hash cache)
    offset))

(cl-defun org-glance-world:get-headline-from-cache (world hash)
  (cl-check-type world org-glance-world)
  (cl-check-type hash org-glance-type:hash)

  (cl-the (org-glance-type:optional org-glance-headline)
    (gethash hash (org-glance? world :headline-by-hash))))

(cl-defun org-glance-world:get-headline-from-stage (world hash)
  (cl-check-type world org-glance-world)
  (cl-check-type hash org-glance-type:hash)

  (cl-the (org-glance-type:optional org-glance-headline)
    (cl-loop for event in (org-glance-changelog:flatten (org-glance? world :changelog*))
       do (cl-typecase event
            (org-glance-event:RM (pcase hash
                                   ((pred (string= (org-glance? event :hash))) (cl-return nil))))
            (org-glance-event:UPDATE (pcase hash
                                       ((pred (string= (org-glance? event :hash))) (cl-return nil))
                                       ((pred (string= (org-glance? event :headline :hash))) (cl-return (org-glance? event :headline)))))
            (org-glance-event:PUT (pcase hash
                                    ((pred (string= (org-glance? event :headline :hash))) (cl-return (org-glance? event :headline)))))))))

(cl-defun org-glance-world:get-headline-from-drive (world hash)
  (cl-check-type world org-glance-world)
  (cl-check-type hash org-glance-type:hash)

  (cl-the (org-glance-type:optional org-glance-headline)
    (org-glance:with-temp-buffer
     (org-glance-log :cache "[org-glance-headline] cache miss: \"%s\"" hash)
     (insert-file-contents (org-glance-world:locate-headline world hash))
     (goto-char (point-min))
     (unless (org-at-heading-p)
       (outline-next-heading))
     (when-let (headline (org-glance-headline-at-point))
       (org-glance-log :cache "[org-glance-headline] cache put: \"%s\"" (org-glance? headline :title))
       (puthash (org-glance? headline :hash) headline (org-glance? world :headline-by-hash))))))

(cl-defun org-glance-world:get-headline (world hash)
  "Return fully qualified `org-glance-headline' by its hash."
  (cl-check-type world org-glance-world)
  (cl-check-type hash string)

  (cl-the org-glance-headline
    (or (org-glance-world:get-headline-from-cache world hash)
        (org-glance-world:get-headline-from-stage world hash)
        (org-glance-world:get-headline-from-drive world hash))))

(cl-defun org-glance-world:events (world)
  (cl-check-type world org-glance-world)

  (org-glance-changelog:flatten
   (org-glance-changelog:merge
    (org-glance? world :changelog*)
    (org-glance? world :changelog))))

(cl-defun org-glance-world:headlines (world)
  "WORLD headlines.
TODO optimize."
  (cl-check-type world org-glance-world)

  (cl-loop
     with removed = (make-hash-table :test #'equal)
     for event in (org-glance-world:events world)
     when (cl-typecase event
            ((or org-glance-event:PUT org-glance-event:UPDATE)
             (not (gethash (org-glance? event :headline :hash) removed))))
     collect (org-glance? event :headline)
     when (cl-typecase event
            ((or org-glance-event:RM org-glance-event:UPDATE) t))
     do (puthash (org-glance? event :hash) t removed)))

(cl-defun org-glance-world:headline-id (world headline)
  (cl-check-type world org-glance-world)
  (cl-check-type headline (or org-glance-headline org-glance-headline-header))

  (cl-labels ((uniquify (id path &optional (tryout 0))
                (let ((dest (if (> tryout 0)
                                (f-join path (format "%s_%d" id tryout))
                              (f-join path (format "%s" id)))))
                  (cond ((f-exists? dest) (uniquify id path (1+ tryout)))
                        (t dest))))
              (truncate (len s ellipsis)
                (declare (pure t) (side-effect-free t))
                (if (> (length s) len)
                    (format "%s%s" (substring s 0 (- len (length ellipsis))) ellipsis)
                  s)))
    (let ((location (uniquify (s-join "_" (list (format-time-string "%Y-%m-%d")
                                                (--> (org-glance? headline :title)
                                                     (replace-regexp-in-string "[^a-z0-9A-Z_]" "_" it)
                                                     (replace-regexp-in-string "\\-+" "-" it)
                                                     (replace-regexp-in-string "\\-+$" "" it)
                                                     (truncate 30 it ""))))
                              (f-join (org-glance? world :location) "resources"))))
      (f-mkdir-full-path location)
      (file-name-nondirectory location))))

(cl-defun org-glance-world:locate-headline (world headline)
  "Return location of HEADLINE in WORLD."
  (cl-check-type world org-glance-world)
  (cl-check-type headline (or org-glance-type:hash
                              org-glance-headline
                              org-glance-headline-header))

  (cl-typecase headline
    ((or org-glance-headline org-glance-headline-header)
     (org-glance-world:locate-headline world (org-glance? headline :hash)))
    (string (let ((prefix (substring headline 0 2))
                  (postfix (substring headline 2 (length headline))))
              (f-join (org-glance? world :location) "data" prefix postfix)))))

(cl-defun org-glance-world:headline-exists? (world headline)
  (cl-check-type world org-glance-world)
  (cl-check-type headline (or org-glance-type:hash
                              org-glance-headline
                              org-glance-headline-header))

  (f-exists? (org-glance-world:locate-headline world headline)))

(cl-defun org-glance-world:make-predicate (world partition)
  (cl-check-type world org-glance-world)
  (cl-check-type partition org-glance-partition)

  (cl-loop for dimension in (org-glance? world :dimensions)
     when (string= (org-glance? partition :dimension)
                   (format "%s" (org-glance? dimension :name)))
     return (org-glance-dimension:make-predicate dimension (org-glance? partition :value))))

(cl-defun org-glance-world:validate-headline (world partition headline)
  (cl-check-type world org-glance-world)
  (cl-check-type partition org-glance-partition)
  (cl-check-type headline org-glance-headline-header)

  (let ((predicate (org-glance-world:make-predicate world partition)))
    (org-glance-dimension:validate predicate headline (org-glance? world :dimensions))))

(cl-defun org-glance-world:get-partition-headlines (world partition)
  (declare (indent 1))
  (cl-check-type world org-glance-world)
  (cl-check-type partition org-glance-partition)

  (let ((predicate (org-glance-world:make-predicate world partition))
        (headlines (org-glance-world:headlines world)))
    (cl-loop for headline in headlines
       when (org-glance-dimension:validate predicate headline (org-glance? world :dimensions))
       collect headline)))

(cl-defun org-glance-world:root (location)
  (cl-typecase location
    (org-glance-type:world-location location)
    (otherwise (org-glance-world:root (f-parent location)))))

(provide 'org-glance-world-model)
