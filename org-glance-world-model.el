;; -*- lexical-binding: t; -*-

(require 'a)
(require 'f)
(require 's)
(require 'ol)
(require 'org)

(declare-function s-replace-regexp 's)
(declare-function f-mkdir-full-path 'f)

(require 'ts)
(require 'cl-macs)
(require 'cl-lib)
(require 'eieio)

(require 'org-glance-dimension)
(require 'org-glance-partition)
(require 'org-glance-changelog)
(require 'org-glance-log)
(require 'org-glance-headline)
(require 'org-glance-helpers)
(require 'org-glance-scope)
(require 'org-glance-types)
(require 'org-glance-view-model)

(org-glance-class org-glance-world nil
    ((location :type org-glance-world-location :initarg :location :documentation "Directory containing all the data.")
     (changelog* :type org-glance-changelog :initarg :changelog* :initform (org-glance-changelog) :documentation "In-memory changelog.")
     (changelog :type org-glance-changelog :initarg :changelog :initform (org-glance-changelog) :documentation "Persistent changelog.")
     (dimensions :type (org-glance-list-of org-glance-dimension) :initarg :dimensions)

     ;; in-memory caches
     (partitions :type (org-glance-list-of org-glance-partition) :initarg :partitions :initform nil)
     (headlines :type hash-table :initarg :headlines :initform (make-hash-table :test #'equal) :documentation "HASH -> HEADLINE")
     (relations :type hash-table :initarg :relations :initform (make-hash-table :test #'equal) :documentation "ID -> HEADLINES")))

(org-glance-declare org-glance-world:create :: OptionalDirectory -> World)
(defun org-glance-world:create (location)
  "Create world located in directory LOCATION."
  (declare (indent 1))
  (f-mkdir-full-path location)
  (f-touch (f-join location "world.md"))
  (org-glance-world :location location))

(org-glance-declare org-glance-world:read :: OptionalDirectory -> (Optional World))
(defun org-glance-world:read (location)
  (cl-typecase location
    (org-glance-world-location (let ((world (org-glance-world:create location)))
                                 (org-glance-world:read-changelog! world)
                                 (org-glance-world:read-relations! world)
                                 world))
    (otherwise nil)))

(org-glance-declare org-glance-world:offset :: World -> Offset)
(defun org-glance-world:offset (world)
  (if-let (event (org-glance-changelog:last (org-glance? world :changelog)))
      (org-glance? event :offset)
    (org-glance-offset:current)))

(org-glance-declare org-glance-world:locate-partition :: World -> Partition -> OptionalFile)
(defun org-glance-world:locate-partition (world partition)
  (f-join (org-glance? world :location)
          "dimensions"
          (org-glance-partition:path partition)
          (format "%s.org" (org-glance-partition:representation partition))))

(org-glance-declare org-glance-world:partitions :: World -> (ListOf Partition))
(defun org-glance-world:partitions (world)
  "Get WORLD partitions considering cache."
  (or (org-glance? world :partitions)
      (org-glance! world :partitions := (org-glance-world:get-partitions world))))

(org-glance-declare org-glance-world:get-partitions :: World -> (ListOf Partition))
(defun org-glance-world:get-partitions (world)
  "Read WORLD partitions from disk.
Ignore cache."
  (--map (--> it
              (file-name-sans-extension it)
              (list (file-name-nondirectory (f-parent (f-parent it)))
                    (file-name-nondirectory (f-parent it)))
              (-zip-lists '(:dimension :value) it)
              (-flatten it)
              (apply #'org-glance-partition it))
         (directory-files-recursively (f-join (org-glance? world :location) "dimensions") ".*\\.org$")))

;; (org-glance-declare org-glance-world:read-partition :: World -> Partition -> list)
;; (defun org-glance-world:read-partition (world partition)
;;   (-> (org-glance-world:locate-partition world partition)
;;       (org-glance-view:locate-header)
;;       (org-glance-view:read-header)))

(org-glance-declare org-glance-world:make-partitions :: World -> (or Headline HeadlineHeader) -> t)
(defun org-glance-world:make-partitions (world headline)
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
                           (org-glance-view:get-or-create partition location (org-glance-offset:zero)))))))

(org-glance-declare org-glance-world:clear-partitions :: World -> Headline -> t)
(defun org-glance-world:clear-partitions (world headline)
  "Remove empty partitions from WORLD considering HEADLINE has been deleted."
  (cl-loop with dimensions = (org-glance? world :dimensions)
     for dimension in dimensions
     for predicates = (org-glance-dimension:predicates dimension headline)
     do (cl-loop for predicate in predicates
           for value = (org-glance-dimension:validate predicate headline dimensions)
           when value
           do (thunk-let* ((partition (org-glance-partition
                                       :dimension (format "%s" (org-glance? dimension :name))
                                       :value value))
                           (view-location (org-glance-world:locate-partition world partition))
                           (view-header-location (org-glance-view:locate-header view-location))
                           (view-header (org-glance-view:read-header view-header-location)))
                (message "Clear partition \"%s\"?" (org-glance-partition:representation partition))
                (message "File exists? \"%s\" %s" view-location (f-exists? view-location))
                (message "View size: %d" (or (org-glance? view-header :size) -1))
                (when (and (f-exists? view-location)
                           (= 0 (or (org-glance? view-header :size) -1))
                           (y-or-n-p (format "Partition \"%s\" became empty. Remove? " (org-glance-partition:representation partition))))
                  (org-glance-log :dimensions "Remove derived view %s (%s)" partition view-location)
                  (org-glance! world :partitions := (cl-remove partition (org-glance? world :partitions)))
                  (f-delete (f-parent view-location) t))))))

(org-glance-declare org-glance-world:persist :: World -> Offset)
(defun org-glance-world:persist (world)
  "Apply modifications of WORLD.

This should be the only point to destructively change underlying
persistent storage.

Return last committed offset."
  (let ((changelog (org-glance? world :changelog)))
    (dolist-with-progress-reporter (event (reverse (org-glance? world :changelog* :events)))
        (format "Persist world %s" (org-glance? world :location))
      (thunk-let* ((source-hash (org-glance? event :hash))
                   (target-hash (org-glance? event :headline :hash))
                   (headline-exists? (org-glance-world:headline-exists? world target-hash))
                   (event-headline (org-glance? event :headline))
                   (headline (org-glance-world:get-headline world target-hash)))
        (cl-typecase event
          (org-glance-event:RM (let ((headline (org-glance-world:get-headline world source-hash)))
                                 (org-glance-world:clear-partitions world headline))
                               (org-glance-world:delete-headline! world source-hash)
                               (org-glance-changelog:push changelog event))
          (org-glance-event:PUT (org-glance-world:write-headline world headline)
                                (org-glance-world:make-partitions world headline)
                                (org-glance-changelog:push changelog event))
          (org-glance-event:UPDATE* (org-glance-world:write-headline world event-headline)
                                    (org-glance-world:make-partitions world event-headline)
                                    (org-glance-world:delete-headline! world source-hash)
                                    (org-glance-changelog:push changelog (org-glance-event:UPDATE :hash source-hash
                                                                                                  :headline (org-glance-headline-header:from-headline event-headline))))
          (otherwise (error "Don't know how to handle event of type %s" (type-of event))))))

    (org-glance-world:write-changelog! world)
    (org-glance! world :changelog* := (org-glance-changelog))
    (org-glance-world:write-relations! world)

    (if (org-glance-changelog:last changelog)
        (org-glance? (org-glance-changelog:last changelog) :offset)
      (org-glance-offset:current))))

(org-glance-declare org-glance-world:write-headline :: World -> Headline -> ReadableFile)
(defun org-glance-world:write-headline (world headline)
  "Persist HEADLINE in WORLD."
  (let ((location (org-glance-world:locate-headline world headline)))
    (unless (f-exists-p location)
      (org-glance-headline:save headline location))
    location))

(org-glance-declare org-glance-world:locate-changelog :: World -> OptionalFile)
(defun org-glance-world:locate-changelog (world)
  (f-join (org-glance? world :location) "event.log"))

(org-glance-declare org-glance-world:write-changelog! :: World -> t)
(defun org-glance-world:write-changelog! (world)
  (let ((changelog (org-glance? world :changelog))
        (location (org-glance-world:locate-changelog world)))
    (org-glance-changelog:write changelog location)))

(org-glance-declare org-glance-world:read-changelog! :: World -> Changelog)
(defun org-glance-world:read-changelog! (world)
  (org-glance! world :changelog := (org-glance-changelog:read (org-glance-world:locate-changelog world))))

(org-glance-declare org-glance-world:locate-relations :: World -> OptionalFile)
(defun org-glance-world:locate-relations (world)
  (f-join (org-glance? world :location) "relations.el"))

(org-glance-declare org-glance-world:write-relations! :: World -> t)
(defun org-glance-world:write-relations! (world)
  (org-glance:with-temp-file (org-glance-world:locate-relations world)
    (insert (pp-to-string (org-glance? world :relations)))))

(org-glance-declare org-glance-world:read-relations! :: World -> t)
(defun org-glance-world:read-relations! (world)
  (pcase (org-glance-world:locate-relations world)
    ((and (cl-struct org-glance-readable-file) location) (with-temp-buffer
                                                           (insert-file-contents-literally location)
                                                           (org-glance! world :relations := (read (buffer-string)))))))

(org-glance-declare org-glance-world:delete-headline! :: World -> (or Hash Headline HeadlineHeader) -> t)
(defun org-glance-world:delete-headline! (world headline)
  (condition-case nil
      (f-delete (org-glance-world:locate-headline world headline))
    (error nil)))

(org-glance-declare org-glance-world:add-headline! :: World -> Headline -> World)
(defun org-glance-world:add-headline! (world headline)
  "Put HEADLINE to WORLD."
  (let* ((id (org-glance-world:headline-id world headline))
         (headline (org-glance-headline:with-properties headline
                     `(("GLANCE_ID" ,id)
                       ("DIR" ,(concat "../../../resources/" id))))))
    (org-glance-log :headline "Add headline: %s" headline)
    (org-glance-world:add-headline-to-cache! world id headline)
    (org-glance-world:add-headline-to-changelog! world headline))

  world)

(org-glance-declare org-glance-world:add-headline-to-changelog! :: World -> (or Headline HeadlineHeader) -> t)
(defun org-glance-world:add-headline-to-changelog! (world headline)
  (org-glance-changelog:push (org-glance? world :changelog*)
    (org-glance-event:PUT :headline (org-glance-headline-header:from-headline headline))))

(org-glance-declare org-glance-world:add-headline-to-cache! :: World -> string -> Headline -> Headline)
(defun org-glance-world:add-headline-to-cache! (world id headline)
  (org-glance-log :cache "[org-glance-headline] cache put: \"%s\"" (org-glance? headline :title))
  (puthash id t (org-glance? world :relations))
  (puthash (org-glance? headline :hash) headline (org-glance? world :headlines)))

(org-glance-declare org-glance-world:remove-headline-from-cache! :: World -> Hash -> t)
(defun org-glance-world:remove-headline-from-cache! (world hash)
  (when-let (headline (gethash hash (org-glance? world :headlines)))
    (org-glance-log :cache "Remove headline \"%s\" from the world cache" (org-glance? headline :title))
    (remhash hash (org-glance? world :headlines))))

(org-glance-declare org-glance-world:remove-headline :: World -> Hash -> t)
(defun org-glance-world:remove-headline (world hash)
  "Return `org-glance-world' with HASH removed from WORLD.

Append RM event to WAL, but do not remove HEADLINES from the
persistent storage.

Actual deletion should be handled in a separate thread and
achieved by calling `org-glance-world:persist' method."
  (let ((event (org-glance-event:RM :hash hash))
        (changelog* (org-glance? world :changelog*)))
    (org-glance-changelog:push changelog* event))
  ;; Remove headline from cache only on persist
  ;; (org-glance-world:remove-headline-from-cache! world hash)
  )

(org-glance-declare org-glance-world:update-headline :: World -> Hash -> Headline -> Offset)
(defun org-glance-world:update-headline (world old-hash headline)
  "Update HEADLINE with OLD-HASH in WORLD."
  (let* ((changelog* (org-glance? world :changelog*))
         (event (org-glance-event:UPDATE* :hash old-hash :headline headline))
         (offset (org-glance? event :offset)))
    (org-glance-changelog:push changelog* event)
    offset))

(org-glance-declare org-glance-world:get-headline-from-cache :: World -> Hash -> (Optional Headline))
(defun org-glance-world:get-headline-from-cache (world hash)
  (gethash hash (org-glance? world :headlines)))

(org-glance-declare org-glance-world:get-headline-from-stage :: World -> Hash -> (Optional Headline))
(defun org-glance-world:get-headline-from-stage (world hash)
  (cl-loop for event in (org-glance-changelog:flatten (org-glance? world :changelog*))
     do (cl-typecase event
          (org-glance-event:RM (pcase hash
                                 ((pred (string= (org-glance? event :hash))) (cl-return nil))))
          (org-glance-event:UPDATE (pcase hash
                                     ((pred (string= (org-glance? event :hash))) (cl-return nil))
                                     ((pred (string= (org-glance? event :headline :hash))) (cl-return (org-glance? event :headline)))))
          (org-glance-event:PUT (pcase hash
                                  ((pred (string= (org-glance? event :headline :hash))) (cl-return (org-glance? event :headline))))))))

(org-glance-declare org-glance-world:get-headline-from-drive :: World -> Hash -> (Optional Headline))
(defun org-glance-world:get-headline-from-drive (world hash)
  (org-glance:with-temp-buffer
   (org-glance-log :cache "[org-glance-headline] cache miss: \"%s\"" hash)
   (insert-file-contents (org-glance-world:locate-headline world hash))
   (goto-char (point-min))
   (unless (org-at-heading-p)
     (outline-next-heading))
   (when-let (headline (org-glance-headline-at-point))
     (org-glance-log :cache "[org-glance-headline] cache put: \"%s\"" (org-glance? headline :title))
     (puthash (org-glance? headline :hash) headline (org-glance? world :headlines)))))

(org-glance-declare org-glance-world:get-headline :: World -> Hash -> Headline)
(defun org-glance-world:get-headline (world hash)
  "Return fully qualified `org-glance-headline' by its hash."
  (or (org-glance-world:get-headline-from-cache world hash)
      (org-glance-world:get-headline-from-stage world hash)
      (org-glance-world:get-headline-from-drive world hash)))

(org-glance-declare org-glance-world:events :: World -> (ListOf Event))
(defun org-glance-world:events (world)
  (org-glance-changelog:flatten
   (org-glance-changelog:merge
    (org-glance? world :changelog*)
    (org-glance? world :changelog))))

(org-glance-declare org-glance-world:headlines :: World -> (ListOf HeadlineHeader))
(defun org-glance-world:headlines (world)
  (cl-loop with removed = (make-hash-table :test #'equal)
     for event in (org-glance-world:events world)
     when (cl-typecase event
            ((or org-glance-event:PUT org-glance-event:UPDATE)
             (not (gethash (org-glance? event :headline :hash) removed))))
     collect (org-glance? event :headline)
     when (cl-typecase event
            ((or org-glance-event:RM org-glance-event:UPDATE) t))
     do (puthash (org-glance? event :hash) t removed)))

(org-glance-declare org-glance-world:headline-id :: World -> (or Headline HeadlineHeader) -> string)
(defun org-glance-world:headline-id (world headline)
  (cl-labels ((uniquify (id ids &optional (tryout 0))
                (let ((try (if (> tryout 0)
                               (format "%s_%d" id tryout)
                             id)))
                  (cond ((gethash try ids) (uniquify id ids (1+ tryout)))
                        (t try))))
              (truncate (len s ellipsis)
                (declare (pure t) (side-effect-free t))
                (if (> (length s) len)
                    (format "%s%s" (substring s 0 (- len (length ellipsis))) ellipsis)
                  s)))
    (uniquify (s-join "_" (list (format-time-string "%Y-%m-%d")
                                (--> (org-glance? headline :title)
                                     (replace-regexp-in-string "[^a-z0-9A-Z_]" "_" it)
                                     (replace-regexp-in-string "\\-+" "-" it)
                                     (replace-regexp-in-string "\\-+$" "" it)
                                     (truncate 30 it ""))))
              (org-glance? world :relations))))

(org-glance-declare org-glance-world:locate-headline :: World -> (or Hash Headline HeadlineHeader) -> OptionalFile)
(defun org-glance-world:locate-headline (world headline)
  "Return location of HEADLINE in WORLD."

  (cl-typecase headline
    ((or org-glance-headline org-glance-headline-header)
     (org-glance-world:locate-headline world (org-glance? headline :hash)))
    (string (let ((prefix (substring headline 0 2))
                  (postfix (format "%s.org" (substring headline 2 (length headline)))))
              (f-join (org-glance? world :location) "data" prefix postfix)))))

(org-glance-declare org-glance-world:headline-exists? :: World -> (or Hash Headline HeadlineHeader) -> boolean)
(defun org-glance-world:headline-exists? (world headline)
  (f-exists? (org-glance-world:locate-headline world headline)))

(org-glance-declare org-glance-world:make-predicate :: World -> Partition -> list)
(defun org-glance-world:make-predicate (world partition)
  (cl-loop for dimension in (org-glance? world :dimensions)
     when (string= (org-glance? partition :dimension) (format "%s" (org-glance? dimension :name)))
     return (org-glance-dimension:make-predicate dimension (org-glance? partition :value))))

(org-glance-declare org-glance-world:validate-headline :: World -> Partition -> HeadlineHeader -> (Optional string))
(defun org-glance-world:validate-headline (world partition headline)
  (let ((predicate (org-glance-world:make-predicate world partition)))
    (org-glance-dimension:validate predicate headline (org-glance? world :dimensions))))

(org-glance-declare org-glance-world:partition-headlines :: World -> Partition -> (ListOf HeadlineHeader))
(defun org-glance-world:partition-headlines (world partition)
  (declare (indent 1))
  (let ((predicate (org-glance-world:make-predicate world partition))
        (headlines (org-glance-world:headlines world)))
    (cl-loop for headline in headlines
       when (org-glance-dimension:validate predicate headline (org-glance? world :dimensions))
       collect headline)))

(org-glance-declare org-glance-world:root :: (Optional string) -> (Optional WorldLocation))
(defun org-glance-world:root (location)
  (pcase location
    ((guard (null location)) nil)
    ((cl-struct org-glance-world-location) location)
    ((cl-struct string) (org-glance-world:root (f-parent location)))))

(provide 'org-glance-world-model)
