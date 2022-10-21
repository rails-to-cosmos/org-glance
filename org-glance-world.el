;; -*- lexical-binding: t; -*-

(require 'a)
(require 'f)
(require 's)

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

(defvar org-glance-worlds (make-hash-table :test #'equal) "List of worlds registered in system.")
(defvar org-glance-current-world nil "Current `org-glance-world'.")
(defvar-local org-glance-local-world nil "World used in temporary buffers.")

(org-glance-class org-glance-dimension nil
    ((name
      :type string
      :initarg :name
      :documentation "Dimension name.")
     (partition
      :type (or list symbol)
      :initarg :partition
      :documentation "Partition method.")
     (read-only
      :type boolean
      :initarg :read-only
      :initform nil
      :documentation "Enable read-only mode and restrict commit in derived views")))

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
     (dimensions
      :type hash-table
      :initarg :dimensions
      :initform (make-hash-table :test #'equal))
     (views
      :type hash-table
      :initarg :views
      :initform (make-hash-table :test #'equal)
      :documentation "Views associated with world by type.")))

(cl-defun org-glance-world:create (location)
  "Create world located in directory LOCATION."
  (declare (indent 1))
  (thunk-let ((cached-world (gethash location org-glance-worlds))
              (persisted-world (org-glance-world:read location))
              (new-world (org-glance-world :location location))
              (location-exists? (and (f-exists? location)
                                     (f-directory? location)
                                     (f-readable? location))))
    (cond
      ((and location-exists? cached-world)
       cached-world)
      (location-exists?
       (puthash location persisted-world org-glance-worlds))
      (t
       (f-mkdir-full-path location)
       (puthash location new-world org-glance-worlds)))))

(cl-defun org-glance-world:read (location)
  "Read `org-glance-world' from LOCATION."
  (or (gethash location org-glance-worlds)
      (puthash location
               (org-glance-world :location location
                                 :changelog (org-glance-changelog:read (f-join location "log" "event.log"))
                                 :dimensions (org-glance-world:load-dimensions location))
               org-glance-worlds)))

(cl-defun org-glance-world:offset (world)
  (if-let (event (org-glance-changelog:last (org-glance- world :changelog)))
      (org-glance- event :offset)
    (org-glance-offset:current)))

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
  (org-glance-world:save-dimensions world)
  (let* ((changelog (org-glance- world :changelog))
         (changelog* (org-glance- world :changelog*))
         (world-location (org-glance- world :location))
         (changelog-location (f-join world-location "log" "event.log")))

    (dolist (event (reverse (org-glance- changelog* :events)))
      (thunk-let ((headline (org-glance-world:get-headline world (org-glance- event :headline :hash))))
        (cl-typecase event
          (org-glance-event:RM
           (user-error "RM operation has not been implemented yet")
           ;; TODO think about when to delete headlines
           ;; (f-delete (org-glance-world:locate-headline world headline))
           )

          (org-glance-event:PUT
           (org-glance-world:save-headline world headline))

          (org-glance-event:UPDATE
           (org-glance-world:save-headline world headline)
           ;; TODO think about when to delete headlines
           ;; (f-delete (org-glance-world:locate-headline world (org-glance- event :hash))
           )))

      (org-glance-changelog:push changelog event))

    (org-glance-changelog:write changelog changelog-location)
    (setf (org-glance- world :changelog*) (org-glance-changelog))
    (if (org-glance-changelog:last changelog)
        (org-glance- (org-glance-changelog:last changelog) :offset)
      (org-glance-offset:current))))

(cl-defun org-glance-world:add-headline (world headline)
  "Put HEADLINE to WORLD."
  (let ((event (org-glance-event:PUT :headline (org-glance-headline-header:from-headline headline))))
    (org-glance-changelog:push (org-glance- world :changelog*) event)
    (puthash (org-glance- headline :hash) headline (org-glance- world :cache))
    world))

(cl-defun org-glance-world:remove (world hash)
  "Return `org-glance-world' with HEADLINES removed from WORLD.

Append RM event to WAL, but do not remove HEADLINES from the
persistent storage.

Actual deletion should be handled in a separate thread and
achieved by calling `org-glance-world:persist' method."
  (let ((event (org-glance-event:RM :hash hash)))
    (org-glance-changelog:push (org-glance- world :changelog*) event)
    (remhash hash (org-glance- world :cache))))

(cl-defun org-glance-world:update (world old-hash headline)
  "Update HEADLINE with HASH in WORLD."
  (let* ((new-hash (org-glance- headline :hash))
               (header (org-glance-headline-header:from-headline headline))
               (changelog (org-glance- world :changelog*))
               (cache (org-glance- world :cache))
               (event (org-glance-event:UPDATE
                       :hash old-hash
                       :headline header))
               (offset (org-glance- event :offset)))
    (org-glance-changelog:push changelog event)
    (puthash new-hash headline cache)
    (remhash old-hash cache)
    offset))

(cl-defun org-glance-world:get-headline (world hash)
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
       (insert-file-contents (org-glance-world:locate-headline world hash))
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
      (and (f-exists? (org-glance-world:locate-headline world hash))
           (f-readable? (org-glance-world:locate-headline world hash)))))

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

;; World headlines manipulation methods

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

(cl-defun org-glance-world:import-headlines (world location)
  "Add headlines from LOCATION to WORLD."
  (cl-dolist (file (org-glance-scope location))
    (org-glance:with-temp-buffer
     (insert-file-contents file)
     (org-glance-headline:map (headline)
       (org-glance-world:add-headline world headline)))))

(cl-defun org-glance-world:save-headline (world headline)
  (let ((location (org-glance-world:locate-headline world headline)))
    (unless (f-exists-p location)
      (org-glance-world:apply-dimensions world headline)
      (org-glance-headline:save headline location))))

(cl-defun org-glance-world:insert-headline (world headline)
  "Return location of THING in WORLD."
  (org-glance-headline:insert (org-glance-world:get-headline world (org-glance- headline :hash))))

(cl-defun org-glance-world:locate-headline (world thing)
  "Return location of THING in WORLD."
  (cl-typecase thing
    (string
     (let ((prefix (substring thing 0 2))
           (postfix (substring thing 2 (length thing))))
       (f-join (org-glance- world :location) "data" prefix postfix)))
    ((or org-glance-headline org-glance-headline-header)
     (org-glance-world:locate-headline world (org-glance- thing :hash)))))

(cl-defun org-glance-world:save-dimensions (world)
  (let ((location (f-join (org-glance- world :location) "dimensions"))
        (filename "dimensions.el"))
    (f-mkdir-full-path location)
    (with-temp-file (f-join location filename)
      (insert (prin1-to-string (org-glance- world :dimensions))))))

(cl-defun org-glance-world:load-dimensions (location)
  (org-glance-log:debug "Load dimensions from \"%s\"" location)
  (let ((source-file (f-join location "dimensions" "dimensions.el")))
    (if (and (f-exists? source-file) (f-readable? source-file))
        (with-temp-buffer
          (insert-file-contents-literally source-file)
          (read (buffer-string)))
      (org-glance-log:debug "Dimensions not found in \"%s\"" location)
      (make-hash-table :test #'equal))))

(cl-defun org-glance-world:add-dimension (world dimension)
  (declare (indent 1))
  (puthash (downcase (org-glance- dimension :name))
           dimension
           (org-glance- world :dimensions))
  world)

(cl-defun org-glance-world:apply-dimensions (world headline)
  (thunk-let ((context (org-glance-headline:eval-ctx headline)))
    (dolist (dimension (hash-table-values (org-glance- world :dimensions)))
      (dolist (partition (eval (org-glance- dimension :partition) context))
        (when (and partition (not (string-empty-p (format "%s" partition))))
          (let* ((name (org-glance- dimension :name))
                 (predicate `(member (quote ,partition) ,(org-glance- dimension :partition)))
                 (location (f-join (org-glance- world :location)
                                   "dimensions"
                                   (downcase name)
                                   (format "%s.org" partition))))
            (org-glance-log:scenario "Create derived view \"%s -> %s\""
              (org-glance- dimension :name)
              partition)
            (org-glance-view:create world predicate location nil (org-glance-offset:zero))))))))

(cl-defun org-glance-world:list-dimensions (world)
  (cl-loop for dimension being the hash-keys of (org-glance- world :dimensions)
     for location = (f-join (org-glance- world :location) "dimensions" (downcase dimension))
     when (and (f-exists? location) (f-readable? location))
     append (--map (format "%s=%s"
                           (downcase dimension)
                           (file-name-sans-extension it))
                   (--filter (member (file-name-extension it) org-glance-scope-extensions)
                             (directory-files location)))))

(cl-defun org-glance-world:choose-dimension (world)
  (completing-read "Choose dimension: "
                   (org-glance-world:list-dimensions world)
                   nil
                   t))

(cl-defun org-glance-world:locate-dimension (world dimension)
  (f-join (org-glance- world :location)
          "dimensions"
          (car (s-split "=" dimension))
          (format "%s.org" (cadr (s-split "=" dimension)))))

(cl-defun org-glance-world:browse (world)
  (let* ((dimension (org-glance-world:choose-dimension world))
         (location (org-glance-world:locate-dimension world dimension)))
    (find-file location)))

(cl-defun org-glance-world:agenda (world)
  (let* ((dimension (org-glance-world:choose-dimension world))
         (location (org-glance-world:locate-dimension world dimension))
         (org-agenda-files (list location))
         (org-agenda-overriding-header "org-glance agenda")
         (org-agenda-start-on-weekday nil)
         (org-agenda-span 21)
         (org-agenda-start-day "-7d"))
    (org-agenda-list)))

(cl-defun org-glance-world:capture (world
                                    &key
                                      (template "* %?")
                                      ;; (_ (cond ((use-region-p) (buffer-substring-no-properties
                                      ;;                           (region-beginning)
                                      ;;                           (region-end)))
                                      ;;          (t "")))
                                      ;; finalize
                                      )
  (declare (indent 1))
  (let ((file (make-temp-file "org-glance-" nil ".org")))
    (find-file file)
    (setq-local org-glance-local-world world)
    (add-hook 'org-capture-after-finalize-hook 'org-glance-capture:after-finalize-hook 0 t)

    (let ((org-capture-templates (list (list "_" "Thing" 'entry (list 'file file) template))))
      (org-capture nil "_"))

    ;; (when finalize
    ;;   (org-capture-finalize))
    ))

(cl-defun org-glance-capture:after-finalize-hook ()
  "Register captured headline in metastore."
  (org-glance-headline:map (headline)
    (org-glance-log:debug "Register headline \"%s\" in world \"%s\" "
      (org-glance- headline :title)
      org-glance-local-world)
    (org-glance-world:add-headline org-glance-local-world headline))

  (org-glance-world:persist org-glance-local-world)

  (let ((file (buffer-file-name)))
    (org-glance-log:debug "Remove temp file \"%s\"" file)
    (kill-buffer (get-file-buffer file))
    (delete-file file)))

(provide 'org-glance-world)
