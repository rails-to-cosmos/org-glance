;; -*- lexical-binding: t; -*-

(require 'a)
(require 'f)
(require 's)
(require 'ol)

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
       (org-glance-log :cache "World has been retrieved from cache (org-glance-worlds)")
       cached-world)
      (location-exists?
       (org-glance-log :cache "Read world from \"%s\"" location)
       (puthash location persisted-world org-glance-worlds))
      (t
       (f-mkdir-full-path location)
       (org-glance-log :cache "World has been created from scratch (org-glance-worlds)")
       (puthash location new-world org-glance-worlds)))))

(cl-defun org-glance-world:read (location)
  "Read `org-glance-world' from LOCATION."
  (or (when-let (world (gethash location org-glance-worlds))
        (org-glance-log :cache "World has been retrieved from cache (org-glance-worlds)")
        world)
      (progn
        (org-glance-log :cache "Read world from hard drive")
        (puthash location
                 (org-glance-world :location location
                                   :changelog (org-glance-log :performance
                                                  (org-glance-changelog:read (f-join location "log" "event.log"))))
                 org-glance-worlds))))

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
  (let* ((changelog (org-glance- world :changelog))
         (changelog* (org-glance- world :changelog*))
         (changelog-location (f-join (org-glance- world :location) "log" "event.log")))

    (dolist (event (reverse (org-glance- changelog* :events)))
      (thunk-let ((headline (org-glance-world:get-headline world (org-glance- event :headline :hash))))
        (cl-typecase event
          (org-glance-event:RM
           (org-glance-log :event "Handle RM event: %s" event)
           (user-error "RM operation has not been implemented yet")
           ;; TODO think about when to delete headlines
           ;; (f-delete (org-glance-world:locate-headline world headline))
           )

          (org-glance-event:PUT
           (org-glance-log :event "Handle PUT event: %s" event)
           (org-glance-log :headline "Generate headline id: %s" (org-glance-world:generate-headline-id world headline))
           (org-glance-world:save-headline world headline)
           )

          (org-glance-event:UPDATE
           (org-glance-log :event "Handle UPDATE event: %s" event)
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
    (org-glance-log :cache "Put headline \"%s\" to the world cache" (org-glance- headline :title))
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
     (org-glance-log :cache "Retrieve headline from cache (hashmap): \"%s\"" (org-glance- result :title))
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
     (org-glance-log :cache "Retrieve headline from cache (changelog*): \"%s\"" result)
     result)

   ;; Search persistent storage
   (org-glance:with-temp-buffer
    (org-glance-log :cache "Retrieve headline from drive: %s" hash)
    (insert-file-contents (org-glance-world:locate-headline world hash))
    (goto-char (point-min))
    (unless (org-at-heading-p)
      (outline-next-heading))
    (when-let (headline (org-glance-headline-at-point))
      (org-glance-log :cache "Put headline \"%s\" to the world cache" (org-glance- headline :title))
      (puthash (org-glance- headline :hash) headline (org-glance- world :cache))))))

(cl-defun org-glance-world:events (world)
  (org-glance-changelog:flatten
   (org-glance-changelog:merge
    (org-glance- world :changelog*)
    (org-glance- world :changelog))))

(cl-defun org-glance-world:get-headlines (world)
  "Return actual headline hashes from WORLD."
  (cl-loop
     with removed = (make-hash-table :test #'equal)
     for event in (org-glance-log :performance (org-glance-world:events world))
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
      (org-glance-world:dim-split world headline)
      (org-glance-headline:save headline location))
    location))

(cl-defun org-glance-world:generate-headline-id (world headline)
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

(cl-defun org-glance-world:insert-headline (world headline)
  "Return location of THING in WORLD."
  (thread-first world
    (org-glance-world:get-headline (org-glance- headline :hash))
    (org-glance-headline:insert)))

(cl-defun org-glance-world:locate-headline (world thing)
  "Return location of THING in WORLD."
  (cl-typecase thing
    (string (let ((prefix (substring thing 0 2))
                  (postfix (substring thing 2 (length thing))))
              (f-join (org-glance- world :location) "data" prefix postfix)))
    ((or org-glance-headline org-glance-headline-header)
     (org-glance-world:locate-headline world (org-glance- thing :hash)))))

(cl-defun org-glance-world:dim-eval (world headline)
  (cl-loop for (dimension . partition-by) in (org-glance- world :dimensions)
     collect (cons dimension (let ((result (eval partition-by (a-list 'headline headline))))
                               (cond ((atom result) (list result))
                                     (t result))))))

(cl-defun org-glance-world:dim-split (world headline)
  (cl-loop
     for (dimension . partitions) in (org-glance-world:dim-eval world headline)
     do (dolist (partition partitions)
          (when (and partition (not (string-empty-p (format "%s" partition))))
            (let ((predicate (cl-typecase partition
                               (symbol `(member (quote ,partition) ,dimension))
                               (t `(member ,partition ,dimension))))
                  (location (f-join (org-glance- world :location)
                                    "dimensions"
                                    (downcase (format "%s" dimension))
                                    (format "%s.org" partition))))
              (org-glance-log :dimension "Create derived view \"%s -> %s\"" dimension partition)
              (org-glance-view:create world predicate location nil (org-glance-offset:zero)))))))

(cl-defun org-glance-world:list-dimensions (world)
  (cl-loop for (dimension . _) in (org-glance- world :dimensions)
     for location = (f-join (org-glance- world :location) "dimensions" (downcase (format "%s" dimension)))
     when (and (f-exists? location) (f-readable? location))
     append (--map (format "%s=%s"
                           (downcase (format "%s" dimension))
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

(cl-defun org-glance-world:browse (world &optional (dimension (org-glance-world:choose-dimension world)))
  (let ((location (org-glance-world:locate-dimension world dimension)))
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

(cl-defun org-glance-world:jump (world)
  (let* ((headlines (cl-loop for headline in (org-glance-log :performance (org-glance-world:get-headlines world))
                       when (org-glance- headline :linked?)
                       collect (cons (org-glance- headline :title) (org-glance- headline :hash))))
         (title (completing-read "Jump to headline: " headlines))
         (hash (a-get headlines title))
         (headline (org-glance-world:get-headline world hash))
         (links (org-glance- headline :links))
         (link (cond ((> (length links) 1) (let ((link-title (completing-read "Choose link to open: " (--map (org-glance- it :title) links))))
                                             (--drop-while (not (string= link-title (org-glance- it :title))) links)))
                     ((= (length links) 1) (car links))
                     (t (user-error "Unable to find links in this headline")))))
    (org-link-open-from-string (org-glance- link :org-link))))

(provide 'org-glance-world)
