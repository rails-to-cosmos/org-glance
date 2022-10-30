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

(defvar org-glance-worlds (make-hash-table :test #'equal) "List of worlds registered in system.")

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

(cl-defun org-glance-world:get-or-create (location)
  "Read `org-glance-world' from LOCATION."
  (or (org-glance-world:get-from-cache location)
      (org-glance-world:read location)
      (org-glance-world:create location)))

(cl-defun org-glance-world:get-from-cache (location)
  (when-let (world (gethash (file-truename location) org-glance-worlds))
    (org-glance-log :cache "[org-glance-world] cache hit: org-glance-worlds")
    world))

(cl-defun org-glance-world:put-to-cache (world)
  (org-glance-log :cache "[org-glance-world] cache put: org-glance-worlds")
  (puthash (org-glance- world :location) world org-glance-worlds))

(cl-defun org-glance-world:read (location)
  (org-glance-log :cache "[org-glance-world] cache miss: %s" location)
  (let ((world (org-glance-world:create location)))
    (setf (org-glance- world :changelog) (org-glance-log :performance
                                             (org-glance-changelog:read (f-join location "log" "event.log"))))
    world))

(cl-defun org-glance-world:create (location)
  "Create world located in directory LOCATION."
  (declare (indent 1))
  (let ((location (file-truename location)))
    (org-glance-log :cache "[org-glance-world] cache miss: create from scratch")
    (f-mkdir-full-path location)
    (f-touch (f-join location "org-glance-world.md"))
    (org-glance-world:put-to-cache
     (org-glance-world :location location))))

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
    (org-glance-log :world "Persist changes")
    (dolist (event (reverse (org-glance- changelog* :events)))
      (org-glance-log :world "Process %s" event)
      (thunk-let ((headline (org-glance-world:get-headline world (org-glance- event :headline :hash))))
        (cl-typecase event
          (org-glance-event:RM
           (user-error "RM operation has not been implemented yet")
           ;; TODO think about when to delete headlines
           ;; (f-delete (org-glance-world:locate-headline world headline))
           )

          (org-glance-event:PUT
           (org-glance-log :world "Generate headline id: %s" (org-glance-world:generate-headline-id world headline))
           (org-glance-log :world "Save headline: %s" headline)
           (org-glance-world:save-headline world headline))

          (org-glance-event:UPDATE
           (org-glance-log :world "Save headline: %s" headline)
           (org-glance-world:save-headline world headline)
           ;; TODO think about when to delete headlines
           ;; (f-delete (org-glance-world:locate-headline world (org-glance- event :hash))
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
      (org-glance-log :world "Actual world offset: %s" (org-glance-world:offset world))
      offset)))

(cl-defun org-glance-world:add-headline (world headline)
  "Put HEADLINE to WORLD."
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
  (dolist-with-progress-reporter (file (org-glance-scope location))
      "Importing headlines"
    (org-glance:with-temp-buffer
     (insert-file-contents file)
     (org-glance-headline:map (headline)
       (org-glance-world:add-headline world headline)))))

(cl-defun org-glance-world:save-headline (world headline)
  (let ((location (org-glance-world:locate-headline world headline)))
    (unless (f-exists-p location)
      (org-glance-headline:save headline location)
      (org-glance-world:apply-dimensions world headline))
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

(cl-defun org-glance-world:locate-dimension (world dimension)
  (f-join (org-glance- world :location)
          "dimensions"
          (format "%s.org" (downcase dimension))))

(cl-defun org-glance-world:evaluate-dimensions (world headline)
  (cl-loop for (dimension . partition-by) in (org-glance- world :dimensions)
     collect (cons dimension (let ((result (eval partition-by (a-list 'headline headline))))
                               (cond ((atom result) (list result))
                                     (t result))))))

(cl-defun org-glance-world:apply-dimensions (world headline)
  (cl-loop
     for (dimension . partitions) in (org-glance-world:evaluate-dimensions world headline)
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

(cl-defun org-glance-world:list-dimensions (world)
  (--map (file-name-sans-extension it)
         (--filter (member (file-name-extension it) org-glance-scope-extensions)
                   (directory-files (f-join (org-glance- world :location) "dimensions")))))

(cl-defun org-glance-world:choose-dimension (world)
  (completing-read "Choose dimension: "
                   (org-glance-world:list-dimensions world)
                   nil
                   t))

(cl-defun org-glance-world:update-dimension (world dim)
  (let* ((view-location (org-glance-world:locate-dimension world dim))
         (view-header (thread-first view-location
                        (org-glance-view:get-header-location-by-view-location)
                        (org-glance-view:read-header)))
         (view (org-glance-view :world world
                                :type (a-get view-header :type)
                                :location view-location
                                :offset (a-get view-header :offset)))
         (world-offset (org-glance-world:offset world)))

    (org-glance-log :events "[world] Fetch view %s" dim)
    (org-glance-log :offsets "[%s] View offset = %s" (org-glance- view :type) (org-glance- view :offset))
    (org-glance-log :offsets "[%s] World offset = %s" (org-glance- view :type) (org-glance-world:offset world))
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

(cl-defun org-glance-world:browse (world &optional (dimension (org-glance-world:choose-dimension world)))
  (find-file (org-glance-world:update-dimension world dimension)))

(cl-defun org-glance-world:agenda (world)
  (let* ((dimension (org-glance-world:choose-dimension world))
         (location (org-glance-world:locate-dimension world dimension)))
    (let ((lexical-binding nil))
      (let ((org-agenda-files (list location))
            (org-agenda-overriding-header "org-glance agenda")
            (org-agenda-start-on-weekday nil)
            (org-agenda-span 21)
            (org-agenda-start-day "-7d"))
        (org-agenda-list)))))

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
  (let ((file (f-join (org-glance- world :location) "capture.org")))
    (delete-file file)
    (find-file file)
    (add-hook 'org-capture-after-finalize-hook 'org-glance-capture:after-finalize-hook 0 t)
    (let ((lexical-binding nil))
      (let ((org-capture-templates (list (list "_" "Thing" 'entry (list 'file file) template))))
        (org-capture nil "_")))
    ;; (when finalize
    ;;   (org-capture-finalize))
    ))

(cl-defun org-glance-capture:after-finalize-hook ()
  "Register captured headline in metastore."
  (let ((world (org-glance-world:current)))
    (org-glance-headline:map (headline)
      (org-glance-world:add-headline world headline))
    (org-glance-world:persist world)
    (let ((file (buffer-file-name)))
      (kill-buffer (get-file-buffer file))
      (delete-file file))))

(cl-defun org-glance-world:filter-headlines (world &optional predicate)
  "TODO cache headlines by predicate."
  (declare (indent 1))
  (cl-loop for headline in (org-glance-log :performance
                               (org-glance-world:get-headlines world))
     when (or (null predicate) (funcall predicate headline))
     collect (cons (org-glance- headline :title) (org-glance- headline :hash))))

(cl-defun org-glance-world:choose-headline (world &optional predicate)
  (declare (indent 1))
  (let ((headlines (org-glance-log :performance
                       (org-glance-world:filter-headlines world predicate))))
    (thread-last (completing-read "Choose headline: " headlines)
      (a-get headlines)
      (org-glance-world:get-headline world))))

(cl-defun org-glance-world:jump (world)
  (let* ((headline (org-glance-world:choose-headline world #'(lambda (headline) (org-glance- headline :linked?))))
         (links (org-glance- headline :links))
         (link (cond ((> (length links) 1) (let ((link-title (completing-read "Choose link to open: " (--map (org-glance- it :title) links))))
                                             (--drop-while (not (string= link-title (org-glance- it :title))) links)))
                     ((= (length links) 1) (car links))
                     (t (user-error "Unable to find links in this headline")))))
    (org-link-open-from-string (org-glance- link :org-link))))

(cl-defun org-glance-world:extract (world)
  (let* ((headline (org-glance-world:choose-headline world #'(lambda (headline) (org-glance- headline :store?))))
         (store (org-glance- headline :store)))
    (condition-case nil
        (while t
          (kill-new (alist-get (org-completing-read "Extract property (press C-g to exit): " store) store nil nil #'string=)))
      (quit
       (setq kill-ring nil)
       (org-glance-log :info "Kill ring has been cleared")))))

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

(cl-defun org-glance-world:get-root-directory (location)
  (cl-typecase location
    (org-glance-world:location location)
    (otherwise (org-glance-world:get-root-directory (f-parent location)))))

(cl-defun org-glance-world:current ()
  "Get `org-glance-world' associated with current buffer."
  (thread-first (buffer-file-name)
    (org-glance-world:get-root-directory)
    (org-glance-world:get-or-create)))

(provide 'org-glance-world)
