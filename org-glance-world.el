;; -*- lexical-binding: t; -*-

(require 'a)
(require 'f)
(require 's)
(require 'ol)
(require 'org)
(require 'org-agenda)
(require 'org-capture)

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

(defconst org-glance-world--cache (make-hash-table :test #'equal) "List of worlds registered in system.")

(org-glance-class org-glance-world nil
    ((location :type WorldLocation :initarg :location :documentation "Directory containing all the data.")
     (changelog* :type Changelog :initarg :changelog* :initform (org-glance-changelog) :documentation "In-memory changelog.")
     (changelog :type Changelog :initarg :changelog :initform (org-glance-changelog) :documentation "Persistent changelog.")
     (dimensions :type (ListOf Dimension) :initarg :dimensions)

     ;; in-memory caches
     (partitions :type (ListOf Partition) :initarg :partitions :initform nil)
     (headlines :type hash-table :initarg :headlines :initform (make-hash-table :test #'equal) :documentation "HASH -> HEADLINE")
     (relations :type hash-table :initarg :relations :initform (make-hash-table :test #'equal) :documentation "ID -> HEADLINES")))

(org-glance-declare org-glance-world:init :: OptionalDirectory -> World)
(defun org-glance-world:init (location)
  "Get or create `org-glance-world' from LOCATION."
  (->> location
       (file-truename)
       (funcall (-orfn #'org-glance-world--cache-get
                       (-compose #'org-glance-world--cache-put #'org-glance-world--read)
                       (-compose #'org-glance-world--cache-put #'org-glance-world--create)))))

(org-glance-declare org-glance-world:materialize :: World -> (Optional Partition) -> t)
(cl-defun org-glance-world:materialize (world partition)
  "Find `org-mode' file containing headlines of WORLD and PARTITION."
  (pcase partition
    ((cl-struct org-glance-partition) (find-file (org-glance-world:updated-partition world partition)))))

(org-glance-declare org-glance-world:agenda :: World -> t)
(defun org-glance-world:agenda (world)
  "Show agenda for all active headlines of WORLD."
  (pcase (if current-prefix-arg
             (org-glance-world:choose-partition world)
           (org-glance-partition:create "active" "t"))
    ((and (cl-struct org-glance-partition) partition)
     (progn
       (setq org-agenda-files (list (org-glance-world:updated-partition world partition))
             org-agenda-overriding-header "org-glance agenda"
             org-agenda-start-on-weekday nil
             org-agenda-span 21
             org-agenda-start-day "-7d")
       (org-agenda-list)))))

(org-glance-declare org-glance-world:current :: World)
(defun org-glance-world:current ()
  "Get `org-glance-world' associated with current buffer."
  (or (-some-> (buffer-file-name)
        (org-glance-world:root)
        (org-glance-world:init))
      (user-error "Buffer is not a member of world: %s" (buffer-file-name))))

(org-glance-declare org-glance-world:capture-location :: World -> OptionalFile)
(defun org-glance-world:capture-location (world)
  (f-join (org-glance? world :location) "capture.org"))

(cl-defun org-glance-world:capture (world &key
                                            (template "* %?")
                                            (text (cond ((use-region-p) (buffer-substring-no-properties
                                                                         (region-beginning)
                                                                         (region-end)))
                                                        (t "")))
                                            finalize)
  (declare (indent 1))
  (cl-check-type world org-glance-world)

  (let ((file (org-glance-world:capture-location world)))
    (delete-file file)
    (find-file file)
    (add-hook 'org-capture-after-finalize-hook 'org-glance-world--after-finalize-hook 0 t)
    (let ((lexical-binding nil))
      (let ((org-capture-templates (list (list "_" "Thing" 'entry (list 'file file) template))))
        (org-capture nil "_")
        (insert text)))
    (when finalize
      (org-capture-finalize))))

(org-glance-declare org-glance-world:dummy-headlines :: World -> Partition -> (ListOf Cons))
(defun org-glance-world:dummy-headlines (world partition)
  (--map (cons (org-glance? it :title) (org-glance? it :hash))
         (org-glance-world:partition-headlines world partition)))

(org-glance-declare org-glance-world:choose-headline :: World -> Partition -> (Optional HeadlineHeader))
(defun org-glance-world:choose-headline (world partition)
  "Ask user to choose headline from WORLD using PARTITION to filter list."
  (declare (indent 1))
  (let ((dummies (org-glance-world:dummy-headlines world partition)))
    (->> (completing-read (format "Choose headline (%s): " (org-glance-partition:representation partition)) dummies)
         (a-get dummies)
         (org-glance-world:get-headline world))))

(org-glance-declare org-glance-world:jump :: World -> t)
(defun org-glance-world:jump (world)
  "Select headline from WORLD and emulate link opening."
  (let* ((partition (org-glance-partition:create "linked" "t"))
         (headline (org-glance-world:choose-headline world partition))
         (links (org-glance? headline :links))
         (link (cond ((> (length links) 1) (let ((link-title (completing-read "Choose link to open: " (--map (org-glance? it :title) links))))
                                             (--drop-while (not (string= link-title (org-glance? it :title))) links)))
                     ((= (length links) 1) (car links))
                     (t (user-error "Unable to find links in this headline")))))
    (org-link-open-from-string (org-glance? link :org-link))))

(org-glance-declare org-glance-world:extract-property :: World -> t)
(defun org-glance-world:extract-property (world)
  (let* ((partition (org-glance-partition:create "extractable" "t"))
         (headline (org-glance-world:choose-headline world partition))
         (store (org-glance? headline :store)))
    (condition-case nil
        (while t
          (kill-new (alist-get (org-completing-read "Extract property (press C-g to exit): " store) store nil nil #'string=)))
      (quit
       (setq kill-ring nil)
       (org-glance-log :info "Kill ring has been cleared")))))

(org-glance-declare org-glance-world:choose-partition :: World -> (org-glance-optional string) -> t)
(defun org-glance-world:choose-partition (world &optional dimension)
  (let* ((partitions (cl-typecase dimension
                       (string (--filter (string= (org-glance? it :dimension) dimension)
                                         (org-glance-world:partitions world)))
                       (otherwise (org-glance-world:partitions world))))
         (reprs (--map (org-glance-partition:representation it) partitions)))
    (when-let (choice (condition-case nil
                          (if reprs
                              (completing-read "Choose partition: " reprs nil t)
                            (user-error "Partitions not found"))
                        (quit nil)))
      (cl-destructuring-bind (dimension value) (--> choice (s-split-up-to "=" it 2))
        (org-glance-partition:create dimension value)))))

(org-glance-declare org-glance-world:partition-view :: World -> Partition -> View)
(defun org-glance-world:partition-view (world partition)
  (let* ((location (org-glance-world--locate-partition world partition))
         (metadata (-> location (org-glance-view:locate-header) (org-glance-view:read-header)))
         (type (org-glance? metadata :type))
         (offset (org-glance? metadata :offset)))
    (org-glance-view:get-or-create type location offset)))

(cl-defmacro org-glance-world:with-locked-partition (world partition &rest forms)
  (declare (indent 2))
  `(let ((location (org-glance-world--locate-partition ,world ,partition)))
     (when (--> location
                (get-file-buffer it)
                (cond ((null it) t)
                      ((buffer-live-p it) (kill-buffer it))))
       (org-glance:with-file-overwrite location
         ,@forms))))

(defun org-glance-world:partition-header (world partition)
  (with-temp-buffer
    (insert-file-contents (-> (org-glance-world--locate-partition world partition)
                              (file-name-sans-extension)
                              (concat org-glance-view-header-extension)))
    (read (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-glance-world--locate-partition-changelog (world partition)
  (f-join (org-glance? world :location)
          "dimensions"
          (org-glance-partition:path partition)
          (format "%s.log" (org-glance-partition:representation partition))))

(cl-defmacro org-glance-world:fetch-partition-updates (params &rest forms)
  (declare (indent 1))
  (cl-destructuring-bind (world partition event) params
    `(thunk-let* ((world ,world)
                  (partition ,partition)
                  (partition-header (org-glance-world:partition-header world partition))
                  (partition-offset (org-glance? partition-header :offset))
                  (partition-changelog-location (org-glance-world--locate-partition-changelog world partition))
                  (partition-changelog (org-glance-changelog:read partition-changelog-location))
                  (world-events (reverse (--take-while (org-glance-offset:less? partition-offset (org-glance? it :offset))
                                                       (org-glance-world:events world))))
                  (new-events (--filter (pcase it
                                          ((and (or (cl-struct org-glance-event:UPDATE)
                                                    (cl-struct org-glance-event:PUT))
                                                (guard (org-glance-world:validate-headline world partition (org-glance? it :headline))))
                                           t)
                                          ((cl-struct org-glance-event:RM) t))
                                        world-events))
                  (result-log (org-glance-changelog:merge (org-glance-changelog :events new-events) partition-changelog)))
       (when (org-glance-offset:less? partition-offset (org-glance-world:offset world))
         (cl-loop
            with progress-reporter = (make-progress-reporter "Update partition" 0 (length new-events))
            for ,event in new-events
            for idx from 0
            do (progn
                 ,@forms
                 (progress-reporter-update progress-reporter idx (format " (processed %d events of %d)" idx (length new-events))))
            finally do
              (org-glance-changelog:write result-log partition-changelog-location)
              (progress-reporter-done progress-reporter)
              (org-glance-log :events "Consumed events: %d" (length new-events))
            finally return ,event)))))

(org-glance-declare org-glance-world:updated-partition :: World -> Partition -> ReadableFile)
(defun org-glance-world:updated-partition (world partition)
  ;; TODO optimize (duplicate location calculation)
  (let* ((partition-header (org-glance-world:partition-header world partition))
         (partition-offset (org-glance? partition-header :offset)))

    ;; (org-glance-world:fetch-partition-updates (world partition event)
    ;;   event)

    (when (org-glance-offset:less? partition-offset (org-glance-world:offset world))
      (org-glance-world:with-locked-partition world partition
        ;; USE Partition log only here
        (let ((view (org-glance-world:partition-view world partition)))
          (org-glance-view:mark-current-buffer view)
          (org-glance-world:update-view world view)
          (org-glance-view:save-header view))))
    (org-glance-world--locate-partition world partition)))

(org-glance-declare org-glance-world:backfill :: World -> t)
(defun org-glance-world:backfill (world)
  (dolist-with-progress-reporter (headline (org-glance-world:headlines world)) "Backfill"
    (when (org-glance-world:headline-exists? world headline)
      (org-glance-world:make-partitions world headline)))

  (org-glance! world :partitions := (cl-remove-if #'null (--map (pcase (org-glance-world--locate-partition world it)
                                                                  ((cl-struct org-glance-readable-file) it)
                                                                  (_ nil))
                                                                (org-glance? world :partitions)))))

(org-glance-declare org-glance-world:commit :: t)
(cl-defun org-glance-world:commit ()
  "Commit current buffer."
  (let* ((world (org-glance-world:current))
         (view (org-glance-view:current))
         (markers (org-glance? view :markers)))
    (org-glance-view:with-current-buffer view
      (cl-loop
         with to-move = '() ;; hashes to be moved from current view to another
         with to-remove = '() ;; hashes to be removed from current world
         for midx from 0 below (org-glance-vector:size markers)
         when (and (org-glance? view :markers [midx] :changed?)
                   (not (org-glance? view :markers [midx] :removed?)))
         do (let* ((headline (org-glance-view:get-marker-headline view midx))
                   (old-hash (org-glance-view:get-marker-hash view midx))
                   (new-hash (org-glance? headline :hash)))

              (org-glance-world:update-headline world old-hash headline)
              (org-glance! view :markers [midx] :changed? := nil)
              (org-glance-view:set-marker-hash view midx new-hash)

              (unless (org-glance-world:validate-headline world (org-glance? view :type) headline)
                (push new-hash to-move)))
         when (org-glance? view :markers [midx] :removed?)
         do (push midx to-remove)
         finally do
           (dolist (hash to-move)
             (org-glance-view:remove-headline view hash))

           (cl-loop for midx in to-remove
              for offset from 0
              do
                (let* ((hash (org-glance? view :markers [(- midx offset)] :hash))
                       (headline (org-glance-world:get-headline world hash)))
                  (when (y-or-n-p (format "Remove headline \"%s\" completely? " (org-glance? headline :title)))
                    (org-glance-world:remove-headline world hash)))
                (org-glance-vector:remove-at! (org-glance? view :markers) (- midx offset)))

           (let ((offset (org-glance-world:persist world)))
             (org-glance-view:set-offset view offset))
           (org-glance-view:save-markers view)))))

(org-glance-declare org-glance-world:update-view :: World -> View -> t)
(defun org-glance-world:update-view (world view)
  (let* ((offset (org-glance-view:get-offset view))
         (events (--drop-while (not (org-glance-offset:less? offset (org-glance? it :offset)))
                               (reverse (org-glance-world:events world))))
         (progress-reporter (make-progress-reporter "Fetching events" 0 (length events)))
         (to-add (org-glance-view:freeze-markers view)))
    (cl-loop
       for event in events
       for idx from 0
       for event-offset = (org-glance? event :offset)

       do (thunk-let* ((headline* (org-glance? event :headline))

                       (event-hash (org-glance? event :hash))
                       (headline-hash (org-glance? headline* :hash))
                       (dimensions (org-glance? world :dimensions))

                       (headline (org-glance-world:get-headline world headline-hash))

                       (hashes-equal? (string= headline-hash event-hash))
                       (dimension-valid? (org-glance-world:validate-headline world (org-glance? view :type) headline*))
                       (dimension-invalid? (not dimension-valid?))
                       (source-exists? (not (null (gethash event-hash to-add))))
                       (source-removed? (not (org-glance-world:headline-exists? world event-hash)))
                       (target-removed? (not (org-glance-world:headline-exists? world headline-hash)))

                       (add-target! (puthash headline-hash headline to-add))
                       (remove-source!  (remhash event-hash to-add))
                       (replace-headline! (progn (puthash headline-hash headline to-add)
                                                 (remhash event-hash to-add))))
            (cl-typecase event
              (org-glance-event:UPDATE (cond (hashes-equal? nil)
                                             ((and source-removed? target-removed?) nil)
                                             ((and (not source-removed?) target-removed?) remove-source!)
                                             ((and dimension-invalid? (not source-exists?)) nil)
                                             ((and dimension-invalid? source-exists?) remove-source!)
                                             ((and dimension-valid? source-exists?) replace-headline!)
                                             (t add-target!)))
              (org-glance-event:PUT (cond (target-removed? nil)
                                          (dimension-valid? add-target!)))
              (org-glance-event:RM remove-source!)
              (otherwise (user-error "Don't know how to handle event of type %s" (type-of event)))))

         (progress-reporter-update progress-reporter idx (format " (processed %d events of %d)" idx (length events)))
       finally do
         (progress-reporter-done progress-reporter)
         (goto-char (point-min))
         (outline-next-heading)
         (delete-region (point) (point-max))
         (org-glance-vector:clear! (org-glance? view :markers))
         (unless (hash-table-empty-p to-add)
           (dolist-with-progress-reporter (headline (hash-table-values to-add)) "Insert headlines"
             (org-glance-view:add-headline view headline)))

         (org-glance-view:set-offset view event-offset)
         (org-glance-view:save-markers view))))

(org-glance-declare org-glance-world:root :: (Optional string) -> (Optional WorldLocation))
(defun org-glance-world:root (location)
  (pcase location
    ((guard (null location)) nil)
    ((cl-struct org-glance-world-location) location)
    ((cl-struct string) (org-glance-world:root (f-parent location)))))

(org-glance-declare org-glance-world:offset :: World -> Offset)
(defun org-glance-world:offset (world)
  (if-let (event (org-glance-changelog:last (org-glance? world :changelog)))
      (org-glance? event :offset)
    (org-glance-offset:current)))

(org-glance-declare org-glance-world:partitions :: World -> (ListOf Partition))
(defun org-glance-world:partitions (world)
  "Get WORLD partitions considering cache."
  (or (org-glance? world :partitions)
      (org-glance! world :partitions := (org-glance-world--read-partitions world))))

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
                              (location (org-glance-world--locate-partition world partition)))
                         (unless (f-exists? location)
                           (org-glance-log :dimensions "Create derived view %s in %s" partition location)
                           (push partition (org-glance? world :partitions))
                           (org-glance-view:get-or-create partition location (org-glance-offset:zero)))))))

;; (org-glance-declare org-glance-world:clear-partitions :: World -> Headline -> t)
;; (defun org-glance-world:clear-partitions (world headline)
;;   "Remove empty partitions from WORLD considering HEADLINE has been deleted."
;;   (cl-loop with dimensions = (org-glance? world :dimensions)
;;      for dimension in dimensions
;;      for predicates = (org-glance-dimension:predicates dimension headline)
;;      do (cl-loop for predicate in predicates
;;            for value = (org-glance-dimension:validate predicate headline dimensions)
;;            when value
;;            do (thunk-let* ((partition (org-glance-partition
;;                                        :dimension (format "%s" (org-glance? dimension :name))
;;                                        :value value))
;;                            (view-location (org-glance-world--locate-partition world partition))
;;                            (view-header-location (org-glance-view:locate-header view-location))
;;                            (view-header (org-glance-view:read-header view-header-location)))
;;                 (message "Clear partition \"%s\"?" (org-glance-partition:representation partition))
;;                 (message "File exists? \"%s\" %s" view-location (f-exists? view-location))
;;                 (message "View size: %d" (or (org-glance? view-header :size) -1))
;;                 (when (and (f-exists? view-location)
;;                            (= 0 (or (org-glance? view-header :size) -1))
;;                            (y-or-n-p (format "Partition \"%s\" became empty. Remove? " (org-glance-partition:representation partition))))
;;                   (org-glance-log :dimensions "Remove derived view %s (%s)" partition view-location)
;;                   (org-glance! world :partitions := (cl-remove partition (org-glance? world :partitions)))
;;                   (f-delete (f-parent view-location) t))))))

(org-glance-declare org-glance-world:persist :: World -> Offset)
(defun org-glance-world:persist (world)
  "Apply modifications of WORLD.

This should be the only point to destructively change underlying
persistent storage.

Return last committed offset."
  (let ((changelog (org-glance? world :changelog)))
    (dolist-with-progress-reporter (event (reverse (org-glance? world :changelog* :events))) (format "Persist world %s" (org-glance? world :location))
      (thunk-let* ((source-hash (org-glance? event :hash))
                   (target-hash (org-glance? event :headline :hash))
                   (headline-exists? (org-glance-world:headline-exists? world target-hash))
                   (event-headline (org-glance? event :headline))
                   (headline (org-glance-world:get-headline world target-hash)))
        (cl-typecase event
          (org-glance-event:RM (let ((headline (org-glance-world:get-headline world source-hash)))
                                 (org-glance-world:remove-headline-from-cache! world source-hash)
                                 ;; (org-glance-world:clear-partitions world headline)
                                 )
                               (org-glance-world:delete-headline! world source-hash)
                               (org-glance-changelog:push! changelog event))
          (org-glance-event:PUT (org-glance-world:write-headline! world headline)
                                (org-glance-world:make-partitions world headline)
                                (org-glance-changelog:push! changelog event))
          (org-glance-event:UPDATE* (org-glance-world:write-headline! world event-headline)
                                    (org-glance-world:make-partitions world event-headline)
                                    (org-glance-world:delete-headline! world source-hash)
                                    (org-glance-changelog:push! changelog (org-glance-event:UPDATE :hash source-hash
                                                                                                  :headline (org-glance-headline-header:from-headline event-headline))))
          (otherwise (error "Don't know how to handle event of type %s" (type-of event))))))

    (org-glance-world:write-changelog! world)
    (org-glance! world :changelog* := (org-glance-changelog))
    (org-glance-world:write-relations! world)

    (if (org-glance-changelog:last changelog)
        (org-glance? (org-glance-changelog:last changelog) :offset)
      (org-glance-offset:current))))

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

(org-glance-declare org-glance-world:write-headline! :: World -> Headline -> ReadableFile)
(defun org-glance-world:write-headline! (world headline)
  "Persist HEADLINE in WORLD."
  (let ((location (org-glance-world:locate-headline world headline)))
    (unless (f-exists-p location)
      (org-glance-headline:save headline location))
    location))

(org-glance-declare org-glance-world:delete-headline! :: World -> (or Hash Headline HeadlineHeader) -> t)
(defun org-glance-world:delete-headline! (world headline)
  (let ((location (org-glance-world:locate-headline world headline)))
    (and (f-exists-p location) (f-delete location))))

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
  (org-glance-changelog:push! (org-glance? world :changelog*)
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
    (org-glance-changelog:push! changelog* event)))

(org-glance-declare org-glance-world:update-headline :: World -> Hash -> Headline -> Offset)
(defun org-glance-world:update-headline (world old-hash headline)
  "Update HEADLINE with OLD-HASH in WORLD."
  (let* ((changelog* (org-glance? world :changelog*))
         (event (org-glance-event:UPDATE* :hash old-hash :headline headline))
         (offset (org-glance? event :offset)))
    (org-glance-changelog:push! changelog* event)
    offset))

(org-glance-declare org-glance-world:get-headline :: World -> Hash -> Headline)
(defun org-glance-world:get-headline (world hash)
  "Return fully qualified `org-glance-headline' from WORLD using HASH."
  (or (org-glance-world:get-headline-from-cache world hash)
      (org-glance-world:get-headline-from-stage world hash)
      (org-glance-world:get-headline-from-drive world hash)))

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

(defun org-glance-world:changelog (world)
  (org-glance-changelog:merge (org-glance? world :changelog*) (org-glance? world :changelog)))

(org-glance-declare org-glance-world:events :: World -> (ListOf Event))
(defun org-glance-world:events (world)
  (org-glance-changelog:flatten (org-glance-world:changelog world)))

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

(defun org-glance-world--locate-dimension (world dimension-name)
  (f-join (org-glance? world :location)
          "dimensions"
          (downcase dimension-name)))

(org-glance-declare org-glance-world--locate-partition :: World -> Partition -> t)
(defun org-glance-world--locate-partition (world partition)
  (f-join (org-glance? world :location)
          "dimensions"
          (org-glance-partition:path partition)
          (format "%s.org" (org-glance-partition:representation partition))))

(org-glance-declare org-glance-world--read-partitions :: World -> (ListOf Partition))
(defun org-glance-world--read-partitions (world)
  "Read WORLD partitions from disk. Ignore cache."
  (--map (--> it
              (file-name-sans-extension it)
              (list (file-name-nondirectory (f-parent (f-parent it)))
                    (file-name-nondirectory (f-parent it)))
              (-zip-lists '(:dimension :value) it)
              (-flatten it)
              (apply #'org-glance-partition it))
         (directory-files-recursively (f-join (org-glance? world :location) "dimensions") ".*\\.el$")))

(org-glance-declare org-glance-world--create :: OptionalDirectory -> World)
(defun org-glance-world--create (location)
  "Create world located in directory LOCATION."
  (declare (indent 1))
  (f-mkdir-full-path location)
  (f-touch (f-join location "Glancefile"))
  (org-glance-world :location location))

(org-glance-declare org-glance-world--read :: OptionalDirectory -> (Optional World))
(defun org-glance-world--read (location)
  (cl-typecase location
    (org-glance-world-location (let ((world (org-glance-world--create location)))
                                 (org-glance-world:read-changelog! world)
                                 (org-glance-world:read-relations! world)
                                 world))
    (otherwise nil)))

(defun org-glance-world--cache-get (location)
  (let ((world (gethash (file-truename location) org-glance-world--cache)))
    (if world
        (org-glance-log :cache "[org-glance-world] cache hit: %s" location)
      (org-glance-log :cache "[org-glance-world] cache miss: %s" location))
    world))

(defun org-glance-world--cache-put (world)
  (cl-typecase world
    (org-glance-world (org-glance-log :cache "[org-glance-world] cache put: %s" (org-glance? world :location))
                      (puthash (org-glance? world :location) world org-glance-world--cache)
                      world)
    (otherwise nil)))

(org-glance-declare org-glance-world:import-headlines :: World -> ReadableDirectory -> World)
(defun org-glance-world:import-headlines (world location)
  "Add headlines from LOCATION to WORLD."
  (dolist-with-progress-reporter (file (org-glance-scope location)) "Import headlines"
    (org-glance:with-temp-buffer
     (insert-file-contents file)
     (org-glance-headline:map (headline)
       (org-glance-world:add-headline! world headline))))
  world)

(defun org-glance-world--after-finalize-hook ()
  "Register captured headline in metastore."
  (let ((world (org-glance-world:current)))
    (org-glance-headline:map (headline)
      (org-glance-world:add-headline! world headline))
    (org-glance-world:persist world)
    (let ((file (buffer-file-name)))
      (save-buffer)
      (kill-buffer (get-file-buffer file))
      (delete-file file))))

(provide 'org-glance-world)
