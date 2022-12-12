;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-glance-headline)
(require 'org-glance-types)
(require 'org-glance-world-model)
(require 'org-glance-world-cache)
(require 'org-glance-dimension)
(require 'org-glance-partition)
(require 'org-glance-vector)

(org-glance-declare org-glance-world:get-or-create :: OptionalDirectory -> World)
(defun org-glance-world:get-or-create (location)
  "Get or create `org-glance-world' from LOCATION."
  (->> location
       (file-truename)
       (funcall (-orfn #'org-glance-world-cache:get
                       (-compose #'org-glance-world-cache:put #'org-glance-world:read)
                       (-compose #'org-glance-world-cache:put #'org-glance-world:create)))))

(org-glance-declare org-glance-world:import :: World -> ReadableDirectory -> World)
(defun org-glance-world:import (world location)
  "Add headlines from LOCATION to WORLD."
  (dolist-with-progress-reporter (file (org-glance-scope location))
      "Import headlines"
    (org-glance:with-temp-buffer
     (insert-file-contents file)
     (org-glance-headline:map (headline)
       (org-glance-world:add-headline! world headline))))
  world)

(org-glance-declare org-glance-world:materialize :: World -> (Optional Partition) -> t)
(cl-defun org-glance-world:materialize (world &optional (partition (org-glance-world:choose-partition world)))
  "Find `org-mode' file containing headlines of WORLD and PARTITION."
  (pcase partition
    ((cl-struct org-glance-partition) (find-file (org-glance-world:updated-partition world partition)))
    (_ nil)))

(org-glance-declare org-glance-world:agenda :: World -> t)
(defun org-glance-world:agenda (world)
  "Show agenda for all active headlines of WORLD."
  (pcase (if current-prefix-arg (org-glance-world:choose-partition world) (org-glance-partition:from-key-value "active" "t"))
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
        (org-glance-world:get-or-create))
      (user-error "Buffer is not a member of world: %s" (buffer-file-name))))

(defun org-glance-world:after-finalize-hook ()
  "Register captured headline in metastore."
  (let ((world (org-glance-world:current)))
    (org-glance-headline:map (headline)
      (org-glance-world:add-headline! world headline))
    (org-glance-world:persist world)
    (let ((file (buffer-file-name)))
      (save-buffer)
      (kill-buffer (get-file-buffer file))
      (delete-file file))))

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
    (add-hook 'org-capture-after-finalize-hook 'org-glance-world:after-finalize-hook 0 t)
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
  (let* ((partition (org-glance-partition:from-string "linked=t"))
         (headline (org-glance-world:choose-headline world partition))
         (links (org-glance? headline :links))
         (link (cond ((> (length links) 1) (let ((link-title (completing-read "Choose link to open: " (--map (org-glance? it :title) links))))
                                             (--drop-while (not (string= link-title (org-glance? it :title))) links)))
                     ((= (length links) 1) (car links))
                     (t (user-error "Unable to find links in this headline")))))
    (org-link-open-from-string (org-glance? link :org-link))))

(org-glance-declare org-glance-world:extract-property :: World -> t)
(defun org-glance-world:extract-property (world)
  (let* ((partition (org-glance-partition:from-string "extractable=t"))
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
      (org-glance-partition:from-string choice))))

(org-glance-declare org-glance-world:partition-view :: World -> Partition -> View)
(defun org-glance-world:partition-view (world partition)
  (let* ((location (org-glance-world:locate-partition world partition))
         (metadata (-> location (org-glance-view:locate-header) (org-glance-view:read-header)))
         (type (org-glance? metadata :type))
         (offset (org-glance? metadata :offset)))
    (org-glance-view:get-or-create type location offset)))

(cl-defmacro org-glance-world:with-locked-partition (world partition &rest forms)
  (declare (indent 2))
  `(let ((location (org-glance-world:locate-partition ,world ,partition)))
     (when (--> location
                (get-file-buffer it)
                (cond ((null it) t)
                      ((buffer-live-p it) (kill-buffer it))))
       (org-glance:with-file-overwrite location
         ,@forms))))

(defun org-glance-world:view-header (world partition)
  (with-temp-buffer
    (insert-file-contents (-> (org-glance-world:locate-partition world partition)
                              (file-name-sans-extension)
                              (concat org-glance-view-header-extension)))
    (read (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-glance-world:locate-partition-changelog (world partition)
  (f-join (org-glance? world :location)
          "views"
          (org-glance-partition:path partition)
          (format "%s.log" (org-glance-partition:representation partition))))

(defun org-glance-world:partition-changelog (world partition)
  (org-glance-changelog:read (org-glance-world:locate-partition-changelog world partition)))

(org-glance-declare org-glance-world:updated-partition :: World -> Partition -> ReadableFile)
(defun org-glance-world:updated-partition (world partition)
  ;; TODO optimize (duplicate location calculation)
  (let* ((partition-header (org-glance-world:view-header world partition))
         (partition-offset (org-glance? partition-header :offset)))

    (when (org-glance-offset:less? partition-offset (org-glance-world:offset world))
      (org-glance-world:with-locked-partition world partition

        (let* ((partition-changelog (org-glance-world:partition-changelog world partition))
               (world-events (reverse (--take-while (org-glance-offset:less? partition-offset (org-glance? it :offset))
                                                    (org-glance-world:events world))))
               (partition-events (--filter (pcase it
                                             ((and (or (cl-struct org-glance-event:UPDATE) (cl-struct org-glance-event:PUT))
                                                   (guard (org-glance-world:validate-headline world partition (org-glance? it :headline))))
                                              t)
                                             ((cl-struct org-glance-event:RM) t))
                                           world-events)))
          (org-glance-changelog:write (org-glance-changelog:merge (org-glance-changelog :events partition-events) partition-changelog)
                                      (org-glance-world:locate-partition-changelog world partition)))

        ;; USE Partition log only here
        (let ((view (org-glance-world:partition-view world partition)))
          (org-glance-view:mark view)
          (org-glance-world:update-view world view)
          (org-glance-view:save-header view))))
    (org-glance-world:locate-partition world partition)))

(org-glance-declare org-glance-world:backfill :: World -> t)
(defun org-glance-world:backfill (world)
  (dolist-with-progress-reporter (headline (org-glance-world:headlines world)) "Backfill"
    (when (org-glance-world:headline-exists? world headline)
      (org-glance-world:make-partitions world headline)))

  (org-glance! world :partitions := (cl-remove-if #'null (--map (pcase (org-glance-world:locate-partition world it)
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

(provide 'org-glance-world)
