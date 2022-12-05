;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'org)
(require 'org-agenda)
(require 'org-glance-headline)
(require 'org-glance-types)
(require 'org-glance-world-model)
(require 'org-glance-world-cache)
(require 'org-glance-dimension)

(org-glance-declare org-glance-world:get-or-create :: OptionalDirectory -> World)
(defun org-glance-world:get-or-create (location)
  "Get or create `org-glance-world' from LOCATION."
  (->> location
       (file-truename)
       (funcall (-orfn #'org-glance-world-cache:get
                       (-compose #'org-glance-world-cache:put
                                 #'org-glance-world:read)
                       (-compose #'org-glance-world-cache:put
                                 #'org-glance-world:create)))))

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

  (pcase (if current-prefix-arg
             (org-glance-world:choose-partition world)
           (org-glance-partition:from-string "active=t"))
    ((and (cl-struct org-glance-partition) partition) (progn
                                                        (setq org-agenda-files (list (org-glance-world:updated-partition world partition))
                                                              org-agenda-overriding-header "org-glance agenda"
                                                              org-agenda-start-on-weekday nil
                                                              org-agenda-span 21
                                                              org-agenda-start-day "-7d")
                                                        (org-agenda-list)))
    (_ nil)))

(org-glance-declare org-glance-world:current :: World)
(defun org-glance-world:current ()
  "Get `org-glance-world' associated with current buffer."
  (or (-> (buffer-file-name)
          (org-glance-world:root)
          (org-glance-world:get-or-create))
      (user-error "World %s is not registered in the system" (buffer-file-name))))

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

(org-glance-declare org-glance-world:dummy-headlines :: World -> Partition -> (ListOf cons))
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

(org-glance-declare org-glance-world:updated-partition :: World -> Partition -> ReadableFile)
(defun org-glance-world:updated-partition (world partition)
  (let* ((location (org-glance-world:locate-partition world partition))
         (header (org-glance-world:read-partition world partition))
         (view-type (a-get header :type))
         (view-offset (a-get header :offset))
         ;; (world-offset (org-glance-world:offset world))
         (view (org-glance-view:get-or-create world view-type location view-offset)))

    ;; (when (org-glance-offset:less? view-offset world-offset)
    ;;   )

    (org-glance-world:with-locked-location location
      (org-glance-view:mark! view)
      (org-glance-view:fetch! view)
      (org-glance-view:save-header view))

    location))

(org-glance-declare org-glance-world:backfill :: World -> t)
(defun org-glance-world:backfill (world)
  (dolist-with-progress-reporter (headline (org-glance-world:headlines world))
      "Backfill"
    (cl-typecase headline
      (org-glance-event:RM nil)
      ((or org-glance-event:PUT org-glance-event:UPDATE)
       (when (org-glance-world:headline-exists? world headline)
         (org-glance-world:make-partitions world headline)))))

  (org-glance! world :partitions := (remove-if #'null (--map (pcase (org-glance-world:locate-partition world it)
                                                               ((and (cl-struct org-glance-readable-file) location) it)
                                                               (otherwise nil))
                                                             (org-glance? world :partitions)))))

(provide 'org-glance-world)
