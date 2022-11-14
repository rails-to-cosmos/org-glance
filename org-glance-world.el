;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'org)
(require 'org-agenda)
(require 'org-glance-headline)
(require 'org-glance-types)
(require 'org-glance-world-model)
(require 'org-glance-world-cache)
(require 'org-glance-dimension)

(cl-defun org-glance-world:get-or-create (location)
  "Get or create `org-glance-world' from LOCATION."
  (cl-check-type location org-glance-type:optional-directory)

  (->> location
       (file-truename)
       (funcall (-orfn #'org-glance-world-cache:get
                       (-compose #'org-glance-world-cache:put
                                 #'org-glance-world:read)
                       (-compose #'org-glance-world-cache:put
                                 #'org-glance-world:create)))))

(cl-defun org-glance-world:import (world location)
  "Add headlines from LOCATION to WORLD."
  (cl-check-type world org-glance-world)
  (cl-check-type location org-glance-type:readable-directory)

  (dolist-with-progress-reporter (file (org-glance-scope location))
      "Import headlines"
    (org-glance:with-temp-buffer
     (insert-file-contents file)
     (org-glance-headline:map (headline)
       (org-glance-world:add-headline! world headline))))

  world)

(cl-defun org-glance-world:materialize (world &optional (partition (org-glance-world:choose-partition world)))
  (cl-check-type world org-glance-world)
  (cl-check-type partition (org-glance-type:optional org-glance-partition))

  (cl-typecase partition
    (org-glance-partition (find-file (org-glance-world:updated-partition world partition)))
    (otherwise nil)))

(cl-defun org-glance-world:agenda (world)
  (cl-check-type world org-glance-world)

  (pcase (org-glance-world:choose-partition world)
    ((and (cl-struct org-glance-partition) partition) (progn
                                                        (setq org-agenda-files (list (org-glance-world:updated-partition world partition))
                                                              org-agenda-overriding-header "org-glance agenda"
                                                              org-agenda-start-on-weekday nil
                                                              org-agenda-span 21
                                                              org-agenda-start-day "-7d")
                                                        (org-agenda-list)))
    (_ nil)))

(cl-defun org-glance-world:current ()
  "Get `org-glance-world' associated with current buffer."
  (or (thread-first (buffer-file-name)
        (org-glance-world:root)
        (org-glance-world:get-or-create))
      (user-error "World %s is not registered in the system" (buffer-file-name))))

(cl-defun org-glance-world:after-finalize-hook ()
  "Register captured headline in metastore."
  (let ((world (org-glance-world:current)))
    (org-glance-headline:map (headline)
      (org-glance-world:add-headline! world headline))
    (org-glance-world:persist world)
    (let ((file (buffer-file-name)))
      (save-buffer)
      (kill-buffer (get-file-buffer file))
      (delete-file file))))

(cl-defun org-glance-world:capture-location (world)
  (cl-check-type world org-glance-world)

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

(cl-defun org-glance-world:choose-headline (world partition)
  "TODO Should be consistent with dimensions."
  (declare (indent 1))
  (cl-check-type world org-glance-world)
  (cl-check-type partition org-glance-partition)

  (let ((dummies (--map (cons (org-glance? it :title) (org-glance? it :hash))
                        (org-glance-world:get-partition-headlines world partition))))
    (thread-last (completing-read (format "Choose headline (%s): " (org-glance-partition:representation partition)) dummies)
      (a-get dummies)
      (org-glance-world:get-headline world))))

(cl-defun org-glance-world:jump (world)
  (cl-check-type world org-glance-world)

  (let* ((partition (org-glance-partition:from-string "linked=t"))
         (headline (org-glance-world:choose-headline world partition))
         (links (org-glance? headline :links))
         (link (cond ((> (length links) 1) (let ((link-title (completing-read "Choose link to open: " (--map (org-glance? it :title) links))))
                                             (--drop-while (not (string= link-title (org-glance? it :title))) links)))
                     ((= (length links) 1) (car links))
                     (t (user-error "Unable to find links in this headline")))))
    (org-link-open-from-string (org-glance? link :org-link))))

(cl-defun org-glance-world:extract-headline (world)
  (cl-check-type world org-glance-world)

  (let* ((partition (org-glance-partition:from-string "store=t"))
         (headline (org-glance-world:choose-headline world partition))
         (store (org-glance? headline :store)))
    (condition-case nil
        (while t
          (kill-new (alist-get (org-completing-read "Extract property (press C-g to exit): " store) store nil nil #'string=)))
      (quit
       (setq kill-ring nil)
       (org-glance-log :info "Kill ring has been cleared")))))

(cl-defun org-glance-world:choose-partition (world &optional dimension)
  (cl-check-type world org-glance-world)
  (cl-check-type dimension (org-glance-type:optional string))

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

(cl-defun org-glance-world:updated-partition (world partition)
  (cl-check-type world org-glance-world)
  (cl-check-type partition org-glance-partition)

  (thunk-let* ((location (org-glance-world:locate-partition world partition))
               (header (org-glance-world:read-partition world partition))
               (view-type (a-get header :type))
               (view-offset (a-get header :offset))
               (world-offset (org-glance-world:offset world))
               (view (org-glance-view:get-or-create world view-type location view-offset)))

    (when (org-glance-offset:less? view-offset world-offset)
      (org-glance-world:with-locked-location location
        (org-glance-view:mark! view)
        (org-glance-view:fetch! view)
        (org-glance-view:save-header view)))

    location))

(cl-defun org-glance-world:backfill (world)
  (cl-check-type world org-glance-world)

  (dolist-with-progress-reporter (event (org-glance-world:events world))
      "Backfill"
    (thunk-let ((headline (org-glance-world:get-headline world (org-glance? event :headline :hash))))
      (when (org-glance-world:headline-exists? world (org-glance? event :headline :hash))
        (cl-typecase event
          (org-glance-event:RM nil)
          (org-glance-event:PUT (org-glance-world:make-partitions world headline))
          (org-glance-event:UPDATE (org-glance-world:make-partitions world headline)))))))

(provide 'org-glance-world)
