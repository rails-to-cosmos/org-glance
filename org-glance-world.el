;; -*- lexical-binding: t; -*-

(require 'dash)
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
  (cl-check-type location org-glance-type:directory)

  (dolist-with-progress-reporter (file (org-glance-scope location))
      "Import headlines"
    (org-glance:with-temp-buffer
     (insert-file-contents file)
     (org-glance-headline:map (headline)
       (org-glance-world:add-headline world headline))))

  world)

(cl-defun org-glance-world:materialize (world &optional (derivation (org-glance-world:choose-derivation world)))
  (cl-check-type world org-glance-world)
  (cl-check-type derivation (org-glance-type:optional org-glance-derivation))

  (cl-typecase derivation
    (org-glance-derivation (find-file (org-glance-world:update-derivation world derivation)))
    (otherwise nil)))

(cl-defun org-glance-world:agenda (world)
  (cl-check-type world org-glance-world)

  (let ((derivation (cl-the (org-glance-type:optional org-glance-derivation)
                      (org-glance-world:choose-derivation world))))
    (cl-typecase derivation
      (org-glance-derivation (let ((location (org-glance-world:update-derivation world derivation))
                                   (lexical-binding nil))
                               (let ((org-agenda-files (list location))
                                     (org-agenda-overriding-header "org-glance agenda")
                                     (org-agenda-start-on-weekday nil)
                                     (org-agenda-span 21)
                                     (org-agenda-start-day "-7d"))
                                 (org-agenda-list))))
      (otherwise nil))))

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
      (org-glance-world:add-headline world headline))
    (org-glance-world:persist world)
    (let ((file (buffer-file-name)))
      (save-buffer)
      (kill-buffer (get-file-buffer file))
      (delete-file file))))

(cl-defun org-glance-world:capture-location (world)
  (cl-check-type world org-glance-world)

  (f-join (org-glance- world :location) "capture.org"))

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
  (cl-check-type world org-glance-world)

  (let ((file (org-glance-world:capture-location world)))
    (delete-file file)
    (find-file file)
    (add-hook 'org-capture-after-finalize-hook 'org-glance-world:after-finalize-hook 0 t)
    (let ((lexical-binding nil))
      (let ((org-capture-templates (list (list "_" "Thing" 'entry (list 'file file) template))))
        (org-capture nil "_")))
    ;; (when finalize
    ;;   (org-capture-finalize))
    ))

(cl-defun org-glance-world:choose-headline--where (world query)
  "TODO Should be consistent with dimensions."
  (declare (indent 1))
  (cl-check-type world org-glance-world)
  (cl-check-type query string)

  (let ((derivation (org-glance-derivation:from-string query)))
    (org-glance-world:choose-headline--derived world derivation)))

(cl-defun org-glance-world:choose-headline--derived (world derivation)
  "TODO Should be consistent with dimensions."
  (declare (indent 1))
  (cl-check-type world org-glance-world)
  (cl-check-type derivation org-glance-derivation)

  (let ((dummies (--map (cons (org-glance- it :title) (org-glance- it :hash))
                        (org-glance-world:headlines--derived world derivation))))
    (thread-last (completing-read (format "Choose headline (%s): " (org-glance-derivation:representation derivation)) dummies)
      (a-get dummies)
      (org-glance-world:get-headline world))))

(cl-defun org-glance-world:jump (world)
  (cl-check-type world org-glance-world)

  (let* ((headline (org-glance-world:choose-headline--where world "linked=t"))
         (links (org-glance- headline :links))
         (link (cond ((> (length links) 1) (let ((link-title (completing-read "Choose link to open: " (--map (org-glance- it :title) links))))
                                             (--drop-while (not (string= link-title (org-glance- it :title))) links)))
                     ((= (length links) 1) (car links))
                     (t (user-error "Unable to find links in this headline")))))
    (org-link-open-from-string (org-glance- link :org-link))))

(cl-defun org-glance-world:extract-headline (world)
  (cl-check-type world org-glance-world)

  (let* ((headline (org-glance-world:choose-headline--where world "store=t"))
         (store (org-glance- headline :store)))
    (condition-case nil
        (while t
          (kill-new (alist-get (org-completing-read "Extract property (press C-g to exit): " store) store nil nil #'string=)))
      (quit
       (setq kill-ring nil)
       (org-glance-log :info "Kill ring has been cleared")))))

(cl-defun org-glance-world:derivations (world)
  (cl-check-type world org-glance-world)

  (or (org-glance- world :derivations)
      (org-glance! world :derivations := (--map (--> it
                                                     (file-name-sans-extension it)
                                                     (list (file-name-nondirectory (f-parent it)) (file-name-nondirectory it))
                                                     (-zip-lists '(:dimension :value) it)
                                                     (-flatten it)
                                                     (apply #'org-glance-derivation it))
                                                (directory-files-recursively (f-join (org-glance- world :location) "views") ".*\\.org$")))))


(cl-defun org-glance-world:choose-derivation (world &optional dimension)
  (cl-check-type world org-glance-world)
  (cl-check-type dimension (org-glance-type:optional string))

  (thunk-let* ((derivations (cl-typecase dimension
                              (string (--filter (string= (org-glance- it :dimension) dimension)
                                                (org-glance-world:derivations world)))
                              (otherwise (org-glance-world:derivations world))))
               (reprs (--map (org-glance-derivation:representation it) derivations)))
    (when-let (choice (condition-case nil
                          (if reprs
                              (completing-read "Choose derivation: " reprs nil t)
                            (user-error "Derivations not found"))
                        (quit nil)))
      (org-glance-derivation:from-string choice))))

(cl-defun org-glance-world:update-derivation (world derivation)
  (cl-check-type world org-glance-world)
  (cl-check-type derivation org-glance-derivation)

  (org-glance-world:with-locked-derivation world derivation
    (let* ((location (org-glance-world:locate-derivation world derivation))
           (header (thread-first location
                     (org-glance-view:locate-header)
                     (org-glance-view:read-header)))
           (type (a-get header :type))
           (offset (a-get header :offset))
           (world-offset (org-glance-world:offset world)))
      (when (org-glance-offset:less? offset world-offset)
        (let ((view (org-glance-view:get-or-create world type location offset)))
          (org-glance:with-temp-file-overwrite location
            (org-glance-view:mark view)
            (org-glance-view:fetch view)
            (org-glance-view:save-header view)
            (org-glance-view:save-markers view))))
      location)))

(cl-defun org-glance-world:backfill (world)
  (cl-check-type world org-glance-world)

  (let ((changelog (org-glance- world :changelog)))
    (dolist (event (reverse (org-glance- changelog :events)))
      (thunk-let ((headline (org-glance-world:get-headline world (org-glance- event :headline :hash))))
        (when (org-glance-world:headline-exists? world (org-glance- event :headline :hash))
          (cl-typecase event
            (org-glance-event:RM nil)
            (org-glance-event:PUT (org-glance-world:make-derivations world headline))
            (org-glance-event:UPDATE (org-glance-world:make-derivations world headline))))))))

(provide 'org-glance-world)
