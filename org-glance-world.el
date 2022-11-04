(require 'dash)
(require 'org-glance-headline)
(require 'org-glance-world-model)
(require 'org-glance-world-cache)
(require 'org-glance-dimension)

(cl-defmacro org-glance-world:with-locked-view (world view-name &rest forms)
  (declare (indent 2))
  `(progn
     (cl-check-type ,world org-glance-world)
     (cl-check-type ,view-name string)
     (when (--> ,world
                (org-glance-world:locate-view it ,view-name)
                (get-file-buffer it)
                (cond ((null it) t)
                      ((buffer-live-p it) (kill-buffer it))))
       ,@forms)))

(cl-defun org-glance-world:get-or-create (location)
  "Get or create `org-glance-world' from LOCATION."
  (cl-check-type location string)
  (->> location
       (file-truename)
       (funcall (-orfn #'org-glance-world-cache:get
                       (-compose #'org-glance-world-cache:put
                                 #'org-glance-world-model:read)
                       (-compose #'org-glance-world-cache:put
                                 #'org-glance-world-model:create)))))

(cl-defun org-glance-world:import (world location)
  "Add headlines from LOCATION to WORLD."
  (cl-check-type world org-glance-world)
  (cl-check-type location string)
  (dolist-with-progress-reporter (file (org-glance-scope location))
      "Import headlines"
    (org-glance:with-temp-buffer
     (insert-file-contents file)
     (org-glance-headline:map (headline)
       (org-glance-world-model:add-headline world headline)))))

(cl-defun org-glance-world:choose-view (world)
  (cl-check-type world org-glance-world)
  (completing-read "Choose view: "
                   (org-glance-world-model:list-views world)
                   nil
                   t))

(cl-defun org-glance-world:browse (world &optional (view-name (org-glance-world:choose-view world)))
  (cl-check-type world org-glance-world)
  (cl-check-type view-name string)
  (org-glance-world:with-locked-view world view-name
    (find-file (org-glance-world:update-view world view-name))))

(cl-defun org-glance-world:agenda (world)
  (cl-check-type world org-glance-world)
  (let ((view (org-glance-world:choose-view world)))
    (org-glance-world:with-locked-view world view
      (let ((location (org-glance-world:update-view world view))
            (lexical-binding nil))
        (let ((org-agenda-files (list location))
              (org-agenda-overriding-header "org-glance agenda")
              (org-agenda-start-on-weekday nil)
              (org-agenda-span 21)
              (org-agenda-start-day "-7d"))
          (org-agenda-list))))))

(cl-defun org-glance-world:current ()
  "Get `org-glance-world' associated with current buffer."
  (or (thread-first (buffer-file-name)
        (org-glance-world-model:root)
        (org-glance-world-cache:get))
      (user-error "World %s is not registered in the system" (buffer-file-name))))

(cl-defun org-glance-world:after-finalize-hook ()
  "Register captured headline in metastore."
  (let ((world (org-glance-world:current)))
    (org-glance-headline:map (headline)
      (org-glance-world-model:add-headline world headline))
    (org-glance-world-model:persist world)
    (let ((file (buffer-file-name)))
      (kill-buffer (get-file-buffer file))
      (delete-file file))))

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
    (add-hook 'org-capture-after-finalize-hook 'org-glance-world:after-finalize-hook 0 t)
    (let ((lexical-binding nil))
      (let ((org-capture-templates (list (list "_" "Thing" 'entry (list 'file file) template))))
        (org-capture nil "_")))
    ;; (when finalize
    ;;   (org-capture-finalize))
    ))

(cl-defun org-glance-world:jump (world)
  (let* ((headline (org-glance-world:choose-headline world #'(lambda (headline) (org-glance- headline :linked?))))
         (links (org-glance- headline :links))
         (link (cond ((> (length links) 1) (let ((link-title (completing-read "Choose link to open: " (--map (org-glance- it :title) links))))
                                             (--drop-while (not (string= link-title (org-glance- it :title))) links)))
                     ((= (length links) 1) (car links))
                     (t (user-error "Unable to find links in this headline")))))
    (org-link-open-from-string (org-glance- link :org-link))))

(cl-defun org-glance-world:choose-headline (world &optional predicate)
  "TODO Should be consistent with dimensions."
  (declare (indent 1))
  (let ((headlines (org-glance-log :performance
                       (org-glance-world-model:filter-headlines world predicate))))
    (thread-last (completing-read "Choose headline: " headlines)
      (a-get headlines)
      (org-glance-world-model:get-headline world))))

(cl-defun org-glance-world:extract (world)
  (let* ((headline (org-glance-world:choose-headline world #'(lambda (headline) (org-glance- headline :store?))))
         (store (org-glance- headline :store)))
    (condition-case nil
        (while t
          (kill-new (alist-get (org-completing-read "Extract property (press C-g to exit): " store) store nil nil #'string=)))
      (quit
       (setq kill-ring nil)
       (org-glance-log :info "Kill ring has been cleared")))))

(cl-defun org-glance-world-model:list-views (world)
  "TODO optimize me later"
  (--map (file-name-sans-extension it)
         (--filter (member (file-name-extension it) org-glance-scope-extensions)
                   (directory-files (f-join (org-glance- world :location) "views")))))

(cl-defun org-glance-world:update-view (world view-name)
  (let* ((view-location (org-glance-world:locate-view world view-name))
         (view-header (thread-first view-location
                        (org-glance-view:get-header-location-by-view-location)
                        (org-glance-view:read-header)))
         (view (org-glance-view :world world
                                :type (a-get view-header :type)
                                :location view-location
                                :offset (a-get view-header :offset)))
         (world-offset (org-glance-world-model:offset world)))

    (org-glance-log :world "Fetch view %s" dim)
    (org-glance-log :offsets "[%s] View offset = %s" (org-glance- view :type) (org-glance- view :offset))
    (org-glance-log :offsets "[%s] World offset = %s" (org-glance- view :type) (org-glance-world-model:offset world))
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

(cl-defun org-glance-world:locate-view (world view-name)
  (cl-check-type world org-glance-world)
  (cl-check-type view-name string)

  (f-join (org-glance- world :location) "views" (format "%s.org" (downcase view-name))))

(cl-defun org-glance-world:save-headline (world headline)
  (cl-check-type world org-glance-world)
  (cl-check-type headline org-glance-headline)

  (let ((location (org-glance-world-model:locate-headline world headline)))

    (unless (f-exists-p location)
      (org-glance-headline:save headline location))

    (cl-loop with dimensions = (org-glance- world :dimensions)
       for dimension in dimensions
       for partitions = (org-glance-dimension:partitions dimension headline)
       for predicates = (org-glance-dimension:predicates dimension headline)
       do (cl-loop for (partition . predicate) in (-zip partitions predicates)
             for validation-result = (org-glance-dimension:validate predicate headline dimensions)
             for view-file = (downcase (format "%s=%s.org" (org-glance- dimension :name) validation-result))
             for location = (f-join (org-glance- world :location) "views" view-file)
             do (org-glance-log :dimensions "Create derived view \"%s -> %s\" in %s" partition validation-result location)
             do (org-glance-view:get-or-create world predicate location (org-glance-offset:zero))))

    location))

(provide 'org-glance-world)
