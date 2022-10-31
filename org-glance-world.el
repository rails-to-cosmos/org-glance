(require 'dash)
(require 'org-glance-world-model)
(require 'org-glance-world-cache)

(cl-defmacro org-glance-world:with-locked-dimension (world dimension &rest forms)
  (declare (indent 2))
  `(when (--> ,world
              (org-glance-world-model:locate-dimension it ,dimension)
              (get-file-buffer it)
              (cond ((null it) t)
                    ((buffer-live-p it) (kill-buffer it))))
     ,@forms))

(cl-defun org-glance-world:get-or-create (location)
  "Get or create `org-glance-world' from LOCATION."
  (->> location
       (file-truename)
       (funcall (-orfn #'org-glance-world-cache:get
                       (-compose #'org-glance-world-cache:put
                                 #'org-glance-world-model:read)
                       (-compose #'org-glance-world-cache:put
                                 #'org-glance-world-model:create)))))

(cl-defun org-glance-world:import-headlines (world location)
  "Add headlines from LOCATION to WORLD."
  (dolist-with-progress-reporter (file (org-glance-scope location))
      "Importing headlines"
    (org-glance:with-temp-buffer
     (insert-file-contents file)
     (org-glance-headline:map (headline)
       (org-glance-world-model:add-headline world headline)))))

(cl-defun org-glance-world:choose-dimension (world)
  (completing-read "Choose dimension: "
                   (org-glance-world-model:list-dimensions world)
                   nil
                   t))

(cl-defun org-glance-world:browse (world &optional (dimension (org-glance-world:choose-dimension world)))
  (org-glance-world:with-locked-dimension world dimension
    (find-file (org-glance-world-model:update-dimension world dimension))))

(cl-defun org-glance-world:agenda (world)
  (let ((dimension (org-glance-world:choose-dimension world)))
    (org-glance-world:with-locked-dimension world dimension
      (let ((location (org-glance-world-model:update-dimension world dimension))
            (lexical-binding nil))
        (let ((org-agenda-files (list location))
              (org-agenda-overriding-header "org-glance agenda")
              (org-agenda-start-on-weekday nil)
              (org-agenda-span 21)
              (org-agenda-start-day "-7d"))
          (org-agenda-list))))))

(cl-defun org-glance-world:current ()
  "Get `org-glance-world' associated with current buffer."
  (thread-first (buffer-file-name)
    (org-glance-world-model:root)
    (org-glance-world:get-or-create)))

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

(provide 'org-glance-world)
