(require 'dash)
(require 'org-glance-world-model)
(require 'org-glance-world-cache)

(cl-defun org-glance-world:get-or-create (location)
  "Get or create `org-glance-world' from LOCATION."
  (->> location
       (file-truename)
       (funcall (-orfn #'org-glance-world-cache:get
                       (-compose #'org-glance-world-cache:put
                                 #'org-glance-world-model:read)
                       (-compose #'org-glance-world-cache:put
                                 #'org-glance-world-model:create)))))

(cl-defun org-glance-world:browse (world &optional (dimension (org-glance-world:choose-dimension world)))
  (org-glance-world:with-locked-dimension world dimension
    (find-file (org-glance-world:update-dimension world dimension))))

(cl-defun org-glance-world:agenda (world)
  (let ((dimension (org-glance-world:choose-dimension world)))
    (org-glance-world:with-locked-dimension world dimension
      (let ((location (org-glance-world:update-dimension world dimension))
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
    (org-glance-world:root)
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

(provide 'org-glance-world)
