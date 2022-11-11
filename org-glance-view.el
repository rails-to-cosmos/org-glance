;; -*- lexical-binding: t; -*-

(require 'org-glance-log)
(require 'org-glance-world)
(require 'org-glance-view-model)
(require 'org-glance-view-cache)

(cl-defun org-glance-view:get-or-create (world type location offset)
  "Create symbol `org-glance-view' instance from WORLD by TYPE and store it in LOCATION."
  (cl-check-type world org-glance-world)
  (cl-check-type type org-glance-derivation)
  (cl-check-type offset org-glance-type:offset)

  (thunk-let* ((location (file-truename (f-join (org-glance- world :location) location)))
               (key (org-glance-view--key :type type :location location))
               (cached-view (org-glance-view-cache:get key))
               (new-view (org-glance-view:create world type location offset)))
    (cond ((and (f-exists? location) cached-view) cached-view)
          (t (org-glance-log :cache "[org-glance-view] cache miss: %s" type)
             (org-glance-view-cache:put new-view)
             new-view))))

(cl-defun org-glance-view:get-buffer-view ()
  (let ((header (thread-first (buffer-file-name)
                  (org-glance-view:locate-header)
                  (org-glance-view:read-header)))
        (world (org-glance-world:current)))
    (org-glance-view:get-or-create world (a-get header :type) (buffer-file-name) (a-get header :offset))))

(provide 'org-glance-view)
