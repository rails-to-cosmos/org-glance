;; -*- lexical-binding: t; -*-

(require 'org-glance-log)
(require 'org-glance-world)
(require 'org-glance-view-model)
(require 'org-glance-view-cache)

;; (org-glance-declare org-glance-view:get-or-create :: Partition -> string -> Offset -> View)
(defun org-glance-view:get-or-create (type location offset)
  "Create `org-glance-view' instance by TYPE, LOCATION and OFFSET."
  (thunk-let* ((location (file-truename location))
               (m-loc (concat (file-name-sans-extension location) org-glance-view-marker-extension))
               (key (org-glance-view--key :type type :location location))
               (cached-view (org-glance-view-cache:get key))
               (new-view (org-glance-view:create type location offset)))
    (cond ((and (f-exists? location) (f-exists? m-loc) cached-view) cached-view)
          (t (org-glance-log :cache "[org-glance-view] cache miss: %s" type)
             (org-glance-view-cache:put new-view)
             new-view))))

;; (org-glance-declare org-glance-view:current :: View)
(defun org-glance-view:current ()
  "Return current view from file-buffer."
  (let ((header (thread-first (buffer-file-name)
                  (org-glance-view:locate-header)
                  (org-glance-view:read-header))))
    (org-glance-view:get-or-create (a-get header :type) (buffer-file-name) (a-get header :offset))))

(provide 'org-glance-view)
