(require 'org-glance-world-model)
(require 'org-glance-log)

(defvar org-glance-world--cache (make-hash-table :test #'equal)
  "List of worlds registered in system.")

(cl-defun org-glance-world-cache:get (location)
  (let ((world (gethash (file-truename location) org-glance-world--cache)))
    (if world
        (org-glance-log :world-cache "cache hit: %s" location)
      (org-glance-log :world-cache "cache miss: %s" location))
    world))

(cl-defun org-glance-world-cache:put (world)
  (cl-typecase world
    (org-glance-world (org-glance-log :world-cache "cache put: %s" (org-glance- world :location))
                      (puthash (org-glance- world :location) world org-glance-world--cache)
                      world)
    (otherwise nil)))

(provide 'org-glance-world-cache)
