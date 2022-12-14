(require 'org-glance-world-model)
(require 'org-glance-log)

(defconst org-glance-world--cache (make-hash-table :test #'equal)
  "List of worlds registered in system.")

(defun org-glance-world-cache:get (location)
  (let ((world (gethash (file-truename location) org-glance-world--cache)))
    (if world
        (org-glance-log :cache "[org-glance-world] cache hit: %s" location)
      (org-glance-log :cache "[org-glance-world] cache miss: %s" location))
    world))

(defun org-glance-world-cache:put (world)
  (cl-typecase world
    (org-glance-world (org-glance-log :cache "[org-glance-world] cache put: %s" (org-glance? world :location))
                      (puthash (org-glance? world :location) world org-glance-world--cache)
                      world)
    (otherwise nil)))

(provide 'org-glance-world-cache)
