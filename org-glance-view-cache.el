(require 'org-glance-view-model)
(require 'org-glance-log)
(require 'org-glance-helpers)

(defvar org-glance-view--cache (make-hash-table :test #'equal)
  "List of views registered in system.")

(org-glance-class org-glance-view--key nil
    ((type
      :type org-glance-partition
      :initarg :type
      :documentation "Type declaration that transforms into predicate of
      one argument: `org-glance-headline'. View is guaranteed to
      contain only headlines for which predicate returns non-nil
      value.")
     (location
      :type org-glance-optional-org-file
      :initarg :location
      :documentation "Location where view persists."))
  "Unique key for `org-glance-view'.")

(cl-defun org-glance-view-cache:get (key)
  (cl-check-type key org-glance-view--key)
  (if-let (result (gethash key org-glance-view--cache))
      (prog1 (cl-the org-glance-view result)
        (org-glance-log :cache "[org-glance-view] cache hit: %s" key))
    (org-glance-log :cache "cache miss: %s" key)))

(cl-defun org-glance-view-cache:put (view)
  (cl-check-type view org-glance-view)
  (let ((key (org-glance-view--key :type (org-glance? view :type)
                                   :location (org-glance? view :location))))
    (org-glance-log :cache "[org-glance-view] cache put: %s" key)
    (puthash key view org-glance-view--cache)))

(provide 'org-glance-view-cache)
