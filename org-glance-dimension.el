(require 'org-glance-helpers)

(org-glance-class org-glance-dimension nil
    ((name :type symbol
           :initarg :name)
     (form :type list
           :initarg :form)))

(provide 'org-glance-dimension)
