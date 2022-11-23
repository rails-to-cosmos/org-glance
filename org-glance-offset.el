(require 'cl-macs)
(require 'org-glance-types)

(defalias 'org-glance-offset:equal-p #'time-equal-p)
(defalias 'org-glance-offset:less? #'time-less-p)

(defun org-glance-offset:read (s)
  (time-convert (read s) 'list))

(defun org-glance-offset:current ()
  (cl-the org-glance-offset
    (time-convert nil 'list)))

(defun org-glance-offset:zero ()
  (cl-the org-glance-offset
    '(0 0 0 0)))

(provide 'org-glance-offset)
