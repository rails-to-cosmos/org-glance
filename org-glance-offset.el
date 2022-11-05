(require 'cl-macs)
(require 'org-glance-types)

(defalias 'org-glance-offset:equal-p #'time-equal-p)
(defalias 'org-glance-offset:less? #'time-less-p)

(cl-defun org-glance-offset:read (s)
  (time-convert (read s) 'list))

(cl-defun org-glance-offset:current ()
  (cl-the org-glance-type:offset
    (time-convert nil 'list)))

(cl-defun org-glance-offset:zero ()
  (cl-the org-glance-type:offset
    '(0 0 0 0)))

(provide 'org-glance-offset)
