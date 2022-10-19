(require 'cl-macs)

(cl-deftype org-glance-offset nil 'time)

(defalias 'org-glance-offset:equal-p #'time-equal-p)
(defalias 'org-glance-offset:less? #'time-less-p)

(cl-defun org-glance-offset:read (s)
  (time-convert (read s) 'list))

(cl-defun org-glance-offset:current ()
  (cl-the org-glance-offset
    (time-convert nil 'list)))

(cl-defun org-glance-offset:zero ()
  (cl-the org-glance-offset
    '(0 0 0 0)))

(provide 'org-glance-offset)
