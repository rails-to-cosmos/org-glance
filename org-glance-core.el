(eval-when-compile
  (require 'cl)
  (require 'subr-x))

(eval-and-compile
  (require 'org)
  (require 'org-element)
  (require 'load-relative)
  (require 'org-glance-scope)
  (require 'org-glance-db))

(cl-defun org-glance-completing-read (headlines &key prompt)
  (org-completing-read prompt
                       (cl-loop for headline in headlines
                                collect (org-glance-format headline))))

(cl-defun org-glance-format (headline)
  (or (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)))

(cl-defun org-glance-browse (headlines &key choice fallback)
  (or (cl-loop for headline in headlines
               when (string= (org-glance-format headline) choice)
               do (cl-return headline))
      (when fallback (funcall fallback choice))))

(provide-me)
