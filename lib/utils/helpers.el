(require 'org-glance-module)

(cl-defun org-glance-ensure-at-heading ()
  "Ensure point is at heading.
Return t if it is or nil otherwise."
  (unless (org-at-heading-p)
    (org-back-to-heading-or-point-min))
  (org-at-heading-p))

(cl-defun org-glance-replace-links-with-titles (ast)
  "Replace links with its titles in AST."
  (cl-loop for link in (org-element-map ast 'link #'identity)
     do (org-element-set-element link (or (-some->> link
                                            org-element-contents
                                            org-element-interpret-data)
                                          (org-element-property :raw-link link)))
     finally return ast))

(cl-defun org-glance-ensure-directory (directory)
  "Make DIRECTORY if not exist."
  (unless (f-exists? directory)
    (mkdir directory t)))

(cl-defun org-glance-list-directories (directory)
  "List directories in DIRECTORY."
  (--filter
   (f-directory? (f-join directory it))
   (directory-files directory nil "^[[:word:]]+")))

(org-glance:provide)
