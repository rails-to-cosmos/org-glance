(require 'org-glance-module)

(cl-defun org-glance-headline-registry:create ()
  "Hash table (id->headline) that lists all registered headlines."
  (make-hash-table))

(cl-defun org-glance-headline-registry:load (file)
  "Load headline registry from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defun org-glance-headline-registry:save (headline-registry file)
  "Save HEADLINE-REGISTRY to FILE."
  (with-temp-file (-org-glance:make-file-directory file)
    (insert (prin1-to-string headline-registry))))

(cl-defun org-glance-headline-registry-get (headline-registry key)
  "Get KEY from HEADLINE-REGISTRY."
  (gethash key headline-registry))

(cl-defun org-glance-headline-registry-set (headline-registry key value)
  "Set KEY to VALUE in headline-registry HEADLINE-REGISTRY."
  (puthash key value headline-registry))

(cl-defun org-glance-headline-registry-remove (headline-registry key)
  "Remove KEY from headline-registry HEADLINE-REGISTRY."
  (remhash key headline-registry))

(cl-defun org-glance-headline-registry:import (scope)
  "Read SCOPE and extract headlines from it.")

(cl-defun org-glance-headline-registry:export (headline-registry file)
  "Write HEADLINE-REGISTRY to specific org-mode FILE.")

(org-glance:provide)
