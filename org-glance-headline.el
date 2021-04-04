(require 'org-glance)
(require 'load-relative)

(defun org-glance-headline:specify-attach-directory ()
  "Specify dir and archive paths for current headline."
  (interactive)

  (unless (org-at-heading-p)
    (org-back-to-heading t))

  (let ((path (read-directory-name
               "Specify story directory: "
               org-glance-resources-directory)))
    (condition-case nil
        (make-directory path t)
      (error nil))
    (org-set-property "DIR" path)
    (org-set-property "ARCHIVE" (f-join path "story.org::"))
    (org-set-property "COOKIE_DATA" "todo recursive")))

(provide-me)
