(require 'load-relative)

(defvar org-glance-resources-directory
  (f-join user-emacs-directory "org-glance" "resources"))

(defun org-glance-headline:specify-attach-directory ()
  "Specify dir and archive paths for current headline."
  (interactive)

  (unless (org-at-heading-p)
    (org-back-to-heading t))

  (let* ((calendar-date-display-form '((format "%s-%.2d-%.2d" year
                                        (string-to-number month)
                                        (string-to-number day))))
         (now (calendar-date-string (calendar-current-date) nil))
         (path (read-directory-name "Specify directory: "
                                    org-glance-resources-directory
                                    nil nil "/")))
    (condition-case nil
        (make-directory path t)
      (error nil))
    (org-set-property "DIR" path)
    (org-set-property "ARCHIVE" (f-join path "story.org::"))
    (org-set-property "COOKIE_DATA" "todo recursive")))

(provide-me)
