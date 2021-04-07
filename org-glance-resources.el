(require 'load-relative)

(defvar org-glance-resources-directory
  (f-join user-emacs-directory "org-glance" "resources"))

(defun org-glance-resources:back-to-view-heading ()
  (unless (org-at-heading-p)
    (org-back-to-heading))
  (or (-intersection (cl-loop for tag in (org--get-local-tags)
                        collect (downcase tag))
                     (cl-loop for tag in (org-glance-list-views)
                        collect (downcase (symbol-name tag))))
      (progn
        (org-up-element)
        (org-glance-resources:back-to-view-heading))))

(defun org-glance-resources:specify-attach-directory ()
  "Specify dir and archive paths for current headline."
  (interactive)
  (let* ((view (save-excursion
                 (car (org-glance-resources:back-to-view-heading))))
         (calendar-date-display-form '((format "%s-%.2d-%.2d" year
                                        (string-to-number month)
                                        (string-to-number day))))
         (now (calendar-date-string (calendar-current-date) nil))
         (path (read-directory-name (format "Specify directory for %s: " view)
                                    (f-join org-glance-resources-directory view)
                                    nil nil "")))
    (condition-case nil
        (make-directory path t)
      (error nil))
    (org-set-property "DIR" path)
    (org-set-property "ARCHIVE" (f-join path (format "%s.org::" view)))
    (org-set-property "COOKIE_DATA" "todo recursive")))

(provide-me)
