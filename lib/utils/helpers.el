(require 'org-glance-module)

(cl-defun org-glance-ensure-at-heading ()
  (unless (org-at-heading-p)
    (org-back-to-heading-or-point-min)))

(cl-defun org-glance-remove-links (&rest types)
  (save-excursion
    (cl-loop while (re-search-forward (concat "[[:blank:]]?" org-link-any-re) nil t)
       do (let* ((link (s-split-up-to ":" (substring-no-properties (or (match-string 2) "")) 1))
                 (type (intern (car link)))
                 (id (cadr link)))
            (when (memq type types)
              (delete-region (match-beginning 0) (match-end 0)))))))

(cl-defun org-glance-parse-links-with-contents ()
  "Simple org-link parser, return list of cons cells (link . contents)."
  (cl-loop
     for element in (org-element-map (org-element-parse-buffer) 'link #'identity)
     for beg = (org-element-property :begin element)
     for end = (org-element-property :end element)
     for title = (substring-no-properties
                  (or (-some->> element
                        org-element-contents
                        org-element-interpret-data)
                      (org-element-property :raw-link element)))
     for link = (s-trim (buffer-substring-no-properties beg end))
     collect title into titles
     collect link into links
     finally return (-zip links titles)))

(cl-defun org-glance-replace-links-with-titles ()
  (save-excursion
    (goto-char (point-min))
    (cl-loop
       for (link . contents) in (org-glance-parse-links-with-contents)
       do (save-excursion (replace-string link contents)))))

(cl-defun org-glance-ensure-directory (directory)
  "Make DIRECTORY if not exist."
  (unless (f-exists? directory)
    (mkdir directory t)))

(cl-defun org-glance-list-directories (directory)
  (--filter
   (f-directory? (f-join directory it))
   (directory-files directory nil "^[[:word:]]+")))

(org-glance:provide)
