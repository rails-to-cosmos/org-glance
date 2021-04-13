(require 'dash)
(require 'load-relative)
(require 'org)
(require 'org-element)

(defun org-glance--back-to-heading ()
  (unless (org-at-heading-p)
    (org-back-to-heading)))

(defun org-glance--collect-tags ()
  (cl-loop for tag in (org--get-local-tags)
     collect (downcase tag)))

(defun org-glance--apply-filter (filter headline)
  (or (null filter) (funcall filter headline)))

(defun org-glance--current-date ()
  (let ((calendar-date-display-form '((format "%s-%.2d-%.2d" year
                                       (string-to-number month)
                                       (string-to-number day)))))
    (calendar-date-string (calendar-current-date) nil)))

(defun org-glance--ensure-path (path)
  (condition-case nil
      (make-directory path t)
    (error nil)))

(defun org-glance-list-files-recursively (dir)
  (directory-files-recursively dir "\\.*.org\\.*"))

(defun org-glance-list-file-archives (filename)
  "Return list of org-mode files for FILENAME."
  (let* ((dir (file-name-directory filename))
         (base-filename (-some->> filename
                          file-name-nondirectory
                          file-name-sans-extension)))
    (directory-files-recursively dir (format "%s.org\\.*" base-filename))))

(defun -org-glance-list-archives ()
  (append (list (buffer-file-name))
          (org-glance-list-file-archives (buffer-file-name))))

(defun -org-glance-agenda-with-archives ()
  (cl-loop for filename in (org-agenda-files)
     append (list filename)
     append (org-glance-list-file-archives filename)))

(defun org-glance-read-file-headlines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (->> (buffer-string)
      substring-no-properties
      read
      eval)))

(defun org-glance-format (headline)
  (or (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)))

(defun org-glance-read-headlines-from-file (file &optional filter)
  (with-temp-buffer
    (insert-file-contents file)
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (headline)
        (when (org-glance--apply-filter filter headline)
          (plist-put (cadr headline) :file file)
          headline)))))

(defun -org-glance-promote-subtree ()
  (let ((promote-level 0))
    (cl-loop while (condition-case nil
                       (org-with-limited-levels (org-map-tree 'org-promote) t)
                     (error nil))
       do (cl-incf promote-level))
    promote-level))

(defun -org-glance-demote-subtree (level)
  (cl-loop repeat level
     do (org-with-limited-levels
         (org-map-tree 'org-demote))))

(defun -org-glance-first-level-heading ()
  (save-excursion
    (unless (org-at-heading-p) (org-back-to-heading))
    (beginning-of-line)
    (point)))

(defun --org-glance-view-end-of-meta-data ()
  (save-excursion
    (org-end-of-meta-data)
    (point)))

(defun -element-at-point-equals-headline (headline)
  (message "Element at point equals headline?")
  (let ((element-title (org-element-property :raw-value (org-element-at-point)))
        (headline-title (org-element-property :raw-value headline)))
    (message "Requested headline: %s" headline-title)
    (message "Visited headline: %s" element-title)
    (condition-case nil
        (s-contains? element-title headline-title)
      (error nil))))

(provide-me)
