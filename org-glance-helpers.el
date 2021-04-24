(require 'aes)
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
    (->> (buffer-substring-no-properties (point-min) (point-max))
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
  (let ((element-title (org-element-property :raw-value (org-element-at-point)))
        (headline-title (org-element-property :raw-value headline)))
    (condition-case nil
        (s-contains? element-title headline-title)
      (error nil))))

(defun org-glance-encrypt-subtree (&optional password)
  "Encrypt subtree at point with PASSWORD."
  (interactive)
  (let* ((beg (save-excursion (org-end-of-meta-data) (point)))
         (end (save-excursion (org-end-of-subtree t)))
         (plain (let ((plain (buffer-substring-no-properties beg end)))
                  (if (with-temp-buffer
                        (insert plain)
                        (aes-is-encrypted))
                      (user-error "Headline is already encrypted")
                    plain)))
         (encrypted (aes-encrypt-buffer-or-string plain password)))
    (save-excursion
      (org-end-of-meta-data)
      (kill-region beg end)
      (insert encrypted))))

(defun org-glance-decrypt-subtree (&optional password)
  "Decrypt subtree at point with PASSWORD."
  (interactive)
  (let* ((beg (save-excursion (org-end-of-meta-data) (point)))
         (end (save-excursion (org-end-of-subtree t)))
         (encrypted (let ((encrypted (buffer-substring-no-properties beg end)))
                      (if (not (with-temp-buffer
                                 (insert encrypted)
                                 (aes-is-encrypted)))
                          (user-error "Headline is not encrypted")
                        encrypted)))
         (plain (aes-decrypt-buffer-or-string encrypted password)))
    (unless plain
      (user-error "Wrong password"))
    (save-excursion
      (org-end-of-meta-data)
      (kill-region beg end)
      (insert plain))))

(cl-defun org-glance-buffer-properties-to-kill-ring (&optional (ignore-patterns org-glance-properties-ignore-patterns))
  "Extract buffer org-properties, run completing read on keys, copy values to kill ring."
  (while t
    (let* ((properties (-filter (lambda (key) (not (--any? (s-matches? it key) ignore-patterns))) (org-buffer-property-keys)))
           (property (org-completing-read "Extract property: " properties))
           (values (org-property-values property)))
      (kill-new (cond
                  ((> (length values) 1) (org-completing-read "Choose property value: " values))
                  ((= (length values) 1) (car values))
                  (t (user-error "Something went wrong: %s" values)))))))

(provide-me)
