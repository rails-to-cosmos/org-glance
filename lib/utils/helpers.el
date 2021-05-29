(require 'aes)
(require 'dash)
(require 'load-relative)
(require 'org)
(require 'org-element)

(defvar org-glance-properties-ignore-patterns
  (append
   org-special-properties
   '("^ARCHIVE_" "^TITLE$" "^ORG_GLANCE" "DIR" "LAST_REPEAT" "ARCHIVE")))

(cl-defun org-glance-headline:id (headline)
  (let ((id (org-element-property :ORG_GLANCE_ID headline)))
    (if (null id)
        nil
      (format "%s" id))))

(cl-defun org-glance-headline:title (hl)
  (org-element-property :raw-value hl))

(cl-defun org-glance-headline:view-id (hl)
  (org-element-property :ORG_GLANCE_VIEW_ID hl))

(cl-defun org-glance-headline:view-ids (&optional hl)
  (when hl
    (org-glance-headline:visit hl))

  (save-excursion
    (org-glance-headline:beginning-of-nearest-headline)
    (cl-loop for tag in (org-glance-view:ids)
       for org-tags = (mapcar #'downcase (org-get-tags nil t))
       when (member (downcase (symbol-name tag)) org-tags)
       collect tag)))

(defun org-glance-expand-template (s plist)
  "expand a template containing {:keyword} with the definitions in plist"
  (replace-regexp-in-string "{\\(:[^}]+\\)}"
                            (lambda (arg)
                              (let ((keyword (intern (substring arg 1 -1))))
                                (format "%s" (plist-get plist keyword)))) s))

(defun org-glance--make-file-directory (file)
  (let ((dir (file-name-directory file)))
    (unless (file-exists-p dir)
    (make-directory dir t))))

(defun org-glance-headline:back-to-heading ()
  (unless (org-at-heading-p)
    (org-back-to-heading)))

(defun org-glance--collect-tags ()
  (cl-loop for tag in (org--get-local-tags)
     collect (downcase tag)))

(defun org-glance-headline:filter (filter headline)
  (or (null filter) (funcall filter headline)))

(defun org-glance--ensure-path (path)
  (condition-case nil
      (make-directory path t)
    (error nil)))

(defun org-glance--list-files-recursively (dir)
  (directory-files-recursively dir "\\.*.org\\.*"))

(defun org-glance--list-file-archives (filename)
  "Return list of org-mode files for FILENAME."
  (let* ((dir (file-name-directory filename))
         (base-filename (-some->> filename
                          file-name-nondirectory
                          file-name-sans-extension)))
    (directory-files-recursively dir (format "%s.org\\.*" base-filename))))

(defun org-glance--list-archives ()
  (append (list (buffer-file-name))
          (org-glance--list-file-archives (buffer-file-name))))

(defun org-glance--agenda-with-archives ()
  (cl-loop for filename in (org-agenda-files)
     append (list filename)
     append (org-glance--list-file-archives filename)))

(defun org-glance-headline:format (headline)
  (or (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)))

(defun org-glance-headline:scan-file (file &optional filter)
  (with-temp-buffer
    (insert-file-contents file)
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (headline)
        (when (and (org-glance-headline-p headline)
                   (org-glance-headline:filter filter headline))
          (plist-put (cadr headline) :file file)
          headline)))))

(defun org-glance-headline:promote ()
  (cl-loop while (condition-case nil
                     (org-with-limited-levels (org-map-tree 'org-promote) t)
                   (error nil))
     with promote-level = 0
     do (cl-incf promote-level)
     finally (return promote-level)))

(defun org-glance-headline:demote (level)
  (cl-loop repeat level
     do (org-with-limited-levels
         (org-map-tree 'org-demote))))

(cl-defun org-glance-headline-p (headline)
  (not (null (org-glance-headline:id headline))))

(cl-defun org-glance-headline:eq (headline &optional (other (org-element-at-point)))
  (string= (org-glance-headline:id headline)
           (org-glance-headline:id other)))

(defun org-glance-headline:encrypt (&optional password)
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

(defun org-glance-headline:decrypt (&optional password)
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

(cl-defun org-glance-headline:goto-first-level-headline ()
  (cl-loop while (org-up-heading-safe)))

(cl-defun org-glance-headline:expand-parents ()
  (save-excursion
    (org-glance-headline:goto-first-level-headline)))

(cl-defun org-glance-headline:beginning-of-nearest-headline ()
  (save-excursion
    (org-glance-headline:goto-beginning-of-nearest-headline)
    (point)))

(cl-defun org-glance-headline:goto-beginning-of-nearest-headline ()
  (unless (org-at-heading-p) (org-back-to-heading))
  (beginning-of-line))

(cl-defun org-glance-headline:end-of-subtree ()
  (save-excursion
    (org-end-of-subtree t)))

(cl-defun org-glance-headline:buffer-contents (beg end)
  (->> (buffer-substring-no-properties beg end)
    (s-trim)))

(org-glance-module-provide)
