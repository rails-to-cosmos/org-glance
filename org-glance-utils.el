;; -*- lexical-binding: t -*-

(require 'f)
(require 'aes)
(require 'dash)
(require 'org-element)

(defconst org-glance:key-value-pair-re "^-?\\([[:word:],[:blank:],_,/,-]+\\)\\:[[:blank:]]*\\(.*\\)$")

(cl-defmacro org-glance:interactive-lambda (&rest forms)
  "Define interactive lambda function with FORMS in its body."
  (declare (indent 0) (debug t))
  `(lambda () (interactive) ,@forms))

(cl-defmacro org-glance--with-file-visited (file &rest forms)
  "Visit FILE, execute FORMS and close it if it was closed before visit."
  (declare (indent 1) (debug t))
  `(save-window-excursion
     (let ((inhibit-startup-hooks t)
           (inhibit-modification-hooks t)
           (buffer-lived-p (buffer-live-p (get-file-buffer ,file)))
           (buffer (find-file-noselect ,file)))
       (unwind-protect
           (with-current-buffer buffer ,@forms)
         (unless buffer-lived-p
           (kill-buffer buffer))))))

(cl-defun org-glance--back-to-heading ()
  (or (org-at-heading-p) (org-back-to-heading-or-point-min)))

(cl-defun org-glance--now ()
  (format-time-string (org-time-stamp-format 'long 'inactive) (current-time)))

(defun org-glance--symbol-downcased? (sym)
  "Return t if SYM is downcased (i.e., all lowercase), nil otherwise."
  (let ((name (symbol-name sym)))
    (string= name (downcase name))))

(cl-defun org-glance--remove-links (&rest types)
  (save-excursion
    (cl-loop while (re-search-forward (concat "[[:blank:]]?" org-link-any-re) nil t)
             for link = (s-split-up-to ":" (substring-no-properties (or (match-string 2) "")) 1)
             for type = (intern (car link))
             when (or (null types) (memq type types))
             do (delete-region (match-beginning 0) (match-end 0)))))

(cl-defun org-glance--buffer-links ()
  (cl-loop for link-element in (org-element-map (org-element-parse-buffer) 'link #'identity)
           for beg = (org-element-property :begin link-element)
           for end = (org-element-property :end link-element)
           for title = (substring-no-properties
                        (or (-some->> link-element
                              (org-element-contents)
                              (org-element-interpret-data))
                            (org-element-property :raw-link link-element)))
           for link = (s-trim (buffer-substring-no-properties beg end))
           collect title into titles
           collect link into links
           collect beg into positions
           finally return (-zip links titles positions)))

(cl-defun org-glance--search-optional (needle)
  (condition-case nil
      (re-search-forward needle)
    (search-failed nil)))

(cl-defun org-glance--buffer-key-value-pairs ()
  "Extract key-value pairs from buffer.
Run completing read on keys and copy selected values to kill ring.

Assume string is a key-value pair if it matches `org-glance:key-value-pair-re'."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (org-glance--search-optional org-glance:key-value-pair-re)
             for key = (s-trim (substring-no-properties (match-string 1)))
             for value = (s-trim (substring-no-properties (match-string 2)))
             collect (cons key value))))

(cl-defun org-glance--parse-links ()
  "Simple org-link parser, return list of cons cells (link . contents)."
  (cl-loop with descriptions = (cl-loop for (key . val) in (org-glance--buffer-key-value-pairs) collect (cons val key))
           with links = (org-glance--buffer-links)
           for (link title pos) in links
           for description = (alist-get link descriptions nil nil #'string=)
           when description
           collect (list link description pos)
           else
           collect (list link title pos)))

(cl-defun org-glance--substitute-links ()
  (cl-loop for (link title _) in (org-glance--parse-links)
           do (save-excursion
                (goto-char (point-min))
                (while (search-forward link nil t)
                  (replace-match title t t)))))

(cl-defun org-glance--join-leading-separator (separator strings)
  (let ((joined-strings (s-join separator strings)))
    (if (string-empty-p joined-strings)
        ""
      (concat separator joined-strings))))

(cl-defun org-glance--join-leading-separator-but-null (separator strings)
  (declare (indent 1))
  (org-glance--join-leading-separator separator (cl-remove-if #'null strings)))

(defun org-glance--file-with-archives ()
  (append (list (buffer-file-name))
          (org-glance--list-file-archives (buffer-file-name))))

(defun org-glance--agenda-with-archives ()
  (cl-loop for filename in (org-agenda-files)
           append (list filename)
           append (org-glance--list-file-archives filename)))

(defun org-glance--list-file-archives (filename)
  "Return list of org-mode files for FILENAME."
  (let* ((dir (file-name-directory filename))
         (base-filename (-some->> filename
                          file-name-nondirectory
                          file-name-sans-extension)))
    (directory-files-recursively dir (format "%s.org\\.*" base-filename))))

(cl-defun org-glance--list-directories (base-dir)
  (--filter (f-directory? (f-join base-dir it)) (directory-files base-dir nil "^[[:word:]]+")))

(cl-defun org-glance--make-file-directory (file)
  (let ((dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  file)

(cl-defun org-glance--valid-directory? (dir)
  "Check if DIR is an existing, readable, and writable directory."
  (and (stringp dir)
       (file-directory-p dir)
       (file-readable-p dir)
       (file-writable-p dir)))

(cl-defun org-glance--encode-string (string)
  (base64-encode-string (encode-coding-string string 'utf-8) t))

(cl-defun org-glance--decode-string (string)
  (decode-coding-string (base64-decode-string string) 'utf-8))

(defun org-glance--encrypt-region (beg end &optional password)
  "Encrypt region from BEG to END using PASSWORD."
  (interactive "r")
  (let* ((original-text (buffer-substring-no-properties beg end))
         (encrypted-text (aes-encrypt-buffer-or-string original-text password)))
    (save-excursion
      (delete-region beg end)
      (goto-char beg)
      (insert encrypted-text))))

(defun org-glance--decrypt-region (beg end &optional password)
  "Decrypt region from BEG to END using PASSWORD."
  (interactive "r")
  (if-let (decrypted-text (let ((encrypted (buffer-substring-no-properties beg end)))
                            (if (with-temp-buffer
                                  (insert encrypted)
                                  (aes-is-encrypted))
                                (aes-decrypt-buffer-or-string encrypted password)
                              (user-error "Headline is not encrypted"))))
      (save-excursion
        (delete-region beg end)
        (goto-char beg)
        (insert decrypted-text))
    (user-error "Wrong password")))

(provide 'org-glance-utils)
