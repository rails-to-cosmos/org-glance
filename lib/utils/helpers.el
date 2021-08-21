(require 'aes)
(require 'dash)
(require 'load-relative)
(require 'org)
(require 'org-element)

(defvar org-glance-properties-ignore-patterns
  (append
   org-special-properties
   '("^ARCHIVE_" "^TITLE$" "^ORG_GLANCE" "DIR" "LAST_REPEAT" "ARCHIVE")))

(defmacro org-glance:format (fmt)
  "Like `s-format' but with format fields in it.
FMT is a string to be expanded against the current lexical
environment. It is like what is used in `s-lex-format', but has
an expanded syntax to allow format-strings. For example:
${user-full-name 20s} will be expanded to the current value of
the variable `user-full-name' in a field 20 characters wide.
  (let ((f (sqrt 5)))  (org-glance:format \"${f 1.2f}\"))
  will render as: 2.24
This function is inspired by the f-strings in Python 3.6, which I
enjoy using a lot.
"
  (let* ((matches (s-match-strings-all "${\\(?3:\\(?1:[^} ]+\\) *\\(?2:[^}]*\\)\\)}" (eval fmt)))
         (agetter (cl-loop
                     for (m0 m1 m2 m3) in matches
                     collect `(cons ,m3  (format (format "%%%s" (if (string= ,m2 "")
                                                                    (if s-lex-value-as-lisp "S" "s")
                                                                  ,m2))
                                                 (symbol-value (intern ,m1))))))
         (result `(s-format ,fmt 'aget (list ,@agetter))))
    `(s-join "\n"
             (cl-loop
                with stripMargin = (-partial 's-replace-regexp "^\\W*|" "")
                for line in (s-split "\n" ,result)
                collect (funcall stripMargin line)))))

(defun --org-glance:make-file-directory (file)
  (let ((dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t))))

(defun org-glance--collect-tags ()
  (cl-loop for tag in (org--get-local-tags)
     collect (downcase tag)))

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

(defun org-glance-headline:demote (level)
  (cl-loop repeat level
     do (org-with-limited-levels
         (org-map-tree 'org-demote))))

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

(cl-defun org-glance-headline:buffer-contents (beg end)
  (->> (buffer-substring-no-properties beg end)
    (s-trim)))

(cl-defun org-glance:title-from-url (url)
  "Return content in <title> tag."
  (let (x1 x2 (download-buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer download-buffer)
      (beginning-of-buffer)
      (setq x1 (progn (search-forward "<title")
                      (search-forward ">")))
      (search-forward "</title>")
      (setq x2 (search-backward "<"))
      (decode-coding-string (mm-url-decode-entities-string (buffer-substring-no-properties x1 x2)) 'utf-8))))

(org-glance-module-provide)
