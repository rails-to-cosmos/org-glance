(require 'aes)
(require 'dash)
(require 'load-relative)
(require 'org)
(require 'org-element)

(defvar org-glance-properties-ignore-patterns
  (append
   org-special-properties
   '("^ARCHIVE_" "^TITLE$" "^ORG_GLANCE" "DIR" "LAST_REPEAT" "ARCHIVE")))

(cl-defun org-glance:f (s &rest kwargs)
  "expand a template containing $keyword with the definitions in KWARGS."
  (replace-regexp-in-string "\\($[A-Za-z_-]+\\)"
                            (lambda (arg)
                              (let ((keyword (intern (format ":%s" (substring arg 1)))))
                                (format "%s" (plist-get kwargs keyword)))) s))

(cl-defun org-glance:f* (s &rest kwargs)
  "Same as `org-glance:f' but strips margins (scala style)."
  (cl-loop
     with stripMargin = (-partial 's-replace-regexp "^\\W*|" "")
     for line in (s-split "\n" s)
     concat (apply #'org-glance:f (append (list (funcall stripMargin line)) kwargs))
     concat "\n"))

(cl-defun org-glance:f! (s args)
  "Applies template format to S and ARGS."
  (apply 'org-glance:f (append (list s) args)))

(cl-defun org-glance:f> (s &rest kwargs)
  (insert (org-glance:f! s kwargs)))

(defmacro org-glance:f** (fmt)
  "Like `s-format' but with format fields in it.
FMT is a string to be expanded against the current lexical
environment. It is like what is used in `s-lex-format', but has
an expanded syntax to allow format-strings. For example:
${user-full-name 20s} will be expanded to the current value of
the variable `user-full-name' in a field 20 characters wide.
  (let ((f (sqrt 5)))  (org-glance:f \"${f 1.2f}\"))
  will render as: 2.24
This function is inspired by the f-strings in Python 3.6, which I
enjoy using a lot.
"
  (let* ((matches (s-match-strings-all"${\\(?3:\\(?1:[^} ]+\\) *\\(?2:[^}]*\\)\\)}" (eval fmt)))
         (agetter (cl-loop for (m0 m1 m2 m3) in matches
                        collect `(cons ,m3  (format (format "%%%s" (if (string= ,m2 "")
                                                                      (if s-lex-value-as-lisp "S" "s")
                                                                   ,m2))
                                                  (symbol-value (intern ,m1)))))))

    `(s-format ,fmt 'aget (list ,@agetter))))

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

(cl-defun org-glance:sort-buffer-headlines (&optional (start (point-min)))
  "Sort headlines by todo state, then sort each group by time.

TODO: implement unit tests."
  (interactive)
  (goto-char start)
  (unless (org-at-heading-p)
    (org-next-visible-heading 1))
  (let* ((state (org-glance-headline:state))
         (beginning-of-region (point))
         (end-of-region beginning-of-region))

    (cl-loop while (and (string= (org-glance-headline:state) state)
                        (< (point) (point-max)))
       do
         (message "Sort %s headlines" state)
         (org-next-visible-heading 1)
         (setq end-of-region (point)))

    (save-restriction
      (narrow-to-region beginning-of-region end-of-region)
      (goto-char (point-min))
      (insert "\n")
      (backward-char)
      (org-sort-entries nil ?t)
      (goto-char (point-min))
      (delete-char 1)
      (goto-char (point-max)))
    (org-overview)
    (unless (= (point) (point-max))
      (org-glance:sort-buffer-headlines (point)))))

(org-glance-module-provide)
