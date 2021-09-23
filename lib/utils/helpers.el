(require 'org-glance-module)

(org-glance:require
  aes
  dash
  org
  org-element)

(cl-defmacro org-glance:format (fmt)
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

(defun -org-glance:make-file-directory (file)
  (let ((dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t))))

(defun -org-glance:collect-tags ()
  (cl-loop for tag in (org--get-local-tags)
     collect (downcase tag)))

(defun -org-glance:list-file-archives (filename)
  "Return list of org-mode files for FILENAME."
  (let* ((dir (file-name-directory filename))
         (base-filename (-some->> filename
                          file-name-nondirectory
                          file-name-sans-extension)))
    (directory-files-recursively dir (format "%s.org\\.*" base-filename))))

(defun -org-glance:file-with-archives ()
  (append (list (buffer-file-name))
          (-org-glance:list-file-archives (buffer-file-name))))

(defun -org-glance:agenda-with-archives ()
  (cl-loop for filename in (org-agenda-files)
     append (list filename)
     append (-org-glance:list-file-archives filename)))

(cl-defun org-glance:get-buffer-key-value-pairs ()
  "Extract key-value pairs from buffer that match pattern (key: value), run completing read on keys, copy values to kill ring."
  (let ((property-re org-glance-headline:key-value-pair-re))
    (save-excursion
      (goto-char (point-min))
      (cl-loop
         while (condition-case nil
                   (re-search-forward property-re)
                 (search-failed nil))
         collect (s-trim (substring-no-properties (match-string 1))) into keys
         collect (s-trim (substring-no-properties (match-string 2))) into vals
         finally (return (-zip keys vals))))))

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

(org-glance:provide)
