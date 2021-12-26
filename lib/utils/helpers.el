(require 'org-glance-module)

(org-glance:require
  s
  aes
  dash
  org
  org-element)

(cl-defun org-glance-now ()
  (format-time-string (org-time-stamp-format 'long 'inactive) (current-time)))

(cl-defun org-glance-ensure-at-heading ()
  (unless (org-at-heading-p)
    (org-back-to-heading-or-point-min)))

(cl-defun org-glance-generate-id (&optional (class (org-glance-view:completing-read)))
  (substring-no-properties
   (format "%s-%s-%s"
           class
           (s-join "-" (mapcar #'number-to-string (current-time)))
           (secure-hash 'md5 (buffer-string)))))

(cl-defun org-glance-class-location (&optional (view-id (org-glance-view:completing-read)))
  "Path to directory where VIEW-ID resources and metadata are stored."
  (abbreviate-file-name
   (f-join org-glance-directory
           (s-downcase (format "%s" view-id))
           "resources")))

(cl-defun org-glance-generate-directory (&optional (class (org-glance-view:completing-read)))
  (save-excursion
    (org-glance-ensure-at-heading)
    (save-restriction
      (org-narrow-to-subtree)
      (org-glance-headline:generate-directory
       (org-glance-class-location class)
       (org-element-property :raw-value (org-element-at-point))))))

(defconst org-glance:key-value-pair-re "^\\([[:word:],[:blank:],_]+\\)\\:[[:blank:]]*\\(.*\\)$")

(cl-defun org-glance-buffer-key-value-pairs ()
  "Extract key-value pairs from buffer.
Run completing read on keys and copy selected values to kill ring.

Assume string is a key-value pair if it matches `org-glance:key-value-pair-re'."
  (goto-char (point-min))
  (cl-loop
     while (condition-case nil
               (re-search-forward org-glance:key-value-pair-re)
             (search-failed nil))
     collect (s-trim (substring-no-properties (match-string 1))) into keys
     collect (s-trim (substring-no-properties (match-string 2))) into vals
     finally (return (-zip keys vals))))

(cl-defun org-glance-buffer-links ()
  "Retrieve all `org-link' positions from current buffer."
  (remove-if #'null
             (org-element-map (org-element-parse-buffer) 'link
               (lambda (link)
                 (let ((raw-link (org-element-property :raw-link link)))
                   (unless (s-starts-with? "org-glance" raw-link)
                     (let ((caption (substring-no-properties (or (nth 2 link) raw-link)))
                           (position (org-element-property :begin link)))
                       (cons caption position))))))))

(cl-defun org-glance:list-directories (base-dir)
  (--filter
   (f-directory? (f-join base-dir it))
   (directory-files base-dir nil "^[[:word:]]+")))

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
                with stripMargin = (-partial 'replace-regexp-in-string "^\\W*|" "")
                for line in (s-split "\n" ,result)
                collect (funcall stripMargin line)))))

(defun -org-glance:make-file-directory (file)
  (let ((dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  file)

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

(cl-defun org-glance-join (separator strings)
  (let ((joined-strings (s-join separator strings)))
    (if (string-empty-p joined-strings)
        ""
      (concat separator joined-strings))))

(cl-defun org-glance-join-but-null (separator strings)
  (org-glance-join separator (cl-remove-if #'null strings)))

(org-glance:provide)
