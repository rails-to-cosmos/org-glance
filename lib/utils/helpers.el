(require 'org-glance-module)

(org-glance:require
  s
  aes
  dash
  org
  org-archive
  org-element)

(cl-defun org-glance--ensure-directory (directory)
  (unless (f-exists? directory)
    (mkdir directory)))

(cl-defun -org-glance:list-directories (directory)
  (--filter
   (f-directory? (f-join directory it))
   (directory-files directory nil "^[[:word:]]+")))

(cl-defun -org-glance:buffer-links ()
  "Extract links from current buffer."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link) (list
               (substring-no-properties
                (or (nth 2 link)
                    (org-element-property :raw-link link)))
               (org-element-property :begin link)
               (org-element-property :end link)))))

(cl-defun -org-glance:remove-links (s)
  "Replace org-links in S with it's titles where possible."
  (with-temp-buffer
    (insert s)
    (cl-loop
       for (title beg end) in (-org-glance:buffer-links)
       collect title into titles
       collect (s-trim (buffer-substring-no-properties beg end)) into links
       finally (return
                 (cl-loop
                    initially (goto-char (point-min))
                    for i upto (1- (length titles))
                    do (replace-string (nth i links) (nth i titles))
                    finally (return (buffer-substring-no-properties (point-min) (point-max))))))))

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
      (make-directory dir 'parents)))
  file)

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

(cl-defun org-glance:ensure-at-heading ()
  (unless (org-at-heading-p)
    (org-back-to-heading-or-point-min)))

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

;; (defun org-glance:recreate-folder-structure-in-subtree-at-point ()
;;   (interactive)
;;   (save-excursion
;;     (org-back-to-heading)
;;     (loop for directory in (directory-files-recursively (org-attach-dir-get-create) ".*" t)
;;        if (file-directory-p directory)
;;        do (save-excursion
;;             (save-restriction
;;               (org-narrow-to-subtree)
;;               (condition-case nil
;;                   (save-excursion (search-forward directory))
;;                 (error (org-insert-heading '(4))
;;                        (insert (file-name-nondirectory directory))
;;                        (org-set-property "DIR" directory)
;;                        (org-demote))))))))

;; (defun org-glance-view:ensure-directory (view-id)
;;   ((and (member view-id (org-get-tags nil t)) (not (org-element-property "ORG_GLANCE_ID" )))
;;    (let* ((event-dir-abs
;;            (let ((default-directory (f-join default-directory "~/sync/resources/stories")))
;;              (read-directory-name "Specify story directory: ")))
;;           (event-dir-rel (file-relative-name event-dir-abs)))
;;      (condition-case nil
;;          (make-directory event-dir-abs)
;;        (error nil))
;;      (org-set-property "DIR" event-dir-rel)
;;      (org-set-property "ARCHIVE" (f-join event-dir-rel "story.org::"))
;;      (org-set-property "COOKIE_DATA" "todo recursive"))))

;; (cl-defun org-glance-view:resource-location (&optional (view-id (org-glance-view:completing-read)))
;;   "Path to directory where VIEW-ID resources and metadata are stored."
;;   (abbreviate-file-name
;;    (f-join org-glance-directory
;;            (s-downcase (format "%s" view-id))
;;            "resources")))

;; (cl-defun org-glance:generate-dir-for-subtree-at-point (class)
;;   (org-glance-headline:generate-directory
;;    (org-glance-view:resource-location class)
;;    (org-element-property :raw-value (org-element-at-point))))

(cl-defun org-glance:first-level-headline ()
  (cl-loop while (org-up-heading-safe)))

(cl-defun org-glance:expand-parents ()
  (save-excursion
    (org-glance:first-level-headline)))

(cl-defun org-glance:get-links-from-string (raw-value)
  (-filter
   (lambda (it) (condition-case nil (eql (car it) 'link) (error nil)))
   (org-element-parse-secondary-string raw-value '(link))))

(defconst org-glance:key-value-pair-re "^\\([[:word:],[:blank:],_]+\\)\\:[[:blank:]]*\\(.*\\)$")

(cl-defun org-glance:get-buffer-key-value-pairs ()
  "Extract key-value pairs from buffer.
Run completing read on keys and copy selected values to kill ring.

Assume string is a key-value pair if it matches `org-glance:key-value-pair-re'."
  (cl-loop
     initially (goto-char (point-min))
     while (condition-case nil
               (re-search-forward org-glance:key-value-pair-re)
             (search-failed nil))
     collect (s-trim (substring-no-properties (match-string 1))) into keys
     collect (s-trim (substring-no-properties (match-string 2))) into vals
     finally (return (-zip keys vals))))

(org-glance:provide)
