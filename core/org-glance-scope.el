(defvar org-glance--default-scopes-alist
  `((file-with-archives . og-scope--list-archives)
    (agenda-with-archives . og-scope--agenda-with-archives)))

(defun og-scope--list-file-archives (filename)
  (-some->> filename
            (file-name-nondirectory)
            (file-name-sans-extension)
            (s-append ".org_archive")
            (directory-files-recursively (file-name-directory filename))))

(defun og-scope--list-archives ()
  (append (list (buffer-file-name))
          (og-scope--list-file-archives (buffer-file-name))))

(defun og-scope--agenda-with-archives ()
  (loop for filename in (org-agenda-files)
        append (list filename)
        append (og-scope--list-file-archives filename)))

(provide 'org-glance-scope)
