(defconst org-glance-loc--glance-dir "GLANCE_DIR")

(defun org-glance-loc--get-rel-dirs (filename)
  "Get related directories from FILENAME.

Header property is specified in `org-glance-loc--glance-dir'.

For example, these lines in your agenda file mean that org-glance
will search views in \"archives\", \"resources\" and
\"~/docs/concerts\" (1) directories recursively:

#+GLANCE_DIR: ../resources
#+GLANCE_DIR: ./archives

* Concerto  (1)
  :PROPERTIES:
  :DIR:  ~/docs/concerts
  :END:

(1) (not implemented yet)"

  (let* ((default-directory (file-name-directory filename))
         (glance-dir-property (format "#+%s:" org-glance-loc--glance-dir)))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (loop while (search-forward glance-dir-property nil t)
            with glance-archive-dir
            do (setq glance-archive-dir
                     (-some->> (thing-at-point 'line)
                       substring-no-properties
                       (s-replace glance-dir-property "")
                       s-trim
                       file-truename))
            if (file-exists-p glance-archive-dir)
            collect glance-archive-dir into result
            else
            do (warn "glance-archive-dir from %s not found: %s" filename glance-archive-dir)
            finally (return (or result (list default-directory)))))))

(defun org-glance-loc--list-archives (filename)
  "Return list of org-archives for FILENAME.

Search glance directories recursively considering related
directories from `org-glance-loc--get-rel-dirs'."

  (let* ((archive-dirs (org-glance-loc--get-rel-dirs filename))
         (base-filename (-some->> filename
                          file-name-nondirectory
                          file-name-sans-extension))
         (archive-filename (-some->> base-filename
                             (s-append ".org_archive")))
         (org-filename (-some->> base-filename
                         (s-append ".org"))))
    (loop for archive-dir in archive-dirs
          append (directory-files-recursively archive-dir archive-filename)
          append (directory-files-recursively archive-dir org-filename))))

(provide-me)
