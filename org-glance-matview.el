(defvar org-glance-views '())

(defun org-glance-list-views ()
  (interactive)
  (let ((view (org-completing-read "View: " org-glance-views)))
    (funcall (intern (format "org-glance--%s-visit" (s-downcase view))))))

(defun org-glance-matview--safe-extract-property (property)
  (condition-case nil
      (org-entry-get (point) property)
    (error (user-error "Materialized properties corrupted, please reread"))))

(defun org-glance-matview--safe-extract-num-property (property)
  (string-to-number (org-glance-matview--safe-extract-property property)))

(defun org-glance-matview--materialize-file (filename &optional minify)
  (let ((headlines (org-glance-load filename))
        (file-entries (make-hash-table))
        (output-filename (make-temp-file "org-glance-materialized-" nil ".org")))

    (loop for hl in headlines
          do (let ((fn (intern (org-element-property :file hl)))
                   (pos (org-element-property :begin hl)))
               (puthash fn (cons pos (gethash fn file-entries)) file-entries)))

    (maphash (lambda (file entries)
               (with-temp-buffer
                 (org-mode)
                 (insert-file-contents (symbol-name file))
                 (loop for pos in entries
                       do (let* ((beg (save-excursion
                                        (goto-char pos)
                                        (beginning-of-line)
                                        (point)))
                                 (end (save-excursion
                                        (goto-char pos)
                                        (org-end-of-subtree)
                                        (point)))
                                 (contents (buffer-substring-no-properties beg end)))
                            (with-temp-buffer
                              (org-mode)
                              (insert contents)
                              (goto-char (point-min))
                              (let ((promote-level 0))
                                (while
                                    (condition-case nil
                                        (org-with-limited-levels
                                         (org-map-tree 'org-promote)
                                         t)
                                      (error nil))
                                  (incf promote-level))

                                (let ((hash (buffer-hash)))
                                  (goto-char (point-min))
                                  (org-set-property "ORG_GLANCE_SOURCE" (symbol-name file))
                                  (org-set-property "ORG_GLANCE_INDENT" (number-to-string promote-level))
                                  (org-set-property "ORG_GLANCE_BEG" (number-to-string beg))
                                  (org-set-property "ORG_GLANCE_END" (number-to-string end))
                                  (org-set-property "ORG_GLANCE_HASH" hash)
                                  (goto-char (point-max))
                                  (insert "\n")
                                  (append-to-file (point-min) (point-max) output-filename))))))))
             file-entries)

    (unless minify
      (append-to-file "* COMMENT Settings
# Local Variables:
# firestarter: org-glance-matview--sync-subtree
# end:" nil output-filename)
      (with-current-buffer (find-file-other-window output-filename)
        (org-mode)
        (org-overview)))

    output-filename))

(defun org-glance-matview--visit-original-heading ()
  (interactive)
  (save-excursion
    (while (org-up-heading-safe) t)

    (let* ((source (org-glance-matview--safe-extract-property "ORG_GLANCE_SOURCE"))
           (beg (org-glance-matview--safe-extract-num-property "ORG_GLANCE_BEG"))
           (end (org-glance-matview--safe-extract-num-property "ORG_GLANCE_END"))
           (glance-hash (org-glance-matview--safe-extract-property "ORG_GLANCE_HASH"))
           (src-hash (org-glance-matview--get-source-hash source beg end)))

      (when (not (string= glance-hash src-hash))
        (user-error "Source file modified or materialized properties corrupted, please reread"))

      (find-file source)
      (widen)
      (goto-char beg)
      (while (org-up-heading-safe) t)
      (org-narrow-to-subtree)
      (org-show-all)
      (widen)
      (goto-char beg))))

(defun org-glance-matview--backup (&optional view dir)
  (interactive)
  (let* ((view (or view (org-completing-read "View: " org-glance-views)))
         (dir (or dir (read-directory-name "Backup directory: ")))
         (vf (funcall (intern (format "org-glance--%s-materialize" (s-downcase view))) 'minify))
         (new-file (concat (s-downcase view) ".org"))
         (new-file-path (f-join dir new-file)))

    (condition-case nil
        (mkdir dir)
      (error nil))

    (if (file-exists-p new-file-path)
        (let ((existed-buffer-hash (with-temp-buffer
                                     (insert-file-contents new-file-path)
                                     (buffer-hash)))
              (new-buffer-hash (with-temp-buffer
                                 (insert-file-contents vf)
                                 (buffer-hash))))
          (if (not (string= existed-buffer-hash new-buffer-hash))
              (copy-file vf new-file-path t)
            (message "View %s backup is up to date" view)))
      (copy-file vf new-file-path t))))

(defun org-glance-matview--sync-buffer ()
  (interactive)
  (org-map-entries #'org-glance-matview--sync-subtree))

(defun org-glance-matview--sync-subtree ()
  (interactive)
  (save-excursion

    (while (org-up-heading-safe) t)

    (let* ((source (org-glance-matview--safe-extract-property "ORG_GLANCE_SOURCE"))
           (beg (org-glance-matview--safe-extract-num-property "ORG_GLANCE_BEG"))
           (end (org-glance-matview--safe-extract-num-property "ORG_GLANCE_END"))
           (end-old end)
           (glance-hash (org-glance-matview--safe-extract-property "ORG_GLANCE_HASH"))
           (promote-level (org-glance-matview--safe-extract-num-property "ORG_GLANCE_INDENT"))
           (mat-hash (org-glance-matview--get-subtree-hash))
           (src-hash (org-glance-matview--get-source-hash source beg end)))

      (when (not (string= glance-hash src-hash))
        (user-error "Source file modified or materialized properties corrupted, please reread"))

      (when (string= glance-hash mat-hash)
        (user-error "No changes made in subtree"))

      (when (y-or-n-p "Subtree has been changed. Apply changes?")

        (let ((new-contents (save-restriction
                              (org-narrow-to-subtree)
                              (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
                                (with-temp-buffer
                                  (org-mode)
                                  (insert buffer-contents)
                                  (goto-char (point-min))

                                  (org-delete-property "ORG_GLANCE_SOURCE")
                                  (org-delete-property "ORG_GLANCE_BEG")
                                  (org-delete-property "ORG_GLANCE_END")
                                  (org-delete-property "ORG_GLANCE_HASH")
                                  (org-delete-property "ORG_GLANCE_INDENT")

                                  (loop repeat promote-level
                                        do (org-with-limited-levels
                                            (org-map-tree 'org-demote)))

                                  (buffer-substring-no-properties (point-min) (point-max)))))))

          (with-temp-file source
            (org-mode)
            (insert-file-contents source)
            (delete-region beg end)
            (goto-char beg)
            (insert new-contents)
            (setq end (point)))

          (org-set-property "ORG_GLANCE_BEG" (number-to-string beg))
          (org-set-property "ORG_GLANCE_END" (number-to-string end))
          (org-set-property "ORG_GLANCE_HASH" (get-src-hash source beg end))

          (let ((end-diff (- end end-old)))
            (org-map-entries
             (lambda ()
               (condition-case nil
                   (when (and (> (org-glance-matview--safe-extract-num-property "ORG_GLANCE_BEG") beg)
                              (string= source (org-glance-matview--safe-extract-property "ORG_GLANCE_SOURCE")))
                     (org-set-property "ORG_GLANCE_BEG" (number-to-string (+ end-diff (org-glance-matview--safe-extract-num-property "ORG_GLANCE_BEG"))))
                     (org-set-property "ORG_GLANCE_END" (number-to-string (+ end-diff (org-glance-matview--safe-extract-num-property "ORG_GLANCE_END"))))
                     (message "Update indentation for headline %s" (org-entry-get (point) "ITEM")))
                 (error (message "Skip headline %s" (org-entry-get (point) "ITEM"))))))))))))

(defun org-glance-matview--get-subtree-hash ()
  (save-restriction
    (org-narrow-to-subtree)
    (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
      (with-temp-buffer
        (org-mode)
        (insert buffer-contents)
        (goto-char (point-min))

        (org-delete-property "ORG_GLANCE_SOURCE")
        (org-delete-property "ORG_GLANCE_BEG")
        (org-delete-property "ORG_GLANCE_END")
        (org-delete-property "ORG_GLANCE_HASH")
        (org-delete-property "ORG_GLANCE_INDENT")

        (buffer-hash)))))

(defun org-glance-matview--get-source-hash (src beg end)
  (with-temp-buffer
    (org-mode)
    (insert-file-contents src)
    (let ((subtree (condition-case nil
                       (buffer-substring-no-properties beg end)
                     (error (user-error "Materialized properties corrupted, please reread")))))
      (with-temp-buffer
        (org-mode)
        (insert subtree)
        (goto-char (point-min))
        (while
            (condition-case nil
                (org-with-limited-levels
                 (org-map-tree 'org-promote)
                 t)
              (error nil))
          t)
        (buffer-hash)))))

(defun org-glance-backup-views (&optional dir)
  (interactive)
  (let ((dir (or dir (read-directory-name "Backup directory: "))))
    (loop for view in org-glance-views
          do (org-glance-matview--backup (symbol-name view) dir))))

(provide-me)
