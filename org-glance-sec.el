(defun org-glance-sec--decrypt-current-headline (&optional return-plain)
  "Decrypt encrypted `org-mode` subtree at point.
If RETURN-PLAIN is non-nil, return decrypted contents as string."
  (let* ((beg (save-excursion (org-end-of-meta-data) (point)))
         (end (save-excursion (org-end-of-subtree t)))
         (encrypted (let ((encrypted (buffer-substring-no-properties beg end)))
                      (if (not (with-temp-buffer
                                 (insert encrypted)
                                 (aes-is-encrypted)))
                          (user-error "Headline is not encrypted")
                        encrypted)))
         (plain (aes-decrypt-buffer-or-string encrypted)))
    (unless plain
      (user-error "Wrong password"))
    (save-excursion
      (org-end-of-meta-data)
      (kill-region beg end)
      (insert plain)
      (if return-plain
          plain
        t))))

(defun org-glance-sec--encrypt-current-headline ()
  "Encrypt org subtree contents at point."
  (let* ((beg (save-excursion (org-end-of-meta-data) (point)))
         (end (save-excursion (org-end-of-subtree t)))
         (plain (let ((plain (buffer-substring-no-properties beg end)))
                  (if (with-temp-buffer
                        (insert plain)
                        (aes-is-encrypted))
                      (user-error "Headline is already encrypted")
                    plain)))
         (encrypted (aes-encrypt-buffer-or-string plain)))
    (save-excursion
      (org-end-of-meta-data)
      (kill-region beg end)
      (insert encrypted))))

(defun org-glance-sec--extract (headline)
  (with-temp-buffer
    (org-mode)
    (insert-file-contents (org-element-property :file headline))
    (goto-char (org-element-property :begin headline))
    (org-narrow-to-subtree)
    (let ((tf (make-temp-file "org-glance-pm"))
          (dc (org-glance-sec--decrypt-current-headline t)))
      (unwind-protect
          (with-temp-file tf
            (insert dc))
        (while
            (condition-case exc
                (org-glance :scope tf
                            :prompt "Copy to kill ring: "
                            :action #'org-glance-sec--copy)
              (quit (kill-new "" t) nil)
              (error (kill-new "" t) nil)))
        (delete-file tf)))))

(defun org-glance-sec--copy (headline)
  (with-temp-buffer
    (org-mode)
    (insert-file-contents (org-element-property :file headline))
    (goto-char (org-element-property :begin headline))
    (let* ((beg (save-excursion (org-end-of-line) (1+ (point))))
           (end (save-excursion (org-end-of-subtree t)))
           (contents (buffer-substring-no-properties beg end)))
      (kill-new contents t))))

(provide-me)
