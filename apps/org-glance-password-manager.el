(require 'aes)

(defvar og-pm-cache-file "~/.emacs.d/org-glance/passwords.el")

(defun org-glance-password-manager-encrypt-current ()
  (interactive)
  (let* ((beg (save-excursion (org-end-of-meta-data) (point)))
         (end (save-excursion (org-end-of-subtree t)))
         (plain (let ((plain (buffer-substring-no-properties beg end)))
                  (if (with-temp-buffer
                        (insert plain)
                        (aes-is-encrypted))
                      (user-error "Headline is already encrypted.")
                    plain)))
         (encrypted (aes-encrypt-buffer-or-string plain "QTC090dfjeduvz")))
    (save-excursion
      (org-end-of-meta-data)
      (kill-region beg end)
      (insert encrypted))))

(defun org-glance-password-manager-decrypt-current ()
  (interactive)
  (let* ((beg (save-excursion (org-end-of-meta-data) (point)))
         (end (save-excursion (org-end-of-subtree t)))
         (encrypted (let ((encrypted (buffer-substring-no-properties beg end)))
                      (if (not (with-temp-buffer
                                 (insert encrypted)
                                 (aes-is-encrypted)))
                          (user-error "Headline is not encrypted.")
                        encrypted)))
         (plain (aes-decrypt-buffer-or-string encrypted)))
    (unless plain
      (user-error "Wrong password."))
    (save-excursion
      (org-end-of-meta-data)
      (kill-region beg end)
      (insert plain)
      plain)))

(defun org-glance-password-manager-visit (&optional org-glance-reread)
  (interactive "P")
  (let ((org-glance-prompt "Visit secure data: ")
        (org-glance-cache og-pm-cache-file)
        (org-glance-fallback (lambda (x) (user-error "Entry not found.")))
        (org-glance-title-property :TITLE)
        (org-glance-filter (lambda (headline)
                             (save-excursion
                               (goto-char (org-element-property :begin headline))
                               (let* ((beg (save-excursion (org-end-of-meta-data) (point)))
                                      (end (save-excursion (org-end-of-subtree t)))
                                      (contents (buffer-substring-no-properties beg end)))
                                 (with-temp-buffer
                                   (insert contents)
                                   (aes-is-encrypted)))))))
    (org-glance 'agenda-with-archives)))

(defun org-glance-password-manager-secure-data-to-kill-ring (&optional org-glance-reread)
  (interactive "P")
  (let ((org-glance-prompt "Extract secure data: ")
        (org-glance-cache og-pm-cache-file)
        (org-glance-fallback (lambda (x) (user-error "Entry not found.")))
        (org-glance-title-property :TITLE)
        (org-glance-action (lambda (headline)
                             (with-temp-buffer
                               (org-mode)
                               (insert-file-contents (org-element-property :file headline))
                               (goto-char (org-element-property :begin headline))
                               (org-narrow-to-subtree)
                               (let ((tf (make-temp-file "ogpm"))
                                     (dc (org-glance-password-manager-decrypt-current))
                                     (org-glance-cache nil)
                                     (org-glance-reread nil)
                                     (org-glance-filter nil)
                                     (org-glance-prompt "Copy to kill ring: ")
                                     (org-glance-action (lambda (headline)
                                                          (with-temp-buffer
                                                            (org-mode)
                                                            (insert-file-contents (org-element-property :file headline))
                                                            (goto-char (org-element-property :begin headline))
                                                            (let* ((beg (save-excursion (org-end-of-line) (1+ (point))))
                                                                   (end (save-excursion (org-end-of-subtree t)))
                                                                   (contents (buffer-substring-no-properties beg end)))
                                                              (kill-new contents t))))))
                                 (unwind-protect
                                     (with-temp-file tf
                                       (insert dc))
                                   (while (condition-case nil
                                              (or (org-glance tf) t)
                                            (quit (kill-new "" t) nil)
                                            (error (kill-new "" t) nil)))
                                   (delete-file tf))))))
        (org-glance-filter (lambda (headline)
                             (save-excursion
                               (goto-char (org-element-property :begin headline))
                               (let* ((beg (save-excursion (org-end-of-meta-data) (point)))
                                      (end (save-excursion (org-end-of-subtree t)))
                                      (contents (buffer-substring-no-properties beg end)))
                                 (with-temp-buffer
                                   (insert contents)
                                   (aes-is-encrypted)))))))
    (org-glance 'agenda-with-archives)))

(provide 'org-glance-password-manager)
