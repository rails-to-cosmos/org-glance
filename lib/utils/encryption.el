(require 'org-glance-module)

(defun org-glance:encrypt-region (beg end &optional password)
  "Encrypt region from BEG to END using PASSWORD."
  (interactive "r")
  (let* ((original-text (buffer-substring-no-properties beg end))
         (encrypted-text (aes-encrypt-buffer-or-string original-text password)))
    (save-excursion
      (kill-region beg end)
      (goto-char beg)
      (insert encrypted-text))))

(defun org-glance:decrypt-region (beg end &optional password)
  "Decrypt region from BEG to END using PASSWORD."
  (interactive "r")
  (if-let (decrypted-text (let ((encrypted (buffer-substring-no-properties beg end)))
                            (if (with-temp-buffer
                                  (insert encrypted)
                                  (aes-is-encrypted))
                                (aes-decrypt-buffer-or-string encrypted password)
                              (user-error "Headline is not encrypted"))))
      (save-excursion
        (kill-region beg end)
        (goto-char beg)
        (insert decrypted-text))
    (user-error "Wrong password")))

(org-glance-module-provide)
