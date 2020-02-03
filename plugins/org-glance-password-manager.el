;;; org-glance-password-manager.el --- encrypt org-headline contents

;; Copyright (C) 2019-2020 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 20 Oct 2019
;; Version: 1.0

;; Keywords: org tools password manager
;; Homepage: https://github.com/rails-to-cosmos/org-glance

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package allows you to manage bookmarks and travel around the
;; digital world with an org-mode power behind your shoulders.

;;; Code:

(defvar org-glance-pm-cache-file "~/.emacs.d/org-glance/passwords.el")

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
         (encrypted (aes-encrypt-buffer-or-string plain)))
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

(defun org-glance-password-manager-visit (&optional force-reread-p)
  (interactive "P")
  (org-glance
   '(agenda-with-archives)
   :prompt "Visit secure data: "
   :cache-file org-glance-pm-cache-file
   :force-reread-p force-reread-p
   :fallback (lambda (x) (user-error "Entry not found."))
   :title-property :TITLE
   :filter #'org-glance-pm--filter
   :action #'org-glance-act--visit-headline))

(defun org-glance-password-manager-secure-data-to-kill-ring (&optional force-reread-p)
  (interactive "P")
  (org-glance
   '(agenda-with-archives)
   :prompt "Extract secure data: "
   :cache-file org-glance-pm-cache-file
   :force-reread-p force-reread-p
   :fallback (lambda (x) (user-error "Entry not found."))
   :title-property :TITLE
   :filter #'org-glance-pm--filter
   :action #'org-glance-pm--extract))

(defun org-glance-pm--extract (headline)
  (with-temp-buffer
    (org-mode)
    (insert-file-contents (org-element-property :file headline))
    (goto-char (org-element-property :begin headline))
    (org-narrow-to-subtree)
    (let ((tf (make-temp-file "org-glance-pm"))
          (dc (org-glance-password-manager-decrypt-current)))
      (unwind-protect
          (with-temp-file tf
            (insert dc))
        (while (condition-case exc
                   (org-glance tf
                               :prompt "Copy to kill ring: "
                               :action #'org-glance-pm--copy)
                 (quit (kill-new "" t) nil)
                 (error (kill-new "" t) nil)))
        (delete-file tf)))))

(defun org-glance-pm--copy (headline)
  (with-temp-buffer
    (org-mode)
    (insert-file-contents (org-element-property :file headline))
    (goto-char (org-element-property :begin headline))
    (let* ((beg (save-excursion (org-end-of-line) (1+ (point))))
           (end (save-excursion (org-end-of-subtree t)))
           (contents (buffer-substring-no-properties beg end)))
      (kill-new contents t))))

(defun org-glance-pm--filter (headline)
  (save-excursion
    (goto-char (org-element-property :begin headline))
    (let* ((beg (save-excursion (org-end-of-meta-data) (point)))
           (end (save-excursion (org-end-of-subtree t)))
           (contents (buffer-substring-no-properties beg end)))
      (with-temp-buffer
        (insert contents)
        (aes-is-encrypted)))))

(provide-me)
;;; org-glance-password-manager.el ends here
