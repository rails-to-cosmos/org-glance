;;; org-glance-sec.el --- encryption utils for glance views    -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@gmail.com>
;; URL: https://github.com/rails-to-cosmos/org-glance
;; Package-Version: 20200430.000
;; Keywords: org, glance, password, security
;; Version: 0.1.0
;; Package-Requires: ((emacs "25") (aes "0.9))

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This package is

;;; Code:

(eval-when-compile
  (require 'cl))

(defun org-glance-sec-decrypt-subtree (&optional pwd)
  "Decrypt subtree at point.
If RETURN-PLAIN is non-nil, return decrypted contents as string."
  (let* ((beg (save-excursion (org-end-of-meta-data) (point)))
         (end (save-excursion (org-end-of-subtree t)))
         (encrypted (let ((encrypted (buffer-substring-no-properties beg end)))
                      (if (not (with-temp-buffer
                                 (insert encrypted)
                                 (aes-is-encrypted)))
                          (user-error "Headline is not encrypted")
                        encrypted)))
         (plain (aes-decrypt-buffer-or-string encrypted pwd)))
    (unless plain
      (user-error "Wrong password"))
    (save-excursion
      (org-end-of-meta-data)
      (kill-region beg end)
      (insert plain))))

(defun org-glance-sec-encrypt-subtree (&optional pwd)
  "Encrypt subtree at point."
  (let* ((beg (save-excursion (org-end-of-meta-data) (point)))
         (end (save-excursion (org-end-of-subtree t)))
         (plain (let ((plain (buffer-substring-no-properties beg end)))
                  (if (with-temp-buffer
                        (insert plain)
                        (aes-is-encrypted))
                      (user-error "Headline is already encrypted")
                    plain)))
         (encrypted (aes-encrypt-buffer-or-string plain pwd)))
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
          (dc (org-glance-sec-decrypt-subtree t)))
      (unwind-protect
          (with-temp-file tf
            (insert dc))
        (cl-loop while (condition-case nil
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
