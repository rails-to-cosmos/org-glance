;;; org-glance-contacts.el --- fast contact information retrieval utility.

;; Copyright (C) 2019 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 8 Sep 2019
;; Version: 0.1

;; Keywords: contacts org tools
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

(defvar og-contacts-cache-file "~/.emacs.d/org-glance/contacts.el")
(defvar og-contacts-filter (lambda (headline)
                             (-contains? (org-element-property :tags headline) "Contact")))

(defun org-glance-contacts-visit (&optional org-glance-reread)
  (interactive "P")
  (let ((org-glance-prompt "Visit contact: ")
        (org-glance-cache og-contacts-cache-file)
        (org-glance-fallback (lambda (x) (user-error "Contact not found.")))
        (org-glance-title-property :TITLE)
        (org-glance-filter og-contacts-filter))
    (org-glance 'agenda-with-archives)))

(provide 'org-glance-contacts)