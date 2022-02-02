;;; org-glance-headline.el --- org helpers for `org-glance'.

;; Copyright (C) 2022 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 2 February, 2022
;; Version: 0.1.0

;; Keywords: org-glance logging
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

(require 'cl-macs)
(require 'dash)
(require 'org-element)

(defun org-glance-ensure-at-heading ()
  "Ensure point is at heading.
Return t if it is or nil otherwise."
  (unless (org-at-heading-p)
    (org-back-to-heading-or-point-min))
  (unless (org-at-heading-p)
    (user-error "Headline not found")))

(defun org-glance-replace-links-with-titles (ast)
  "Replace links with its titles in AST."
  (cl-loop for link in (org-element-map ast 'link #'identity)
     do (org-element-set-element link (or (-some->> link
                                            org-element-contents
                                            org-element-interpret-data)
                                          (org-element-property :raw-link link)))
     finally return ast))

(defun org-glance-ensure-directory (directory)
  "Make DIRECTORY if not exist."
  (unless (f-exists? directory)
    (mkdir directory t)))

;; (cl-defun org-glance-list-directories (directory)
;;   "List directories in DIRECTORY."
;;   (--filter
;;    (f-directory? (f-join directory it))
;;    (directory-files directory nil "^[[:word:]]+")))

(provide 'org-glance-helpers)
;;; org-glance-helpers.el ends here
