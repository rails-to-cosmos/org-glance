;;; org-glance-bookmarks.el --- use org-link headlines in your agenda files as bookmarks.

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

(require 'load-relative)

(defvar org-glance-bmkp-cache-file "~/.emacs.d/org-glance/bookmarks.el")
(defvar org-glance-bmkp-filter
  (lambda (headline)
    (and (s-matches? org-bracket-link-regexp (org-element-property :raw-value headline))
         (-contains? (org-element-property :tags headline) "Bookmark"))))

;;;###autoload
(defun org-glance-bookmarks-jump (&optional force-reread-p)
  (interactive "P")
  (org-glance
   '(agenda-with-archives)
   :prompt "Jump to bookmark: "
   :cache-file og-bmkp-cache-file
   :fallback (lambda (x) (user-error "Bookmark not found"))
   :force-reread-p force-reread-p
   :title-property :TITLE
   :filter og-bmkp-filter
   :action #'org-glance-act--open-org-link))

;;;###autoload
(defun org-glance-bookmarks-visit (&optional force-reread-p)
  (interactive "P")
  (org-glance
   '(agenda-with-archives)
   :prompt "Visit bookmark: "
   :cache-file og-bmkp-cache-file
   :fallback (lambda (x) (user-error "Bookmark not found"))
   :action #'org-glance-act--visit-headline
   :force-reread-p force-reread-p
   :title-property :TITLE
   :filter org-glance-bmkp-filter))

(provide-me)
;;; org-glance-bookmarks.el ends here
