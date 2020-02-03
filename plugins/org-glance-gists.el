;;; org-glance-gists.el --- fast src-blocks retrieval utility.

;; Copyright (C) 2019-2020 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 2 Dec 2019
;; Version: 0.1

;; Keywords: babel org tools
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

(defvar og-gists-cache-file "~/.emacs.d/org-glance/gists.el")

(defun org-glance-gists-visit (&optional force-reread-p)
  (interactive "P")
  (org-glance '(agenda-with-archives)
              :prompt "Visit gist: "
              :cache-file og-gists-cache-file
              :fallback (lambda (x) (user-error "Gist not found."))
              :title-property :TITLE
              :filter (lambda (headline) (-contains? (org-element-property :tags headline) "Gist"))
              :force-reread-p force-reread-p
              :action #'org-glance-act--visit-headline))

(provide-me)
;;; org-glance-gists.el ends here
