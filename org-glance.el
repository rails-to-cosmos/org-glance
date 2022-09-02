;;; org-glance.el --- org-mode traversing. Fast and convenient. -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 29 September, 2018
;; Version: 1.0.0

;; Keywords: org-mode tools
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

(require 'org)
(require 'org-element)
(require 'org-glance-helpers)
(require 'org-glance-scope)
(require 'org-glance-headline)
(require 'org-glance-store)
(require 'org-glance-material-mode)

(require 'org-glance-buffer)

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :version "27.2"
  :package-version "1.0.0"
  :group 'org)

(defcustom org-glance-directory org-directory
  "Directory with Org files."
  :group 'org-glance
  :type 'directory)

;; (cl-defun org-glance-init ()
;;   "Update system state from `org-glance-directory'."
;;   (unless (f-exists? org-glance-directory)
;;     (mkdir org-glance-directory t))

;;   ;; Read `org-glance-store' from `org-glance-directory'

;;   ;; Actualize `org-glance-class-store'

;;   ;; Headline -- node
;;   ;; Relationship -- (headline1, headline2, class)
;;   ;; Indexes on classes and timestamps
;;   )

(provide 'org-glance)
;;; org-glance.el ends here
