;;; org-glance.el --- org-mode traversing. Fast and convenient.

;; Copyright (C) 2018-2019 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 29 September, 2018
;; Version: 1.0

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
(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'dash-functional)

(defgroup org-glance nil
  "\nOptions concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(defun require-from (dir &rest packages)
  (let ((load-path (list (expand-file-name dir (file-name-directory (or load-file-name buffer-file-name))))))
    (dolist (package packages)
      (require package))))

(require-from "core"
             'org-glance-entry
             'org-glance-adapter
             'org-glance-act
             'org-glance-scope)

(require-from "app"
             'org-glance-bookmark)

(defvar org-glance-prompt "Glance: ")

(defvar org-glance-cache nil
  "Visited headlines file storage.")

(defvar org-glance-title-property :org-glance-title
  "Entry property considered as a title.")

(defvar org-glance-fallback nil)
(defvar org-glance-action nil)
(defvar org-glance-filter nil)

(defun org-glance (&rest org-files)
  (let* ((cache org-glance-cache)
         (fallback org-glance-fallback)
         (action org-glance-action)
         (filter org-glance-filter)
         (headlines (if (and cache (file-exists-p cache))
                        (org-glance-load cache)
                      (org-glance-read org-files filter))))
    (unwind-protect
        (when-let (headline (org-glance-browse headlines fallback))
          (org-glance-act headline action))
      (when (and cache (not (file-exists-p cache)))
        (org-glance-save cache headlines)))))

(provide 'org-glance)
;;; org-glance.el ends here
