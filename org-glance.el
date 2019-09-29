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
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(defmacro from (relpath &rest body)
  (declare (debug (form body))
           (indent 1))
  `(let* ((file (or load-file-name buffer-file-name))
          (path (file-name-directory file))
          (load-path (list (expand-file-name ,relpath path))))
     ,@body))

(from "core"
  (require 'org-glance-entry)
  (require 'org-glance-adapter)
  (require 'org-glance-act)
  (require 'org-glance-scope))

(from "apps"
  (require 'org-glance-bookmarks)
  (require 'org-glance-contacts)
  (require 'org-glance-inventory))

(defvar org-glance-prompt "Glance: "
  "Completing read prompt.")
(defvar org-glance-cache nil
  "Visited headlines file storage.")
(defvar org-glance-title-property :org-glance-title
  "Entry property considered as a title.")
(defvar org-glance-fallback nil
  "Fallback result of completing read.")
(defvar org-glance-action nil
  "Function to call on selected entry.")
(defvar org-glance-filter nil
  "Function to filter entries.")
(defvar org-glance-choice nil
  "Headline title to glance without prompt.")

(defun org-glance (&rest org-files)
  "Completing read on entries of ORG-FILES filtered by org-glance-filter.
Call org-glance-action on selected headline."
  (let* ((cache org-glance-cache)
         (fallback org-glance-fallback)
         (action org-glance-action)
         (filter org-glance-filter)
         (headlines (if (and cache (file-exists-p cache))
                        (org-glance-load cache)
                      (org-glance-read org-files filter))))
    (unless headlines
      (user-error "Nothing to glance for"))
    (unwind-protect
        (let* ((prompt org-glance-prompt)
               (choice (or org-glance-choice
                           (org-completing-read prompt (mapcar #'org-glance-format headlines))))
               (headline (org-glance-browse headlines choice fallback)))

          (unless headline
            (user-error "Headline not found"))

          (condition-case nil
              (org-glance-act headline action)
            (user-error
             (message "Cache file %s is outdated, actualizing..." cache)
             (redisplay)
             (delete-file cache)
             (let ((org-glance-choice choice))
               (apply #'org-glance org-files)))))

      (when (and cache (not (file-exists-p cache)))
        (org-glance-save cache headlines)))))

(provide 'org-glance)
;;; org-glance.el ends here
