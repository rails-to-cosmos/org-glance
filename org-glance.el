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
(require 'aes)

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(defvar org-glance-prompt
  "Glance: "
  "Completing read prompt.")

(defvar org-glance-cache
  nil
  "Visited headlines file storage.")

(defvar org-glance-title-property
  :org-glance-title
  "Entry property considered as a title.")

(defvar org-glance-fallback
  nil
  "Fallback result of completing read.")

(defvar org-glance-action
  nil
  "Function to call on selected entry.")

(defvar org-glance-filter
  nil
  "Function to filter entries.")

(defvar org-glance-choice
  nil
  "Headline title to glance without prompt.")

(defvar org-glance-reread
  nil
  "Reread scope to org-glance-cache (if specified).")

(require 'org-glance-helpers)

(from :core import :all)
(from :plugins import :all)

(defun org-glance (&rest org-files)
  "Completing read on entries of ORG-FILES filtered by org-glance-filter.
Call org-glance-action on selected headline."
  (if-let ((headlines (if (and org-glance-cache
                               (file-exists-p org-glance-cache)
                               (null org-glance-reread))
                          (org-glance-load org-glance-cache)
                        (org-glance-read org-files org-glance-filter))))
      (unwind-protect
          (if-let ((choice (or org-glance-choice
                               (org-completing-read org-glance-prompt
                                                    (mapcar #'org-glance-format headlines))))
                   (headline (org-glance-browse headlines choice org-glance-fallback)))

            (unless headline
              (user-error "Headline not found"))

            (condition-case exc
                (org-glance-act headline org-glance-action)

              (org-glance-cache-outdated
               (message "Cache file %s is outdated, actualizing..." org-glance-cache)
               (redisplay)
               (let ((org-glance-choice choice)
                     (org-glance-reread t))
                 (apply #'org-glance org-files)))))

        ;; Unwind forms:
        (when (and org-glance-cache
                   (or (not (file-exists-p org-glance-cache))
                       org-glance-reread))
          (org-glance-save org-glance-cache headlines)))

    (user-error "Nothing to glance for")))

(provide 'org-glance)
;;; org-glance.el ends here
