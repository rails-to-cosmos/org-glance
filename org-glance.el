;;; org-glance.el --- org-mode traversing. Fast and convenient.     -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Dmitry Akatov

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

;; glance view types:

;; default
;; - define interactive methods:
;;   - visit
;;   - materialize
;;   - reread
;; - define private methods:
;;   - filter
;; - create cache file in ~/.emacs.d/org-glance/org-glance-[view].el

;; link
;; - interactive methods:
;;   - open

;; type=link: open
;; type=password: decrypt-extract, encrypt-current, decrypt-current
;; type=babel: execute

;; 'link -- headline must contain org-link
;; 'kv -- key-value storage, can copy values to kill-ring
;; 'babel -- contain one or many blocks
;; 'encrypted -- encrypted subtree

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'cl-generic)
  (require 'cl-lib)
  (require 'dash-functional)
  (require 'load-relative)
  (require 'org)
  (require 'seq)
  (require 'subr-x))

(require 'org-glance-db)
(require 'org-glance-scope)
(require 'org-glance-transient)

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(cl-defun org-glance
    (&key db
          default-choice
          (db-init nil)
          (filter #'(lambda (_) t))
          (scope '(agenda))
          (action #'org-glance--visit--all)
          (prompt "Glance: "))
  "Run completing read on org entries from SCOPE asking a PROMPT.
Scope can be file name or list of file names.
Filter headlines by FILTER method.
Call ACTION method on selected headline.
Specify DB to save headlines in read-optimized el-file.
Specify DB-INIT predicate to reread cache file. Usually this flag is set by C-u prefix."
  (let* ((headlines
          (org-glance-headlines
           :db db
           :db-init db-init
           :scope scope
           :filter filter)))
    (unwind-protect
        (when-let (choice (or default-choice (org-glance-prompt-headlines prompt headlines)))
          (if-let (headline (org-glance-choose-headline choice headlines))
              (condition-case nil (funcall action headline)
                (org-glance-db-outdated
                 (message "Database %s is outdated, actualizing..." db)
                 (redisplay)
                 (org-glance :scope scope
                             :prompt prompt
                             :filter filter
                             :action action
                             :db db
                             :db-init t
                             :default-choice choice)))
            (user-error "Headline not found"))))))

(provide-me)
;;; org-glance.el ends here
