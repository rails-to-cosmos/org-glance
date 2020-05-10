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
;;   - fallback
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
  (require 'aes)
  (require 'cl)
  (require 'cl-generic)
  (require 'cl-lib)
  (require 'dash-functional)
  (require 'load-relative)
  (require 'org)
  (require 'seq)
  (require 'subr-x))

(require 'org-glance-views)
(require 'org-glance-cache)
(require 'org-glance-core)
(require 'org-glance-ledger)
(require 'org-glance-loc)
(require 'org-glance-scope)
(require 'org-glance-sec)

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(cl-defun org-glance (&key
                      filter
                      fallback
                      default-choice
                      cache-file
                      reread-p
                      (scope '(agenda))
                      (action #'org-glance--visit--any)
                      (prompt "Glance: ")
                      (title-property :TITLE))
  "Run completing read on org-files entries from SCOPE list prompting a PROMPT.
Scope can be file name or list of file names.
Filter headlines by FILTER method.
Call ACTION method on selected headline.
Specify CACHE-FILE to save headlines in read-optimized el-file.
Specify REREAD-P predicate to reread cache file. Usually this flag is set by C-u prefix.
If user input doesn't match any entry, call FALLBACK method with user input as argument.
Read headline title in completing read prompt from org property named TITLE-PROPERTY."
  (let (headlines)
    (when (or reread-p (not cache-file) (not (file-exists-p cache-file)))
      (when (and reread-p cache-file)
        (message "Reread cache file %s..." cache-file))
      (setq headlines
            (org-glance-cache-reread
             :scope scope
             :filter filter
             :cache-file cache-file
             :title-property title-property)))
    (unless headlines
      (setq headlines (org-glance-load cache-file :title-property title-property)))
    (unless headlines
      (user-error "Nothing to glance at (scope: %s)" scope))
    (unwind-protect
        (when-let (choice (or default-choice
                              (org-glance-completing-read headlines
                                                          :prompt prompt
                                                          :title-property title-property)))
          (if-let (headline (org-glance-browse headlines
                                               :choice choice
                                               :fallback fallback
                                               :title-property title-property))
              (condition-case nil
                  (if (functionp action)
                      (funcall action headline)
                    (user-error "Specify ACTION method to call on headline"))
                (org-glance-cache-outdated
                 (message "Cache file %s is outdated, actualizing..." cache-file)
                 (redisplay)
                 (org-glance :scope scope
                             :prompt prompt
                             :filter filter
                             :action action
                             :cache-file cache-file
                             :fallback fallback
                             :default-choice choice
                             :title-property title-property
                             :reread-p t)))
            (user-error "Headline not found")))
      ;; Unwind
      (when (and cache-file
                 (or reread-p
                     (not (file-exists-p cache-file))))
        (org-glance-save cache-file headlines :title-property title-property)))))

(provide-me)
;;; org-glance.el ends here
