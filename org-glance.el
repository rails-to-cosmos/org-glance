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
(require 'load-relative)

(require-relative-list
 '("./core/org-glance-act.el"
   "./core/org-glance-adapter.el"
   "./core/org-glance-entry.el"
   "./core/org-glance-exceptions.el"
   "./core/org-glance-scope.el"

   "./plugins/org-glance-bookmarks.el"
   "./plugins/org-glance-password-manager.el"
   "./plugins/org-glance-contacts.el"))

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(cl-defun org-glance (scope
                      &key
                      filter
                      action
                      fallback
                      default-choice
                      cache-file force-reread-p
                      (prompt "Glance: ")
                      (title-property :org-glance-title))
  "Run completing read on org-files entries from SCOPE list prompting a PROMPT.
Filter headlines by FILTER method.
Call ACTION method on selected headline.
Specify CACHE file name to save headlines to read-optimized el-file.
Specify FORCE-REREAD-P predicate to reread cache file.
If user input doesn't match any entry, call FALLBACK method with user input as argument.
Read headline title in completing read prompt from org-property TITLE-PROPERTY."
  (if-let ((headlines (if (and cache-file
                               (file-exists-p cache-file)
                               (null force-reread-p))
                          (org-glance-load cache-file
                           :title-property title-property)
                        (org-glance-read scope
                         :filter filter))))
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
                     (org-glance scope
                                 :prompt prompt
                                 :filter filter
                                 :action action
                                 :cache-file cache-file
                                 :fallback fallback
                                 :default-choice choice
                                 :force-reread-p t)))
                (user-error "Headline not found")))

        ;; Unwind
        (when (and cache-file
                   (or force-reread-p
                       (not (file-exists-p cache-file))))
          (org-glance-save cache-file headlines
                           :title-property title-property)))

    (user-error "Nothing to glance for")))

(provide-me)
;;; org-glance.el ends here
