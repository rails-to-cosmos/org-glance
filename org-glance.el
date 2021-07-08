;;; org-glance.el --- org-mode traversing. Fast and convenient.     -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021 Dmitry Akatov

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

(require 'org-glance-module)

(org-glance-module-import lib.utils.helpers)
(org-glance-module-import lib.utils.org)

(org-glance-module-import lib.core.metastore)  ;; TODO refactor to headline structure
;; (declare-function org-glance-headlines (org-glance-module-filename lib.core.metastore))

(org-glance-module-import lib.core.headline)
(org-glance-module-import lib.core.headline.helpers)
(org-glance-module-import lib.core.headline.visit)

(org-glance-module-import lib.core.actions)

(org-glance-module-import lib.modes.overview-mode)

(org-glance-module-import lib.core.scope) ;; TODO refactor

;; (declare-function org-glance-scope--prompt-headlines (org-glance-module-filename lib.core.view))
;; (declare-function org-glance-scope--choose-headline (org-glance-module-filename lib.core.view))

(org-glance-module-import lib.core.view)
(org-glance-module-import lib.core.view.agenda)
(org-glance-module-import lib.core.view.doctor)
(org-glance-module-import lib.core.view.summary)

;; (declare-function org-glance-view:completing-read (org-glance-module-filename lib.core.view))
;; (declare-function org-glance-view:get-view-by-id (org-glance-module-filename lib.core.view))
;; (declare-function org-glance-view:headlines (org-glance-module-filename lib.core.view))
;; (declare-function org-glance-view:ids (org-glance-module-filename lib.core.view))
;; (declare-function org-glance-view:update (org-glance-module-filename lib.core.view))

(org-glance-module-import lib.core.view.doctor)

(org-glance-module-import lib.links.visit)
(org-glance-module-import lib.core.relations)

(org-glance-module-import lib.transient.headlines)
(org-glance-module-import lib.plugins.metadata)

;; Preload default actions

(org-glance-module-import lib.actions.main.materialize)
(org-glance-module-import lib.actions.main.open)
(org-glance-module-import lib.actions.main.visit)

(org-glance-module-import lib.actions.babel.insert)

;; Actions for encrypted headlines
(org-glance-module-import lib.actions.encrypted-headlines.extract)
(org-glance-module-import lib.actions.encrypted-headlines.materialize)

;; When headline is defined as a key-value storage, one can extract properties in efficient manner
(org-glance-module-import lib.actions.key-value-headlines.extract)

(eval-and-compile
  (require 'org)
  (require 'org-element)
  (require 'eieio-core))

(eval-when-compile
  (require 'cl-lib)
  (require 'cl-generic)
  (require 'cl-macs)
  (require 'org)
  (require 'seq)
  (require 'subr-x))

(require 'gv)

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(defvar org-glance-prompt "Glance: ")

(defun org-glance-show-report ()
  (interactive)
  (let ((begin_src "#+BEGIN: clocktable :maxlevel 9 :scope org-glance-exports :link yes :narrow 100 :formula % :properties (\"TAGS\") :block today :fileskip0 t :hidefiles t")
        (end_src "#+END:")
        (report-buffer (get-buffer-create "*org-glance-report*")))
    (with-current-buffer report-buffer
      (org-mode)
      (delete-region (point-min) (point-max))
      (insert begin_src)
      (insert "\n")
      (insert end_src)
      (goto-char (point-min))
      (org-ctrl-c-ctrl-c))
    (switch-to-buffer report-buffer)))

(cl-defun org-glance
    (&key db
       default-choice
       (db-init nil)
       (filter #'(lambda (_) t))
       (scope '(agenda))
       (action #'org-glance--visit--all))
  "Run completing read on org entries from SCOPE asking a `org-glance-prompt'.
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
         (when-let (choice (or default-choice (org-glance-scope--prompt-headlines org-glance-prompt headlines)))
           (if-let (headline (org-glance-scope--choose-headline choice headlines))
               (condition-case nil (funcall action headline)
                 (org-glance-db-outdated
                  (message "Database %s is outdated, actualizing..." db)
                  (redisplay)
                  (org-glance :scope scope
                              :filter filter
                              :action action
                              :db db
                              :db-init t
                              :default-choice choice)))
             (user-error "Headline not found"))))))

(provide 'org-glance)
;;; org-glance.el ends here
