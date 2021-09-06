;;; org-glance.el --- org-mode traversing. Fast and convenient.

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

(defcustom org-glance-directory (f-join user-emacs-directory "org-glance" "views")
  "The location where view metadata should be stored."
  :group 'org-glance
  :type 'string)

(cl-defstruct (org-glance-view (:constructor org-glance-view:create))
  "This structure contains metadata about categorized `org-mode' headlines."
  (id nil '(:read-only t
            :documentation "ID slot is a primary key that uniqly identifies `org-glance-view'."
            :type 'symbol))
  (type nil '(:read-only nil
              :documentation "List of actions allowed to use on headlines of this view."
              :type 'list))
  (scope nil '(:read-only nil
               :documentation "List of files/directories where org-glance should search for headlines for this view."
               :type 'list)))

(defvar org-glance:views (make-hash-table))
(defvar org-glance:actions nil)

(defvar -org-glance-initialized-views (make-hash-table :test #'equal))

(eval-and-compile
  (org-glance:require org
                      org-element))

(eval-when-compile
  (org-glance:require cl-generic
                      cl-lib
                      cl-macs
                      seq
                      subr-x))

(org-glance:require
  lib.utils.encryption                  ; encryption utils
  lib.utils.helpers                     ; unsorted, deprecated
  lib.utils.org                         ; org-mode shortcuts

;;; Core APIs
  ;; Description of high-level org-glance entities: Headline, View,
  ;; Scope and Metastore.

;;; Headline API
  ;; Org-glance headline is an org-element headline enriched by some
  ;; shortcuts and helper methods.
  lib.core.headline                     ; good
  lib.core.metastore                    ; ok
  lib.core.scope                        ; ? deprecated
  lib.core.view                         ; migrate to overview
  lib.core.actions                      ; deprecated

  lib.modes.overview-mode               ; good one, improve

  lib.links.visit

  lib.transient.headlines

  lib.plugins.metadata

  lib.actions.babel.insert
  lib.actions.encrypted-headlines.extract
  lib.actions.encrypted-headlines.materialize
  lib.actions.key-value-headlines.extract
  lib.actions.main.materialize
  lib.actions.main.open
  lib.actions.main.visit)

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(defvar org-glance-prompt "Glance: ")

(cl-defun org-glance:insert-relation
    (&optional (target (condition-case choice
                           (org-glance-metastore:choose-headline)
                         (org-glance-exception:headline-not-found
                          (org-glance-overview:capture-headline
                           (org-glance-view:choose "Unknown thing. Please, specify it's class to capture: ")
                           (cadr choice))))))
  "Insert relation from `org-glance-headline' at point to TARGET.
C-u means not to insert relation at point, but register it in logbook."
  (interactive)
  (unless current-prefix-arg
    (insert (org-glance-headline:format target)))
  (when-let (source (org-glance-headline:at-point))
    (org-glance-headline:add-biconnected-relation source target)))

(cl-defun org-glance
    (&key db
       default-choice
       (db-init nil)
       (filter #'(lambda (_) t))
       (scope '(agenda))
       (action #'org-glance--visit--all))
  "Run completing read on org entries from SCOPE asking an `org-glance-prompt'.
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
