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

(eval-when-compile
  (org-glance:require org))

(defcustom org-glance-directory (f-join user-emacs-directory "org-glance" "views")
  "The location where view metadata should be stored."
  :group 'org-glance
  :type 'string)

(cl-defstruct (org-glance-view (:constructor org-glance-view:create))
  "This structure contains metadata about categorized `org-mode' headlines."
  (id
   nil
   :read-only t
   :documentation "ID slot is a primary key that uniqly identifies `org-glance-view'."
   :type 'symbol)
  (type
   nil
   :read-only nil
   :documentation "List of actions allowed to use on headlines of this view."
   :type 'list)
  (scope
   nil
   :read-only nil
   :documentation "List of files/directories where org-glance should search for headlines for this view."
   :type 'list))

(defvar org-glance:views (make-hash-table)
  "Hash table (id->view) that lists all registered classes of things.")

(defvar org-glance:views-loaded nil
  "Alist of views registered in current emacs session.")

(defvar org-glance:actions nil
  "Alist containing registered actions.")

(org-glance:require
  cl-generic
  cl-lib
  cl-macs
  json
  org
  org-element
  seq
  subr-x

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
  lib.modes.materialized-headline-mode

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

(declare-function org-glance:format (org-glance-module-filename lib.utils.helpers))
(declare-function org-glance-metastore:choose-headline (org-glance-module-filename lib.core.metastore))
(declare-function org-glance-headlines (org-glance-module-filename lib.core.metastore))
(declare-function org-glance-overview:capture (org-glance-module-filename lib.modes.overview-mode))
(declare-function org-glance-view:choose (org-glance-module-filename lib.core.view))
(declare-function org-glance-headline:format (org-glance-module-filename lib.core.headline))
(declare-function org-glance-headline:at-point (org-glance-module-filename lib.core.headline))
(declare-function org-glance-headline:add-biconnected-relation (org-glance-module-filename lib.core.headline))
(declare-function org-glance-scope--choose-headline (org-glance-module-filename lib.core.scope))

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(cl-defun org-glance:use-@-for-relations ()
  "Rebind `@' key in `org-mode' buffers for relation management."
  (define-key org-mode-map (kbd "@")
    #'(lambda () (interactive)
        (if (or (looking-back "^" 1)
                (looking-back "[[:space:]]" 1))
            (org-glance:insert-relation)
          (insert "@")))))

(cl-defun org-glance:read-view-directories ()
  (directory-files org-glance-directory nil "^[[:word:]]+"))

(cl-defun org-glance:view-directory-loaded? (view-directory)
  (alist-get view-directory org-glance:views-loaded nil nil #'string=))

(cl-defun org-glance:view-config-file-location (view-directory)
  (f-join org-glance-directory view-directory (concat view-directory ".config.json")))

(cl-defun org-glance:view-config-file-read (view-directory)
  (json-read-file (org-glance:view-config-file-location view-directory)))

(cl-defun org-glance:view-directory-register (view-directory)
  (push (cons view-directory (current-time)) org-glance:views-loaded))

(cl-defun org-glance:system-init ()
  "Update all changed entities from `org-glance-directory'."
  (cl-loop
     for view-directory in (org-glance:read-view-directories)
     unless (org-glance:view-directory-loaded? view-directory)
     do
       (message "Read directory %s" view-directory)
       (apply 'org-glance-def-view
              (cl-loop
                 for (k . v) in (org-glance:view-config-file-read view-directory)
                 for pk = (intern (org-glance:format ":${k}"))
                 for pv = (cond ((member k '(type)) (mapcar 'intern v))
                                (t (intern v)))
                 when pk
                 append (list pk pv)))
       (org-glance:view-directory-register view-directory)

     unless
       (org-glance:view-directory-loaded? "posit")
     do
       (org-glance-def-view :id 'posit)
       (org-glance:view-directory-register "posit")

     unless
       (org-glance:view-directory-loaded? "ascertains")
     do
       (org-glance-def-view :id 'ascertains)
       (org-glance:view-directory-register "ascertains")

     unless
       (org-glance:view-directory-loaded? "thing")
     do
       (org-glance-def-view :id 'thing)
       (org-glance:view-directory-register "thing")

     unless
       (org-glance:view-directory-loaded? "class")
     do
       (org-glance-def-view :id 'class)
       (org-glance:view-directory-register "class")))

(cl-defun org-glance:get-or-capture ()
  "Choose thing from metastore or capture it if not found."
  (condition-case choice
      (org-glance-metastore:choose-headline)
    (org-glance-exception:headline-not-found
     (save-window-excursion
       (org-glance-overview:capture
        (org-glance-view:choose "Unknown thing. Please, specify it's class to capture: ")
        (cadr choice))))))

(cl-defun org-glance:insert-relation (&optional (target (org-glance:get-or-capture)))
  "Insert relation from `org-glance-headline' at point to TARGET.
C-u means not to insert relation at point, but register it in logbook instead."
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
       (action #'org-glance--visit--all)
       (prompt "Glance: "))
  "Deprecated main method, refactoring needed."
  (let ((headlines (org-glance-headlines :db db
                                         :db-init db-init
                                         :scope scope
                                         :filter filter)))
    (unwind-protect
         (when-let (choice (or default-choice
                               (org-completing-read prompt (mapcar #'org-glance-headline:title headlines))))
           (if-let (headline (org-glance-scope--choose-headline choice headlines))
               (condition-case nil (funcall action headline)
                 (org-glance-exception:metastore-outdated
                  (message "Database %s is outdated, actualizing..." db)
                  (redisplay)
                  (org-glance :scope scope
                              :filter filter
                              :action action
                              :db db
                              :db-init t
                              :default-choice choice
                              :prompt prompt)))
             (user-error "Headline not found"))))))

(provide 'org-glance)
;;; org-glance.el ends here
