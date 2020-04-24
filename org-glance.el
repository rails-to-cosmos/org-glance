;;; org-glance.el --- org-mode traversing. Fast and convenient.

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

;; glance types:

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

;; -*- lexical-binding: t -*-

(require 'org)
(require 'seq)
(require 'cl-lib)
(require 'load-relative)

(eval-when-compile
  (require 'aes)
  (require 'cl-generic)
  (require 'dash-functional)
  (require 'subr-x))

(require 'org-glance-loc)
(require 'org-glance-ledger)
(require 'org-glance-scope)
(require 'org-glance-matview)
(require 'org-glance-sec)
(require 'org-glance-cache)
(require 'org-glance-core)

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(cl-defun org-glance (&key
                      filter
                      fallback
                      default-choice
                      cache-file
                      force-reread-p
                      (scope '(agenda))
                      (action #'org-glance-act--visit-headline)
                      (prompt "Glance: ")
                      (title-property :TITLE))
  "Run completing read on org-files entries from SCOPE list prompting a PROMPT.
Scope can be file name or list of file names.
Filter headlines by FILTER method.
Call ACTION method on selected headline.
Specify CACHE-FILE to save headlines in read-optimized el-file.
Specify FORCE-REREAD-P predicate to reread cache file. Usually this flag is set by C-u prefix.
If user input doesn't match any entry, call FALLBACK method with user input as argument.
Read headline title in completing read prompt from org property named TITLE-PROPERTY."

  (let (headlines)
    (when (or force-reread-p (not cache-file) (not (file-exists-p cache-file)))
      (when (and force-reread-p cache-file)
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
                 (org-glance
                  :scope scope
                  :prompt prompt
                  :filter filter
                  :action action
                  :cache-file cache-file
                  :fallback fallback
                  :default-choice choice
                  :title-property title-property
                  :force-reread-p t)))
            (user-error "Headline not found")))
      ;; Unwind
      (when (and cache-file
                 (or force-reread-p
                     (not (file-exists-p cache-file))))
        (org-glance-save cache-file headlines :title-property title-property)))))

(cl-defmacro org-glance-def-view (tag
                                  &key
                                  bind
                                  type
                                  (scope '(agenda-with-archives))
                                  (title-property :TITLE)
                                  &allow-other-keys)

  (declare (indent 1))

  (let* ((dtag (s-downcase tag))
         (ctag (s-capitalize tag))
         (ns (format "org-glance--%s-" dtag))

         ;; default params
         (cache-file-name (format "~/.emacs.d/org-glance/org-glance-%s.el" dtag))

         ;; function names
         (fn-open (intern (concat ns "open")))
         (fn-reread (intern (concat ns "reread")))
         (fn-fallback (intern (concat ns "fallback")))
         (fn-filter (intern (concat ns "filter")))
         (fn-visit (intern (concat ns "visit")))
         (fn-materialize (intern (concat ns "materialize")))

         ;; encrypted views
         (fn-encrypt-current-headline (intern (concat ns "encrypt-current-headline")))
         (fn-decrypt-current-headline (intern (concat ns "decrypt-current-headline")))
         (fn-decrypt-extract (intern (concat ns "decrypt-extract")))

         ;; ledger views
         (fn-build-ledger-report (intern (concat ns "build-ledger-report"))))

    `(progn

       (cl-pushnew (intern ,tag) org-glance-views)

       (defun ,fn-filter (headline)
         (-contains? (mapcar #'s-downcase (org-element-property :tags headline)) ,dtag))

       (defun ,fn-fallback (_)
         (user-error "%s not found." ,ctag))

       (defun ,fn-visit (&optional force-reread-p)
         (interactive "P")
         (org-glance
          :scope (quote ,scope)
          :prompt ,(format "Visit %s: " dtag)
          :cache-file ,cache-file-name
          :force-reread-p force-reread-p
          :filter (function ,fn-filter)
          :fallback (function ,fn-fallback)
          :action (function org-glance-act--visit-headline)
          :title-property ,title-property))

       (defun ,fn-reread ()
         (interactive)
         (org-glance-cache-reread
          :scope (quote ,scope)
          :filter (function ,fn-filter)
          :cache-file ,cache-file-name
          :title-property ,title-property))

       (defun ,fn-materialize (&optional minify)
         (interactive)
         (,fn-reread)
         (org-glance-matview--materialize-file ,cache-file-name minify))

       (when (cond ((symbolp ,type) (eq ,type 'link))
                   ((listp ,type) (-contains? ,type 'link))
                   (t nil))

         (defun ,fn-open (&optional force-reread-p)
           (interactive "P")
           (org-glance
            :scope (quote ,scope)
            :prompt ,(format "Open %s: " dtag)
            :cache-file ,cache-file-name
            :force-reread-p force-reread-p
            :filter (function ,fn-filter)
            :fallback (function ,fn-fallback)
            :action (function org-glance-act--open-org-link)
            :title-property ,title-property)))

       (when (cond ((symbolp ,type) (eq ,type 'ledger))
                   ((listp ,type) (-contains? ,type 'ledger))
                   (t nil))

         (defun ,fn-build-ledger-report ()
           (interactive)
           (,fn-visit)
           (org-glance-ledger--build-report-from-subtree-at-point)))

       (when (cond ((symbolp ,type) (eq ,type 'encrypted))
                   ((listp ,type) (-contains? ,type 'encrypted))
                   (t nil))

         (defun ,fn-encrypt-current-headline ()
           (interactive)
           (org-glance-sec--encrypt-current-headline))

         (defun ,fn-decrypt-current-headline ()
           (interactive)
           (org-glance-sec--decrypt-current-headline))

         (when (cond ((symbolp ,type) (eq ,type 'kv))
                     ((listp ,type) (-contains? ,type 'kv))
                     (t nil))

           (defun ,fn-decrypt-extract (&optional force-reread-p)
             (interactive "P")
             (org-glance
              :scope (quote ,scope)
              :prompt ,(format "Decrypt and extract %s: " dtag)
              :cache-file ,cache-file-name
              :force-reread-p force-reread-p
              :filter (function ,fn-filter)
              :fallback (function ,fn-fallback)
              :action #'org-glance-sec--extract
              :title-property ,title-property))))

       (when (quote ,bind)
         (cl-loop for (binding . cmd) in (quote ,bind)
                  do (global-set-key (kbd binding) cmd))))))

(provide-me)

;;; org-glance.el ends here
