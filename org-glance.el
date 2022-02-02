;;; org-glance.el --- org-mode traversing. Fast and convenient.

;; Copyright (C) 2018-2022 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 29 September, 2018
;; Version: 0.1.0

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
(require 'org-element)
(require 'cl-generic)
(require 'cl-lib)
(require 'cl-macs)
(require 'dash)
(require 'json)
(require 'seq)
(require 'subr-x)

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :version "27.2"
  :package-version "0.1.0"
  :group 'org)

(defcustom org-glance-directory org-directory
  "Directory with Org files."
  :group 'org-glance
  :type 'directory)

(defcustom org-glance-clone-on-repeat-p nil
  "Clone repeated headlines on complete."
  :group 'org-glance
  :type 'boolean)

(defvar org-glance-classes (make-hash-table)
  "Class registry. Maps class name symbol to config.")

(defvar org-glance-headlines (make-hash-table)
  "Headline registry. Maps headline id symbol to headline ast.")

;;; Exceptions
(define-error 'org-glance-error "Unknown org-glance error" 'user-error)
(define-error 'org-glance-error:SOURCE-CORRUPTED "Headline source corrupted, please reread" 'org-glance-error)
(define-error 'org-glance-error:PROPERTIES-CORRUPTED "Headline metadata corrupted, please reread" 'org-glance-error)
(define-error 'org-glance-error:METASTORE-OUTDATED "Metastore is outdated, please rebuild" 'org-glance-error)
(define-error 'org-glance-error:HEADLINE-NOT-FOUND "Headline not found" 'org-glance-error)
(define-error 'org-glance-error:HEADLINE-ALREADY-REGISTERED "Headline has already been registered" 'org-glance-error)
(define-error 'org-glance-error:CLASS-NOT-FOUND "Class not found" 'org-glance-error)

;;; Logging
(require 'org-glance-log)

;;; Helpers
(cl-defun org-glance-ensure-at-heading ()
  "Ensure point is at heading.
Return t if it is or nil otherwise."
  (unless (org-at-heading-p)
    (org-back-to-heading-or-point-min))
  (unless (org-at-heading-p)
    (user-error "Headline not found.")))

(cl-defun org-glance-replace-links-with-titles (ast)
  "Replace links with its titles in AST."
  (cl-loop for link in (org-element-map ast 'link #'identity)
     do (org-element-set-element link (or (-some->> link
                                            org-element-contents
                                            org-element-interpret-data)
                                          (org-element-property :raw-link link)))
     finally return ast))

(cl-defun org-glance-ensure-directory (directory)
  "Make DIRECTORY if not exist."
  (unless (f-exists? directory)
    (mkdir directory t)))

;; (cl-defun org-glance-list-directories (directory)
;;   "List directories in DIRECTORY."
;;   (--filter
;;    (f-directory? (f-join directory it))
;;    (directory-files directory nil "^[[:word:]]+")))

;;; Models

;; Headline
(cl-defun org-glance-create-headline ()
  "Create headline from `org-element' at point.
`org-glance-headline' is an `org-element' of type `org-data'
with some meta properties and `org-element' of type `headline' in contents."
  (save-excursion
    (org-glance-ensure-at-heading)
    (save-restriction
      (org-narrow-to-subtree)
      (let* ((ast (org-element-parse-buffer))
             (headline (car (org-element-contents ast)))
             (title (with-temp-buffer
                      (insert (or (org-element-property :TITLE headline)
                                  (org-element-property :raw-value headline)
                                  ""))
                      (->> (org-element-parse-buffer)
                           (org-glance-replace-links-with-titles)
                           (org-element-interpret-data)
                           (s-trim))))
             (hash (with-temp-buffer
                     (insert title)
                     (buffer-hash)))
             (tags (--map (downcase it) (org-element-property :tags headline))))

        ;; enrich basic ast with additional properties
        (org-element-put-property ast :title title)
        (org-element-put-property ast :tags (--map (intern it) tags))
        (org-element-put-property ast :id (intern (concat hash "_" (s-join "-" tags))))

        ;; no mutation restrictions on complete ast
        (org-element-put-property headline :level 1)

        ast))))

(cl-defun org-glance-headline-id (headline)
  (org-element-property :id headline))

(cl-defun org-glance-headline-title (headline)
  (org-element-property :title headline))

(cl-defun org-glance-headline-hash (headline)
  (org-element-property :hash headline))

(cl-defun org-glance-headline-tags (headline)
  (org-element-property :tags headline))

(cl-defun org-glance-headline-contents (headline)
  (s-trim (org-element-interpret-data headline)))

(cl-defun org-glance-register-headline (headline)
  (let ((id (org-glance-headline-id headline)))
    (if (gethash id org-glance-headlines)
        (signal 'org-glance-error:HEADLINE-ALREADY-REGISTERED "Headline has already been registered.")
      (puthash id headline org-glance-headlines))))

(cl-defun org-glance-init ()
  "Update system state from `org-glance-directory'."
  (org-glance-ensure-directory org-glance-directory))

(provide 'org-glance)
;;; org-glance.el ends here
