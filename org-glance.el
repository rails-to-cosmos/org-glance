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
  "Class registry. Maps class name to class config.")

(defvar org-glance-headlines (make-hash-table)
  "Headline registry. Maps headline id symbol to headline ast.")

;;; Exceptions
(define-error 'org-glance-error "Unknown org-glance error" 'user-error)
(define-error 'org-glance-error:SOURCE-CORRUPTED "Headline source corrupted, please reread" 'org-glance-error)
(define-error 'org-glance-error:PROPERTIES-CORRUPTED "Headline metadata corrupted, please reread" 'org-glance-error)
(define-error 'org-glance-error:METASTORE-OUTDATED "Metastore is outdated, please rebuild" 'org-glance-error)
(define-error 'org-glance-error:HEADLINE-NOT-FOUND "Headline not found" 'org-glance-error)
(define-error 'org-glance-error:CLASS-NOT-FOUND "Class not found" 'org-glance-error)

;;; Logging
(defconst org-glance-log-level-OFF most-positive-fixnum
    "OFF is a special level that can be used to turn off logging.

This level is initialized to `most-positive-fixnum'.")

(defconst org-glance-log-level-SEVERE 1000
  "SEVERE is a message level indicating a serious failure.
In general SEVERE messages should describe events that are of
considerable importance and which will prevent normal program
execution.

They should be reasonably intelligible to end users and to system
administrators. This level is initialized to 1000.")

(defconst org-glance-log-level-WARNING 900
  "WARNING is a message level indicating a potential problem.
In general WARNING messages should describe events that will be
of interest to end users or system managers, or which indicate
potential problems.

This level is initialized to 900.")

(defconst org-glance-log-level-INFO 800
  "INFO is a message level for informational messages.
Typically INFO messages will be written to the console or its
equivalent.

So the INFO level should only be used for reasonably significant
messages that will make sense to end users and system
administrators.

This level is initialized to 800.")

(defconst org-glance-log-level-CONFIG 700
  "CONFIG is a message level for static configuration messages.
CONFIG messages are intended to provide a variety of static
configuration information, to assist in debugging problems that
may be associated with particular configurations.

For example, CONFIG message might include the CPU type, the
graphics depth, the GUI look-and-feel, etc. This level is
initialized to 700.")

(defconst org-glance-log-level-FINE 500
  "FINE is a message level providing tracing information.
All of FINE, FINER, and FINEST are intended for relatively
detailed tracing.

The exact meaning of the three levels will vary between
subsystems, but in general, FINEST should be used for the most
voluminous detailed output, FINER for somewhat less detailed
output, and FINE for the lowest volume (and most important)
messages.

In general the FINE level should be used for information that
will be broadly interesting to developers who do not have a
specialized interest in the specific subsystem.

FINE messages might include things like minor (recoverable)
failures. Issues indicating potential performance problems are
also worth logging as FINE. This level is initialized to 500.")

(defconst org-glance-log-level-FINER 400
  "FINER indicates a fairly detailed tracing message.
By default logging calls for entering, returning, or throwing an
exception are traced at this level. This level is initialized to
400.")

(defconst org-glance-log-level-FINEST 300
  "ALL indicates that all messages should be logged.
This level is initialized to `most-negative-fixnum'.")

(defconst org-glance-log-level-ALL most-negative-fixnum
  "FINEST indicates a highly detailed tracing message.
This level is initialized to 300.")

(defcustom org-glance-log-level org-glance-log-level-INFO "Logging level."
  :type `(choice (const :tag "OFF" ,org-glance-log-level-OFF)
                 (const :tag "SEVERE" ,org-glance-log-level-SEVERE)
                 (const :tag "WARNING" ,org-glance-log-level-WARNING)
                 (const :tag "INFO" ,org-glance-log-level-INFO)
                 (const :tag "CONFIG" ,org-glance-log-level-CONFIG)
                 (const :tag "FINE" ,org-glance-log-level-FINE)
                 (const :tag "FINER" ,org-glance-log-level-FINER)
                 (const :tag "FINEST" ,org-glance-log-level-FINEST)
                 (const :tag "ALL" ,org-glance-log-level-ALL))
  :group 'org-glance)

(cl-defun org-glance-log (log-level format-string &rest args)
  (when (<= org-glance-log-level log-level)
    (apply #'message format-string args)))

(cl-defun org-glance-log-warning (format-string &rest args)
  "Log warning if `org-glance-log-level' allows."
  (apply #'org-glance-log org-glance-log-level-WARNING format-string args))

(cl-defun org-glance-log-info (format-string &rest args)
  "Log info if `org-glance-log-level' allows."
  (apply #'org-glance-log org-glance-log-level-INFO format-string args))

(cl-defun org-glance-log-debug (format-string &rest args)
  "Log debug message if `org-glance-log-level' allows."
  (apply #'org-glance-log org-glance-log-level-FINEST format-string args))

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
             (headline (car (org-element-contents ast))))

        (org-element-put-property ast :title (with-temp-buffer
                                               (insert (or (org-element-property :TITLE headline)
                                                           (org-element-property :raw-value headline)
                                                           ""))
                                               (->> (org-element-parse-buffer)
                                                    (org-glance-replace-links-with-titles)
                                                    (org-element-interpret-data)
                                                    (s-trim))))

        (org-element-put-property ast :tags (--map (intern (downcase it)) (org-element-property :tags headline)))

        ;; no mutation restrictions on complete ast
        (org-element-put-property headline :level 1)

        ast))))

(cl-defun org-glance-headline-title (headline)
  (org-element-property :title headline))

(cl-defun org-glance-headline-tags (headline)
  (org-element-property :tags headline))

(cl-defun org-glance-headline-contents (headline)
  (s-trim (org-element-interpret-data headline)))

(cl-defun org-glance-init ()
  "Update all changed entities from `org-glance-directory'."
  (org-glance-ensure-directory org-glance-directory))

(provide 'org-glance)
;;; org-glance.el ends here
