;;; org-glance-log.el --- logging utilities for `org-glance'.

;; Copyright (C) 2022 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 2 February, 2022
;; Version: 0.1.0

;; Keywords: org-glance logging
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
  "Print FORMAT-STRING formatted with ARGS when LOG-LEVEL is acceptable.
See `org-glance-log-level' for the details."
  (when (<= org-glance-log-level log-level)
    (apply #'message format-string args)))

(cl-defun org-glance-log-warning (format-string &rest args)
  "Log FORMAT-STRING formatted with ARGS in warning log level."
  (apply #'org-glance-log org-glance-log-level-WARNING format-string args))

(cl-defun org-glance-log-info (format-string &rest args)
  "Log FORMAT-STRING formatted with ARGS in info log level."
  (apply #'org-glance-log org-glance-log-level-INFO format-string args))

(cl-defun org-glance-log-debug (format-string &rest args)
  "Log FORMAT-STRING formatted with ARGS in debug log level."
  (apply #'org-glance-log org-glance-log-level-FINEST format-string args))

(provide 'org-glance-log)
;;; org-glance-log.el ends here
