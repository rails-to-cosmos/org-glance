;;; org-glance.el --- org-mode traversing. Fast and convenient.

;; Copyright (C) 2018-2022 Dmitry Akatov

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
(require 'org-glance-module)

(eval-and-compile
  (cl-defmacro org-glance:interactive-lambda (&rest forms)
    "Define interactive lambda function with FORMS in its body."
    (declare (indent 0) (debug t))
    `(lambda ()
       (interactive)
       ,@forms)))

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(defcustom org-glance-directory org-directory
  "Directory with Org files."
  :group 'org-glance
  :type 'directory)

(defcustom org-glance-clone-on-repeat-p nil
  "Clone repeated headlines on complete."
  :group 'org-glance
  :type 'boolean)

(org-glance:require
  cl-generic
  cl-lib
  cl-macs
  dash
  eieio
  json
  seq
  subr-x

  lib.utils.helpers

  lib.logging
  lib.exceptions

  lib.models.headline
  lib.models.scope)

(declare-function org-glance-ensure-directory (org-glance-module-filename lib.utils.helpers))

(defvar org-glance-classes (make-hash-table))
(defvar org-glance-headlines (make-hash-table))

(cl-defun org-glance-init ()
  "Update all changed entities from `org-glance-directory'."
  (org-glance-ensure-directory org-glance-directory))

(provide 'org-glance)
;;; org-glance.el ends here
