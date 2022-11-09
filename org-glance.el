;;; org-glance.el --- org-mode traversing. Fast and convenient. -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 29 September, 2018
;; Version: 1.0.0

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
(require 'org-glance-log)
(require 'org-glance-helpers)
(require 'org-glance-types)
(require 'org-glance-scope)
(require 'org-glance-headline)
(require 'org-glance-dimension)
(require 'org-glance-world)
(require 'org-glance-material-mode)

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :version "27.2"
  :package-version "1.0.0"
  :group 'org)

(defcustom org-glance-directory org-directory
  "Directory that contains current `org-glance-world'."
  :group 'org-glance
  :type 'directory)

;; TODO prolog implementation is possible
(defconst org-glance-dimensions
  (list (org-glance-dimension :name 'tag       :form '(org-glance- headline :tags))
        (org-glance-dimension :name 'state     :form '(org-glance- headline :state))
        (org-glance-dimension :name 'title     :form '(org-glance- headline :title))
        (org-glance-dimension :name 'linked    :form '(org-glance- headline :linked?))
        (org-glance-dimension :name 'store     :form '(org-glance- headline :store?))
        (org-glance-dimension :name 'encrypted :form '(org-glance- headline :encrypted?))
        (org-glance-dimension :name 'closed    :form '(org-glance- headline :closed?))))

(defvar org-glance-current-world nil
  "Current `org-glance-world'.")

(cl-defun org-glance-capture ()
  (interactive)
  (org-glance-world:capture org-glance-current-world))

(cl-defun org-glance-materialize ()
  (interactive)
  (org-glance-world:materialize org-glance-current-world))

(cl-defun org-glance-agenda ()
  (interactive)
  (org-glance-world:agenda org-glance-current-world))

(cl-defun org-glance-jump ()
  (interactive)
  (org-glance-world:jump org-glance-current-world))

(cl-defun org-glance-extract ()
  (interactive)
  (org-glance-world:extract-headline org-glance-current-world))

(cl-defun org-glance-backfill ()
  (interactive)
  (org-glance-world:backfill org-glance-current-world))

(cl-defun org-glance-import (location)
  (interactive "DDirectory: ")
  (-> org-glance-current-world
      (org-glance-world:import location)
      (org-glance-world:persist)))

(cl-defun org-glance-init ()
  "Update system state from `org-glance-directory'."
  (let ((world (org-glance-world:get-or-create org-glance-directory)))
    (setf (org-glance- world :dimensions) org-glance-dimensions)
    (setq org-glance-current-world world)))

(provide 'org-glance)
;;; org-glance.el ends here
