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
;; TODO could be implemented in headline header
(defconst org-glance-dimensions
  (list (org-glance-dimension :name 'tag       :form '(org-glance? headline :tags))
        (org-glance-dimension :name 'state     :form '(org-glance? headline :state))
        (org-glance-dimension :name 'title     :form '(org-glance? headline :title))
        (org-glance-dimension :name 'linked    :form '(org-glance? headline :linked?))
        (org-glance-dimension :name 'store     :form '(org-glance? headline :store?))
        (org-glance-dimension :name 'encrypted :form '(org-glance? headline :encrypted?))
        (org-glance-dimension :name 'closed    :form '(org-glance? headline :closed?))
        (org-glance-dimension :name 'repeated  :form '(org-glance? headline :repeated?))
        (org-glance-dimension :name 'active    :form '(org-glance? headline :active?))))

(defvar org-glance-current-world nil
  "Current `org-glance-world'.")

(lance-dec Capture :: t)
(lance-def Capture ()
  (interactive)
  (org-glance-world:capture org-glance-current-world
    ;; :text (apply #'org-link-make-string (org-store-link t t))
    ))

(lance-dec Materialize :: t)
(lance-def Materialize ()
  (interactive)
  (org-glance-world:materialize org-glance-current-world))

(lance-dec Agenda :: t)
(lance-def Agenda ()
  (interactive)
  (org-glance-world:agenda org-glance-current-world))

(lance-dec Jump :: t)
(lance-def Jump ()
  (interactive)
  (org-glance-world:jump org-glance-current-world))

(lance-dec Extract :: t)
(lance-def Extract ()
  (interactive)
  (org-glance-world:extract-headline org-glance-current-world))

(lance-dec Backfill :: t)
(lance-def Backfill ()
  (interactive)
  (org-glance-world:backfill org-glance-current-world))

(lance-dec Import :: ReadableDirectory -> t)
(lance-def Import (location)
  (interactive "DDirectory: ")
  (-> org-glance-current-world
      (org-glance-world:import location)
      (org-glance-world:persist)))

(lance-dec Init :: t)
(lance-def Init ()
  "Update system state from `org-glance-directory'."
  (interactive)
  (clrhash org-glance-world--cache)
  (clrhash org-glance-view--cache)
  (let ((world (org-glance-world:get-or-create org-glance-directory)))
    (setf (org-glance? world :dimensions) org-glance-dimensions)
    (setq org-glance-current-world world)))

(provide 'org-glance)
;;; org-glance.el ends here
