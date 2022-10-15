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
(require 'org-glance-debug)
(require 'org-glance-helpers)
(require 'org-glance-types)
(require 'org-glance-scope)
(require 'org-glance-headline)
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

(defvar org-glance-current-world nil
  "Current `org-glance-world'.")

(cl-defun org-glance-init ()
  "Update system state from `org-glance-directory'."
  (interactive)

  (unless (f-exists? org-glance-directory)
    (mkdir org-glance-directory t))

  (setq org-glance-current-world (org-glance-world:create org-glance-directory))

  ;; /tmp/world
  ;; /tmp/world/log/000.log
  ;; /tmp/world/log/001.log
  ;; /tmp/world/log/002.log
  ;; /tmp/world/log/003.log
  ;; /tmp/world/view/by-state/todo
  ;; /tmp/world/view/by-tag/bookmark.org
  ;; /tmp/world/view/by-tag/task.org
  ;; /tmp/world/view/by-tag/author.org
  ;; /tmp/world/view/by-feature/archived
  ;; /tmp/world/view/by-feature/commented
  ;; /tmp/world/view/by-feature/linked
  ;; /tmp/world/view/by-property/author=HELLO.org
  ;; /tmp/world/view/by-date/2022-01-01.org

  ;; (add-rule org-glance-current-world
  ;;           (lambda (headline)
  ;;             (when (org-glance- headline :state)
  ;;               (org-glance-))))

  ;; (lambda (headline)
  ;;   (let (result)
  ;;     (push (upcase (org-glance- headline :state)) result)
  ;;     (dolist (tag (org-glance- headline :class))
  ;;       (push (downcase tag) result))
  ;;     (-non-nil result)))

  ;; Read `org-glance-world' from `org-glance-directory'

  ;; Actualize `org-glance-class-world'

  ;; Headline -- node
  ;; Relationship -- (headline1, headline2, class)
  ;; Indexes on classes and timestamps
  )

(provide 'org-glance)
;;; org-glance.el ends here
