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

  (setq org-glance-current-world
        (org-glance-benchmark
          (org-glance-world:create org-glance-directory
            (list (a-list :name "State"
                          :partitions '(list state)
                          :location '(format "%s.org" partition)
                          :predicate `(eq state partition)
                          :capture '(concat "* " partition " %?"))
                  ;; (a-list :name "Tag"
                  ;;         :partitions "tags"
                  ;;         :location "{{ PARTITION }}.org"
                  ;;         :predicate "(member '{{ PARTITION }} tags)"
                  ;;         :capture "* %? :{{ PARTITION }}:")
                  ))))

  (org-glance-world:add org-glance-current-world (org-glance-headline-from-string "* TODO task1 :task:"))
  (org-glance-world:add org-glance-current-world (org-glance-headline-from-string "* DONE task2 :task:"))
  (org-glance-world:persist org-glance-current-world)

  ;; Headline -- node
  ;; Relationship -- (headline1, headline2, class)
  ;; Indexes on classes and timestamps
  )


(provide 'org-glance)
;;; org-glance.el ends here
