;;; org-glance.el --- org-mode traversing. Fast and convenient. -*- lexical-binding: t; -*-

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

(require 'org-glance-customs)
(require 'org-glance-exceptions)
(require 'org-glance-log)

;; (require 'org-glance-class)
(require 'org-glance-headline)
(require 'org-glance-headline-header)
(require 'org-glance-registry)

(require 'org-glance-material-mode)

(defun org-glance-init ()
  "Update system state from `org-glance-directory'."
  (org-glance--ensure-directory org-glance-directory)
  ;; Read `org-glance-registry' from `org-glance-directory'

  ;; Actualize `org-glance-class-registry'

  ;; Storage partitioning schema: class/created-date/headline-id/headline.el
  ;; Archive partitioning schema: class/closed-date/headline-id/headline.el
  )

(provide 'org-glance)
;;; org-glance.el ends here
