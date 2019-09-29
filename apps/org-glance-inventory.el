;;; org-glance-inventory.el --- fast schedule your regular shopping list.

;; Copyright (C) 2019 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 29 Sep 2019
;; Version: 0.1

;; Keywords: contacts org tools
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

(defvar og-inv-scope "~/sync/notes/household.org#regular-shopping")
(defvar og-inv-filter (lambda (headline)
                        (and (-contains? (org-element-property :tags headline) "Item")
                             (s-contains? "DONE" (org-element-property :raw-value headline)))))

(defun og-inv-repeat-headline (headline)
  (save-window-excursion
    (og-act--visit-headline headline)
    (org-todo 'todo)
    (org-schedule nil "+1d 20:00")
    (save-buffer)
    (redisplay)
    (message "%s added to shopping list on %s"
             (org-entry-get (point) "ITEM")
             (org-entry-get (point) "SCHEDULED"))))

;;;###autoload
(defun org-glance-inventory-add-to-shopping-list ()
  (interactive)
  (let ((org-glance-prompt "Add to shopping list: ")
        (org-glance-fallback (lambda (x) (user-error "Item not found.")))
        (org-glance-title-property :TITLE)
        (org-glance-action #'og-inv-repeat-headline)
        (org-glance-filter og-inv-filter))
    (org-glance og-inv-scope)))

(provide 'org-glance-inventory)
