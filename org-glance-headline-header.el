;;; org-glance-headline-header.el --- headline header for `org-glance' interaction.

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

;;; Code:

(require 'bindat)
(require 'org-glance-headline)

(defvar org-glance-headline:header--bindat-spec
  '((title str 255)
    (file str 255)
    (archived byte)
    (commented byte)
    (closed byte)
    (encrypted byte)
    (linked byte)
    (propertized byte)))

(cl-defmethod org-glance-headline:header ((headline org-glance-headline))
  "Read HEADLINE features."
  (cl-flet ((bool->int (bool) (if (null bool) 0 1)))
    (bindat-pack
     org-glance-headline:header--bindat-spec
     (list
      (cons 'title (org-glance-headline:title headline))
      (cons 'file (org-glance-headline:file headline))
      (cons 'archived (bool->int (org-glance-headline:archived-p headline)))
      (cons 'commented (bool->int (org-glance-headline:commented-p headline)))
      (cons 'closed (bool->int (org-glance-headline:closed-p headline)))
      (cons 'encrypted (bool->int (org-glance-headline:encrypted-p headline)))
      (cons 'linked (bool->int (org-glance-headline:linked-p headline)))
      (cons 'propertized (bool->int (org-glance-headline:propertized-p headline)))))))

(provide 'org-glance-headline-header)
;;; org-glance-headline-header.el ends here
