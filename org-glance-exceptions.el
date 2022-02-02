;;; org-glance-exceptions.el --- exceptions library for `org-glance'.

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

(define-error 'org-glance-error "Unknown org-glance error" 'user-error)
(define-error 'org-glance-error:SOURCE-CORRUPTED "Headline source corrupted, please reread" 'org-glance-error)
(define-error 'org-glance-error:PROPERTIES-CORRUPTED "Headline metadata corrupted, please reread" 'org-glance-error)
(define-error 'org-glance-error:METASTORE-OUTDATED "Metastore is outdated, please rebuild" 'org-glance-error)
(define-error 'org-glance-error:HEADLINE-NOT-FOUND "Headline not found" 'org-glance-error)
(define-error 'org-glance-error:HEADLINE-ALREADY-REGISTERED "Headline has already been registered" 'org-glance-error)
(define-error 'org-glance-error:CLASS-NOT-FOUND "Class not found" 'org-glance-error)

(provide 'org-glance-exceptions)
;;; org-glance-exceptions.el ends here
