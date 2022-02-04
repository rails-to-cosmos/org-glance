;;; org-glance-class.el --- class model for `org-glance'.

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

(require 'cl-macs)

(cl-defun org-glance-class-create (id &key capture-template header)
  "Create `org-glance-class' instance with specified parameters.

- ID is the unique identifier to manage class instances.
- CAPTURE-TEMPLATE is the default template to capture class headlines.
- HEADER is the custom header that will be added to `org-glance-class-overview' and materialized headline."
  (declare (indent 1))
  (cl-assert (symbolp id))
  (list :id id
        :capture-template capture-template
        :header header))

(defun org-glance-class-id (class)
  "Get CLASS ID."
  (plist-get class :id))

(provide 'org-glance-class)
;;; org-glance-class.el ends here
