;;; org-glance-headline-registry.el --- headline storage for `org-glance'.

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

(require 'org-glance-headline)

(defvar org-glance-headlines (make-hash-table)
  "Headline registry.  Maps headline id symbol to headline ast.")

;; (defun org-glance-headline-registry:headline-id (headline)
;;   "Generate unique ID for HEADLINE to store it in registry."
;;   (error "Not implemented"))

;; (defun org-glance-headline-registry:headline-origin (headline)
;;   (org-element-put-property ast :origin (list :file (buffer-file-name) :buffer (current-buffer))))

(defun org-glance-headline-registry:capture-headline-at-point ()
  "Add HEADLINE to `org-glance-headlines'."
  ;; (let ((id (org-glance-headline-registry:generate-id headline)))
  ;;   (puthash id headline org-glance-headlines))
  )

(provide 'org-glance-headline-registry)
;;; org-glance-headline-registry.el ends here
