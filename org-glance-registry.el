;;; org-glance-registry.el --- headline storage for `org-glance'.

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

(require 'eieio)
(require 'org-glance-headline)

(defclass org-glance-registry nil
  ((id
    :initarg :id
    :type string
    :reader org-glance-registry:id)
   (headlines
    :initform (make-hash-table)
    :reader org-glance-headlines)
   ;; relations
   ;; queries
   ))

(cl-defmethod org-glance-registry-put
    ((headline org-glance-headline)
     (registry org-glance-registry))
  "Put HEADLINE into REGISTRY."
  ;; (org-glance-headline-materialize headline registry)
  ;; (let ((id-key (org-glance-registry:id-key registry)))
  ;;   (or
  ;;    (org-glance-headline-property-get headline id-key)
  ;;    (let ((id (org-glance-registry--generate-id registry headline)))
  ;;      (org-glance-headline-property-set headline id-key id)
  ;;      (puthash id headline (org-glance-headlines registry)))))
  )

(cl-defmethod org-glance-registry:id-key ((registry org-glance-registry))
  "Determine id key for the specified REGISTRY.

This id key will be used to extract ids from existing headlines."
  (upcase (s-join "__" (list "org_glance" (org-glance-registry:id registry) "id"))))

(cl-defmethod org-glance-registry--generate-id
    ((registry org-glance-registry)
     (headline org-glance-headline))
  "Determine HEADLINE id for the specified REGISTRY.

- ID should be unique per REGISTRY.
- ID determination should be stable."
  (s-join "-" (list
               (s-join "|" (mapcar #'symbol-name (org-glance-headline:class headline)))
               (s-join ":" (mapcar #'number-to-string (current-time)))
               (secure-hash 'md5 (org-glance-headline:title headline)))))

(provide 'org-glance-registry)
;;; org-glance-registry.el ends here
