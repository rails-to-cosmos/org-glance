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

(require 'eieio)

(require 'org-glance-helpers)
(require 'org-glance-serializable)
(require 'org-glance-headline)
(require 'org-glance-headline-header)

(defclass org-glance-headline-registry (org-glance-serializable)
  ((id :initarg :id
       :type string
       :reader org-glance-headline-registry:id)
   (headlines :initform (make-hash-table)
              :reader org-glance-headlines)))

;; Headline registry
;; - Register headlines providing unique headline ID
;; - ID should be persisted in org headline
;; - Read headline properties by ID ~O(1)
;;; - Basic properties: ID, ORIGIN, TITLE, FEATURES
;; - Registry should be serializeable
;; - Remove/add headlines

(cl-defmethod org-glance-headline:generate-id
    ((registry org-glance-headline-registry)
     (headline org-glance-headline))
  "Determine HEADLINE id for the specified REGISTRY.

- ID should be unique inside registry.
- ID should be stored in HEADLINE."
  (s-join "-" (list
               (s-join "|" (mapcar #'symbol-name (org-glance-headline:class headline)))
               (s-join ":" (mapcar #'number-to-string (current-time)))
               (secure-hash 'md5 (org-glance-headline:title headline)))))

(cl-defmethod org-glance-headline:register ((headline org-glance-headline)
                                            (registry org-glance-headline-registry))
  "Add HEADLINE to REGISTRY."
  (let ((id-key (upcase (s-join "__" (list "org_glance" (org-glance-headline-registry:id registry) "id")))))
    (or
     (org-glance-headline-get-property headline id-key)
     (let ((id (org-glance-headline:generate-id registry headline)))
       (org-glance-headline-set-property headline id-key id)))

    ;; apply changes to headline
    ;; add headline to registry
    ))

(provide 'org-glance-headline-registry)
;;; org-glance-headline-registry.el ends here
