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
  ((headlines
    :initform (make-hash-table)
    :reader org-glance-headlines)))

;; Headline registry
;; - Register headlines providing unique headline ID
;; - ID should be persisted in org headline
;; - Read headline properties by ID ~O(1)
;;; - Basic properties: ID, ORIGIN, TITLE, FEATURES
;; - Registry should be serializeable
;; - Remove/add headlines

(defun org-glance-headline-at-point ()
  "Create headline from `org-element' at point.
`org-glance-headline' is an `org-element' of type `org-data'
with some meta properties and `org-element' of type `headline' in contents."
  (org-glance:with-heading-at-point
    (let* ((subtree (->> (org-element-parse-buffer)
                         (org-element-contents)))
           (element (car subtree)))

      ;; normalize indentation
      (let ((indent-offset (1- (org-element-property :level element))))
        (when (> indent-offset 0)
          (cl-loop
             for headline in (org-element-map subtree 'headline #'identity)
             for level = (org-element-property :level headline)
             do (org-element-put-property headline :level (- level indent-offset)))))

      (let ((contents (->> subtree
                           org-element-interpret-data
                           substring-no-properties
                           s-trim)))
        (org-glance-headline
         :id (-some->> element
               (org-element-property :ORG_GLANCE_ID)
               (intern))
         :title (with-temp-buffer
                  (insert (or (org-element-property :TITLE element)
                              (org-element-property :raw-value element)
                              ""))
                  (->> (org-element-parse-buffer)
                       org-glance-replace-links-with-titles
                       org-element-interpret-data
                       substring-no-properties
                       s-trim))
         :class (--map (intern (downcase it)) (org-element-property :tags element))
         :archived (not (null (org-element-property :archivedp element)))
         :commented (not (null (org-element-property :commentedp element)))
         :closed (not (null (org-element-property :closed element)))
         :encrypted (not (null (s-match-strings-all "aes-encrypted V [0-9]+.[0-9]+-.+\n" contents)))
         :linked (not (null (s-match-strings-all org-link-any-re contents)))
         :propertized (not (null (s-match-strings-all "\\([[:word:],[:blank:],_]+\\)\\:[[:blank:]]*\\(.*\\)" contents)))
         :contents contents
         :file (buffer-file-name)
         :buffer (current-buffer))))))

(cl-defmethod org-glance-headline:register ((headline org-glance-headline)
                                            (registry org-glance-headline-registry))
  "Add HEADLINE to REGISTRY."
  (let ((id (org-glance-headline:id headline))
        (headlines (org-glance-headlines registry)))
    (cond
      ((null id) (user-error "Headline ID is NULL"))
      ((gethash id headlines) (user-error "Headline with ID = %s already registered" id))
      (t (puthash id (org-glance-headline:header headline) headlines)))))

(provide 'org-glance-headline-registry)
;;; org-glance-headline-registry.el ends here
