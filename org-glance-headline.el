;;; org-glance-headline.el --- headline model for `org-glance'.

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
(require 'dash)
(require 's)
(require 'eieio)
(require 'org-glance-helpers)

(defclass org-glance-headline ()
  ((title
    :initarg :title
    :type string
    :reader org-glance-headline:title)
   (class
    :initarg :class
    :type (satisfies (lambda (x) (or (symbolp x)
                                (-all? #'symbolp x))))
    :reader org-glance-headline:class)
   (contents
    :initarg :contents
    :reader org-glance-headline:contents)
   (features
    :initarg :features))
  "Headline model.")

(defvar org-glance-headline:features--bindat-spec
  '((archived byte)
    (commented byte)
    (closed byte)
    (encrypted byte)
    (linked byte)
    (propertized byte)))

(defun org-glance:ensure-at-heading ()
  "Ensure point is at heading.
Return t if it is or raise `user-error' otherwise."
  (unless (org-at-heading-p)
    (org-back-to-heading-or-point-min))
  (unless (org-at-heading-p)
    (user-error "Before the first headline")))

(cl-defmacro org-glance:with-heading-at-point (&rest forms)
  "Execute FORMS only if point is at heading."
  (declare (indent 0))
  `(save-excursion
     (org-glance:ensure-at-heading)
     (save-restriction
       (org-narrow-to-subtree)
       ,@forms)))

(defun org-glance-headline:create-from-heading-at-point ()
  "Create headline from `org-element' at point.
`org-glance-headline' is an `org-element' of type `org-data'
with some meta properties and `org-element' of type `headline' in contents."
  (org-glance:with-heading-at-point
      (let* ((ast (org-element-parse-buffer))
             (subtree (org-element-contents ast))
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
           :features (cl-flet ((bool-to-int (bool) (if (null bool) 0 1)))
                       (bindat-pack
                        org-glance-headline:features--bindat-spec
                        (list (cons 'archived (bool-to-int (org-element-property :archivedp element)))
                              (cons 'commented (bool-to-int (org-element-property :commentedp element)))
                              (cons 'closed (bool-to-int (org-element-property :closed element)))
                              (cons 'encrypted (bool-to-int (s-match-strings-all "aes-encrypted V [0-9]+.[0-9]+-.+\n" contents)))
                              (cons 'linked (bool-to-int (s-match-strings-all org-link-any-re contents)))
                              (cons 'propertized (bool-to-int (s-match-strings-all "\\([[:word:],[:blank:],_]+\\)\\:[[:blank:]]*\\(.*\\)" contents))))))
           :contents contents)))))

(cl-defmethod org-glance-headline:serialize ((headline org-glance-headline))
  "Serialize HEADLINE."
  (prin1-to-string headline))

(defun org-glance-headline:deserialize (dump)
  "Deserialize headline from DUMP."
  (read dump))

(cl-defmethod org-glance-headline:save ((headline org-glance-headline) file)
  "Write HEADLINE to FILE."
  (with-temp-file file
    (insert (org-glance-headline:serialize headline))))

(defun org-glance-headline:load (file)
  "Load headline from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-glance-headline:deserialize (buffer-string))))

(cl-defmethod org-glance-headline:features ((headline org-glance-headline))
  "Read HEADLINE features."
  (->> (slot-value headline 'features)
       (bindat-unpack org-glance-headline:features--bindat-spec)))

(cl-defmethod org-glance-headline:archived-p ((headline org-glance-headline))
  "Is HEADLINE archived?"
  (= 1 (alist-get 'archived (org-glance-headline:features headline))))

(cl-defmethod org-glance-headline:commented-p ((headline org-glance-headline))
  "Is HEADLINE commented?"
  (= 1 (alist-get 'commented (org-glance-headline:features headline))))

(cl-defmethod org-glance-headline:closed-p ((headline org-glance-headline))
  "Is HEADLINE closed?"
  (= 1 (alist-get 'closed (org-glance-headline:features headline))))

(cl-defmethod org-glance-headline:encrypted-p ((headline org-glance-headline))
  "Is HEADLINE encrypted?"
  (= 1 (alist-get 'encrypted (org-glance-headline:features headline))))

(cl-defmethod org-glance-headline:linked-p ((headline org-glance-headline))
  "Does the HEADLINE contain at least one `org-link'?"
  (= 1 (alist-get 'linked (org-glance-headline:features headline))))

(cl-defmethod org-glance-headline:propertized-p ((headline org-glance-headline))
  "Does the HEADLINE contain at least one user-defined property?"
  (= 1 (alist-get 'propertized (org-glance-headline:features headline))))

(provide 'org-glance-headline)
;;; org-glance-headline.el ends here
