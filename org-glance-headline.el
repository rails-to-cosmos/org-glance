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

(require 'dash)
(require 's)
(require 'eieio)
(require 'ol)

(require 'org-glance-serializable)

(defclass org-glance-headline (org-glance-serializable)
  ((title
    :initarg :title
    :type string
    :reader org-glance-headline:title)
   (class
    :initarg :class
    :type (satisfies (lambda (x) (or (symbolp x)
                                (-all? #'symbolp x))))
    :reader org-glance-headline:class)
   (contents    :initarg :contents    :reader org-glance-headline:contents)
   (archived    :initarg :archived    :reader org-glance-headline:archived-p)
   (commented   :initarg :commented   :reader org-glance-headline:commented-p)
   (closed      :initarg :closed      :reader org-glance-headline:closed-p)
   (encrypted   :initarg :encrypted   :reader org-glance-headline:encrypted-p)
   (linked      :initarg :linked      :reader org-glance-headline:linked-p)
   (propertized :initarg :propertized :reader org-glance-headline:propertized-p)
   (properties  :initarg :properties :reader org-glance-headline:properties)
   (file        :initarg :file        :reader org-glance-headline:file)
   (buffer      :initarg :buffer
                :initform nil
                :reader org-glance-headline:buffer))
  "Headline model.")

(cl-defmacro org-glance:with-heading-at-point (&rest forms)
  "Execute FORMS only if point is at heading."
  (declare (indent 0))
  `(save-excursion
     (org-glance--ensure-at-heading)
     (save-restriction
       (org-narrow-to-subtree)
       ,@forms)))

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
                           (org-element-interpret-data)
                           (substring-no-properties)
                           (s-trim))))
        (org-glance-headline
         :title (with-temp-buffer
                  (insert (or (org-element-property :TITLE element)
                              (org-element-property :raw-value element)
                              ""))
                  (->> (org-element-parse-buffer)
                       (org-glance--links-to-titles)
                       (org-element-interpret-data)
                       (substring-no-properties)
                       (s-trim)))
         :class (--map (intern (downcase it)) (org-element-property :tags element))
         :archived (not (null (org-element-property :archivedp element)))
         :commented (not (null (org-element-property :commentedp element)))
         :closed (not (null (org-element-property :closed element)))
         :encrypted (not (null (s-match-strings-all "aes-encrypted V [0-9]+.[0-9]+-.+\n" contents)))
         :linked (not (null (s-match-strings-all org-link-any-re contents)))
         :propertized (not (null (s-match-strings-all "\\([[:word:],[:blank:],_]+\\)\\:[[:blank:]]*\\(.*\\)" contents)))
         :properties (org-entry-properties)
         :contents contents
         :file (buffer-file-name)
         :buffer (current-buffer))))))

(cl-defmethod org-glance-headline-copy ((headline org-glance-headline) (slots list))
  (let ((args (cl-loop for slot in slots
                 append (list (intern (format ":%s" slot))
                              (slot-value headline slot)))))
    (apply 'org-glance-headline args)))

;; override
(cl-defmethod org-glance-serialize ((headline org-glance-headline))
  "Serialize HEADLINE."
  (prin1-to-string (org-glance-headline-copy headline '(title class archived commented closed encrypted linked propertized contents file))))

(cl-defmethod org-glance-headline-save ((headline org-glance-headline) file)
  "Write HEADLINE to FILE."
  (with-temp-file file
    (insert (org-glance-serialize headline))))

(defun org-glance-headline-load (file)
  "Load headline from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-glance-deserialize (buffer-string))))

(cl-defmethod org-glance-headline-get-property ((headline org-glance-headline) key)
  "Retrieve KEY from HEADLINE properties."
  (alist-get key (org-glance-headline:properties headline) nil nil #'string=))

(cl-defmethod org-glance-headline-set-property ((headline org-glance-headline) key (value string))
  "Set HEADLINE property KEY to VALUE."
  (let ((properties (org-glance-headline:properties headline)))
    (setf (alist-get key properties nil t #'string=) value)
    (setf (slot-value headline 'properties) properties)
    (setf (slot-value headline 'contents)
          (with-temp-buffer
            (org-mode)
            (save-excursion
              (insert (org-glance-headline:contents headline))
              (org-set-property key value)
              (buffer-substring-no-properties (point-min) (point-max)))))
    value))

(cl-defmethod org-glance-headline-equal-p ((a org-glance-headline) (b org-glance-headline))
  (string= (org-glance-headline:contents a)
           (org-glance-headline:contents b)))

(provide 'org-glance-headline)
;;; org-glance-headline.el ends here
