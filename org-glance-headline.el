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

(require 'org-glance-serializable)

(defclass org-glance-headline (org-glance-serializable)
  ((id
    :initarg :id
    :initform nil
    :type symbol
    :reader org-glance-headline:id)
   (title
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
   (file        :initarg :file        :reader org-glance-headline:file)
   (buffer      :initarg :buffer
                :initform nil
                :reader org-glance-headline:buffer))
  "Headline model.")

(cl-defmethod org-glance-headline:copy ((headline org-glance-headline) (slots list))
  (let ((args (cl-loop for slot in slots
                 append (list (intern (format ":%s" slot))
                              (slot-value headline slot)))))
    (apply 'org-glance-headline args)))

(cl-defmethod org-glance-serialize ((headline org-glance-headline))
  "Serialize HEADLINE."
  (prin1-to-string (org-glance-headline:copy headline '(title class archived commented closed encrypted linked propertized contents file))))

(cl-defmacro org-glance:with-heading-at-point (&rest forms)
  "Execute FORMS only if point is at heading."
  (declare (indent 0))
  `(save-excursion
     (org-glance:ensure-at-heading)
     (save-restriction
       (org-narrow-to-subtree)
       ,@forms)))

(cl-defmethod org-glance-headline:save ((headline org-glance-headline) file)
  "Write HEADLINE to FILE."
  (with-temp-file file
    (insert (org-glance-serialize headline))))

(defun org-glance-headline:load (file)
  "Load headline from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-glance-deserialize (buffer-string))))

(provide 'org-glance-headline)
;;; org-glance-headline.el ends here
