;;; org-glance-headline.el --- headline model for `org-glance'.  -*- lexical-binding: t; -*-

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

(require 'a)
(require 'bindat)
(require 'dash)
(require 'eieio)
(require 'ol)
(require 'org)
(require 'org-element)
(require 's)

(defclass org-glance-headline nil
  ((title :initarg :title :reader org-glance-headline:title :type string)
   (class :initarg :class :reader org-glance-headline:class :type (satisfies (lambda (x) (or (symbolp x) (-all? #'symbolp x)))))
   (contents :initarg :contents :reader org-glance-headline:contents)
   (archived :initarg :archived :reader org-glance-headline:archived-p)
   (commented :initarg :commented :reader org-glance-headline:commented-p)
   (closed :initarg :closed :reader org-glance-headline:closed-p)
   (encrypted :initarg :encrypted :reader org-glance-headline:encrypted-p)
   (linked :initarg :linked :reader org-glance-headline:linked-p)
   (propertized :initarg :propertized :reader org-glance-headline:propertized-p)
   (properties :initarg :properties :reader org-glance-headline:properties)
   (file :initarg :file :reader org-glance-headline:file)
   (buffer :initarg :buffer :reader org-glance-headline:buffer :initform nil))
  "Headline model.")

(defvar org-glance-headline--bindat-spec
  '((title str 255)
    (file str 255)
    (archived byte)
    (commented byte)
    (closed byte)
    (encrypted byte)
    (linked byte)
    (propertized byte)))

(defun org-glance:links-to-titles (ast)
  "Replace links with its titles in AST."
  (cl-loop for link in (org-element-map ast 'link #'identity)
     do (org-element-set-element link (or (-some->> link
                                            org-element-contents
                                            org-element-interpret-data)
                                          (org-element-property :raw-link link)))
     finally return ast))

(defun org-glance:ensure-at-heading ()
  "Ensure point is at heading.
Return t if it is or raise `user-error' otherwise."
  (or (org-at-heading-p)
      (progn
        (org-back-to-heading-or-point-min)
        (org-at-heading-p))))

(cl-defmacro org-glance:with-heading-at-point (&rest forms)
  "Execute FORMS only if point is at heading."
  (declare (indent 0))
  `(save-excursion
     (when (org-glance:ensure-at-heading)
       (save-restriction
         (org-narrow-to-subtree)
         ,@forms))))

(defun org-glance-headline-at-point ()
  "Create headline from `org-element' at point.
`org-glance-headline' is an `org-element' of type `org-data'
with additional properties and `org-element' of type `headline' in contents."
  (org-glance:with-heading-at-point
    (let* ((subtree (let* ((subtree (org-element-contents (org-element-parse-buffer)))
                           (element (car subtree))
                           ;; get offset of the topmost element:
                           (indent-offset (1- (org-element-property :level element))))
                      ;; side-effect on SUBTREE: we change indentation levels of all nested subtrees
                      (when (> indent-offset 0)
                        (cl-loop for headline in (org-element-map subtree 'headline #'identity)
                           do (let ((level (org-element-property :level headline)))
                                (org-element-put-property headline :level (- level indent-offset)))))
                      subtree))
           (element (car subtree))  ;; topmost heading of current headline
           (contents (->> subtree
                          org-element-interpret-data
                          substring-no-properties
                          s-trim)))
      (org-glance-headline
       :title (with-temp-buffer
                (insert (or (org-element-property :TITLE element)
                            (org-element-property :raw-value element)
                            ""))
                (->> (org-element-parse-buffer)
                     org-glance:links-to-titles
                     org-element-interpret-data
                     substring-no-properties
                     s-trim))
       :class (--map (intern (downcase it)) (org-element-property :tags element))
       :archived (org-element-property :archivedp element)
       :commented (org-element-property :commentedp element)
       :closed (org-element-property :closed element)
       :encrypted (not (null (s-match-strings-all "aes-encrypted V [0-9]+.[0-9]+-.+\n" contents)))
       :linked (not (null (s-match-strings-all org-link-any-re contents)))
       :propertized (not (null (s-match-strings-all "\\([[:word:],[:blank:],_]+\\)\\:[[:blank:]]*\\(.*\\)" contents)))
       :properties (org-entry-properties)
       :contents contents
       :file (buffer-file-name)
       :buffer (current-buffer)))))

(cl-defmethod org-glance-headline:directory ((headline org-glance-headline))
  (let ((file (org-glance-headline:file headline))
        (buffer (org-glance-headline:buffer headline)))
    (cond ((file-exists-p file) (file-name-directory file))
          ((buffer-live-p buffer) (with-current-buffer buffer
                                    default-directory)))))

(cl-defmethod org-glance-headline-copy ((headline org-glance-headline) (slots list))
  (cl-loop for slot in slots
     append (list (intern (format ":%s" slot)) (slot-value headline slot))
     into result
     finally return (apply 'org-glance-headline result)))

;; override
(cl-defmethod org-glance-headline-serialize ((headline org-glance-headline))
  "Serialize HEADLINE."
  (prin1-to-string (org-glance-headline-copy headline '(title class archived commented closed encrypted linked propertized contents file))))

(cl-defmethod org-glance-headline-save ((headline org-glance-headline) file)
  "Write HEADLINE to FILE."
  (with-temp-file file
    (insert (org-glance-headline-serialize headline))))

(defun org-glance-headline-load (file)
  "Load headline from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (buffer-string))))

(cl-defmethod org-glance-headline:hash ((headline org-glance-headline))
  "Serialize HEADLINE."
  ;; (message "Create hash from string: \"%s\"" (org-glance-headline:contents headline))
  ;; (message "Result hash: %s" (secure-hash 'md5 (org-glance-headline:contents headline)))
  (secure-hash 'md5 (org-glance-headline:contents headline)))

(cl-defmethod org-glance-headline-property-get ((headline org-glance-headline) key &optional default)
  "Retrieve KEY from HEADLINE properties."
  (alist-get (upcase key) (org-glance-headline:properties headline) default nil #'string=))

(cl-defmethod org-glance-headline-property-set ((headline org-glance-headline) key (value string))
  "Set HEADLINE property KEY to VALUE."
  (let ((properties (org-glance-headline:properties headline)))
    (setf (alist-get key properties nil t #'string=) value
          (slot-value headline 'properties) properties
          (slot-value headline 'contents) (with-temp-buffer
                                            (org-mode)
                                            (save-excursion
                                              (insert (org-glance-headline:contents headline))
                                              (org-set-property key value)
                                              (buffer-substring-no-properties (point-min) (point-max)))))
    value))

(cl-defmethod org-glance-headline-property-remove ((headline org-glance-headline) key)
  "Set HEADLINE property KEY to VALUE."
  (let ((properties (org-glance-headline:properties headline)))
    (setf (slot-value headline 'properties) (a-dissoc properties key)
          (slot-value headline 'contents) (with-temp-buffer
                                            (org-mode)
                                            (save-excursion
                                              (insert (org-glance-headline:contents headline)))
                                            (org-delete-property key)
                                            (buffer-substring-no-properties (point-min) (point-max))))))

(cl-defmethod org-glance-headline-equal-p ((a org-glance-headline) (b org-glance-headline))
  (string= (org-glance-headline:contents a) (org-glance-headline:contents b)))

;; (cl-defmethod org-glance-headline:pack ((headline org-glance-headline))
;;   "Pack HEADLINE according to `org-glance-headline--bindat-spec'."
;;   (cl-flet ((bool->int (bool) (if (null bool) 0 1)))
;;     (bindat-pack
;;      org-glance-headline--bindat-spec
;;      (a-list 'title (string-as-unibyte (org-glance-headline:title headline))
;;              'file (string-as-unibyte (org-glance-headline:file headline))
;;              'archived (bool->int (org-glance-headline:archived-p headline))
;;              'commented (bool->int (org-glance-headline:commented-p headline))
;;              'closed (bool->int (org-glance-headline:closed-p headline))
;;              'encrypted (bool->int (org-glance-headline:encrypted-p headline))
;;              'linked (bool->int (org-glance-headline:linked-p headline))
;;              'propertized (bool->int (org-glance-headline:propertized-p headline))))))

;; (cl-defmethod org-glance-headline:unpack (bindat-raw)
;;   (bindat-unpack org-glance-headline--bindat-spec bindat-raw))

;; (cl-defmacro comment (&rest forms)
;;   nil)

;; (comment
;;  (f-write-bytes (org-glance-headline:pack (org-glance-headline-at-point)) "/tmp/headline.bin")
;;  (a-get (bindat-unpack org-glance-headline--bindat-spec (f-read-bytes "/tmp/headline.bin")) 'file)
;;  (string-make-unibyte "hello"))

(provide 'org-glance-headline)
;;; org-glance-headline.el ends here
