;;; org-glance-headline.el --- base headline model for `org-glance'.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 2 February, 2022
;; Version: 1.0.1

;; Keywords: org org-element org-headline org-glance
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

;;; Code:

(require 'a)
(require 'bindat)
(require 'dash)
(require 'cl-macs)
(require 'cl-generic)
(require 'ol)
(require 'org)
(require 'org-element)
(require 's)

(require 'org-glance-helpers)
(require 'org-glance-scope)

(cl-defstruct (org-glance-headline-header (:constructor org-glance-headline-header--create)
                                          (:copier nil))
  "Limited edition of `org-glance-headline'."
  (-hash nil :type string :read-only t :documentation "Hash of original headline contents.")
  (-title nil :type string :read-only t :documentation "Original headline title."))

(cl-defstruct (org-glance-headline (:include org-glance-headline-header)
                                   (:constructor org-glance-headline)
                                   (:copier nil))
  "Serializable headline with additional features on top of `org-element'."
  (class nil :type list :read-only t :documentation "List of downcased tags.")
  (contents nil :type string :read-only t :documentation "Raw contents of headline.")
  (org-properties nil :type list :read-only t :documentation "Org-mode properties.")
  (user-properties nil :type list :read-only t :documentation "Properties specified by user in headline contents.")
  (archived-p nil :type bool :read-only t)
  (commented-p nil :type bool :read-only t)
  (closed-p nil :type bool :read-only t)
  (encrypted-p nil :type bool :read-only t)
  (linked-p nil :type bool :read-only t)
  (propertized-p nil :type bool :read-only t))

(cl-defgeneric org-glance-headline-hash (object)
  "Get hash of OBJECT.")

(cl-defmethod org-glance-headline-hash ((headline org-glance-headline))
  (org-glance-headline--hash headline))

(cl-defmethod org-glance-headline-hash ((headline org-glance-headline-header))
  (org-glance-headline-header--hash headline))

(cl-defgeneric org-glance-headline-title (object)
  "Get title of OBJECT.")

(cl-defmethod org-glance-headline-title ((headline org-glance-headline))
  (org-glance-headline--title headline))

(cl-defmethod org-glance-headline-title ((headline org-glance-headline-header))
  (org-glance-headline-header--title headline))

(cl-defgeneric org-glance-headline-header (headline)
  "Make instance of `org-glance-headline-header' from HEADLINE.")

(cl-defmethod org-glance-headline-header ((headline org-glance-headline-header))
  "Make instance of `org-glance-headline-header' from HEADLINE."
  headline)

(cl-defmethod org-glance-headline-header ((headline org-glance-headline))
  "Make instance of `org-glance-headline-header' from HEADLINE."
  (org-glance-headline-header--create
   :-hash (org-glance-headline-hash headline)
   :-title (org-glance-headline-title headline)))

(cl-defun org-glance-headline-at-point ()
  "Create `org-glance-headline' instance from `org-element' at point."
  (org-glance--with-headline-at-point
    (let* ((subtree (let* ((subtree (org-element-contents (org-element-parse-buffer)))
                           (element (car subtree))
                           ;; get offset of the topmost element:
                           (indent-offset (1- (org-element-property :level element))))
                      ;; Consider side-effect on subtree: we change indentation levels of all nested subtrees
                      (when (> indent-offset 0)
                        (cl-loop for headline in (org-element-map subtree 'headline #'identity)
                           do (let ((level (org-element-property :level headline)))
                                (org-element-put-property headline :level (- level indent-offset)))))
                      subtree))
           (element (car subtree)) ;; topmost heading of current headline
           (contents (->> subtree
                          org-element-interpret-data
                          substring-no-properties
                          s-trim)))
      (org-glance-headline
       :-title (with-temp-buffer
                 (insert (or (org-element-property :TITLE element)
                             (org-element-property :raw-value element)
                             ""))
                 (->> (org-element-parse-buffer)
                      org-glance--links-to-titles
                      org-element-interpret-data
                      substring-no-properties
                      s-trim))
       :-hash (->> contents
                   s-trim
                   downcase
                   (replace-regexp-in-string "[[:space:][:blank:][:cntrl:]]+" " ")
                   (secure-hash 'md5))
       :class (--map (intern (downcase it)) (org-element-property :tags element))
       :contents contents
       :archived-p (org-element-property :archivedp element)
       :commented-p (org-element-property :commentedp element)
       :closed-p (org-element-property :closed element)
       :encrypted-p (not (null (s-match-strings-all "aes-encrypted V [0-9]+.[0-9]+-.+\n" contents)))
       :linked-p (not (null (s-match-strings-all org-link-any-re contents)))
       :propertized-p (not (null (s-match-strings-all "\\([[:word:],[:blank:],_]+\\)\\:[[:blank:]]*\\(.*\\)" contents)))
       :org-properties (org-entry-properties)
       :user-properties nil))))

(cl-defun org-glance-headline-from-string (string)
  "Create `org-glance-headline' from string."
  (org-glance--with-temp-buffer
   (insert string)
   (goto-char (point-min))
   (outline-next-heading)
   (org-glance-headline-at-point)))

(cl-defun org-glance-headline-from-region (beg end)
  "Create `org-glance-headline' from region."
  (org-glance-headline-from-string (buffer-substring-no-properties beg end)))

(cl-defun org-glance-headline-serialize (headline)
  "Serialize HEADLINE."
  (prin1-to-string headline))

(cl-defgeneric org-glance-headline-equal-p (a b)
  "Return t if A equals B.")

(cl-defmethod org-glance-headline-equal-p ((a org-glance-headline) (b org-glance-headline))
  "Return t if A equals B."
  (string= (org-glance-headline-hash a) (org-glance-headline-hash b)))

(cl-defmethod org-glance-headline-equal-p ((a org-glance-headline-header) (b org-glance-headline-header))
  "Return t if A equals B."
  (string= (org-glance-headline-hash a) (org-glance-headline-hash b)))

(cl-defun org-glance-headline-save (headline dest)
  "Write HEADLINE to DEST."
  (cond ;; ((and (f-exists? dest) (not (f-empty? dest))) (user-error "Destination exists and is not empty."))
    ((and (f-exists? dest) (not (f-readable? dest))) (user-error "Destination exists and not readable.")))
  (org-glance--with-temp-file dest
    (org-glance-headline-insert headline))
  headline)

(cl-defmethod org-glance-headline-insert ((headline org-glance-headline))
  (insert (org-glance-headline-contents headline) "\n"))

(cl-defmethod org-glance-headline:get-org-property ((headline org-glance-headline) key &optional default)
  "Retrieve KEY from HEADLINE org properties."
  (alist-get (upcase key) (org-glance-headline-org-properties headline) default nil #'string=))

(cl-defmethod org-glance-headline-set-org-properties ((headline org-glance-headline) key (value string))
  "Return new HEADLINE with property KEY set to VALUE."
  (org-glance--with-temp-buffer
   (insert (org-glance-headline-contents headline))
   (goto-char (point-min))
   (org-set-property key value)
   (org-glance-headline-at-point)))

(cl-defmethod org-glance-headline-remove-org-properties ((headline org-glance-headline) &rest keys)
  "Set HEADLINE property KEY to VALUE."
  (org-glance--with-temp-buffer
   (insert (org-glance-headline-contents headline))
   (goto-char (point-min))
   (let ((inhibit-message t))
     (cl-loop for key in keys
          do (org-delete-property key)))
   (org-glance-headline-at-point)))

;; (defvar org-glance-headline--bindat-spec
;;   '((title str 255)
;;     (archived-p byte)
;;     (commented-p byte)
;;     (closed-p byte)
;;     (encrypted-p byte)
;;     (linked-p byte)
;;     (propertized-p byte)))

;; (cl-defmethod org-glance-headline:pack ((headline org-glance-headline))
;;   "Pack HEADLINE according to `org-glance-headline--bindat-spec'."
;;   (cl-flet ((bool->int (bool) (if (null bool) 0 1)))
;;     (bindat-pack
;;      org-glance-headline--bindat-spec
;;      (a-list 'title (string-as-unibyte (org-glance-headline-title headline))
;;              'archived (bool->int (org-glance-headline-archived-p headline))
;;              'commented (bool->int (org-glance-headline-commented-p headline))
;;              'closed (bool->int (org-glance-headline-closed-p headline))
;;              'encrypted (bool->int (org-glance-headline-encrypted-p headline))
;;              'linked (bool->int (org-glance-headline-linked-p headline))
;;              'propertized (bool->int (org-glance-headline-propertized-p headline))))))

;; (cl-defmethod org-glance-headline:unpack (bindat-raw)
;;   (bindat-unpack org-glance-headline--bindat-spec bindat-raw))

;; (cl-defmacro comment (&rest forms)
;;   nil)

;; (comment
;;  (f-write-bytes (org-glance-headline:pack (org-glance-headline-at-point)) "/tmp/headline.bin")
;;  (a-get (bindat-unpack org-glance-headline--bindat-spec (f-read-bytes "/tmp/headline.bin")) 'file)
;;  (string-make-unibyte "hello"))

(cl-defun org-glance-buffer-headlines ()
  "Return list of buffer headline positions (:begin of org-element).

This function defines the meaning of `org-glance-headline': non-nil org-element of level 1."
  (-non-nil
   (org-element-map (org-element-parse-buffer 'headline) 'headline
     (lambda (headline)
       (when (= (org-element-property :level headline) 1)
         (org-element-property :begin headline))))))

(cl-defmacro org-glance-map (var &rest forms)
  "Map buffer headlines and execute FORMS on each.
This is the anaphoric method, you can use `<headline>' to call headline in forms."
  (declare (indent 1))
  `(cl-loop for begin in (org-glance-buffer-headlines)
      for result = (save-excursion
                     (goto-char begin)
                     (let ((,(car var) (org-glance-headline-at-point)))
                       (org-glance--with-headline-at-point
                         ,@forms)))
      when (not (null result))
      collect result))

(cl-defmacro org-glance-map-filter (var filter &rest forms)
  "Map buffer headlines and execute FORMS on each.
This is the anaphoric method, you can use `<headline>' to call headline in forms."
  (declare (indent 1))
  `(cl-loop for begin in (org-glance-buffer-headlines)
      for result = (save-excursion
                     (goto-char begin)
                     (when (funcall ,filter)
                       (let ((,(car var) (org-glance-headline-at-point)))
                         (org-glance--with-headline-at-point
                           ,@forms))))
      when (not (null result))
      collect result))

(cl-defmacro org-glance-map-file (file &rest forms)
  (declare (indent 1))
  `(org-glance--with-temp-buffer
    (insert-file-contents ,file)
    (org-glance-map (<headline>) ,@forms)))

(cl-defun org-glance-file-headlines (file)
  "List FILE headlines."
  (org-glance-map-file file <headline>))

(cl-defmacro org-glance-file-contents (file)
  "Return list of FILE contents.

CAR of this list is a string before the first heading, CDR is a
list of `org-glance-headline'."
  (declare (indent 1))
  `(org-glance--with-temp-buffer
    (insert-file-contents ,file)
    (append
     (list (buffer-substring-no-properties (point-min) (save-excursion
                                                         (goto-char (point-min))
                                                         (unless (org-at-heading-p)
                                                           (outline-next-heading))
                                                         (point))))
     (org-glance-map (headline) headline))))

(cl-defun org-glance-headline-load (file)
  "Load headline from FILE."
  (car (org-glance-map-file file <headline>)))

(provide 'org-glance-headline)
;;; org-glance-headline.el ends here
