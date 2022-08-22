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
(require 'thunk)

(require 'org-glance-helpers)
(require 'org-glance-scope)

(cl-defstruct (org-glance-headline-header
                (:constructor org-glance-headline-header--create)
                (:copier nil))
  "Limited edition of `org-glance-headline'."
  (-hash nil :type string :read-only t :documentation "Hash of original headline contents.")
  (-title nil :type string :read-only t :documentation "Original headline title.")
  (-state nil :type string :read-only t :documentation "TODO state of headline.")
  (-class nil :type list :read-only t :documentation "List of downcased tags.")
  (-commented-p nil :type boolean :read-only t :documentation "Is the headline commented?")
  (-archived-p nil :type boolean :read-only t :documentation "Is the headline archived?")
  (-closed-p nil :type boolean :read-only t :documentation "Is the headline closed?")
  (-encrypted-p nil :type boolean :read-only t :documentation "Is the headline encrypted?")
  (-linked-p nil :type boolean :read-only t :documentation "Does the headline contain org links?")
  (-propertized-p nil :type bool :read-only t :documentation "Does the headline contain user properties?"))

(cl-defstruct (org-glance-headline
                (:include org-glance-headline-header)
                (:constructor org-glance-headline--create)
                (:copier nil))
  "Serializable headline with additional features on top of `org-element'."
  (contents nil :type string :read-only t :documentation "Raw contents of headline.")
  (org-properties nil :type list :read-only t :documentation "Org-mode properties.")
  (user-properties nil :type list :read-only t :documentation "Properties specified by user in headline contents."))

(defconst org-glance-encrypted-re "aes-encrypted V [0-9]+.[0-9]+-.+\n"
  "Encrypted header pattern.")

(defconst org-glance-user-property-1-re
  "[[:blank:]]+\\([[:word:]][[:word:],[:blank:],_]?+\\)\\:[[:blank:]]+\\(.+\\)"
  "How to parse user specified properties.")

(defconst org-glance-user-property-2-re
  "^\\([[:word:]][[:word:],[:blank:],_]?+\\)\\:[[:blank:]]+\\(.+\\)"
  "How to parse user specified properties.")

(cl-defgeneric org-glance-headline:hash (object)
  "Get hash of OBJECT.")

(cl-defmethod org-glance-headline:hash ((headline org-glance-headline))
  (org-glance-headline--hash headline))

(cl-defmethod org-glance-headline:hash ((headline org-glance-headline-header))
  (org-glance-headline-header--hash headline))

(cl-defgeneric org-glance-headline-title (object)
  "Get title of OBJECT.")

(cl-defmethod org-glance-headline-title ((headline org-glance-headline))
  (org-glance-headline--title headline))

(cl-defmethod org-glance-headline-title ((headline org-glance-headline-header))
  (org-glance-headline-header--title headline))

(cl-defgeneric org-glance-headline-class (object)
  "Get class of OBJECT.")

(cl-defmethod org-glance-headline-class ((headline org-glance-headline))
  (org-glance-headline--class headline))

(cl-defmethod org-glance-headline-class ((headline org-glance-headline-header))
  (org-glance-headline-header--class headline))

(cl-defgeneric org-glance-headline-commented-p (headline)
  "Return t if HEADLINE is commented and nil otherwise.")

(cl-defmethod org-glance-headline-commented-p ((headline org-glance-headline))
  (org-glance-headline--commented-p headline))

(cl-defmethod org-glance-headline-commented-p ((headline org-glance-headline-header))
  (org-glance-headline-header--commented-p headline))

(cl-defgeneric org-glance-headline-closed-p (headline)
  "Return t if HEADLINE is closed and nil otherwise.")

(cl-defmethod org-glance-headline-closed-p ((headline org-glance-headline))
  (org-glance-headline--closed-p headline))

(cl-defmethod org-glance-headline-closed-p ((headline org-glance-headline-header))
  (org-glance-headline-header--closed-p headline))

(cl-defgeneric org-glance-headline-archived-p (headline)
  "Return t if HEADLINE is archived and nil otherwise.")

(cl-defmethod org-glance-headline-archived-p ((headline org-glance-headline))
  (org-glance-headline--archived-p headline))

(cl-defmethod org-glance-headline-archived-p ((headline org-glance-headline-header))
  (org-glance-headline-header--archived-p headline))

(cl-defgeneric org-glance-headline-encrypted-p (headline)
  "Return t if HEADLINE is encrypted and nil otherwise.")

(cl-defmethod org-glance-headline-encrypted-p ((headline org-glance-headline))
  (org-glance-headline--encrypted-p headline))

(cl-defmethod org-glance-headline-encrypted-p ((headline org-glance-headline-header))
  (org-glance-headline-header--encrypted-p headline))

(cl-defgeneric org-glance-headline-linked-p (headline)
  "Return t if HEADLINE is linked and nil otherwise.")

(cl-defmethod org-glance-headline-linked-p ((headline org-glance-headline))
  (org-glance-headline--linked-p headline))

(cl-defmethod org-glance-headline-linked-p ((headline org-glance-headline-header))
  (org-glance-headline-header--linked-p headline))

(cl-defgeneric org-glance-headline-propertized-p (headline)
  "Return t if HEADLINE is propertized and nil otherwise.")

(cl-defmethod org-glance-headline-propertized-p ((headline org-glance-headline))
  (org-glance-headline--propertized-p headline))

(cl-defmethod org-glance-headline-propertized-p ((headline org-glance-headline-header))
  (org-glance-headline-header--propertized-p headline))

(cl-defgeneric org-glance-headline-header (headline)
  "Make instance of `org-glance-headline-header' from HEADLINE.")

(cl-defmethod org-glance-headline-header ((headline org-glance-headline))
  "Make instance of `org-glance-headline-header' from HEADLINE."
  (org-glance-headline-header--create
   :-hash (org-glance-headline:hash headline)
   :-title (org-glance-headline-title headline)
   :-state (org-glance-headline-state headline)
   :-class (org-glance-headline-class headline)
   :-commented-p (org-glance-headline-commented-p headline)
   :-archived-p (org-glance-headline-archived-p headline)
   :-closed-p (org-glance-headline-closed-p headline)
   :-encrypted-p (org-glance-headline-encrypted-p headline)
   :-linked-p (org-glance-headline-linked-p headline)
   :-propertized-p (org-glance-headline-propertized-p headline)))

(cl-defmethod org-glance-headline-header ((headline org-glance-headline-header))
  "Make instance of `org-glance-headline-header' from HEADLINE."
  headline)

(cl-defun org-glance-headline--ast-normalized ()
  (thunk-let* ((subtree (org-element-contents (org-element-parse-buffer)))
               (element (car subtree))
               ;; get offset of the topmost element:
               (indent-offset (1- (org-element-property :level element))))
    ;; Consider side-effect on subtree: we change indentation levels of all nested subtrees
    (when (> indent-offset 0)
      (cl-loop for headline in (org-element-map subtree 'headline #'identity)
         for level = (org-element-property :level headline)
         do (org-element-put-property headline :level (- level indent-offset))))
    subtree))

(cl-defun org-glance-headline--user-properties (contents)
  (cl-loop for (_ key value)
     in (append (s-match-strings-all org-glance-user-property-1-re contents)
                (s-match-strings-all org-glance-user-property-2-re contents))
     when (not (member key org-special-properties))
     collect (cons key value) into result
     finally return (seq-uniq result #'(lambda (a b) (and (string= (car a) (car b))
                                                     (string= (cdr a) (cdr b)))))))

(cl-defun org-glance-headline--ast-contents (ast)
  (->> ast
       org-element-interpret-data
       substring-no-properties
       s-trim))

(cl-defun org-glance-headline--ast-title (ast)
  (with-temp-buffer
    (insert (or (org-element-property :TITLE (car ast))
                (org-element-property :raw-value (car ast))
                ""))
    (->> (org-element-parse-buffer)
         org-glance--links-to-titles
         org-element-interpret-data
         substring-no-properties
         s-trim)))

(cl-defun org-glance-headline--ast-state (contents)
  (org-glance:with-temp-buffer
   (insert contents)
   (goto-char (point-min))
   (unless (org-at-heading-p)
     (outline-next-heading))
   (-some->> (org-get-todo-state)
     substring-no-properties
     upcase)))

(cl-defun org-glance-headline--ast-hash (contents)
  (->> contents
       s-trim
       downcase
       (replace-regexp-in-string "[[:space:][:blank:][:cntrl:]]+" " ")
       (secure-hash 'md5)))

(cl-defun org-glance-headline--ast-class (ast)
  (-map #'downcase (org-element-property :tags (car ast))))

(cl-defun org-glance-headline--ast-commented-p (ast)
  (not (null (org-element-property :commentedp (car ast)))))

(cl-defun org-glance-headline--ast-closed-p (ast)
  (not (null (org-element-property :closed (car ast)))))

(cl-defun org-glance-headline--ast-archived-p (ast)
  (not (null (org-element-property :archivedp (car ast)))))

(cl-defun org-glance-headline--ast-encrypted-p (contents)
  (not (null (s-match-strings-all org-glance-encrypted-re contents))))

(cl-defun org-glance-headline--ast-linked-p (contents)
  (not (null (s-match-strings-all org-link-any-re contents))))

(cl-defun org-glance-headline-at-point ()
  "Create `org-glance-headline' instance from `org-element' at point."
  (org-glance--with-headline-at-point
    (let* ((ast (org-glance-headline--ast-normalized))
           (contents (org-glance-headline--ast-contents ast))
           (user-properties (org-glance-headline--user-properties contents)))
      (org-glance-headline--create
       :-title (org-glance-headline--ast-title ast)
       :-state (org-glance-headline--ast-state contents)
       :-hash (org-glance-headline--ast-hash contents)
       :-class (org-glance-headline--ast-class ast)
       :-commented-p (org-glance-headline--ast-commented-p ast)
       :-archived-p (org-glance-headline--ast-archived-p ast)
       :-closed-p (org-glance-headline--ast-closed-p ast)
       :-encrypted-p (org-glance-headline--ast-encrypted-p contents)
       :-linked-p (org-glance-headline--ast-linked-p contents)
       :-propertized-p (not (null user-properties))
       :contents contents
       :org-properties (org-entry-properties)
       :user-properties user-properties))))

(cl-defun org-glance-headline-from-string (string)
  "Create `org-glance-headline' from string."
  (org-glance:with-temp-buffer
   (insert string)
   (goto-char (point-min))
   (unless (org-at-heading-p)
     (outline-next-heading))
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
  (string= (org-glance-headline:hash a) (org-glance-headline:hash b)))

(cl-defmethod org-glance-headline-equal-p ((a org-glance-headline-header) (b org-glance-headline-header))
  "Return t if A equals B."
  (string= (org-glance-headline:hash a) (org-glance-headline:hash b)))

(cl-defgeneric org-glance-headline-state (headline)
  "Infer HEADLINE todo state from its title.")

(cl-defmethod org-glance-headline-state ((headline org-glance-headline-header))
  (org-glance-headline-header--state headline))

(cl-defmethod org-glance-headline-state ((headline org-glance-headline))
  (org-glance-headline--state headline))

(cl-defun org-glance-headline-save (headline dest)
  "Write HEADLINE to DEST."
  (cond ;; ((and (f-exists? dest) (not (f-empty? dest))) (user-error "Destination exists and is not empty."))
    ((and (f-exists? dest) (not (f-readable? dest))) (user-error "Destination exists and not readable.")))
  (org-glance--with-temp-file dest
    (org-glance-headline-insert headline))
  headline)

(cl-defmethod org-glance-headline-insert ((headline org-glance-headline))
  (insert (org-glance-headline-contents headline) "\n"))

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

(cl-defmacro org-glance-headline:map-buffer (var &rest forms)
  "Map buffer headlines and execute FORMS on each binding headline to VAR."
  (declare (indent 1))
  `(save-excursion
     (cl-loop
        initially do (progn
                       (goto-char (point-min))
                       (unless (org-at-heading-p)
                         (outline-next-heading)))
        while (org-at-heading-p)
        collect (save-excursion
                  (let ((,(car var) (org-glance-headline-at-point)))
                    (org-glance--with-headline-at-point ,@forms)))
        into result
        when (condition-case nil
                 (outline-forward-same-level 1)
               (error t))
        return result)))

(cl-defun org-glance-headline:read (file)
  "Load headline from FILE."
  (org-glance:with-temp-buffer
   (insert-file-contents file)
   (goto-char (point-min))
   (unless (org-at-heading-p)
     (outline-next-heading))
   (org-glance-headline-at-point)))

(provide 'org-glance-headline)
;;; org-glance-headline.el ends here
