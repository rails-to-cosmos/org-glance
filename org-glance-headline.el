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

(cl-defstruct (org-glance-headline (:constructor org-glance-headline)
                                   (:copier org-glance-headline-copy))
  "Serializable headline with additional features on top of `org-element'."
  title
  class
  (contents nil :type string :read-only t :documentation "Raw contents of headline.")
  org-properties
  user-properties
  archived-p
  commented-p
  closed-p
  encrypted-p
  linked-p
  propertized-p
  hash)

(cl-defun org-glance--links-to-titles (ast)
  "Replace links with its titles in AST."
  (cl-loop for link in (org-element-map ast 'link #'identity)
     do (org-element-set-element link (or (-some->> link
                                            org-element-contents
                                            org-element-interpret-data)
                                          (org-element-property :raw-link link)))
     finally return ast))

(cl-defun org-glance--ensure-at-heading ()
  "Ensure point is at heading.
Return t if it is or raise `user-error' otherwise."
  (or (org-at-heading-p)
      (progn
        (org-back-to-heading-or-point-min)
        (org-at-heading-p))))

(cl-defmacro org-glance--with-heading-at-point (&rest forms)
  "Execute FORMS only if point is at heading."
  (declare (indent 0))
  `(save-excursion
     (when (org-glance--ensure-at-heading)
       (save-restriction
         (org-narrow-to-subtree)
         ,@forms))))

(cl-defun org-glance-headline-at-point ()
  "Create `org-glance-headline' instance from `org-element' at point."
  (org-glance--with-heading-at-point
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
          (element (car subtree))  ;; topmost heading of current headline
          (contents (->> subtree
                         org-element-interpret-data
                         substring-no-properties
                         s-trim)))
     (org-glance-headline :title (with-temp-buffer
                                   (insert (or (org-element-property :TITLE element)
                                               (org-element-property :raw-value element)
                                               ""))
                                   (->> (org-element-parse-buffer)
                                        org-glance--links-to-titles
                                        org-element-interpret-data
                                        substring-no-properties
                                        s-trim))
                          :class (--map (intern (downcase it)) (org-element-property :tags element))
                          :contents contents
                          :archived-p (org-element-property :archivedp element)
                          :commented-p (org-element-property :commentedp element)
                          :closed-p (org-element-property :closed element)
                          :encrypted-p (not (null (s-match-strings-all "aes-encrypted V [0-9]+.[0-9]+-.+\n" contents)))
                          :linked-p (not (null (s-match-strings-all org-link-any-re contents)))
                          :propertized-p (not (null (s-match-strings-all "\\([[:word:],[:blank:],_]+\\)\\:[[:blank:]]*\\(.*\\)" contents)))
                          :org-properties (org-entry-properties)
                          :user-properties nil
                          :hash (->> contents
                                     s-trim
                                     downcase
                                     (replace-regexp-in-string "[[:space:][:blank:][:cntrl:]]+" " ")
                                     (secure-hash 'md5))))))

(cl-defmethod org-glance-serialize ((headline org-glance-headline))
  "Serialize HEADLINE."
  (prin1-to-string headline))

(cl-defmethod org-glance-equal-p ((a org-glance-headline) (b org-glance-headline))
  "Return t if A equals B."
  (string= (org-glance-headline-contents a) (org-glance-headline-contents b)))

(cl-defmethod org-glance-save ((headline org-glance-headline) (file string))
  "Write HEADLINE to FILE."
  (with-temp-file file
    (let ((standard-output (current-buffer))
          (print-circle t))
      (prin1 headline))))

(cl-defmethod org-glance-hash ((headline org-glance-headline))
  "Get hash of HEADLINE."
  (org-glance-headline-hash headline))

(cl-defun org-glance-headline-load (file)
  "Load headline from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(cl-defmethod org-glance-headline-insert ((headline org-glance-headline))
  (insert (org-glance-headline-contents headline) "\n"))

(cl-defmethod org-glance-headline:get-org-property ((headline org-glance-headline) key &optional default)
  "Retrieve KEY from HEADLINE org properties."
  (alist-get (upcase key) (org-glance-headline-org-properties headline) default nil #'string=))

(cl-defmethod org-glance-headline-set-org-properties ((headline org-glance-headline) key (value string))
  "Return new HEADLINE with property KEY set to VALUE."
  (with-temp-buffer
    (org-mode)
    (save-excursion
      (insert (org-glance-headline-contents headline)))
    (org-set-property key value)
    (org-glance-headline-at-point)))

(cl-defmethod org-glance-headline-remove-org-properties ((headline org-glance-headline) &rest keys)
  "Set HEADLINE property KEY to VALUE."
  (let ((inhibit-message t))
    (with-temp-buffer
      (org-mode)
      (save-excursion
        (insert (org-glance-headline-contents headline)))
      (cl-loop for key in keys
         do (org-delete-property key))
      (org-glance-headline-at-point))))

(cl-defmethod org-glance-headline:pop-org-property ((headline org-glance-headline) key &optional default)
  "Retrieve KEY from HEADLINE properties, return new headline with KEY removed.

TODO: optimize it when key not found."
  (prog1
      (alist-get (upcase key) (org-glance-headline-org-properties headline) default nil #'string=)
    (org-glance-headline-remove-org-properties headline key)))

(cl-defmethod org-glance-export ((headline org-glance-headline) dest)
  (cond ((and (f-exists? dest) (not (f-empty? dest))) (user-error "Destination exists and not empty."))
        ((and (f-exists? dest) (not (f-readable? dest))) (user-error "Destination exists and not readable.")))
  (with-temp-file dest
    (org-glance-headline-insert headline)))

(cl-defmethod org-glance-headlines ((headline org-glance-headline))
  "Return list with one HEADLINE."
  (list headline))

;; (cl-defmethod org-glance-headlines ((headline org-glance-headline))
;;   "Return list of HEADLINE."
;;   (list headline))

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

(cl-defmacro org-glance-loop (&rest forms)
  "Loop over buffer headlines and execute FORMS on each.
This is the anaphoric method, you can use `<headline>' to call headline in forms."
  `(cl-loop for begin in (org-glance-buffer-headlines)
      collect (save-excursion
                (goto-char begin)
                (let ((<headline> (org-glance-headline-at-point)))
                  (org-glance--with-heading-at-point
                   ,@forms)))))

;; (cl-defmacro org-glance-map (&rest forms)
;;   "Loop over buffer headlines and execute FORMS on each.

;; FORMS should return `org-glance-headline' that will be persisted in current buffer after changes.

;; This is the anaphoric method, you can use `<headline>' to call headline in forms."
;;   `(cl-loop for begin in (org-glance-buffer-headlines)
;;       collect (save-excursion
;;                 (goto-char begin)
;;                 (let ((<headline> (org-glance-headline-at-point)))
;;                   (org-glance--with-heading-at-point
;;                    (when-let (new-headline (progn ,@forms))
;;                      (delete-region (point-min) (point-max))
;;                      (org-glance-headline-insert new-headline)))))))

(cl-defmacro org-glance-with-file (file &rest forms)
  (declare (indent 1))
  `(with-temp-file ,file
     (org-mode)
     ,@forms))

(cl-defmacro org-glance-loop-file (file &rest forms)
  (declare (indent 1))
  `(org-glance-with-file ,file
     (insert-file-contents ,file)
     (org-glance-loop ,@forms)))

(cl-defmacro org-glance-loop-file-ro (file &rest forms)
  (declare (indent 1))
  `(with-temp-buffer
     (insert-file-contents ,file)
     (org-glance-loop ,@forms)))

(cl-defun org-glance-file-headlines (file)
  "List FILE headlines."
  (org-glance-loop-file-ro file <headline>))

(cl-defmacro org-glance-file-contents (file)
  "Return list of FILE contents.

CAR of this list is a string before the first heading, CDR is a
list of `org-glance-headline'."
  (declare (indent 1))
  `(with-temp-buffer
     (org-mode)
     (insert-file-contents ,file)
     (append
      (list (buffer-substring-no-properties (point-min) (save-excursion
                                                          (goto-char (point-min))
                                                          (unless (org-at-heading-p)
                                                            (outline-next-heading))
                                                          (point))))
      (org-glance-loop <headline>))))

(provide 'org-glance-headline)
;;; org-glance-headline.el ends here
