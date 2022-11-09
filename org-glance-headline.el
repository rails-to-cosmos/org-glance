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
(require 'type-break)

(require 'org-glance-helpers)
(require 'org-glance-scope)
(require 'org-glance-offset)

(defconst org-glance-encrypted-re "aes-encrypted V [0-9]+.[0-9]+-.+\n"
  "Encrypted header pattern.")

(defconst org-glance-user-property-1-re
  "[[:blank:]]+\\([[:word:]][[:word:],[:blank:],_]?+\\)\\:[[:blank:]]+\\(.+\\)"
  "How to parse user specified properties.")

(defconst org-glance-user-property-2-re
  "^\\([[:word:]][[:word:],[:blank:],_]?+\\)\\:[[:blank:]]+\\(.+\\)"
  "How to parse user specified properties.")

(cl-defmacro org-glance-headline:with-headline-at-point (&rest forms)
  "Execute FORMS only if point is at heading."
  (declare (indent 0))
  `(save-match-data
     (save-excursion
       (when (and (org-glance-headline:back-to-headline)
                  (org-at-heading-p))
         (save-restriction
           (narrow-to-region
            (save-excursion (org-back-to-heading t) (point))
            (save-excursion (org-end-of-subtree t t)))
           ,@forms)))))

;; Extend events suitable for headlines
(org-glance-class org-glance-event ()
    ((offset :type org-glance-type:offset
             :initarg :offset
             :initform (org-glance-offset:current))))

(org-glance-class org-glance-event:PUT (org-glance-event)
    ((headline :type org-glance-headline-header
               :initarg :headline)))

(org-glance-class org-glance-event:RM (org-glance-event)
    ((hash :type org-glance-type:hash
           :initarg :hash)))

(org-glance-class org-glance-event:UPDATE (org-glance-event)
    ((hash :type org-glance-type:hash
           :initarg :hash)
     (headline :type org-glance-headline-header
               :initarg :headline)))

(org-glance-class org-glance-link nil
    ((title :type string
            :initarg :title)
     (org-link :type string
               :initarg :org-link)
     (position :type number
               :initarg :position)))

(org-glance-class org-glance-headline-header ()
    ((hash :type org-glance-type:hash
           :initarg :hash
           :documentation "Hash of original headline contents.")
     (title :type string
            :initarg :title
            :documentation "Original headline title.")
     (state :type string
            :initarg :state
            :documentation "TODO state of headline.")
     (tags :type list
           :initarg :tags
           :documentation "List of downcased tags.")
     (commented? :type boolean
                 :initarg :commented?
                 :documentation "Is the headline commented?")
     (archived? :type boolean
                :initarg :archived?
                :documentation "Is the headline archived?")
     (closed? :type boolean
              :initarg :closed?
              :documentation "Is the headline closed?")
     (encrypted? :type boolean
                 :initarg :encrypted?
                 :documentation "Is the headline encrypted?")
     (linked? :type boolean
              :initarg :linked?
              :documentation "Does the headline contain org links?")
     (store? :type boolean
             :initarg :store?
             :documentation "Does the headline contain user properties?"))
  "Limited edition of `org-glance-headline'.")

(org-glance-class org-glance-headline (org-glance-headline-header)
    ((links :type list
            :initarg :links
            :documentation "Links.")
     (contents :type string
               :initarg :contents
               :documentation "Raw contents of headline.")
     (properties :type list
                 :initarg :properties
                 :documentation "Org-mode properties.")
     (store :type list
            :initarg :store
            :documentation "Properties specified by user in headline contents."))
  "Serializable headline with additional features on top of `org-element'.")

(cl-defun org-glance-headline-header:from-headline (headline)
  "Infer instance of `org-glance-headline-header' from HEADLINE."
  (cl-check-type headline (or org-glance-headline-header org-glance-headline))

  (cl-typecase headline
    (org-glance-headline (org-glance-headline-header
                          :hash (org-glance- headline :hash)
                          :title (org-glance- headline :title)
                          :state (org-glance- headline :state)
                          :tags (org-glance- headline :tags)
                          :commented? (org-glance- headline :commented?)
                          :archived? (org-glance- headline :archived?)
                          :closed? (org-glance- headline :closed?)
                          :encrypted? (org-glance- headline :encrypted?)
                          :linked? (org-glance- headline :linked?)
                          :store? (org-glance- headline :store?)))
    (org-glance-headline-header headline)))

(cl-defun org-glance-ast:get-buffer-ast ()
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

(cl-defun org-glance-headline:extract-store (contents)
  (cl-loop for (_ key value)
     in (append (s-match-strings-all org-glance-user-property-1-re contents)
                (s-match-strings-all org-glance-user-property-2-re contents))
     when (not (member key org-special-properties))
     collect (cons key value) into result
     finally return (seq-uniq result #'(lambda (a b) (and (string= (car a) (car b))
                                                     (string= (cdr a) (cdr b)))))))

(cl-defun org-glance-ast:contents (ast)
  (->> ast
       org-element-interpret-data
       substring-no-properties
       s-trim))

(cl-defun org-glance-ast:substitute-links-with-titles (ast)
  (cl-loop for link in (org-element-map ast 'link #'identity)
     do (org-element-set-element link (or (-some->> link
                                            org-element-contents
                                            org-element-interpret-data)
                                          (org-element-property :raw-link link)))
     finally return ast))

(cl-defun org-glance-ast:title (ast)
  (with-temp-buffer
    (insert (or (org-element-property :TITLE (car ast))
                (org-element-property :raw-value (car ast))
                ""))
    (->> (org-element-parse-buffer)
         org-glance-ast:substitute-links-with-titles
         org-element-interpret-data
         substring-no-properties
         s-trim)))

(cl-defun org-glance-ast:state (contents)
  (org-glance:with-temp-buffer
   (insert contents)
   (goto-char (point-min))
   (unless (org-at-heading-p)
     (outline-next-heading))
   (-some->> (org-get-todo-state)
     substring-no-properties
     upcase)))

(cl-defun org-glance-ast:hash (contents)
  (->> contents
       s-trim
       downcase
       (replace-regexp-in-string "[[:space:][:blank:][:cntrl:]]+" " ")
       (secure-hash 'md5)))

(cl-defun org-glance-ast:class (ast)
  (-map #'downcase (org-element-property :tags (car ast))))

(cl-defun org-glance-ast:commented? (ast)
  (not (null (org-element-property :commentedp (car ast)))))

(cl-defun org-glance-ast:closed? (ast)
  (not (null (org-element-property :closed (car ast)))))

(cl-defun org-glance-ast:archived? (ast)
  (not (null (org-element-property :archivedp (car ast)))))

(cl-defun org-glance-ast:encrypted? (contents)
  (not (null (s-match-strings-all org-glance-encrypted-re contents))))

(cl-defun org-glance-ast:linked? (contents)
  (not (null (s-match-strings-all org-link-any-re contents))))

(cl-defun org-glance-headline-at-point ()
  "Create `org-glance-headline' instance from `org-element' at point."
  (save-excursion
    (cond ((org-before-first-heading-p) nil)
          (t (org-glance-headline:with-headline-at-point
               (let* ((ast (org-glance-ast:get-buffer-ast))
                      (contents (org-glance-ast:contents ast))
                      (store (org-glance-headline:extract-store contents))
                      (links (cl-loop
                                for element in (org-element-map (org-element-parse-buffer) 'link #'identity)
                                collect (org-glance-link
                                         :title (substring-no-properties
                                                 (or (-some->> element
                                                       org-element-contents
                                                       org-element-interpret-data)
                                                     (org-element-property :raw-link element)))
                                         :org-link (s-trim (buffer-substring-no-properties
                                                            (org-element-property :begin element)
                                                            (org-element-property :end element)))
                                         :position (org-element-property :begin element)))))
                 (org-glance-headline
                  :title (org-glance-ast:title ast)
                  :state (pcase (org-glance-ast:state contents)
                           ((pred null) "")
                           (state state))
                  :hash (org-glance-ast:hash contents)
                  :tags (org-glance-ast:class ast)
                  :commented? (org-glance-ast:commented? ast)
                  :archived? (org-glance-ast:archived? ast)
                  :closed? (org-glance-ast:closed? ast)
                  :encrypted? (org-glance-ast:encrypted? contents)
                  :linked? (not (null links))
                  :links links
                  :store? (not (null store))
                  :store store
                  :contents contents
                  :properties (org-entry-properties))))))))

(cl-defun org-glance-headline-from-string (string)
  "Create `org-glance-headline' from string."
  (org-glance:with-temp-buffer
   (insert string)
   (goto-char (point-min))
   (unless (org-at-heading-p)
     (outline-next-heading))
   (org-glance-headline-at-point)))

(cl-defun org-glance-headline:save (headline dest)
  "Write HEADLINE to DEST."
  (cond ;; ((and (f-exists? dest) (not (f-empty? dest))) (user-error "Destination exists and is not empty."))
    ((and (f-exists? dest) (not (f-readable? dest))) (user-error "Destination exists and not readable.")))
  (org-glance:with-temp-file dest
    (org-glance-headline:insert headline))
  headline)

(cl-defun org-glance-headline:with-properties (headline properties)
  (declare (indent 1))
  (org-glance:with-temp-buffer
   (org-glance-headline:insert headline)
   (cl-loop for (key value) in properties
      do (org-set-property key value))
   (org-glance-headline-at-point)))

(cl-defmethod org-glance-headline:insert ((headline org-glance-headline))
  (insert (org-glance- headline :contents) "\n"))

;; (defvar org-glance-headline--bindat-spec
;;   '((title str 255)
;;     (archived? byte)
;;     (commented? byte)
;;     (closed? byte)
;;     (encrypted? byte)
;;     (linked? byte)
;;     (store? byte)))

;; (cl-defmethod org-glance-headline:pack ((headline org-glance-headline))
;;   "Pack HEADLINE according to `org-glance-headline--bindat-spec'."
;;   (cl-flet ((bool->int (bool) (if (null bool) 0 1)))
;;     (bindat-pack
;;      org-glance-headline--bindat-spec
;;      (a-list 'title (string-as-unibyte (org-glance- headline :title))
;;              'archived (bool->int (org-glance- headline :archived?))
;;              'commented (bool->int (org-glance- headline :commented?))
;;              'closed (bool->int (org-glance- headline :closed?))
;;              'encrypted (bool->int (org-glance- headline :encrypted?))
;;              'linked (bool->int (org-glance- headline :linked?))
;;              'store (bool->int (org-glance- headline :store?))))))

;; (cl-defmethod org-glance-headline:unpack (bindat-raw)
;;   (bindat-unpack org-glance-headline--bindat-spec bindat-raw))

;; (cl-defmacro comment (&rest forms)
;;   nil)

;; (comment
;;  (f-write-bytes (org-glance-headline:pack (org-glance-headline-at-point)) "/tmp/headline.bin")
;;  (a-get (bindat-unpack org-glance-headline--bindat-spec (f-read-bytes "/tmp/headline.bin")) 'file)
;;  (string-make-unibyte "hello"))

(cl-defmacro org-glance-headline:map (var &rest forms)
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
                    (org-glance-headline:with-headline-at-point ,@forms)))
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

(cl-defun org-glance-headline:back-to-headline ()
  "Ensure point is at heading.
Return t if it is or raise `user-error' otherwise."
  (or (org-at-heading-p)
      (progn
        (org-back-to-heading-or-point-min)
        (org-at-heading-p))))

(provide 'org-glance-headline)
;;; org-glance-headline.el ends here
