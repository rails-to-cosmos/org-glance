;;; org-glance.el --- org-mode traversing. Fast and convenient.

;; Copyright (C) 2018-2019 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 29 September, 2018
;; Version: 0.1

;; Keywords: org-mode tools
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
;; This package allows you to manage bookmarks and travel around the
;; digital world with an org-mode power behind your shoulders.

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'dash-functional)

(defgroup org-glance nil
  "\nOptions concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(defcustom org-glance-cache-file
  (concat user-emacs-directory "org-glance--org-glance-cache.org")
  "A file to store headlines visited."
  :group 'org-glance
  :type 'string)

(condition-case nil
    (make-directory (file-name-directory org-glance-cache-file))
  (error nil))

(defvar org-glance-defaults--separator " → ")

(defun buffer-mode (&optional buffer-or-name)
  "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
  (buffer-local-value 'major-mode
   (if buffer-or-name (get-buffer buffer-or-name) (current-buffer))))

(defun org-glance-headline-contains-tags-p (&rest tags)
  (equal (seq-intersection tags (org-get-tags)) tags))

(defun org-glance (&rest args)
  "Use optional ARGS to customize your glancing blows:

- :scope
  Org-file or SCOPE from org-map-entries (org.el)

- :filter
  List or one filter of type lambda/symbol/string to specify entries in completing read.

  Possible default filters:
  - links - keep entries with link in title
  - encrypted - keep entries with :crypt: tag

  You can customize default filters by setting org-glance--default-filters variable.

- :action
  - if specified, call it with point on selected entry
  - if entry has an org-link in title, browse it
\(fn [:scope SCOPE] [:filter FILTER] [:action ACTION])"
  (let* ((filter (-some->> :filter
                           (plist-get args)
                           (org-glance-filter-create)))
         (action (-some->> :action
                           (plist-get args)))
         (entry (-some->> :scope
                          (plist-get args)
                          (org-glance-scope-create)
                          (org-glance-scope-entries)
                          (org-glance-filter-apply filter)
                          (org-glance-entries-browse))))
    (org-glance-entry-act entry action)))

(cl-defstruct (org-glance-scope-file (:constructor org-glance-scope-file--create)
                                     (:copier nil))
  name file)

(cl-defstruct (org-glance-scope-buffer (:constructor org-glance-scope-buffer--create)
                                       (:copier nil))
  name buffer)

(cl-defmethod org-glance-scope-create (scope)
  "Create list of scopes from file/buffer/function/symbol or sequence of it."
  (-some->> scope
            org-glance-scope--adapt
            org-glance-scope--create))

(cl-defmethod org-glance-scope-p (scope)
  (or (org-glance-scope-file-p scope)
      (org-glance-scope-buffer-p scope)))

(cl-defgeneric org-glance-scope--create (lfob))

(cl-defmethod org-glance-scope--create ((lfob string))
  (when (file-exists-p lfob)
    `(,(org-glance-scope-file--create
        :name (file-name-nondirectory lfob)
        :file (expand-file-name lfob)))))

(cl-defmethod org-glance-scope--create ((lfob buffer))
  (if (buffer-file-name lfob)
      `(,(org-glance-scope-file--create
          :name (expand-file-name (buffer-file-name lfob))
          :file (buffer-file-name lfob)))
    `(,(org-glance-scope-buffer--create
        :name (buffer-name lfob)
        :buffer lfob))))

(cl-defmethod org-glance-scope--create ((lfob list))
  (-some->> lfob
            (-keep #'org-glance-scope--create)
            (-flatten)))

(cl-defgeneric org-glance-scope--adapt (lfob)
  "Adapt list-file-or-buffer to list of file-or-buffers.")

(cl-defmethod org-glance-scope--adapt ((lfob sequence))
  "Adapt each element of LFOB."
  (-some->> lfob
            (-keep #'(lambda (fob) (->> fob org-glance-scope--adapt)))
            (-flatten)
            (seq-uniq)))

(cl-defmethod org-glance-scope--adapt ((lfob symbol))
  "Return extracted LFOB from `org-glance--default-scopes-alist'."
  (-some->> lfob
            (funcall (-cut alist-get <> org-glance--default-scopes-alist))
            (funcall)))

(cl-defmethod org-glance-scope--adapt ((lfob buffer))
  "Return list of LFOB."
  (list lfob))

(cl-defmethod org-glance-scope--adapt ((lfob function))
  "Adapt result of LFOB."
  (-some->> lfob
            (funcall)
            (org-glance-scope--adapt)))

(cl-defmethod org-glance-scope--adapt ((lfob string))
  "Return list of file LFOB if exists."
  (when (file-exists-p (expand-file-name lfob))
    (list (or (get-file-buffer (expand-file-name lfob))
              (expand-file-name lfob)))))

;; (org-glance-scope--adapt 'file-with-archives)
;; (org-glance-scope--adapt `((,(current-buffer))))
;; (org-glance-scope--adapt `(file-with-archives ,(current-buffer)))

(cl-defgeneric org-glance-scope-visit (scope))

(cl-defmethod org-glance-scope-visit ((scope org-glance-scope-file))
  (->> scope
       org-glance-scope-file-file
       find-file))

(cl-defmethod org-glance-scope-visit ((scope org-glance-scope-buffer))
  (->> scope
       org-glance-scope-buffer-buffer
       find-file))

(cl-defmethod org-glance-scope-visit ((scope list))
  (-some->> scope
            (-keep #'org-glance-scope-visit)))

;; (defun org-glance-scope-insert (scope)
;;   (case (org-glance-scope-type scope)
;;     (:file (insert-file-contents (org-glance-scope-handle scope)))
;;     (:file-buffer (insert-file-contents (buffer-file-name (org-glance-scope-handle scope))))
;;     (:buffer (insert-buffer-substring-no-properties (org-glance-scope-handle scope)))))

(cl-defgeneric org-glance-scope-name (scope))

(cl-defmethod org-glance-scope-name ((scope org-glance-scope-file))
  (-some->> scope
            org-glance-scope-file-name
            list))

(cl-defmethod org-glance-scope-name ((scope org-glance-scope-buffer))
  (-some->> scope
            org-glance-scope-buffer-name
            list))

(cl-defmethod org-glance-scope-name ((scope list))
  (-some->> scope
            (-keep #'org-glance-scope-name)
            (-flatten)))

(cl-defstruct (org-glance-entry (:constructor org-glance-entry--create)
                                (:copier nil))
  scope outline point)

(cl-defun org-glance-entry-create (&optional scope point)
  (let ((scope (or scope
                   (org-glance-scope-create (current-buffer)))))
    (org-glance-entry--create
     :scope scope
     :outline (cl-list*
               (s-join " " (org-glance-scope-name scope))
               (org-get-outline-path t))
     :point (or point (point)))))

(cl-defun org-glance-entry-serialize (entry)
  (let ((scope (org-glance-entry-scope entry)))
    (when (-all-p #'org-glance-scope-file-p scope)
      (prin1-to-string (list (cons 'outline (org-glance-entry-outline entry))
                             (cons 'point   (org-glance-entry-point entry))
                             (cons 'scope    (org-glance-scope-file-file (car scope))))))))

(cl-defun org-glance-entry-deserialize (string)
  (let* ((params (read string))
         (scope (org-glance-scope-create
                 (alist-get 'scope params)))
         (point (alist-get 'point params)))
    (org-glance-entry-create scope point)))

(defun org-glance-entry-format (entry)
  (->> entry
       (org-glance-entry-outline)
       (s-join org-glance-defaults--separator)))

(defun org-glance-entry-visit (entry)
  (->> entry
       org-glance-entry-scope
       org-glance-scope-visit)
  (goto-char (org-glance-entry-point)))

(defun org-glance-entry-act (entry &optional action)
  (org-glance-entry-visit entry)
  (if action
      (funcall action)
    (let* ((line (thing-at-point 'line t))
           (search (string-match org-any-link-re line))
           (link (substring line (match-beginning 0) (match-end 0)))
           (org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
      (org-open-link-from-string link))))

(defun org-glance-entries-browse (entries)
  (let* ((prompt "Glance: ")
         (separator org-glance-defaults--separator)
         (choice (org-completing-read prompt (mapcar #'org-glance-entry-format entries)))
         (entry (loop for entry in entries
                      when (string= (org-glance-entry-format entry)
                                    choice)
                      do (return entry))))
    entry))

(cl-defgeneric org-glance-scope-entries (scope))

(cl-defmethod org-glance-scope-entries ((scope org-glance-scope-file))
  (save-window-excursion
    (org-glance-scope-visit scope)
    (save-excursion
      (org-save-outline-visibility nil
        (org-map-entries #'org-glance-entry-create)))))

(cl-defmethod org-glance-scope-entries ((scope org-glance-scope-buffer))
  (save-window-excursion
    (org-glance-scope-visit scope)
    (save-excursion
      (org-save-outline-visibility nil
        (org-map-entries #'org-glance-entry-create)))))

(cl-defmethod org-glance-scope-entries ((scope list))
  (-some->> scope
            (mapcar #'org-glance-scope-entries)
            (-flatten)))

(defvar org-glance--default-scopes-alist
  `((file-with-archives . org-glance-scope--list-archives)))

(defun org-glance-scope--list-archives ()
  (-some->> (buffer-file-name)
            (file-name-nondirectory)
            (file-name-sans-extension)
            (s-append ".org_archive")
            (directory-files-recursively default-directory)))

(defvar org-glance--default-filters
  '((links . (lambda () (org-match-line (format "^.*%s.*$" org-bracket-link-regexp))))
    (encrypted . (lambda () (seq-intersection (list "crypt") (org-get-tags-at))))))

(cl-defstruct (org-glance-filter (:constructor org-glance-filter--create)
                                 (:copier nil))
  handler)

(cl-defun org-glance-filter-create (&optional filter)
  "Factorize FILTER into list of filters. Acceptable FILTER values:
- list of symbols (possible default filters) and lambdas (custom filters)
- string name of default filter
- symbolic name of default filter
- lambda function with no params called on entry"
  (cond ((null filter) (org-glance-filter-create #'(lambda () t)))
        ((functionp filter) (-some->> filter
                                      (org-glance-filter--create :handler)
                                      list))
        ((symbolp filter) (-some->> (alist-get filter org-glance--default-filters)
                                    (org-glance-filter--create :handler)
                                    list))
        ((stringp filter) (-some->> filter
                                    intern
                                    org-glance-filter-create))
        ((listp filter) (mapcar #'(lambda (f) (thread-first f org-glance-filter-create car)) filter))
        (t (error "Unable to recognize filter."))))

(cl-defgeneric org-glance-filter-apply (filter entry))

(cl-defmethod org-glance-filter-apply ((filter org-glance-filter) (entry org-glance-entry))
  (save-window-excursion
    (save-excursion
      (org-save-outline-visibility
          (when (org-glance-entry-p entry)
            (org-glance-entry-visit entry))
        (condition-case nil
            (-some->> filter org-glance-filter-handler funcall)
          (error nil))))))

(cl-defmethod org-glance-filter-apply ((filter list) (entry list))
  (-filter (lambda (e) (-all? (-cut org-glance-filter-apply <> e) filter)) entry))

(provide 'org-glance)
;;; org-glance.el ends here
