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
                            (org-glance-filter-entries filter)
                            (org-glance-entries-browse))))
    (org-glance-entry-act entry action)))

(cl-defstruct (org-glance-scope (:constructor org-glance-scope--create)
                                (:copier nil))
  type name handle)

(defvar org-glance--default-scopes-alist
  `((file-with-archives . (lambda () (org-glance-scope--list-archives)))))

(defun org-glance-scope--preprocess (scope)
  (cond ((bufferp scope)
         (list scope))

        ((and (symbolp scope) (alist-get scope org-glance--default-scopes-alist))
         (funcall (alist-get scope org-glance--default-scopes-alist)))

        ((functionp scope)
         (-some->> (funcall scope)
                   (org-glance-scope--preprocess)))

        ((and (stringp scope)
              (file-exists-p (expand-file-name scope)))
         (list (or (get-file-buffer (expand-file-name scope))
                   (expand-file-name scope))))

        ((listp scope)
         (-some->> scope
                   (-keep #'org-glance-scope--preprocess)
                   -flatten))))

(defun org-glance-scope--detect (scope)
  (cond
   ((and (stringp scope) (file-exists-p scope))
    `(:type :file
            :name ,(expand-file-name scope)
            :handle ,scope))
   ((bufferp scope)
    `(:type :buffer
            :name ,(buffer-name scope)
            :handle ,scope))
   ((and (bufferp scope) (buffer-file-name scope) (file-exists-p (buffer-file-name scope)))
    `(:type :file-buffer
            :name ,(expand-file-name (buffer-file-name scope))
            :handle ,scope))
   ((listp scope)
    (->> scope
         (-keep #'org-glance-scope--detect)))))

(defun org-glance-scope-visit (scope)
  (case (org-glance-scope-type scope)
    (:file (find-file (org-glance-scope-handle scope)))
    (:file-buffer (switch-to-buffer (org-glance-scope-handle scope)))
    (:buffer (switch-to-buffer (org-glance-scope-handle scope)))))

(defun org-glance-scope-insert (scope)
  (case (org-glance-scope-type scope)
    (:file (insert-file-contents (org-glance-scope-handle scope)))
    (:file-buffer (insert-file-contents (buffer-file-name (org-glance-scope-handle scope))))
    (:buffer (insert-buffer-substring-no-properties (org-glance-scope-handle scope)))))

(defun org-glance-scope-entries (scope)
  (loop for fob in scope
        append (save-window-excursion
                 (save-excursion
                   (org-glance-scope-visit fob)
                   (org-map-entries #'org-glance-entry-at-point)))))

(defun org-glance-entry-format-batch (separator entries)
  (mapcar (apply-partially #'org-glance-entry-format separator) entries))

(defun org-glance-entries-browse (entries)
  (let* ((prompt "Glance: ")
         (separator org-glance-defaults--separator)
         (choice (org-completing-read prompt (org-glance-entry-format-batch separator entries)))
         (entry (loop for entry in entries
                      when (string= (org-glance-entry-format separator entry)
                                    choice)
                      do (return entry)))
         (marker (org-glance-entry-marker entry)))
    entry))

(cl-defun org-glance-scope-create (lfob)
  (if (listp lfob)
      (-some->> lfob
                (-keep #'(lambda (f) (->> f org-glance-scope-create car)))
                -flatten
                seq-uniq)
    (or (-some->> lfob
                  org-glance-scope--preprocess
                  org-glance-scope--detect
                  (-keep #'(lambda (r) (apply #'org-glance-scope--create r))))
        (-some->> (current-buffer)
                  org-glance-scope-create))))

(cl-defstruct (org-glance-entry (:constructor org-glance-entry--create)
                                         (:copier nil))
  scope outline marker)

(cl-defun org-glance-entry-at-point ()
  (let ((scope (car (org-glance-scope-create (current-buffer)))))
      (org-glance-entry--create
       :scope scope
       :outline (cl-list* (org-glance-scope-name scope)
                          (org-get-outline-path t))
       :marker (point-marker))))

(defun org-glance-entry-format (separator entry)
  (->> entry
       org-glance-entry-outline
       (s-join separator)))

(defun org-glance-entry-visit (entry)
  (->> entry
       org-glance-entry-marker
       org-goto-marker-or-bmk))

(defun org-glance-entry-act (entry &optional action)
  (org-glance-entry-visit entry)
  (if action
      (funcall action)
    (let* ((line (thing-at-point 'line t))
           (search (string-match org-any-link-re line))
           (link (substring line (match-beginning 0) (match-end 0)))
           (org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
      (org-open-link-from-string link))))

(defun org-glance-scope--list-archives ()
  (let ((fn (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (directory-files-recursively default-directory (concat fn ".org_archive"))))

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

(defun org-glance-filter-apply (filter &optional entry)
  (assert (org-glance-filter-p filter))
  (assert (or (null entry)
              (org-glance-entry-p entry)))
  (save-window-excursion
    (save-excursion
      (when (org-glance-entry-p entry)
        (org-glance-entry-visit entry))
      (condition-case nil
          (-some->> filter org-glance-filter-handler funcall)
        (error nil)))))

(defun org-glance-filter-entries (filters &optional entries)
  (assert (-all? #'org-glance-filter-p filters))
  (assert (or (null entries)
              (-all? #'org-glance-entry-p entries)))
  (-filter (lambda (entry) (-all? (-cut org-glance-filter-apply <> entry) filters)) entries))

(provide 'org-glance)
;;; org-glance.el ends here
