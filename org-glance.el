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
- SCOPE :: org-file or SCOPE from org-map-entries (org.el)
- PROMPT :: completing read title (default: \"Glance: \")
- SEPARATOR :: completing read entry separator (default: \" → \")
- FILTER :: list or one filter of type lambda/symbol/string to specify entries in completing read.

  Possible default filters:
  - links :: keep entries with link in title
  - encrypted :: keep entries with :crypt: tag

  You can customize default filters by setting org-glance--default-filters variable.

- ACTION
  - if specified, call it with point on selected entry
  - if entry has an org-link in title, browse it
- HANDLER :: property name to read-eval on select (default: \"HANDLER\")
- OUTLINE-IGNORE :: list of strings to ignore in outline-path

- INPLACE :: do not build scope file if specified

\(fn [:scope SCOPE] [:prompt PROMPT] [:separator SEPARATOR] [:filter FILTER] [:action ACTION] [:handler HANDLER])"
  (let* ((user-scopes (or (plist-get args :scope)          nil))
         (aggregated-scopes (org-glance--aggregate-scopes user-scopes))
         (err-nothing-found (or (plist-get args :nothing-found-msg) "Nothing to glance for"))

         (user-filter (or (plist-get args :filter)       (lambda () t)))
         (filters (org-glance--filter-predicates user-filter))
         (outline-ignore (or (plist-get args :outline-ignore) nil))
         (save-outline-visibility-p (plist-get args :save-outline-visibility))
         (inplace-p                 t ;; (plist-get args :inplace)
                                    ;; temporary while outplace completions fail
                                    )
         (no-cache-file-p           (plist-get args :no-cache-file))
         (org-glance-cache-file (if no-cache-file-p
                                    (make-temp-file "org-glance-")
                                  org-glance-cache-file))

         (handler   (or (plist-get args :handler)        "HANDLER"))
         (prompt    (or (plist-get args :prompt)         "Glance: "))
         (separator (or (plist-get args :separator)      " → "))
         (action    (or (plist-get args :action)         nil))

         (entries (or (org-glance--entries
                       :scope aggregated-scopes
                       :separator separator
                       :outline-ignore outline-ignore
                       :filters filters
                       :inplace inplace-p)
                      (error "%s %s"
                             err-nothing-found
                             (prin1-to-string aggregated-scopes))))
         (result (org-glance--compl-visit prompt entries action save-outline-visibility-p)))
    (when no-cache-file-p
      (when-let ((fb (get-file-buffer org-glance-cache-file)))
        (kill-buffer fb))
      (delete-file org-glance-cache-file))
    result))

(defun org-glance--get-entry-coordinates (&rest args)
  "Return outline path of current `'org-mode`' entry.

Org node titles separated by SEPARATOR, titles specified in
OUTLINE-IGNORE will be ignored.

All FILTERS lambdas must be t."
  (let* ((separator           (or (plist-get args :separator)           org-glance-defaults--separator))
         (outline-ignore      (or (plist-get args :outline-ignore)      nil))
         (filters             (or (plist-get args :filters)             nil))
         (inplace-p           (or (plist-get args :inplace)             nil))
         (fob                 (or (plist-get args :fob)                 nil))
         (item (org-entry-get (point) "ITEM"))
         (path (funcall (if inplace-p 'append 'cdr) (org-get-outline-path t)))
         (outline (cl-set-difference path outline-ignore :test 'string=))
         (title (mapconcat 'identity outline separator)))
    (when (and (cl-every (lambda (fp) (if fp (funcall fp) nil)) filters)
               (not (string-empty-p (s-trim title))))
      (list title (point-marker)))))

(defun org-glance--visit-entry-at-point ()
  (save-excursion
    (let* ((line (thing-at-point 'line t))
           (search (string-match org-any-link-re line))
           (link (substring line (match-beginning 0) (match-end 0))))
      (org-open-link-from-string link))))

(defun org-glance--compl-visit (prompt entries action &optional save-outline-visibility-p)
  "PROMPT org-completing-read on ENTRIES and call ACTION on selected.
If there are no entries, raise exception."
  (when (seq-empty-p entries)
    (error "Empty set"))

  (let* ((choice (org-completing-read prompt entries))
         (marker (cadr (assoc-string choice entries)))
         (org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
    (with-current-buffer (marker-buffer marker)
      (org-goto-marker-or-bmk marker)
      (org-glance--visit-entry-at-point))))

(defun org-glance--entries (&rest args)
  "Return glance entries by SCOPE.

Specify SEPARATOR and OUTLINE-IGNORE to customize
outline-paths appearence.

When INPLACE flag specified, do not modify *org-glance-scope* buffer.

Add some FILTERS to filter unwanted entries."
  (let* ((scope               (or (plist-get args :scope)               (list (current-buffer))))
         (_ (assert (listp scope) nil "Scope must be an instance of list."))

         (separator           (or (plist-get args :separator)           org-glance-defaults--separator))
         (outline-ignore      (or (plist-get args :outline-ignore)      nil))
         (inplace-p           (or (plist-get args :inplace)             nil))
         (filters             (or (plist-get args :filters)             nil))

         ;; Possible beautify and optimization: switch to opened buffer instead of finding file
         ;; (live-buffers (remove nil (mapcar 'buffer-file-name (buffer-list))))

         (scope-type-getter (lambda (fob)
                              (cond ((and (stringp fob) (file-exists-p fob)) 'file)
                                    ((and (bufferp fob) (buffer-file-name fob) (file-exists-p (buffer-file-name fob))) 'file-buffer)
                                    ((bufferp fob) 'buffer))))

         (scope-name-getter (lambda (fob scope-type)
                              (s-trim
                               (case scope-type
                                 ('file (expand-file-name fob))
                                 ('file-buffer (expand-file-name (buffer-file-name fob)))
                                 ('buffer (buffer-name fob))))))

         (implant (lambda (fob scope-type)
                    (with-temp-file org-glance-cache-file
                      (org-mode)

                      (when (file-exists-p org-glance-cache-file)
                        (insert-file-contents org-glance-cache-file))

                      (let* ((contents (org-glance-cache--get-scope-state-headlines fob scope-type))
                             (state (car contents))
                             (entries (cadr contents))
                             (scope-name (funcall scope-name-getter fob scope-type))
                             (cached-scope (org-glance-cache--get-scope scope-name)))

                        (when (and (or (not cached-scope)
                                       (not (string= state (car cached-scope))))
                                   (> (length entries) 0)
                                   (not (string= org-glance-cache-file scope-name)))
                          (org-glance-cache--remove-scope scope-name)
                          (org-glance-cache--add-scope scope-name entries state)
                          ;; TODO: possible optimization/add-scope can return scope
                          (setq cached-scope (org-glance-cache--get-scope scope-name)))

                        (when-let ((scope-point (cadr cached-scope)))
                          (let ((outliner (apply-partially
                                           'org-glance--get-entry-coordinates
                                           :separator separator
                                           :outline-ignore outline-ignore
                                           :filters filters
                                           :inplace inplace-p
                                           :fob org-glance-cache-file)))
                            (save-excursion
                              (goto-char scope-point)
                              (org-map-entries outliner nil 'tree))))))))

         (visitor (lambda (fob scope-type)
                    (save-window-excursion
                      (let ((outliner
                             (apply-partially
                              'org-glance--get-entry-coordinates
                              :separator separator
                              :outline-ignore outline-ignore
                              :filters filters
                              :inplace inplace-p
                              :fob fob)))
                        (org-glance-cache--read-contents fob scope-type)
                        (org-map-entries outliner)))))

         (handler (if inplace-p visitor implant)))

    (loop for fob in scope
          append (let* ((scope-type (funcall scope-type-getter fob))
                        (entries (funcall handler fob scope-type)))
                   (remove nil entries)))))

(defun og-build-scope-from-buffer-with-mode (buffer-major-mode)
  (lexical-let ((bmm buffer-major-mode))
    (lambda () (when (eq major-mode bmm) (current-buffer)))))

(defun org-glance--get-file-archives ()
  (let ((fn (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    (directory-files-recursively default-directory (concat fn ".org_archive"))))

(defvar org-glance--default-scopes-alist
  `((file-with-archives . org-glance--get-file-archives)))

(defun org-glance--aggregate-scopes (&optional scopes)
  "Provides list of scopes (scope may be buffer or existing file).
Without specifying SCOPES it returns list with current buffer."

  (let* ((scopes (cond ((or (stringp scopes)
                            (and (symbolp scopes) (not (null scopes))))
                        (list scopes))
                       (t scopes)))

         (ascopes (cl-loop for scope in scopes

                           ;; collect buffers
                           when (bufferp scope) collect scope
                           when (and (symbolp scope) (alist-get scope org-glance--default-scopes-alist))
                           collect (funcall (alist-get scope org-glance--default-scopes-alist))

                           ;; collect functions that return buffers or filenames
                           when (functionp scope)
                           collect (when-let ((fob (funcall scope)))
                                     (if (bufferp fob)
                                         fob
                                       (or (get-file-buffer (expand-file-name fob))
                                           (expand-file-name fob))))

                           ;; collect file names
                           when (and (stringp scope) (file-exists-p (expand-file-name scope)))
                           collect (or (get-file-buffer (expand-file-name scope))
                                       (expand-file-name scope)))))

    (or (remove 'nil (seq-uniq (-flatten ascopes)))
        (list (current-buffer)))))

(defvar org-glance--default-filters
  '((links . (lambda () (org-match-line (format "^.*%s.*$" org-bracket-link-regexp))))
    (encrypted . (lambda () (seq-intersection (list "crypt") (org-get-tags-at))))))

(defun org-glance--filter-predicates (filter)
  "Factorize FILTER into list of predicates. Acceptable FILTER values:
- list of symbols (possible default filters) and lambdas (custom filters)
- string name of default filter
- symbolic name of default filter
- lambda function with no params called on entry"
  (cond ((functionp filter) (list filter))
        ((symbolp filter) (list (alist-get filter org-glance--default-filters)))
        ((stringp filter) (list (alist-get (intern filter) org-glance--default-filters)))
        ((listp filter) (cl-loop for elt in filter
                                 when (functionp elt) collect elt
                                 when (symbolp elt)   collect (alist-get elt org-glance--default-filters)
                                 when (stringp elt)   collect (alist-get (intern elt) org-glance--default-filters)))
        (t (error "Unable to recognize filter."))))

;; org-element-interpret-data

(defun org-glance-cache--add-scope (scope entries state)
  (cl-loop for (title level) in entries
           for i below (length entries)
           with prev-level
           initially (progn
                       (goto-char (point-max))
                       (org-insert-heading nil nil t)
                       (insert scope)
                       (org-set-property "CREATED" (current-time-string))
                       (org-set-property "STATE" state)
                       (org-insert-heading-respect-content)
                       (org-do-demote))
           do (progn
                (insert title)
                (when prev-level
                  (cond ((> prev-level level) (dotimes (ld (- prev-level level)) (org-do-promote)))
                        ((< prev-level level) (dotimes (ld (- level prev-level)) (org-do-demote))))))

           when (< (+ i 1) (length entries))
           do (progn
                (org-insert-heading-respect-content)
                (setq prev-level level))))

(defun org-glance-cache--get-scope (scope-name)
  (car
   (org-element-map (org-element-parse-buffer 'headline) 'headline
     (lambda (hl)
       (let* (
              ;; maybe map properties?
              ;; (org-element-map hl 'node-property
              ;;   (lambda (np)
              ;;     (cons (org-element-property :key np)
              ;;           (org-element-property :value np))))

              (level (org-element-property :level hl))
              (title (org-element-property :title hl))
              (begin (org-element-property :begin hl))

              (end (org-element-property :end hl)))
         (when (and (= level 1) (string= title scope-name))
           (save-excursion
             (goto-char begin)
             (let* ((props (org-element--get-node-properties))
                    (state (plist-get props :STATE)))
               (org-set-property "USED" (current-time-string))
               (list state begin end)))))))))

(defun org-glance-cache--get-scope-state-headlines (fob scope-type)
  (with-temp-buffer
    (org-mode)
    (org-glance-cache--insert-contents fob scope-type)
    (list (buffer-hash)
          (org-element-parse-buffer 'headline))))

(defun org-glance-cache--remove-scope (scope-name)
  (when-let (scope (org-glance-cache--get-scope scope-name))
    (delete-region (cadr scope) (caddr scope))))

(defun org-glance-cache--insert-contents (fob scope-type)
  (case scope-type
    ('file (insert-file-contents fob))
    ('file-buffer (insert-file-contents (buffer-file-name fob)))
    ('buffer (insert-buffer-substring-no-properties fob))))

(defun org-glance-cache--read-contents (fob scope-type)
  (case scope-type
      ('file (find-file fob))
      ('file-buffer (switch-to-buffer fob))
      ('buffer (switch-to-buffer fob))))

(provide 'org-glance)
;;; org-glance.el ends here
