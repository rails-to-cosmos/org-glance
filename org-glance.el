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
- OUTLINE-IGNORE :: list of strings to ignore in outline-path

- INPLACE :: do not build scope file if specified

\(fn [:scope SCOPE] [:prompt PROMPT] [:separator SEPARATOR] [:filter FILTER] [:action ACTION])"
  (let* ((scope (org-glance-scope-create (plist-get args :scope)))
         (filter (org-glance-filter-create (plist-get args :filter)))
         (action (plist-get args :action))

         (prompt (or (plist-get args :prompt) "Glance: "))
         (err-nothing-found (or (plist-get args :nothing-found-msg) "Nothing to glance for"))

         ;; (save-outline-visibility-p (plist-get args :save-outline-visibility))
         ;; (no-cache-file-p           (plist-get args :no-cache-file))
         ;; (org-glance-cache-file (if no-cache-file-p
         ;;                            (make-temp-file "org-glance-")
         ;;                          org-glance-cache-file))
         ;; (outline-settings (record 'org-glance-settings--outline
         ;;                           :scope scope
         ;;                           :separator (or (plist-get args :separator) org-glance-defaults--separator)
         ;;                           :outline-ignore (plist-get args :outline-ignore)
         ;;                           :inplace t ;; (plist-get args :inplace)
         ;;                           ;; temporary while outplace completions fail
         ;;                           :filters (org-glance--filter-predicates user-filter)))

         (entries (or (org-glance-scope-entries scope)
                      (error "%s %s"
                             err-nothing-found
                             (prin1-to-string scope))))
         (entry (-some->> entries
                          ;; (apply-partially #'org-glance-entries-filter filter)
                          org-glance-entries-browse)))
    (org-glance-entry-location-visit entry action)
    ;; (when no-cache-file-p
    ;;   (when-let ((fb (get-file-buffer org-glance-cache-file)))
    ;;     (kill-buffer fb))
    ;;   (delete-file org-glance-cache-file))
    ;; result
    ))

(cl-defstruct (org-glance-scope (:constructor org-glance-scope--create)
                                (:copier nil))
  type name handle)

(defvar org-glance--default-scopes-alist
  `((file-with-archives . (lambda () (org-glance-scope--list-archives)))))

(defun org-glance-scope--prepr (scope)
  (cond ((bufferp scope) scope)
        ((and (symbolp scope) (alist-get scope org-glance--default-scopes-alist))
         (funcall (alist-get scope org-glance--default-scopes-alist)))
        ((functionp scope) (when-let ((fob (funcall scope)))
                             (if (bufferp fob)
                                 fob
                               (or (get-file-buffer (expand-file-name fob))
                                   (expand-file-name fob)))))
        ((and (stringp scope)
              (file-exists-p (expand-file-name scope))) (or (get-file-buffer (expand-file-name scope))
              (expand-file-name scope)))))

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
            :handle ,scope))))

(defun org-glance-scope-visit (scope)
  (case (org-glance-scope-type scope)
    (:file (find-file (org-glance-scope-handle scope)))
    (:file-buffer (switch-to-buffer (org-glance-scope-handle scope)))
    (:buffer (switch-to-buffer (org-glance-scope-handle scope)))))

(defun org-glance-scope-contents-insert (scope)
  (case (org-glance-scope-type scope)
    (:file (insert-file-contents (org-glance-scope-handle scope)))
    (:file-buffer (insert-file-contents (buffer-file-name (org-glance-scope-handle scope))))
    (:buffer (insert-buffer-substring-no-properties (org-glance-scope-handle scope)))))

(defun org-glance-scope-entries (scope)
  (loop for fob in scope
        append (save-window-excursion
                 (save-excursion
                   (org-glance-scope-visit fob)
                   (org-map-entries #'org-glance-entry-location-at-point)))))

;; (->> (org-agenda-files)
;;      org-glance-scope-create
;;      org-glance-scope-entries
;;      org-glance-entries-browse)

(defun org-glance-entries-browse (entries)
  (let* ((prompt "Glance: ")
         (separator org-glance-defaults--separator)
         (choice (org-completing-read prompt
                                      (loop for entry in entries
                                            collect (org-glance-entry-location-outline-format separator entry))))
         (entry (loop for entry in entries
                      when (string= (org-glance-entry-location-outline-format separator entry)
                                    choice)
                      do (return entry)))
         (marker (org-glance-entry-location-marker entry)))
    entry))

(cl-defun org-glance-scope-create (lfob)
  (if (listp lfob)
      (-some->> lfob
                (-keep #'(lambda (f) (->> f org-glance-scope-create car)))
                -flatten
                seq-uniq)
    (or (-some->> lfob
                  org-glance-scope--prepr
                  org-glance-scope--detect
                  (apply #'org-glance-scope--create)
                  list)
        (-some->> current-buffer
                  org-glance-scope-create
                  list))))

(cl-defstruct (org-glance-entry-location (:constructor org-glance-entry-location--create)
                                         (:copier nil))
  scope outline marker)

(cl-defun org-glance-entry-location-at-point ()
  (let ((scope (car (org-glance-scope-create (current-buffer)))))
    (org-glance-entry-location--create
     :scope scope
     :outline (cl-list* (org-glance-scope-name scope)
                        (org-get-outline-path t))
     :marker (point-marker))))

(defun org-glance-entry-location-outline-format (separator entry)
  (->> entry
       org-glance-entry-location-outline
       (s-join separator)))

(defun org-glance-entry-location-visit (entry &optional action)
  (let ((marker (org-glance-entry-location-marker entry)))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (org-goto-marker-or-bmk marker)
        (if action
            (funcall action)
          (let* ((line (thing-at-point 'line t))
                 (search (string-match org-any-link-re line))
                 (link (substring line (match-beginning 0) (match-end 0)))
                 (org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
            (org-open-link-from-string link)))))))

(defun org-glance-scope--implant (scope)
  (with-temp-file org-glance-cache-file
    (org-mode)

    (when (file-exists-p org-glance-cache-file)
      (insert-file-contents org-glance-cache-file))

    (let* ((contents (org-glance-cache--get-scope-state-headlines scope))
           (state (car contents))
           (entries (cadr contents))
           (scope-name (org-glance-scope--get-name scope))
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
                         :scope org-glance-cache-file)))
          (save-excursion
            (goto-char scope-point)
            (org-map-entries outliner nil 'tree)))))))

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
  "Factorize FILTER into list of predicates. Acceptable FILTER values:
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

(defun org-glance-cache--get-scope-state-headlines (scope)
  (with-temp-buffer
    (org-mode)
    (org-glance-cache--insert-contents scope)
    (list (buffer-hash)
          (org-element-parse-buffer 'headline))))

(defun org-glance-cache--remove-scope (scope-name)
  (when-let (scope (org-glance-cache--get-scope scope-name))
    (delete-region (cadr scope) (caddr scope))))

(provide 'org-glance)
;;; org-glance.el ends here
