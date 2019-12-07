;;; org-glance.el --- org-mode traversing. Fast and convenient.

;; Copyright (C) 2018-2019 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 29 September, 2018
;; Version: 1.0

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
(require 'aes)
(require 'load-relative)

(load-relative "plugins/org-glance-bookmarks.el")
(load-relative "plugins/org-glance-password-manager.el")
(load-relative "plugins/org-glance-contacts.el")

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(defvar org-glance--default-scopes-alist
  `((file-with-archives . org-glance-scope--list-archives)
    (agenda-with-archives . org-glance-scope--agenda-with-archives)))

(define-error 'org-glance-cache-outdated
  "Cache file is outdated"
  'user-error)

(defun org-glance-cache-outdated (format &rest args)
  (signal 'org-glance-cache-outdated
          (list (apply #'format-message format args))))

(defun org-glance-scope--list-file-archives (filename)
  (-some->> filename
    (file-name-nondirectory)
    (file-name-sans-extension)
    (s-append ".org_archive")
    (directory-files-recursively (file-name-directory filename))))

(defun org-glance-scope--list-archives ()
  (append (list (buffer-file-name))
          (org-glance-scope--list-file-archives (buffer-file-name))))

(defun org-glance-scope--agenda-with-archives ()
  (cl-loop for filename in (org-agenda-files)
           append (list filename)
           append (org-glance-scope--list-file-archives filename)))

(cl-defgeneric org-glance-adapt-scope (lfob)
  "Adapt list-file-or-buffer to list of file-or-buffers.")

(cl-defmethod org-glance-adapt-scope ((lfob string))
  "Return list of file LFOB if exists."
  (list (or (expand-file-name lfob)
            (-some->> lfob
                      expand-file-name
                      get-file-buffer
                      buffer-name))))

(cl-defmethod org-glance-adapt-scope ((lfob sequence))
  "Adapt each element of LFOB."
  (-some->> lfob
            (-keep #'(lambda (fob) (->> fob org-glance-adapt-scope)))
            (-flatten)
            (seq-uniq)))

(cl-defmethod org-glance-adapt-scope ((lfob symbol))
  "Return extracted LFOB from `org-glance--default-scopes-alist'."
  (-some->> lfob
            (funcall (-cut alist-get <> org-glance--default-scopes-alist))
            (funcall)))

(cl-defmethod org-glance-adapt-scope ((lfob buffer))
  "Return list of LFOB."
  (list
   (condition-case nil
       (get-file-buffer lfob)
     (error lfob))))

(cl-defmethod org-glance-adapt-scope ((lfob function))
  "Adapt result of LFOB."
  (-some->> lfob
            funcall
            org-glance-adapt-scope))

(cl-defun org-glance-serialize (headline &key title-property)
  (prin1-to-string
   (list (when title-property
           (org-element-property title-property headline))
         (org-element-property :raw-value headline)
         (org-element-property :begin headline)
         (org-element-property :file headline))))

(cl-defun org-glance-deserialize (input &key title-property)
  (cl-destructuring-bind (alias title begin file) input
    (org-element-create 'headline
                        `(,title-property ,alias
                                          :raw-value ,title
                                          :begin ,begin
                                          :file ,file))))

(cl-defun org-glance-completing-read (headlines &key prompt title-property)
  (org-completing-read prompt
                       (cl-loop for headline in headlines
                                collect (org-glance-format headline
                                                           :title-property title-property))))

(cl-defun org-glance-format (headline &key title-property)
  (or (when title-property
        (org-element-property title-property headline))
      (org-element-property :raw-value headline)))

(cl-defun org-glance-browse (headlines &key choice fallback title-property)
  (or (cl-loop for headline in headlines
               when (string= (org-glance-format headline
                                                :title-property title-property) choice)
               do (cl-return headline))
      (when fallback (funcall fallback choice))))

(cl-defgeneric org-glance-read (file &key filter)
  "Read org-element headlines from one or many files.")

(cl-defmethod org-glance-read ((files list) &key filter)
  (cl-loop for file in (org-glance-adapt-scope files)
           do (message "Glance %s" file)
           append (org-glance-read file :filter filter) into result
           when (not (sit-for 0))
           do (cl-return result)
           finally (cl-return result)))

(cl-defmethod org-glance-read ((file string) &key filter)
  (pcase-let ((`(,file ,id) (s-split-up-to "#" file 2)))
    (when (and (file-exists-p file)
               (not (f-directory? file)))
      (with-temp-buffer
        (insert-file-contents file)
        (when id
          (goto-char (org-find-entry-with-id id))
          (org-narrow-to-subtree))
        (org-element-map (org-element-parse-buffer 'headline) 'headline
          (lambda (headline)
            (when-let (headline (if filter
                                    (when (funcall filter headline)
                                      headline)
                                  headline))
              (plist-put (cadr headline) :file file)
              headline)))))))

(cl-defun org-glance-save (file entries &key title-property)
  (unless (file-exists-p (file-name-directory file))
    (make-directory (file-name-directory file) t))
  (with-temp-file file
    (insert "`(")
    (dolist (entry entries)
      (insert (org-glance-serialize entry
               :title-property title-property) "\n"))
    (insert ")"))
  entries)

(cl-defun org-glance-load (file &key title-property)
  (let ((entries
         (with-temp-buffer (insert-file-contents file)
                           (->> (buffer-string)
                                substring-no-properties
                                read
                                eval))))
    (cl-loop for entry in entries
             collect (org-glance-deserialize entry
                                             :title-property title-property))))

(defun org-glance-act--visit-headline (headline)
  "Goto HEADLINE."
  (let* ((file (org-element-property :file headline))
         (point (org-element-property :begin headline))
         (file-buffer (get-file-buffer file)))
    (if (file-exists-p file)
        (find-file file)
      (org-glance-cache-outdated "File not found: %s" file))
    (goto-char point)
    (if (condition-case nil
            (s-contains? (org-element-property :raw-value (org-element-at-point))
                         (org-element-property :raw-value headline))
          (error nil))
        (org-show-context 'org-goto)
      (unless file-buffer
        (kill-buffer))
      (org-glance-cache-outdated "Cache file is outdated"))))

(defun org-glance-act--open-org-link (headline)
  "Open org-link at HEADLINE."
  (let* ((file (org-element-property :file headline))
         (file-buffer (get-file-buffer file))
         (org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
    (org-glance-act--visit-headline headline)
    (org-open-at-point)
    (if file-buffer
        (bury-buffer file-buffer)
      (kill-buffer (get-file-buffer file)))))

(cl-defun org-glance (scope
                      &key
                      filter
                      action
                      fallback
                      default-choice
                      cache-file force-reread-p
                      (prompt "Glance: ")
                      (title-property :org-glance-title))
  "Run completing read on org-files entries from SCOPE list prompting a PROMPT.
Filter headlines by FILTER method.
Call ACTION method on selected headline.
Specify CACHE file name to save headlines to read-optimized el-file.
Specify FORCE-REREAD-P predicate to reread cache file.
If user input doesn't match any entry, call FALLBACK method with user input as argument.
Read headline title in completing read prompt from org-property TITLE-PROPERTY."
  (if-let ((headlines (if (and cache-file
                               (file-exists-p cache-file)
                               (null force-reread-p))
                          (org-glance-load cache-file :title-property title-property)
                        (org-glance-read scope :filter filter))))
      (unwind-protect
          (when-let (choice (or default-choice
                                (org-glance-completing-read headlines
                                                            :prompt prompt
                                                            :title-property title-property)))
            (if-let (headline (org-glance-browse headlines
                                                 :choice choice
                                                 :fallback fallback
                                                 :title-property title-property))
                (condition-case nil
                    (if (functionp action)
                        (funcall action headline)
                      (user-error "Specify ACTION method to call on headline"))
                  (org-glance-cache-outdated
                   (message "Cache file %s is outdated, actualizing..." cache-file)
                   (redisplay)
                   (org-glance scope
                               :prompt prompt
                               :filter filter
                               :action action
                               :cache-file cache-file
                               :fallback fallback
                               :default-choice choice
                               :title-property title-property
                               :force-reread-p t)))
              (user-error "Headline not found")))
        ;; Unwind
        (when (and cache-file
                   (or force-reread-p
                       (not (file-exists-p cache-file))))
          (org-glance-save cache-file headlines :title-property title-property)))
    (user-error "Nothing to glance at (scope: %s)" scope)))

(provide-me)
;;; org-glance.el ends here
