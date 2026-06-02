;; -*- lexical-binding: t -*-

;;; org-glance.el --- Org-mode graph database.

;; Copyright (C) 2018-2026 Dmitry Akatov

;; Author: Dmitry Akatov <dmitry.akatov@protonmail.com>
;; Created: 29 September, 2018
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (org) (aes) (dash) (f) (s) (highlight) (transient) (ht) (ert-runner "20231110.1358"))
;; Keywords: org-mode, graph
;; Homepage: https://github.com/rails-to-cosmos/org-glance
;; Source: gnu, melpa, org
;; License: GPL-3+

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

(require 'aes)
(require 'cl-generic)
(require 'cl-lib)
(require 'cl-macs)
(require 'dash)
(require 'f)
(require 'json)
(require 'ol)
(require 'org)
(require 'org-element)
(require 's)
(require 'seq)
(require 'subr-x)

(require 'org-glance-datetime-mode)
(require 'org-glance-exception)
(require 'org-glance-headline)
(require 'org-glance-graph)
(require 'org-glance-capture)
(require 'org-glance-material-mode)
(require 'org-glance-overview)
(require 'org-glance-tag)
(require 'org-glance-ui)
(require 'org-glance-utils)

(eval-when-compile
  (require 'org-macs)
  (require 'org-element))

(defmacro org-element-with-disabled-cache (&rest body)
  "Run BODY without active org-element-cache."
  (declare (debug (form body)) (indent 0))
  `(progn ,@body))

(defcustom org-glance-clone-on-repeat-p nil
  "Create a new headline copy when repeating rather than modifying in place."
  :group 'org-glance
  :type 'boolean)

(defgroup org-glance nil "Org-mode mindmap explorer."
  :tag "Org Glance"
  :group 'org)

(defvar org-glance-tags (make-hash-table)
  "Hash table {tag -> `org-glance-tag-info'}")

(cl-defun org-glance:tags ()
  (hash-table-keys org-glance-tags))

(cl-defun org-glance:tags-sorted ()
  (sort (org-glance:tags) #'s-less?))

(cl-defun org-glance-tags:completing-read (&optional (prompt "Tag: ") (require-match t))
  "Run completing read PROMPT on registered tags."
  (let ((tags (org-glance:tags-sorted)))
    (org-glance-tag:from-string (completing-read prompt tags nil require-match))))

(cl-defun org-glance:create-tag (tag)
  (unless (and (symbolp tag) (org-glance--symbol-downcased? tag))
    (error "Expected downcased symbol for tag, but got \"%s\" of type \"%s\"." tag (type-of tag)))

  (when (org-glance-tag:register tag org-glance-tags :namespace org-glance-directory)
    (org-glance-overview:create tag)))

(cl-defun org-glance-init (&optional (directory org-glance-directory))
  "Initialize org-glance from DIRECTORY."

  (load-library "org-element.el")  ;; temp fix https://github.com/doomemacs/doomemacs/issues/7347

  (unless (f-exists? directory)
    (mkdir directory t))

  (org-glance-overview-init)

  (add-hook 'org-glance-material-mode-hook #'org-glance-datetime-mode)

  ;; Initialize the graph
  (org-glance-graph directory)

  (cl-loop for dir in (org-glance--list-directories directory)
           for tag = (org-glance-tag:read dir)
           do (org-glance:create-tag tag))

  (cl-loop for tag being the hash-keys of org-glance-tags
           unless (f-exists? (f-join directory (org-glance-tag:to-string tag)))
           do (org-glance-tag:remove tag org-glance-tags))

  (setq org-agenda-files (mapcar 'org-glance-overview:file-name (org-glance:tags-sorted))))

(cl-defun org-glance-initialized? ()
  "Return non-nil if org-glance is initialized."
  (not (null (ignore-errors (org-glance-graph)))))

(cl-defun org-glance:materialize (headline)
  "Materialize HEADLINE in a new buffer."
  (interactive (list (let* ((graph (org-glance-graph))
                            (meta (org-glance-graph:completing-read graph)))
                       (org-glance-graph:load-headline graph (org-glance-headline-metadata:id meta)))))
  (cl-check-type headline org-glance-headline)
  (switch-to-buffer (org-glance-headline:materialize headline))
  headline)

(cl-defun org-glance:open (headline)
  "Open a link inside HEADLINE."
  (interactive (list (let* ((graph (org-glance-graph))
                            (meta (org-glance-graph:completing-read graph
                                    :filter #'org-glance-headline-metadata:active?)))
                       (org-glance-graph:load-headline graph (org-glance-headline-metadata:id meta)))))
  (cl-check-type headline org-glance-headline)
  (org-glance-headline:with-contents headline
    (cl-loop for (link title pos) in (org-glance--parse-links)
             unless (s-starts-with-p "[[org-glance-" link)
             collect (list title pos)
             into links
             finally
             do (goto-char (cond ((> (length links) 1) (cadr (assoc (completing-read "Open link: " links nil t) links #'string=)))
                                 ((= (length links) 1) (cadar links))
                                 (t (user-error "Unable to find links in headline"))))
             (org-open-at-point))))

(cl-defun org-glance:extract (headline &optional choice)
  "Extract key-value pairs from HEADLINE contents."
  (interactive (list (let* ((graph (org-glance-graph))
                            (meta (org-glance-graph:completing-read graph
                                    :filter #'org-glance-headline-metadata:active?)))
                       (org-glance-graph:load-headline graph (org-glance-headline-metadata:id meta)))))
  (cl-check-type headline org-glance-headline)
  (org-glance-headline:with-contents headline
    (let* ((pairs (org-glance--buffer-key-value-pairs))
           (interaction (not choice))
           result)
      (condition-case nil
          (if interaction
              (while t
                (let ((choice (completing-read "Extract: " pairs nil t)))
                  (setq result (alist-get choice pairs nil nil #'string=))
                  (kill-new result)))
            (setq result (alist-get choice pairs nil nil #'string=))
            (kill-new result))
        (quit (setq kill-ring nil)
              (message "Kill ring has been cleared")))
      result)))

(cl-defun org-glance:@ ()
  "Choose headline to refer. Insert link to it at point."
  (interactive)
  (let ((mention? (and (not (org-in-src-block-p)) (or (looking-back "^" 1) (looking-back "[[:space:]]" 1)))))
    (condition-case nil
        (cond (mention?
               (let* ((graph (org-glance-graph))
                      (meta (org-glance-graph:completing-read graph))
                      (id (org-glance-headline-metadata:id meta))
                      (title (org-glance-headline-metadata:title meta)))
                 (insert (format "[[org-glance-visit:%s][%s]]" id title))))
              (t (keyboard-quit)))
      (quit (self-insert-command 1 64)))))

(cl-defun org-glance:insert-pin-block ()
  (interactive)
  (insert "#+begin_pin" "\n\n" "#+end_pin")
  (forward-line -1))

;; Scope utilities (kept for agenda file scanning)

(defvar org-glance-scope:extensions
  '("org" "org_archive"))

(defvar org-glance-scope--default-scope-alist
  '((file-with-archives . org-glance--file-with-archives)
    (agenda . org-agenda-files)
    (agenda-with-archives . org-glance--agenda-with-archives)))

(cl-defgeneric org-glance-scope (_)
  "Convert input to list of files if possible.")

(cl-defmethod org-glance-scope ((file string))
  (let ((files (cond
                ((not (file-exists-p file)) (message "File \"%s\" does not exist" file) nil)
                ((not (file-readable-p file)) (message "File \"%s\" is not readable" file) nil)
                ((f-directory? file) (org-glance-scope (directory-files-recursively file "\\.*.org\\.*")))
                ((with-temp-buffer
                   (insert-file-contents file)
                   (hack-local-variables)
                   (alist-get 'org-glance-overview-mode (buffer-local-variables))) nil)
                (t (list file)))))
    (cl-loop for file in files
             when (member (file-name-extension file) org-glance-scope:extensions)
             collect file)))

(cl-defmethod org-glance-scope ((l sequence))
  (-some->> l
    (-keep #'org-glance-scope)
    -flatten
    seq-uniq))

(cl-defmethod org-glance-scope ((s symbol))
  (if-let (reserved-scope (assoc s org-glance-scope--default-scope-alist))
      (funcall (cdr reserved-scope))
    (org-glance-scope (symbol-name s))))

(cl-defmethod org-glance-scope ((b buffer))
  (list (condition-case nil (get-file-buffer b) (error b))))

(cl-defmethod org-glance-scope ((f function))
  (-some->> f funcall org-glance-scope))

;; Link types

(defface org-glance-link-materialize-face
  '((((background dark)) (:inherit default :underline "MediumPurple3"))
    (t (:inherit default :underline "Magenta")))
  "*Face used to highlight evaluated paragraph."
  :group 'org-glance
  :group 'faces)

(defface org-glance-link-overview-face
  '((((background dark)) (:inherit default :slant italic))
    (t (:inherit default :slant italic)))
  "*Face used to highlight evaluated paragraph."
  :group 'org-glance
  :group 'faces)

(defface org-glance-link-state-face
  '((((background dark)) (:inherit default :weight bold))
    (t (:inherit default :weight bold)))
  "*Face used to highlight evaluated paragraph."
  :group 'org-glance
  :group 'faces)

(defun org-glance-link:materialize (id &optional _)
  "Materialize org-glance headline identified by ID."
  (let* ((graph (org-glance-graph))
         (headline (org-glance-graph:load-headline graph id)))
    (org-glance:materialize headline)))

(defun org-glance-link:open (id &optional _)
  "Open org-glance headline identified by ID."
  (let* ((graph (org-glance-graph))
         (headline (org-glance-graph:load-headline graph id)))
    (org-glance:open headline)))

(defun org-glance-link:overview (tag &optional _)
  "Open org-glance overview for TAG."
  (org-glance-overview (intern (downcase tag))))

(defun org-glance-link:state (_state &optional _)
  "Get all headlines with todo state equal STATE."
  (user-error "Not implemented."))

(org-link-set-parameters
 "org-glance-visit"
 :follow #'org-glance-link:materialize
 :face 'org-glance-link-materialize-face)

(org-link-set-parameters
 "org-glance-open"
 :follow #'org-glance-link:open)

(org-link-set-parameters
 "org-glance-overview"
 :follow #'org-glance-link:overview
 :face 'org-glance-link-overview-face)

(org-link-set-parameters
 "org-glance-state"
 :follow #'org-glance-link:state
 :face 'org-glance-link-state-face)

(provide 'org-glance)
;;; org-glance.el ends here
