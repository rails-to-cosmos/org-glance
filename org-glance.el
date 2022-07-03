;;; org-glance.el --- org-mode traversing. Fast and convenient. -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 29 September, 2018
;; Version: 0.1.0

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
(require 'org-element)

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :version "27.2"
  :package-version "0.1.0"
  :group 'org)

(defcustom org-glance-directory org-directory
  "Directory with Org files."
  :group 'org-glance
  :type 'directory)

(defcustom org-glance-clone-on-repeat-p nil
  "Clone repeated headlines on complete."
  :group 'org-glance
  :type 'boolean)

(define-error 'org-glance-error "Unknown org-glance error" 'user-error)
(define-error 'org-glance-error:SOURCE-CORRUPTED "Headline source corrupted, please reread" 'org-glance-error)
(define-error 'org-glance-error:PROPERTIES-CORRUPTED "Headline metadata corrupted, please reread" 'org-glance-error)
(define-error 'org-glance-error:METASTORE-OUTDATED "Metastore is outdated, please rebuild" 'org-glance-error)
(define-error 'org-glance-error:HEADLINE-NOT-FOUND "Headline not found" 'org-glance-error)
(define-error 'org-glance-error:HEADLINE-ALREADY-REGISTERED "Headline has already been registered" 'org-glance-error)
(define-error 'org-glance-error:CLASS-NOT-FOUND "Class not found" 'org-glance-error)

(require 'org-glance-headline)

(defvar org-glance-material-buffers (make-hash-table))

(cl-defun org-glance-init ()
  "Update system state from `org-glance-directory'."
  (unless (f-exists? org-glance-directory)
    (mkdir org-glance-directory t))

  ;; Read `org-glance-registry' from `org-glance-directory'

  ;; Actualize `org-glance-class-registry'

  ;; Storage partitioning schema: class/created-date/headline-id/headline.el
  ;; Archive partitioning schema: class/closed-date/headline-id/headline.el
  )

(cl-defmacro org-glance-loop (&rest forms)
  "Loop over headlines and execute FORMS on each.
This is the anaphoric method, you can use HEADLINE in forms."
  `(cl-loop for pos in (org-element-map (org-element-parse-buffer 'headline) 'headline
                         (lambda (headline) (org-element-property :begin headline)))
      collect (save-excursion
                (goto-char pos)
                (let ((headline (org-glance-headline-at-point)))
                  (org-glance:with-heading-at-point
                    ,@forms)))))

(cl-defmacro org-glance-with-file (file &rest forms)
  (declare (indent 1))
  `(with-temp-file ,file
     ,@forms))

(cl-defmacro org-glance-loop-file (file &rest forms)
  (declare (indent 1))
  `(org-glance-with-file ,file
     (insert-file-contents-literally ,file)
     (org-glance-loop ,@forms)))

(cl-defun org-glance-materialize (file &rest headlines)
  (declare (indent 1))
  (org-glance-with-file file
    (--map (if-let (file (org-glance-headline:file it))
               (progn
                 (org-glance-headline-property-set it "Hash" (org-glance-headline:hash it))
                 (org-glance-headline-property-set it "Origin" (org-glance-headline:file it))
                 (insert (org-glance-headline:contents it) "\n"))
             (warn "Unable to materialize headline without file origin"))
           headlines)))

(cl-defun org-glance-commit ()
  "Apply all changes of buffer headlines to its origins."
  (interactive)
  (org-glance-loop
   (let* ((new-headline headline)
          (origin (org-glance-headline-property-get new-headline "Origin"))
          (hash (org-glance-headline-property-get new-headline "Hash"))
          (new-hash (when (and origin hash)
                      (org-glance-loop-file origin
                        (when (string= hash (org-glance-headline:hash headline))
                          (delete-region (point-min) (point-max))
                          (org-glance-headline-property-remove new-headline "Hash" "Origin")
                          (insert (org-glance-headline:contents new-headline))
                          (org-glance-headline:hash new-headline))))))
     (cond ((or (null new-hash) (null (car new-hash))) (user-error "Unable to apply changes"))
           ((string= (car new-hash) hash) (message "Nothing changed"))
           (t (org-set-property "Hash" (car new-hash))
              (message "Changes applied to %s" origin)))))

  ;; (unless org-glance-material-mode--original-headline
  ;;   (org-glance-material-mode:ORIGINAL-HEADLINE-NOT-FOUND))

  ;; (with-demoted-errors "Hook error: %s"
  ;;   (run-hooks 'org-glance-before-materialize-sync-hook))

  ;; (goto-char (point-min))

  ;; Ensure current buffer is materialized

  ;; Narrow to headline
  ;; Determine output location
  ;; Check hashes
  ;; Write
  )

;; (org-glance-loop-file "/tmp/org-glance-vYZTrF/tmp/phones.org"
;;   (org-glance-materialize "/tmp/org-glance-vYZTrF/tmp/material.org" headline))

;; (org-glance-loop-file "/tmp/org-glance-eQqFt5/tmp/phones.org"
;;   (org-glance-headline:hash headline))

(provide 'org-glance)
;;; org-glance.el ends here
