;;; org-glance.el --- org-mode traversing. Fast and convenient.     -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021 Dmitry Akatov

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

(require 'org-glance-helpers)

(require 'org-glance-scope)
(require 'org-glance-headline)
(require 'org-glance-db)
(require 'org-glance-view)
(require 'org-glance-action)
(require 'org-glance-transient)

(eval-and-compile
  (require 'org)
  (require 'org-element)
  (require 'eieio-core))

(eval-when-compile
  (require 'cl-lib)
  (require 'cl-generic)
  (require 'cl-macs)
  (require 'org)
  (require 'seq)
  (require 'subr-x))

(require 'gv)


(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(defvar org-glance-prompt "Glance: ")

(defvar org-glance-properties-ignore-patterns
  (append
   org-special-properties
   '("^ARCHIVE_" "^TITLE$")))

(defun org-glance-show-report ()
  (interactive)
  (let ((begin_src "#+BEGIN: clocktable :maxlevel 9 :scope org-glance-exports :link yes :narrow 100 :formula % :properties (\"TAGS\") :block today :fileskip0 t :hidefiles t")
        (end_src "#+END:")
        (report-buffer (get-buffer-create "*org-glance-report*")))
    (with-current-buffer report-buffer
      (org-mode)
      (delete-region (point-min) (point-max))
      (insert begin_src)
      (insert "\n")
      (insert end_src)
      (goto-char (point-min))
      (org-ctrl-c-ctrl-c))
    (switch-to-buffer report-buffer)))

(cl-defun org-glance-view-update (&optional
                                    (view-id (org-glance-read-view))
                                    (destination org-glance-export-directory))
  (interactive)
                                        ; Make generic?
  (cond ((string= view-id org-glance-view-selector:all)
         (cl-loop for view in (org-glance-list-views)
            do (org-glance-view-update view destination)))
        (t (let ((dest-file-name (org-glance-view-export-filename view-id destination)))
             (when (file-exists-p dest-file-name)
               (delete-file dest-file-name t))
             (cl-loop for headline in (->> view-id
                                        org-glance-view-reread
                                        org-glance-view-headlines)
                do (org-glance-with-headline-materialized headline
                       (append-to-file (point-min) (point-max) dest-file-name)
                     (append-to-file "\n" nil dest-file-name)))
             (progn ;; sort headlines by TODO order
               (find-file dest-file-name)
               (goto-char (point-min))
               (set-mark (point-max))
               (condition-case nil
                   (org-sort-entries nil ?o)
                 (error 'nil))
               (org-overview)
               (save-buffer)
               (bury-buffer))
             dest-file-name))))

(org-glance-action-define visit (headline) :for all
  "Visit HEADLINE."
  (let* ((file (org-element-property :file headline))
         (point (org-element-property :begin headline))
         (buffer (get-file-buffer file)))
    (message "Attempt to visit file %s" file)
    (cond ((file-exists-p file) (find-file file))
          (t (org-glance-db-outdated "File not found: %s" file)))
    (widen)
    (goto-char point)
    (cond ((-element-at-point-equals-headline headline)
           (cl-loop while (org-up-heading-safe)) ;; expand parents
           (org-narrow-to-subtree)
           (widen)
           (goto-char point)
           (org-show-children))
          (t (unless buffer (kill-buffer))
             (message "Unable to visit headline %s" headline)
             (org-glance-db-outdated "Visited headline cache corrupted, please reread")))))

(define-key org-glance-view-mode-map (kbd "C-c C-v") #'org-glance-view-visit-original-heading)

(defun org-glance-view-visit-original-heading ()
  (interactive)
  (save-excursion
    (cl-loop while (org-up-heading-safe))
    (let* ((heading (list :file --org-glance-view-src
                          :begin --org-glance-view-beg
                          :raw-value (org-element-property :raw-value (org-element-at-point))))
           (virtual-element (org-element-create 'headline heading)))
      (org-glance-action-call 'visit :on virtual-element))))

;; (org-glance-def-type all "Doc string")
;; (org-glance-def-type crypt)
;; (org-glance-def-type kvs)

;; (org-glance-action-define ... for type)

;; (org-glance-def-capture (headline) for type

(org-glance-action-define materialize (headline) :for all
  "Materialize HEADLINE in separate buffer."
  (cl-labels ((first-level-heading () (save-excursion
                                        (unless (org-at-heading-p) (org-back-to-heading))
                                        (beginning-of-line)
                                        (point)))
              (end-of-subtree () (save-excursion (org-end-of-subtree t)))
              (buffer-contents (beg end) (->> (buffer-substring-no-properties beg end)
                                           (s-trim))))
    (let ((buffer org-glance-materialized-view-buffer))
      (save-window-excursion
        (org-glance-action-call 'visit :on headline)
        (let* ((file (org-element-property :file headline))
               (beg (first-level-heading))
               (end (end-of-subtree))
               (contents (buffer-contents beg end)))
          (when (get-buffer buffer)
            (switch-to-buffer buffer)
            (condition-case nil
                (org-glance-view-sync-subtree)
              (org-glance-view-not-modified nil))
            (kill-buffer buffer))
          (with-current-buffer (get-buffer-create buffer)
            (delete-region (point-min) (point-max))
            (org-mode)
            (org-glance-view-mode)
            (insert contents)
            (goto-char (point-min))
            (org-content 1)
            (org-cycle-hide-drawers 'all)
            (setq-local --org-glance-view-src file)
            (setq-local --org-glance-view-beg beg)
            (setq-local --org-glance-view-end end)
            ;; extract hash from promoted subtree
            (setq-local --org-glance-view-hash (org-glance-view-subtree-hash))
            ;; run hooks on original subtree
            (with-demoted-errors (run-hooks 'org-glance-after-materialize-hook))
            ;; then promote it saving original level
            (setq-local --org-glance-view-indent (-org-glance-promote-subtree)))
          (org-cycle 'contents)))
      (switch-to-buffer buffer))))

(org-glance-action-define open (headline) :for link
  "Search for `org-any-link-re' under the HEADLINE
then run `org-completing-read' to open it."
  (org-glance-with-headline-narrowed headline
      (let* ((links (org-element-map (org-element-parse-buffer) 'link
                      (lambda (link)
                        (cons
                         (substring-no-properties
                          (or (nth 2 link) ;; link alias
                              (org-element-property :raw-link link))) ;; full link if alias is none
                         (org-element-property :begin link)))))
             (point (cond
                      ((> (length links) 1) (cdr (assoc (org-completing-read "Open link: " links) links)))
                      ((= (length links) 1) (cdar links))
                      (t (user-error "Unable to find links in %s" (buffer-file-name))))))
        (goto-char point)
        (org-open-at-point))))

(org-glance-action-define insert (headline) :for babel
  "Visit HEADLINE, get contents and insert it."
  (insert (save-window-excursion
            (save-excursion
              (org-glance-action-call 'visit :on headline)
              (org-babel-next-src-block)
              (org-narrow-to-block)
              (buffer-substring-no-properties
               (save-excursion (goto-char (point-min))
                               (forward-line)
                               (point))
               (save-excursion (goto-char (point-max))
                               (previous-line)
                               (end-of-line)
                               (point)))))))

(org-glance-action-define extract-property (headline) :for kvs
  "Completing read all properties from HEADLINE and its successors to kill ring."
  (save-window-excursion
    (org-glance-action-call 'materialize :on headline)
    (org-glance-buffer-properties-to-kill-ring)))

(org-glance-action-define materialize (headline) :for crypt
  "Decrypt encrypted HEADLINE, then call MATERIALIZE action on it."
  (cl-flet ((decrypt ()
              (setq-local --org-glance-view-pwd (read-passwd "Password: "))
              (org-glance-decrypt-subtree --org-glance-view-pwd)))
    (add-hook 'org-glance-after-materialize-hook #'decrypt t)
    (unwind-protect
         (progn
           (org-glance-action-call 'materialize :on headline)
           (org-cycle-hide-drawers 'all))
      (remove-hook 'org-glance-after-materialize-hook #'decrypt)))
  (add-hook 'org-glance-before-materialize-sync-hook
            (lambda ()
              (-org-glance-demote-subtree --org-glance-view-indent)
              (org-glance-encrypt-subtree --org-glance-view-pwd)
              (-org-glance-promote-subtree))
            'append 'local)
  (add-hook 'org-glance-after-materialize-sync-hook
            (lambda ()
              (-org-glance-demote-subtree --org-glance-view-indent)
              (org-glance-decrypt-subtree --org-glance-view-pwd)
              (-org-glance-promote-subtree))
            'append 'local))

(org-glance-action-define extract-property (headline) :for (kvs crypt)
  "Materialize HEADLINE, decrypt it, then run completing read on all properties to kill ring."
  (save-window-excursion
    (org-glance-action-call 'materialize :on headline :for 'crypt)
    (org-cycle-hide-drawers 'all)
    (unwind-protect
         (org-glance-buffer-properties-to-kill-ring)
      (kill-buffer org-glance-materialized-view-buffer))))

(cl-defun org-glance-buffer-properties-to-kill-ring (&optional (ignore-patterns org-glance-properties-ignore-patterns))
  "Extract buffer org-properties, run completing read on keys, copy values to kill ring."
  (while t
    (let* ((properties (-filter (lambda (key) (not (--any? (s-matches? it key) ignore-patterns))) (org-buffer-property-keys)))
           (property (org-completing-read "Extract property: " properties))
           (values (org-property-values property)))
      (kill-new (cond
                  ((> (length values) 1) (org-completing-read "Choose property value: " values))
                  ((= (length values) 1) (car values))
                  (t (user-error "Something went wrong: %s" values)))))))

(defun org-glance-read-scope ()
  (completing-read
   "Scope: "
   '(agenda
     agenda-with-archives
     file)))

(cl-defun org-glance
    (&key db
       default-choice
       (db-init nil)
       (filter #'(lambda (_) t))
       (scope '(agenda))
       (action #'org-glance--visit--all))
  "Run completing read on org entries from SCOPE asking a `org-glance-prompt'.
Scope can be file name or list of file names.
Filter headlines by FILTER method.
Call ACTION method on selected headline.
Specify DB to save headlines in read-optimized el-file.
Specify DB-INIT predicate to reread cache file. Usually this flag is set by C-u prefix."
  (let* ((headlines
          (org-glance-headlines
           :db db
           :db-init db-init
           :scope scope
           :filter filter)))
    (unwind-protect
         (when-let (choice (or default-choice (org-glance-prompt-headlines org-glance-prompt headlines)))
           (if-let (headline (org-glance-choose-headline choice headlines))
               (condition-case nil (funcall action headline)
                 (org-glance-db-outdated
                  (message "Database %s is outdated, actualizing..." db)
                  (redisplay)
                  (org-glance :scope scope
                              :filter filter
                              :action action
                              :db db
                              :db-init t
                              :default-choice choice)))
             (user-error "Headline not found"))))))

(provide 'org-glance)
;;; org-glance.el ends here
