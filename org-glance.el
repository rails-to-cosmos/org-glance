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

(require 'aes)
(require 'gv)
(require 'transient)

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(defun org-glance-encrypt-subtree (&optional password)
  "Encrypt subtree at point with PASSWORD."
  (interactive)
  (let* ((beg (save-excursion (org-end-of-meta-data) (point)))
         (end (save-excursion (org-end-of-subtree t)))
         (plain (let ((plain (buffer-substring-no-properties beg end)))
                  (if (with-temp-buffer
                        (insert plain)
                        (aes-is-encrypted))
                      (user-error "Headline is already encrypted")
                    plain)))
         (encrypted (aes-encrypt-buffer-or-string plain password)))
    (save-excursion
      (org-end-of-meta-data)
      (kill-region beg end)
      (insert encrypted))))

(defun org-glance-decrypt-subtree (&optional password)
  "Decrypt subtree at point with PASSWORD."
  (interactive)
  (let* ((beg (save-excursion (org-end-of-meta-data) (point)))
         (end (save-excursion (org-end-of-subtree t)))
         (encrypted (let ((encrypted (buffer-substring-no-properties beg end)))
                      (if (not (with-temp-buffer
                                 (insert encrypted)
                                 (aes-is-encrypted)))
                          (user-error "Headline is not encrypted")
                        encrypted)))
         (plain (aes-decrypt-buffer-or-string encrypted password)))
    (unless plain
      (user-error "Wrong password"))
    (save-excursion
      (org-end-of-meta-data)
      (kill-region beg end)
      (insert plain))))

(defvar org-glance-properties-ignore-patterns
  (append
   org-special-properties
   '("^ARCHIVE_" "^TITLE$")))

(defun org-glance-act-arguments nil
  (transient-args 'org-glance-act))

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

(eval-and-compile
  (cl-defmethod org-glance-generic-method-name ((name symbol))
    (intern (format "org-glance-action-%s" name)))

  (cl-defmethod org-glance-concrete-method-name ((name symbol) (type symbol))
    (org-glance-concrete-method-name name (list type)))

  (cl-defmethod org-glance-concrete-method-name ((name symbol) (type list))
    (->> type
      (-map #'symbol-name)
      (-sort #'s-less?)
      (s-join "-")
      (format "org-glance-action-%s-%s" name)
      (intern))))

(cl-defun org-glance-view-export (&optional
                                    (view-id (org-glance-read-view))
                                    (destination org-glance-export-directory))
  (interactive)
                                        ; Make generic?
  (cond ((string= view-id org-glance-view-selector:all)
         (cl-loop for view in (org-glance-list-views)
            do (org-glance-view-export view destination)))
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

(cl-defun org-glance-action-call (name &key (on 'current-headline) (for 'all))
  (when (eq on 'current-headline)
    (setq on (org-element-at-point)))
  (let ((fn (intern (format "org-glance--%s--%s" name for))))
    (unless (fboundp fn)
      (user-error "Unbound function %s" fn))
    (funcall fn on)))

(defun org-glance-action-headlines (action)
  (cl-loop for view being the hash-values of org-glance-views
     when (org-glance-view-action-resolve view action)
     append (mapcar #'(lambda (headline) (cons headline view)) (org-glance-view-headlines/formatted view))))

(eval-and-compile
  (cl-defmethod org-glance-action-register ((name symbol) (type symbol))
    (org-glance-action-register name (list type)))

  (cl-defmethod org-glance-action-register ((name symbol) (type list))
    (let ((type (cl-pushnew type (gethash name org-glance-view-actions) :test #'seq-set-equal-p)))
      (puthash name type org-glance-view-actions))))

(defmacro org-glance-action-define (name args _ type &rest body)
  "Defun method NAME (ARGS) BODY.
Make it accessible for views of TYPE in `org-glance-view-actions'."
  (declare (debug
            ;; Same as defun but use cl-lambda-list.
            (&define [&or name ("setf" :name setf name)]
                     cl-lambda-list
                     symbolp
                     cl-declarations-or-string
                     [&optional ("interactive" interactive)]
                     def-body))
           (doc-string 6)
           (indent 4))

  (org-glance-action-register name type)

  (let* ((res (cl--transform-lambda (cons args body) name))
         (generic-fn (org-glance-generic-method-name name))
         (concrete-fn (org-glance-concrete-method-name name type))
         (action-private-method (intern (format "org-glance--%s--%s" name type)))
	 (form `(progn
                  (unless (fboundp (quote ,generic-fn))
                    (defun ,generic-fn (&optional args)
                      (interactive (list (org-glance-act-arguments)))
                      (let* ((action (quote ,name))
                             (headlines (org-glance-action-headlines action))
                             (choice (unwind-protect
                                          (org-completing-read (format "%s: " action) headlines)
                                       (message "Unwind protected")
                                       ;; (pp headlines)
                                       ))
                             (view (alist-get choice headlines nil nil #'string=))
                             (method-name (->> action
                                            (org-glance-view-action-resolve view)
                                            (org-glance-concrete-method-name action)))
                             (headline (replace-regexp-in-string "^\\[.*\\] " "" choice)))
                        (funcall method-name args view headline))))

                  (defun ,concrete-fn (&optional args view headline)
                    (interactive (list (org-glance-act-arguments)))
                    args
                    (org-glance
                     :default-choice headline
                     :scope (org-glance-view-scope view)
                     :prompt (org-glance-view-prompt view (quote ,name))
                     :db (org-glance-view-db view)
                     :filter (org-glance-view-filter view)
                     :action (function ,action-private-method)))

                  (defun ,action-private-method
                      ,@(cdr res)))))

    (if (car res)
        `(progn ,(car res) ,form)
      form)))

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

(defvar org-glance-transient--scope "agenda")

(defclass org-glance-transient-variable (transient-variable)
  ((default     :initarg :default     :initform nil)))

(cl-defmethod transient-init-value ((obj org-glance-transient-variable))
  "Override transient value initialization."
  (let ((variable (oref obj variable))
        (default (oref obj default)))
    (oset obj variable variable)
    (oset obj value (or (eval variable) default))))

(cl-defmethod transient-infix-set ((obj org-glance-transient-variable) value)
  "Override setter."
  (oset obj value value)
  (set (oref obj variable) value))

(cl-defmethod transient-format-description ((obj org-glance-transient-variable))
  "Override description format."
  (or (oref obj description)
      (oref obj variable)))

(cl-defmethod transient-format-value ((obj org-glance-transient-variable))
  "Override value format."
  (propertize (oref obj value) 'face 'transient-inactive-value))

(defun org-glance-read-scope ()
  (completing-read
   "Scope: "
   '(agenda
     agenda-with-archives
     file)))

(defclass org-glance-transient-variable:scope (org-glance-transient-variable)
  ())

(cl-defmethod transient-infix-read ((obj org-glance-transient-variable:scope))
  (oset obj value (org-glance-read-scope)))

(cl-defmethod transient-format-value ((obj org-glance-transient-variable:scope))
  (let* ((val (or (oref obj value) (oref obj default)))
         (val-pretty (propertize val 'face 'transient-argument)))
    (format "(%s)" val-pretty)))

(transient-define-infix org-glance-act.scope ()
  :class 'org-glance-transient-variable:scope
  :variable 'org-glance-transient--scope
  :reader 'org-glance-read-scope
  :default "false")

(transient-define-prefix org-glance-act ()
  "In Glance-View buffer, perform action on selected view"
  ;; ["Arguments"
  ;;  ("-s" "Scope" org-glance-act.scope)]
  ["Views"
   [("A" "Agenda" org-glance-view-agenda)]
   [("D" "Dashboard" org-glance-show-report)]
   [("E" "Export" org-glance-view-export)]
   [("R" "Reread" org-glance-view-reread)]
   [("V" "Visit" org-glance-view-visit)]]
  ["Headlines"
   ;; [("c" "Capture" org-glance-action-extract-property)]
   [("e" "Extract" org-glance-action-extract-property)]
   [("j" "Jump" org-glance-action-open)]
   [("m" "Materialize" org-glance-action-materialize)]
   [("v" "Visit" org-glance-action-visit)]])

(cl-defun org-glance
    (&key db
       default-choice
       (db-init nil)
       (filter #'(lambda (_) t))
       (scope '(agenda))
       (action #'org-glance--visit--all)
       (prompt "Glance: "))
  "Run completing read on org entries from SCOPE asking a PROMPT.
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
         (when-let (choice (or default-choice (org-glance-prompt-headlines prompt headlines)))
           (if-let (headline (org-glance-choose-headline choice headlines))
               (condition-case nil (funcall action headline)
                 (org-glance-db-outdated
                  (message "Database %s is outdated, actualizing..." db)
                  (redisplay)
                  (org-glance :scope scope
                              :prompt prompt
                              :filter filter
                              :action action
                              :db db
                              :db-init t
                              :default-choice choice)))
             (user-error "Headline not found"))))))

(provide 'org-glance)
;;; org-glance.el ends here
