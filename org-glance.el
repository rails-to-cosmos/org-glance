;;; org-glance.el --- org-mode traversing. Fast and convenient.

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

(require 'org)
(require 'org-glance-module)

(defcustom org-glance-directory org-directory
  "Directory with Org files."
  :group 'org-glance
  :type 'directory)

(defcustom org-glance-clone-on-repeat-p nil
  "Clone repeated headlines instead of repeating it."
  :group 'org-glance
  :type 'boolean)

(cl-defstruct (org-glance-view (:constructor org-glance-view:create))
  "This structure contains metadata about categorized `org-mode' headlines."
  (id
   nil
   :read-only t
   :documentation "ID slot is a primary key that uniqly identifies `org-glance-view'."
   :type 'symbol)
  (type
   nil
   :read-only nil
   :documentation "List of actions allowed to use on headlines of this view."
   :type 'list)
  (scope
   nil
   :read-only nil
   :documentation "List of files/directories where org-glance should search for headlines for this view."
   :type 'list))

(defvar org-glance:classes (make-hash-table)
  "Hash table (id->view) that lists all registered classes of things.")

(defun org-glance:get-class (class) (gethash class org-glance:classes))

(eval-and-compile
  (cl-defmacro org-glance:interactive-lambda (&rest forms)
    "Define interactive lambda function with FORMS in its body."
    (declare (indent 0) (debug t))
    `(lambda ()
       (interactive)
       ,@forms)))

(eval-when-compile
  (org-glance:require
    cl
    lib.modes.material-mode))

(org-glance:require
  cl-generic
  cl-lib
  cl-macs
  json
  seq
  subr-x

  lib.core.logging
  lib.core.exceptions
  lib.core.posit

  lib.utils.encryption                  ; encryption utils
  lib.utils.helpers                     ; unsorted, deprecated
  lib.utils.org                         ; org-mode shortcuts

;;; Core APIs
  ;; Description of high-level org-glance entities: Headline, View,
  ;; Scope and Metastore.

;;; Headline API
  ;; Org-glance headline is an org-element headline enriched by some
  ;; shortcuts and helper methods.

  lib.core.headline                     ; good
  lib.core.metastore                    ; ok
  lib.core.scope                        ; ? deprecated
  lib.core.view                         ; migrate to overview

  lib.modes.overview-mode               ; good one, improve
  lib.modes.material-mode
  lib.modes.agenda-mode

  lib.view.links

  lib.transient.headlines)

;; (org-glance:import org-glance:format :from lib.utils.helpers)

(declare-function org-completing-read "org-macs.el")

(declare-function org-glance-def-view (org-glance-module-filename lib.core.view))
(declare-function org-glance-headline:materialize (org-glance-module-filename lib.core.headline))
(declare-function org-glance-headline:title (org-glance-module-filename lib.core.headline))

(declare-function org-glance:format (org-glance-module-filename lib.utils.helpers))
(declare-function org-glance-metastore:choose-headline (org-glance-module-filename lib.core.metastore))
(declare-function org-glance-headlines (org-glance-module-filename lib.core.metastore))
(declare-function org-glance-overview:capture (org-glance-module-filename lib.modes.overview-mode))
(declare-function org-glance:choose-class (org-glance-module-filename lib.core.view))
(declare-function org-glance-headline:format (org-glance-module-filename lib.core.headline))
(declare-function org-glance-headline:at-point (org-glance-module-filename lib.core.headline))
;; (declare-function org-glance-headline:add-biconnected-relation (org-glance-module-filename lib.core.headline))
(declare-function org-glance-scope--choose-headline (org-glance-module-filename lib.core.scope))

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(cl-defun org-glance:remove-class (class)
  (org-glance:log-debug "Remove class \"%s\"" class)
  (remhash class org-glance:classes))

(cl-defun org-glance:create-class (class)
  (org-glance:log-debug "Create class \"%s\"" class)

  (org-glance-def-view :id class)

  (org-glance:log-debug "Metastore exists?")
  (unless (f-exists? (org-glance-view:metastore-location (org-glance:get-class class)))
    (org-glance:log-debug "Create metastore")
    (org-glance-metastore:create (org-glance-view:metastore-location (org-glance:get-class class))))

  (org-glance:log-debug "Overview exists?")
  (unless (f-exists? (org-glance-overview:location class))
    (org-glance:log-debug "Create overview")
    (org-glance-overview:create class)))

(cl-defun org-glance:init ()
  "Update all changed entities from `org-glance-directory'."
  (unless (f-exists? org-glance-directory)
    (mkdir org-glance-directory))

  (advice-add 'org-auto-repeat-maybe :before
              (lambda (&rest args) (when (and
                                     (or org-glance-material-mode org-glance-overview-mode)
                                     org-glance-clone-on-repeat-p
                                     (member (org-get-todo-state) org-done-keywords)
                                     (org-glance-headline:repeated-p))
                                (org-glance:clone-headline))))

  (cl-loop
     for directory in (org-glance:list-directories org-glance-directory)
     do (let ((class (intern directory)))
          (unless (gethash class org-glance:classes nil)
            (org-glance:create-class class))))

  (cl-loop
     for class being the hash-keys of org-glance:classes
     do (let ((class-name (s-downcase (format "%s" class))))
          (unless (f-exists? (f-join org-glance-directory class-name))
            (org-glance:remove-class class))))

  ;; (cl-loop
  ;;    for class in '(posit thing class role ascertains)
  ;;    unless (gethash class org-glance:classes nil)
  ;;    do (org-glance:create-class class))

  (setq org-agenda-files (mapcar 'org-glance-overview:location (org-glance-view:ids))))

(cl-defun org-glance:@ ()
  "Choose headline to refer. Insert link at point."
  (interactive)
  (org-glance:init)
  (if (and (not (org-in-src-block-p))
           (or (looking-back "^" 1)
               (looking-back "[[:space:]]" 1)))
      (condition-case nil
          (org-glance:choose-headline-apply
           :action (lambda (headline)
                     (let ((source (org-glance-headline:at-point))
                           (target-title (org-glance-headline:format headline))
                           (ts (org-glance-now)))
                       (org-glance-headline:add-log-note "- Mentioned %s on %s" target-title ts)
                       (insert target-title)
                       (when source
                         (let ((source-title (org-glance-headline:format source)))
                           (save-window-excursion
                             (save-excursion
                               (org-glance-headline:with-materialized-headline headline
                                 (org-glance-headline:add-log-note
                                  (format "- Mentioned by %s on %s" source-title ts))))))))))
        (quit (insert "@")))
    (insert "@")))

(cl-defmacro org-glance:apply-on (headline &key action)
  "If HEADLINE specified, apply ACTION on it.

If HEADLINE is not specified, ask user to choose HEADLINE from
existing headlines filtered by FILTER.

If user chooses unexisting headline, capture it and apply ACTION
after capture process has been finished."
  (declare (indent 1))
  `(funcall ,action ,headline))

(cl-defmacro org-glance:choose-headline-apply (&key filter action)
  "If HEADLINE specified, apply ACTION on it.

If HEADLINE is not specified, ask user to choose HEADLINE from
existing headlines filtered by FILTER.

If user chooses unexisting headline, capture it and apply ACTION
after capture process has been finished."
  `(condition-case default
       (cond (,filter (funcall ,action (org-glance-metastore:choose-headline :filter ,filter)))
             (t (funcall ,action (org-glance-metastore:choose-headline))))
     (org-glance-exception:HEADLINE-NOT-FOUND (lexical-let ((<buffer> (current-buffer))
                                                            (<point> (point)))
                                                (org-glance-overview:capture
                                                 :default (cadr default)
                                                 :class (org-glance:choose-class "Unknown thing. Please, specify it's class to capture: ")
                                                 :callback (lambda ()
                                                             (let ((<hl> (org-glance-overview:original-headline)))
                                                               (switch-to-buffer <buffer>)
                                                               (goto-char <point>)
                                                               (funcall ,action <hl>))))))))

(cl-defun org-glance:materialize (&optional headline)
  "Materialize HEADLINE in new buffer."
  (interactive)
  (let ((action (lambda (headline)
                  (let ((buffer (org-glance-headline:materialized-buffer headline)))
                    (if (buffer-live-p buffer)
                        (switch-to-buffer buffer)
                      (org-glance-headline:materialize headline))))))
    (if headline
        (org-glance:apply-on headline
          :action action)
      (org-glance:choose-headline-apply
       :filter #'org-glance-headline:active?
       :action action))))

(cl-defun org-glance:open (&optional headline)
  "Run `org-open-at-point' on any `org-link' inside HEADLINE.
If there is only one link, open it.
If there is more than one link, prompt user to choose which one to open.
If headline doesn't contain links, role `can-be-opened' should be revoked."
  (interactive)
  (let ((action (lambda (headline)
                  (org-glance-headline:with-materialized-headline headline
                    ;; (org-end-of-meta-data t)
                    ;; (narrow-to-region (point) (point-max))
                    (let* ((links (--filter (not (s-contains? "org-glance" (car it))) (org-glance:buffer-links)))
                           (position (cond
                                       ((> (length links) 1) (cdr (assoc (org-completing-read "Open link: " links) links)))
                                       ((= (length links) 1) (cdar links))
                                       (t (user-error "Unable to find links in headline")))))
                      (goto-char position)
                      (org-open-at-point))))))
    (if headline
        (org-glance:apply-on headline
          :action action)
      (org-glance:choose-headline-apply
       :filter (lambda (headline)
                 (and
                  (org-glance-headline:active? headline)
                  (org-glance-headline:contains-link? headline)))
       :action action))))

(cl-defun org-glance:extract (&optional headline)
  (interactive)
  "Materialize HEADLINE and retrieve key-value pairs from its contents.
If headline doesn't contain key-value pairs, role `can-be-extracted' should be revoked."
  (let ((action (lambda (headline)
                  (let ((pairs (org-glance-headline:with-materialized-headline headline
                                 (org-glance:get-buffer-key-value-pairs))))
                    (while t
                      (kill-new (alist-get (org-completing-read "Extract property: " pairs) pairs nil nil #'string=)))))))
    (if headline
        (org-glance:apply-on headline
          :action action)
      (org-glance:choose-headline-apply
       :filter (lambda (headline)
                 (and
                  (org-glance-headline:active? headline)
                  (or (org-glance-headline:contains-property? headline)
                      (org-glance-headline:encrypted? headline))))
       :action action))))

(cl-defun org-glance:clone-headline ()
  (when org-glance-material-mode
    (lexical-let ((buffer (current-buffer)))
      (run-with-idle-timer 1 nil #'(lambda () (with-current-buffer buffer
                                           (condition-case nil
                                               (org-glance-materialized-headline:sync)
                                             (org-glance-exception:HEADLINE-NOT-MODIFIED nil)))))))

  (lexical-let ((contents (save-excursion
                            (org-back-to-heading t)
                            (let ((header (buffer-substring-no-properties (point) (save-excursion (org-end-of-meta-data) (point))))
                                  (pinned (save-excursion
                                            (cl-loop
                                               while (search-forward "#+begin_pin" nil t)
                                               collect (save-excursion
                                                         (beginning-of-line)
                                                         (buffer-substring-no-properties (point) (save-excursion
                                                                                                   (search-forward "#+end_pin" nil t)
                                                                                                   (point))))))))
                              (s-join "\n\n" (append (list header) pinned))))))
    (run-with-idle-timer 1 nil #'(lambda () (with-temp-buffer
                                         (insert contents)
                                         (goto-char (point-min))
                                         (cl-loop
                                            for class in (org-glance-headline:classes)
                                            do (let ((captured-headline (org-glance:capture-headline-at-point class :remove-original nil)))
                                                 (org-glance-overview:register-headline-in-metastore captured-headline class)
                                                 (org-glance-overview:register-headline-in-overview captured-headline class))))))))

(cl-defun org-glance
    (&key db
       default-choice
       (db-init nil)
       (filter #'(lambda (_) t))
       (scope '(agenda))
       (action #'org-glance-headline:visit)
       (prompt "Glance: "))
  "Deprecated main method, refactoring needed."
  (let ((headlines (org-glance-headlines :db db
                                         :db-init db-init
                                         :scope scope
                                         :filter filter)))
    (unwind-protect
         (when-let (choice (or default-choice
                               (org-completing-read prompt (mapcar #'org-glance-headline:title headlines))))
           (if-let (headline (org-glance-scope--choose-headline choice headlines))
               (condition-case nil (funcall action headline)
                 (org-glance-exception:DB-OUTDATED
                  (org-glance:log-info "Metastore %s is outdated, actualizing..." db)
                  (redisplay)
                  (org-glance :scope scope
                              :filter filter
                              :action action
                              :db db
                              :db-init t
                              :default-choice choice
                              :prompt prompt)))
             (user-error "Headline not found"))))))

(provide 'org-glance)
;;; org-glance.el ends here
