;;; org-glance.el --- org-mode traversing. Fast and convenient.  ;; -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024 Dmitry Akatov

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

(require 'cl-generic)
(require 'cl-lib)
(require 'cl-macs)
(require 'json)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'f)

(defcustom org-glance-directory org-directory
  "Directory containing org-mode files and metadata."
  :group 'org-glance
  :type 'directory)

(defcustom org-glance-resource-directory (f-join org-directory "resources")
  "Directory containing various non-org resources like attachments, media, binary files etc"
  :group 'org-glance
  :type 'directory)

(defcustom org-glance-clone-on-repeat-p nil
  "Clone repeated headlines instead of repeating it."
  :group 'org-glance
  :type 'boolean)

(eval-and-compile  ;; TODO remove it
  (cl-defmacro org-glance:interactive-lambda (&rest forms)
    "Define interactive lambda function with FORMS in its body."
    (declare (indent 0) (debug t))
    `(lambda () (interactive) ,@forms)))

(defconst org-glance.log-level.OFF most-positive-fixnum
  "OFF is a special level that can be used to turn off logging.

This level is initialized to `most-positive-fixnum'.")

(defconst org-glance.log-level.SEVERE 1000
  "SEVERE is a message level indicating a serious failure.
In general SEVERE messages should describe events that are of
considerable importance and which will prevent normal program
execution.

They should be reasonably intelligible to end users and to system
administrators. This level is initialized to 1000.")

(defconst org-glance.log-level.WARNING 900
  "WARNING is a message level indicating a potential problem.
In general WARNING messages should describe events that will be
of interest to end users or system managers, or which indicate
potential problems.

This level is initialized to 900.")

(defconst org-glance.log-level.INFO 800
  "INFO is a message level for informational messages.
Typically INFO messages will be written to the console or its
equivalent.

So the INFO level should only be used for reasonably significant
messages that will make sense to end users and system
administrators.

This level is initialized to 800.")

(defconst org-glance.log-level.CONFIG 700
  "CONFIG is a message level for static configuration messages.
CONFIG messages are intended to provide a variety of static
configuration information, to assist in debugging problems that
may be associated with particular configurations.

For example, CONFIG message might include the CPU type, the
graphics depth, the GUI look-and-feel, etc. This level is
initialized to 700.")

(defconst org-glance.log-level.FINE 500
  "FINE is a message level providing tracing information.
All of FINE, FINER, and FINEST are intended for relatively
detailed tracing.

The exact meaning of the three levels will vary between
subsystems, but in general, FINEST should be used for the most
voluminous detailed output, FINER for somewhat less detailed
output, and FINE for the lowest volume (and most important)
messages.

In general the FINE level should be used for information that
will be broadly interesting to developers who do not have a
specialized interest in the specific subsystem.

FINE messages might include things like minor (recoverable)
failures. Issues indicating potential performance problems are
also worth logging as FINE. This level is initialized to 500.")

(defconst org-glance.log-level.FINER 400
  "FINER indicates a fairly detailed tracing message.
By default logging calls for entering, returning, or throwing an
exception are traced at this level. This level is initialized to
400.")

(defconst org-glance.log-level.FINEST 300
  "ALL indicates that all messages should be logged.
This level is initialized to `most-negative-fixnum'.")

(defconst org-glance.log-level.ALL most-negative-fixnum
  "FINEST indicates a highly detailed tracing message.
This level is initialized to 300.")

(defcustom org-glance:log-level org-glance.log-level.INFO "Logging level."
  :type `(choice (const :tag "OFF" ,org-glance.log-level.OFF)
                 (const :tag "SEVERE" ,org-glance.log-level.SEVERE)
                 (const :tag "WARNING" ,org-glance.log-level.WARNING)
                 (const :tag "INFO" ,org-glance.log-level.INFO)
                 (const :tag "CONFIG" ,org-glance.log-level.CONFIG)
                 (const :tag "FINE" ,org-glance.log-level.FINE)
                 (const :tag "FINER" ,org-glance.log-level.FINER)
                 (const :tag "FINEST" ,org-glance.log-level.FINEST)
                 (const :tag "ALL" ,org-glance.log-level.ALL))
  :group 'org-glance)

(cl-defun org-glance:log (log-level format-string &rest args)
  (when (<= org-glance:log-level log-level)
    (apply #'message format-string args)))

(cl-defun org-glance:log-warning (format-string &rest args)
  "Log warning if `org-glance:log-level' allows."
  (apply #'org-glance:log org-glance.log-level.WARNING format-string args))

(cl-defun org-glance:log-info (format-string &rest args)
  "Log info if `org-glance:log-level' allows."
  (apply #'org-glance:log org-glance.log-level.INFO format-string args))

(cl-defun org-glance:log-debug (format-string &rest args)
  "Log debug message if `org-glance:log-level' allows."
  (apply #'org-glance:log org-glance.log-level.FINEST format-string args))

(cl-defmacro org-glance-with-debug-msg (msg &rest forms)
  (declare (indent 1))
  `(progn
     (org-glance:log-debug ,msg)
     ,@forms
     (org-glance:log-debug (concat ,msg " done"))))

(cl-defmacro org-glance:define-exception (name message &optional (parent 'user-error))
  `(progn
     (define-error (quote ,name) ,message (quote ,parent))
     (cl-defun ,name (format &rest args)
       (signal (quote ,name) (list (apply #'format format args))))))

(org-glance:define-exception org-glance-exception:SOURCE-CORRUPTED "Headline source corrupted, please reread")
(org-glance:define-exception org-glance-exception:PROPERTIES-CORRUPTED "Headline metadata corrupted, please reread")
(org-glance:define-exception org-glance-exception:METASTORE-OUTDATED "Metastore is outdated, please rebuild")
(org-glance:define-exception org-glance-exception:HEADLINE-NOT-FOUND "Headline not found")
(org-glance:define-exception org-glance-exception:CLASS-NOT-FOUND "Class not found")

(org-glance:require
  src.data.view

  src.utils.crypt                       ; encryption utils
  src.utils.helpers                     ; unsorted, deprecated
  src.utils.org-tss-mode

;;; Core APIs
  ;; Description of high-level org-glance entities: Headline, View,
  ;; Scope and Metastore.

;;; Headline API
  ;; Org-glance headline is an org-element headline enriched by some
  ;; shortcuts and helper methods.

  src.core.headline                     ; good
  src.core.relations
  src.core.metastore                    ; ok
  src.core.scope                        ; ? deprecated

  src.modes.overview-mode               ; good one, improve
  src.modes.material-mode
  src.modes.agenda-mode

  src.view.links

  src.transient.headlines)

;; (org-glance:import org-glance:format :from src.utils.helpers)

(declare-function org-glance-def-view (org-glance-module-filename src.data.view))
(declare-function org-glance-headline:materialize (org-glance-module-filename src.core.headline))
(declare-function org-glance-headline:title (org-glance-module-filename src.core.headline))

(declare-function org-glance:format (org-glance-module-filename src.utils.helpers))
(declare-function org-glance-metastore:choose-headline (org-glance-module-filename src.core.metastore))
(declare-function org-glance-headlines (org-glance-module-filename src.core.metastore))
(declare-function org-glance:choose-class (org-glance-module-filename src.data.view))
(declare-function org-glance-headline:at-point (org-glance-module-filename src.core.headline))
(declare-function org-glance-scope--choose-headline (org-glance-module-filename src.core.scope))

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(cl-defun org-glance-class-remove (class)
  (remhash class -org-glance-views))

(cl-defun org-glance-class-create (class)
  (org-glance-def-view :id class)

  (unless (f-exists? (org-glance-view:metastore (org-glance-view:get class)))
    (org-glance-metastore:create (org-glance-view:metastore (org-glance-view:get class))))

  (unless (f-exists? (org-glance-overview:location class))
    (org-glance-overview:create class)))

(cl-defun org-glance-materialized-headline:preserve-history-before-auto-repeat (&rest args)
  (when (and
         org-glance-clone-on-repeat-p
         (or org-glance-material-mode org-glance-overview-mode)
         (member (org-get-todo-state) org-done-keywords)
         (org-glance-headline:repeated-p))
    (let ((contents (org-glance-headline-contents)))
      (run-with-idle-timer 1 nil
                           #'(lambda () (save-window-excursion
                                     (with-temp-buffer
                                       (insert contents)
                                       (goto-char (point-min))

                                       (org-tss-reset-buffer-timestamps-except-earliest)

                                       (cl-loop
                                        for class in (org-glance-headline:classes)
                                        do (let ((headline (org-glance-capture-headline-at-point class)))
                                             (org-glance-overview:register-headline-in-archive headline class))))))))))

(cl-defun org-glance-materialized-headline:cleanup-after-auto-repeat (&rest args)
  "Do only if headline has been cloned before auto repeat.
Cleanup new headline considering auto-repeat ARGS.

- Remove all data but PINNED of cloned headline."
  (when (and (or org-glance-material-mode org-glance-overview-mode)
             org-glance-clone-on-repeat-p
             (org-glance-headline:repeated-p))
    (let ((contents (org-glance:with-headline-at-point
                     (let ((header (s-trim (buffer-substring-no-properties (point) (save-excursion (org-end-of-meta-data) (point)))))
                           (pinned (save-excursion
                                     (cl-loop
                                      while (search-forward "#+begin_pin" nil t)
                                      collect (save-excursion
                                                (beginning-of-line)
                                                (buffer-substring-no-properties (point) (save-excursion
                                                                                          (search-forward "#+end_pin" nil t)
                                                                                          (point))))))))
                       (s-join "\n\n" (append (list header) pinned))))))
      (delete-region (point-min) (point-max))
      (insert contents)
      (org-delete-property "LAST_REPEAT"))))

(cl-defmacro org-glance-choose-and-apply (&key filter action)
  "If HEADLINE specified, apply ACTION on it.

If HEADLINE is not specified, ask user to choose HEADLINE from
existing headlines filtered by FILTER.

If user chooses unexisting headline, capture it and apply ACTION
after capture process has been finished."
  `(condition-case default
       (cond (,filter (funcall ,action (org-glance-metastore:choose-headline :filter ,filter)))
             (t (funcall ,action (org-glance-metastore:choose-headline))))
     (org-glance-exception:HEADLINE-NOT-FOUND
      (let ((<buffer> (current-buffer))
            (<point> (point)))
        (org-glance-capture
         :default (cadr default)
         :class (org-glance:choose-class "Unknown headline. Please, specify it's class to capture: ")
         :callback (lambda ()
                     (let ((<hl> (org-glance-overview:original-headline)))
                       (switch-to-buffer <buffer>)
                       (goto-char <point>)
                       (funcall ,action <hl>))))))))

(cl-defun org-glance-init ()
  "Update all changed entities from `org-glance-directory'."

  (unless (f-exists? org-glance-directory)
    (mkdir org-glance-directory))

  (org-glance-overview-init)

  (add-hook 'org-glance-material-mode-hook #'org-tss-mode)
  (advice-add 'org-auto-repeat-maybe :before #'org-glance-materialized-headline:preserve-history-before-auto-repeat (list :depth -90))
  (advice-add 'org-auto-repeat-maybe :after #'org-glance-materialized-headline:cleanup-after-auto-repeat)
  (advice-add 'org-glance-headline:materialize :around #'org-glance-enable-encrypted-headlines)

  (cl-loop
   for directory in (org-glance:list-directories org-glance-directory)
   do (let ((class (intern directory)))
        (unless (gethash class -org-glance-views nil)
          (org-glance-class-create class))))

  (cl-loop
   for class being the hash-keys of -org-glance-views
   do (let ((class-name (s-downcase (format "%s" class))))
        (unless (f-exists? (f-join org-glance-directory class-name))
          (org-glance-class-remove class))))

  (setq org-agenda-files (mapcar 'org-glance-overview:location (org-glance-views:list))))

(cl-defun org-glance:@ ()
  "Choose headline to refer. Insert link at point."
  (interactive)
  (org-glance-init)
  (condition-case nil
      (cond
       ;; active region?
       ((and (not (org-in-src-block-p))
             (region-active-p))
        (let ((<buffer> (current-buffer))
              (<region-beginning> (region-beginning))
              (<region-end> (region-end))
              (<point> (point)))
          (org-glance-capture
           :default (buffer-substring-no-properties <region-beginning> <region-end>)
           :class (org-glance:choose-class (format "Specify class for \"%s\": " (buffer-substring-no-properties <region-beginning> <region-end>)))
           :finalize t
           :callback (lambda ()
                       (let ((<hl> (org-glance-overview:original-headline)))
                         (switch-to-buffer <buffer>)
                         (goto-char <region-beginning>)
                         (delete-region <region-beginning> <region-end>)
                         (insert (org-glance:with-headline-narrowed <hl>
                                   (org-glance-headline-reference))))))))

       ;; mention
       ((and (not (org-in-src-block-p))
             (or (looking-back "^" 1) (looking-back "[[:space:]]" 1)))
        (org-glance-choose-and-apply
         :action (lambda (headline)
                   (insert
                    (org-glance:with-headline-narrowed headline
                      (org-glance-headline-reference))))))

       ;; simple @
       (t (keyboard-quit)))
    (quit (self-insert-command 1 64))))

(cl-defun org-glance:materialize (&optional headline)
  "Materialize HEADLINE in new buffer."
  (interactive)
  (let ((action (lambda (headline)
                  (let ((buffer (org-glance-materialized-headline-buffer headline)))
                    (switch-to-buffer
                     (if (buffer-live-p buffer)
                         buffer
                       (org-glance-headline:materialize headline)))))))
    (if headline
        (funcall action headline)
      (org-glance-choose-and-apply
       :filter #'org-glance-headline:active?
       :action action))))

(cl-defun org-glance:open (&optional headline)
  "Run `org-open-at-point' on any `org-link' inside HEADLINE.
If there is only one link, open it.
If there is more than one link, prompt user to choose which one to open.
If headline doesn't contain links, role `can-be-opened' should be revoked."
  (interactive)
  (let ((action (lambda (headline)
                  (org-glance:with-headline-materialized headline
                    (cl-loop
                     for (link title pos) in (org-glance-parse-links)
                     unless (s-starts-with-p "[[org-glance" link)
                     collect (list title pos)
                     into links
                     finally
                     do (goto-char (cond
                                    ((> (length links) 1) (cadr (assoc (completing-read "Open link: " links nil t) links #'string=)))
                                    ((= (length links) 1) (cadar links))
                                    (t (user-error "Unable to find links in headline"))))
                     (org-open-at-point))))))
    (if headline
        (funcall action headline)
      (org-glance-choose-and-apply
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
                  (let ((pairs (org-glance:with-headline-materialized headline
                                 (org-glance-buffer-key-value-pairs))))
                    (condition-case nil
                        (while t
                          (kill-new (alist-get (completing-read "Extract property: " pairs nil t) pairs nil nil #'string=)))
                      (quit
                       (setq kill-ring nil)
                       (org-glance:log-info "Kill ring has been cleared")))))))
    (if headline
        (funcall action headline)
      (org-glance-choose-and-apply
       :filter (lambda (headline)
                 (pp headline)
                 (and
                  (org-glance-headline:active? headline)
                  (or (org-glance-headline:contains-property? headline)
                      (org-glance-headline:encrypted-p headline))))
       :action action))))

;; (cl-defun org-glance:prototype ()
;;   (interactive)
;;   "Capture headline based on chosen prototype."
;;   (org-glance-choose-and-apply
;;    :action (lambda (headline)
;;              (org-glance-capture
;;               :class (org-element-property :class headline)
;;               :template (org-glance-headline-contents headline)))))

(cl-defun org-glance-capture
    (&key
     (class (org-glance:choose-class))
     (file (make-temp-file "org-glance-" nil ".org"))
     (default
      (cond
       ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
       (t "")))
     (callback nil)
     (finalize nil)
     (template (org-glance-capture-template class :default default)))
  (interactive)

  (let ((class (if (symbolp class) class (intern class))))
    (find-file file)
    (setq-local org-glance-capture:id (org-glance-generate-id class)
                org-glance-capture:class class
                org-glance-capture:default default)
    (add-hook 'org-capture-prepare-finalize-hook 'org-glance-capture:prepare-finalize-hook 0 t)
    (add-hook 'org-capture-after-finalize-hook 'org-glance-capture:after-finalize-hook 0 t)
    (when callback (add-hook 'org-capture-after-finalize-hook callback 1 t))
    (let ((org-capture-templates (list (list "_" "Thing" 'entry (list 'file file) template))))
      (org-capture nil "_")
      (when finalize
        (org-capture-finalize)))))

(cl-defun org-glance-headline-remove ()
  (interactive)
  (org-glance-choose-and-apply
   :action (lambda (headline)
             (org-glance:with-headline-materialized headline
               (org-set-tags '())))))

(cl-defun org-glance:insert-pin-block ()
  (interactive)
  (insert "#+begin_pin" "\n\n" "#+end_pin")
  (forward-line -1))

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
                              (completing-read prompt (mapcar #'org-glance-headline:title headlines) nil t)))
          (if-let (headline (org-glance-scope--choose-headline choice headlines))
              (condition-case nil
                  (funcall action headline)
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
