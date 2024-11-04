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

(require 'aes)
(require 'cl-generic)
(require 'cl-lib)
(require 'cl-macs)
(require 'dash)
(require 'f)
(require 'json)
(require 'org)
(require 'org-element)
(require 's)
(require 'seq)
(require 'subr-x)

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

(cl-defmacro org-glance-with-debug-msg (msg &rest forms)
  (declare (indent 1))
  `(progn
     (message ,msg)
     ,@forms
     (message (concat ,msg " done"))))

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

(defun org-glance:encrypt-region (beg end &optional password)
  "Encrypt region from BEG to END using PASSWORD."
  (interactive "r")
  (let* ((original-text (buffer-substring-no-properties beg end))
         (encrypted-text (aes-encrypt-buffer-or-string original-text password)))
    (save-excursion
      (kill-region beg end)
      (goto-char beg)
      (insert encrypted-text))))

(defun org-glance:decrypt-region (beg end &optional password)
  "Decrypt region from BEG to END using PASSWORD."
  (interactive "r")
  (if-let (decrypted-text (let ((encrypted (buffer-substring-no-properties beg end)))
                            (if (with-temp-buffer
                                  (insert encrypted)
                                  (aes-is-encrypted))
                                (aes-decrypt-buffer-or-string encrypted password)
                              (user-error "Headline is not encrypted"))))
      (save-excursion
        (kill-region beg end)
        (goto-char beg)
        (insert decrypted-text))
    (user-error "Wrong password")))

(cl-defun org-glance-now ()
  (format-time-string (org-time-stamp-format 'long 'inactive) (current-time)))

(cl-defun org-glance-ensure-at-heading ()
  (unless (org-at-heading-p)
    (org-back-to-heading-or-point-min)))

(cl-defun org-glance-generate-id (&optional (class (org-glance-view:completing-read)))
  (substring-no-properties
   (format "%s-%s-%s"
           class
           (s-join "-" (mapcar #'number-to-string (current-time)))
           (secure-hash 'md5 (buffer-string)))))

(cl-defun org-glance-class-location (&optional (view-id (org-glance-view:completing-read)))
  "Path to directory where VIEW-ID resources and metadata are stored."
  (abbreviate-file-name
   (f-join org-glance-directory
           (s-downcase (format "%s" view-id))
           "resources")))

(cl-defun org-glance-generate-directory (&optional (class (org-glance-view:completing-read)))
  (save-excursion
    (org-glance-ensure-at-heading)
    (save-restriction
      (org-narrow-to-subtree)
      (org-glance-headline:generate-directory
       (org-glance-class-location class)
       (org-element-property :raw-value (org-element-at-point))))))

(defconst org-glance:key-value-pair-re "^-?\\([[:word:],[:blank:],_,/,-]+\\)\\:[[:blank:]]*\\(.*\\)$")

(cl-defun org-glance-buffer-key-value-pairs ()
  "Extract key-value pairs from buffer.
Run completing read on keys and copy selected values to kill ring.

Assume string is a key-value pair if it matches `org-glance:key-value-pair-re'."
  (save-excursion
    (goto-char (point-min))
    (cl-loop
       while (condition-case nil
                 (re-search-forward org-glance:key-value-pair-re)
               (search-failed nil))
       collect (s-trim (substring-no-properties (match-string 1))) into keys
       collect (s-trim (substring-no-properties (match-string 2))) into vals
       finally (return (-zip keys vals)))))

(cl-defun org-glance:list-directories (base-dir)
  (--filter
   (f-directory? (f-join base-dir it))
   (directory-files base-dir nil "^[[:word:]]+")))

(cl-defmacro org-glance:format (fmt)
  "Like `s-format' but with format fields in it.
FMT is a string to be expanded against the current lexical
environment. It is like what is used in `s-lex-format', but has
an expanded syntax to allow format-strings. For example:
${user-full-name 20s} will be expanded to the current value of
the variable `user-full-name' in a field 20 characters wide.
  (let ((f (sqrt 5)))  (org-glance:format \"${f 1.2f}\"))
  will render as: 2.24
This function is inspired by the f-strings in Python 3.6, which I
enjoy using a lot.
"
  (let* ((matches (s-match-strings-all "${\\(?3:\\(?1:[^} ]+\\) *\\(?2:[^}]*\\)\\)}" (eval fmt)))
         (agetter (cl-loop
                     for (m0 m1 m2 m3) in matches
                     collect `(cons ,m3  (format (format "%%%s" (if (string= ,m2 "")
                                                                    (if s-lex-value-as-lisp "S" "s")
                                                                  ,m2))
                                                 (symbol-value (intern ,m1))))))
         (result `(s-format ,fmt 'aget (list ,@agetter))))
    `(s-join "\n"
             (cl-loop
                with stripMargin = (-partial 'replace-regexp-in-string "^\\W*|" "")
                for line in (s-split "\n" ,result)
                collect (funcall stripMargin line)))))

(defun -org-glance:make-file-directory (file)
  (let ((dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  file)

(defun -org-glance:collect-tags ()
  (cl-loop for tag in (org--get-local-tags)
     collect (downcase tag)))

(defun -org-glance:list-file-archives (filename)
  "Return list of org-mode files for FILENAME."
  (let* ((dir (file-name-directory filename))
         (base-filename (-some->> filename
                          file-name-nondirectory
                          file-name-sans-extension)))
    (directory-files-recursively dir (format "%s.org\\.*" base-filename))))

(defun -org-glance:file-with-archives ()
  (append (list (buffer-file-name))
          (-org-glance:list-file-archives (buffer-file-name))))

(defun -org-glance:agenda-with-archives ()
  (cl-loop for filename in (org-agenda-files)
     append (list filename)
     append (-org-glance:list-file-archives filename)))

(cl-defun org-glance-join (separator strings)
  (let ((joined-strings (s-join separator strings)))
    (if (string-empty-p joined-strings)
        ""
      (concat separator joined-strings))))

(cl-defun org-glance-join-but-null (separator strings)
  (declare (indent 1))
  (org-glance-join separator (cl-remove-if #'null strings)))

(defvar-local -org-glance-ts:local-timestamps '())

(define-minor-mode org-glance-ts-mode "Handle multiple repeatable timestamps."
  (cond (org-glance-ts-mode (advice-add 'org-auto-repeat-maybe :before #'org-glance-ts-capture)
                      (advice-add 'org-auto-repeat-maybe :after #'org-glance-ts-restore))
        (t (advice-remove 'org-auto-repeat-maybe #'org-glance-ts-capture)
           (advice-remove 'org-auto-repeat-maybe #'org-glance-ts-restore))))

(cl-defun org-glance-ts-sort-timestamps (tss)
  "Sort TSS."
  (sort tss #'(lambda (lhs rhs) (time-less-p
                            (org-time-string-to-time (org-element-property :raw-value lhs))
                            (org-time-string-to-time (org-element-property :raw-value rhs))))))

(cl-defun org-glance-ts-buffer-timestamps (&optional (org-data (org-element-parse-buffer)))
  (org-element-map org-data '(timestamp) #'identity))

(cl-defun org-glance-ts-buffer-schedules (&optional (org-data (org-element-parse-buffer)))
  (org-element-map org-data '(headline) #'(lambda (headline) (org-element-property :scheduled headline))))

(cl-defun org-glance-ts-buffer-deadlines (&optional (org-data (org-element-parse-buffer)))
  (org-element-map org-data '(headline) #'(lambda (headline) (org-element-property :deadline headline))))

(cl-defun org-glance-ts-headline-timestamps (&rest includes)
  (save-restriction
    (org-narrow-to-subtree)
    (cl-loop for timestamp in (let ((org-data (org-element-parse-buffer)))
                                (append (org-glance-ts-buffer-timestamps org-data)
                                        (when (member 'include-schedules includes)
                                          (org-glance-ts-buffer-schedules org-data))
                                        (when (member 'include-deadlines includes)
                                          (org-glance-ts-buffer-deadlines org-data))))
       collect timestamp)))

(cl-defun org-glance-ts-filter-active (tss)
  (--filter (member (org-element-property :type it) '(active active-range)) tss))

(cl-defun org-glance-ts-filter-repeated (tss)
  (--filter (and (member (org-element-property :type it) '(active active-range))
                 (> (or (org-element-property :repeater-value it) 0) 0))
            tss))

(cl-defun org-glance-ts-capture (&rest args)
  (setq-local -org-glance-ts:local-timestamps (-some->> (org-glance-ts-headline-timestamps)
                                          (org-glance-ts-filter-active)
                                          (org-glance-ts-filter-repeated)
                                          (org-glance-ts-sort-timestamps))))

(cl-defun org-glance-ts-restore (&rest args)
  (let ((standard-output 'ignore)
        (tss* (-some->> (org-glance-ts-headline-timestamps)
                (org-glance-ts-filter-active)
                (org-glance-ts-filter-repeated)
                (org-glance-ts-sort-timestamps))))
    (cl-loop
       for tsi from 1 below (length tss*)
       for ts = (nth tsi -org-glance-ts:local-timestamps)
       for ts* = (nth tsi tss*)
       do (save-excursion
            (goto-char (org-element-property :begin ts*))
            (delete-region (org-element-property :begin ts*)
                           (org-element-property :end ts*))
            (insert (org-element-property :raw-value ts))))))

(cl-defun org-glance-ts-reset-buffer-timestamps-except-earliest ()
  "Reset active timestamps in buffer except earliest."
  (let ((standard-output 'ignore)
        (tss (-some->> (org-glance-ts-headline-timestamps 'include-schedules 'include-deadlines)
               (org-glance-ts-filter-active)
               (org-glance-ts-filter-repeated)
               (org-glance-ts-sort-timestamps))))
    (cl-loop
       for ts in tss
       for index from 0
       do
         (goto-char (org-element-property :begin ts))
         (if (> index 0)
             (org-toggle-timestamp-type)
           ;; reset repeater
           (save-excursion
             (let ((bound1 (org-element-property :begin ts))
                   (bound0 (org-element-property :end ts)))
               (when (and (re-search-forward
                           (concat "\\(" org-scheduled-time-regexp "\\)\\|\\("
                                   org-deadline-time-regexp "\\)\\|\\("
                                   org-ts-regexp "\\)")
                           bound0 t)
                          (re-search-backward "[ \t]+\\(?:[.+]\\)?\\+\\([0-9]+\\)[hdwmy]"
                                              bound1 t))
                 (replace-match "0" t nil nil 1))))))))

(cl-defun org-glance-ts-headline-repeated-p ()
  "Is headline at point repeated?"
  (save-excursion
    (unless (org-at-heading-p)
      (org-back-to-heading-or-point-min))
    (when (append
           (-some->> (org-glance-ts-headline-timestamps 'include-schedules 'include-deadlines)
             (org-glance-ts-filter-active)
             (org-glance-ts-filter-repeated)
             (org-glance-ts-sort-timestamps)))
      t)))

(org-glance:require
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
  (remhash class org-glance-views))

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

                                       (org-glance-ts-reset-buffer-timestamps-except-earliest)

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

  (add-hook 'org-glance-material-mode-hook #'org-glance-ts-mode)
  (advice-add 'org-auto-repeat-maybe :before #'org-glance-materialized-headline:preserve-history-before-auto-repeat (list :depth -90))
  (advice-add 'org-auto-repeat-maybe :after #'org-glance-materialized-headline:cleanup-after-auto-repeat)
  (advice-add 'org-glance-headline:materialize :around #'org-glance-enable-encrypted-headlines)

  (cl-loop
   for directory in (org-glance:list-directories org-glance-directory)
   do (let ((class (intern directory)))
        (unless (gethash class org-glance-views nil)
          (org-glance-class-create class))))

  (cl-loop
   for class being the hash-keys of org-glance-views
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
                       (message "Kill ring has been cleared")))))))
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

(cl-defun org-glance (&key db
                           default
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
        (when-let (choice (or default
                              (completing-read prompt (mapcar #'org-glance-headline:title headlines) nil t)))
          (if-let (headline (org-glance-scope--choose-headline choice headlines))
              (condition-case nil
                  (funcall action headline)
                (org-glance-exception:DB-OUTDATED
                 (message "Metastore %s is outdated, actualizing..." db)
                 (redisplay)
                 (org-glance :scope scope
                             :filter filter
                             :action action
                             :db db
                             :db-init t
                             :default choice
                             :prompt prompt)))
            (user-error "Headline not found"))))))

(provide 'org-glance)
;;; org-glance.el ends here
