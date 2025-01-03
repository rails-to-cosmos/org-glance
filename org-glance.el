;; -*- lexical-binding: t -*-

;;; org-glance.el --- Org-mode mindmap.

;; Copyright (C) 2018-2025 Dmitry Akatov

;; Author: Dmitry Akatov <dmitry.akatov@protonmail.com>
;; Created: 29 September, 2018
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (org) (aes) (dash) (f) (highlight) (transient) (elsa) (ht))
;; Keywords: org-mode, graph, mindmap
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
(require 'org-glance-material-mode)
(require 'org-glance-metadata)
(require 'org-glance-overview)
(require 'org-glance-tag)
(require 'org-glance-ui)
(require 'org-glance-utils)

(declare-function org-glance--back-to-heading "org-glance-utils.el")
(declare-function org-glance--buffer-key-value-pairs "org-glance-utils.el")
(declare-function org-glance--join-leading-separator "org-glance-utils.el" (separator strings))
(declare-function org-glance--join-leading-separator-but-null "org-glance-utils.el" (spearator strings))
(declare-function org-glance--list-directories "org-glance-utils.el" (base-dir))
(declare-function org-glance--make-file-directory "org-glance-utils.el" (file))
(declare-function org-glance--parse-links "org-glance-utils.el")
(declare-function org-glance--remove-links "org-glance-utils.el" (&rest types))
(declare-function org-glance--substitute-links "org-glance-utils.el")
(declare-function org-glance-headline:at-point "org-glance-headline.el")
(declare-function org-glance-headline:deserialize "org-glance-headline.el" (id value))
(declare-function org-glance-headline:from-element "org-glance-headline.el" (element))
(declare-function org-glance-headline:search-buffer-by-id "org-glance-headline.el" (id))
(declare-function org-glance-headline:search-parents "org-glance-headline.el")
(declare-function org-glance-headline:serialize "org-glance-headline.el" (headline))
(declare-function org-glance-headline:with-headline-at-point "org-glance-headline.el" (&rest forms))
(declare-function org-glance-headline:with-narrowed-headline "org-glance-headline.el" (headline &rest forms))
(declare-function org-glance-headline:not-found! "org-glance-exceptions.el")

(defcustom org-glance-directory org-directory
  "Main location for all Org mode content managed by `org-glance`."
  :group 'org-glance
  :type 'directory)

(defcustom org-glance-resource-directory (f-join org-directory "resources")
  "Directory for non-Org resources associated with `org-glance`."
  :group 'org-glance
  :type 'directory)

(defcustom org-glance-clone-on-repeat-p nil
  "Create a new headline copy when repeating rather than modifying in place."
  :group 'org-glance
  :type 'boolean)

(defgroup org-glance nil "Org-mode mindmap explorer."
  :tag "Org Glance"
  :group 'org)

(defvar org-glance-tags (make-hash-table) "Hash table {tag -> `org-glance-tag-info'}")

(cl-defun org-glance:tags ()  ;; -> list[symbol]
  (hash-table-keys org-glance-tags))

(cl-defun org-glance:tags-sorted () ;; -> list[symbol]
  (sort (org-glance:tags) #'s-less?))

;; TODO refactor is needed for all the filters
(cl-defun org-glance:tag-filter (tag) ;; -> callable
  #'(lambda (headline)
      (when (-contains? (mapcar #'downcase (org-element-property :tags headline)) (symbol-name tag))
        headline)))

(cl-defun org-glance:tag-headlines (tag) ;; -> list[headline]
  (org-glance-headlines :db (org-glance-metadata:location tag)
                        :scope org-glance-directory
                        :filter (org-glance:tag-filter tag)))

(cl-defun org-glance-tags:completing-read (&optional (prompt "Tag: ") (require-match t))
  "Run completing read PROMPT on registered tags filtered by TYPE."
  (let ((tags (org-glance:tags-sorted)))
    (org-glance-tag:from-string (completing-read prompt tags nil require-match))))

(cl-defun org-glance:capture-template (tag &key (default ""))
  (let ((capture-template-config-file (f-join (org-glance-overview:directory tag) "capture-template.org")))
    (s-replace "%?" (concat default "%?")
               (cond ((f-exists-p capture-template-config-file) (with-temp-buffer
                                                                  (insert-file-contents capture-template-config-file)
                                                                  (buffer-substring-no-properties (point-min) (point-max))))
                     (t "* %?")))))

(cl-defun org-glance:tag-file-name (&optional (tag (org-glance-tags:completing-read)))
  "Path to directory where TAG-ID resources and metadata are stored."
  (abbreviate-file-name (f-join org-glance-directory (s-downcase (format "%s" tag)) "resources")))

(cl-defun org-glance:make-tag-directory (&optional (tag (org-glance-tags:completing-read)))
  (save-excursion
    (org-glance--back-to-heading)
    (save-restriction
      (org-narrow-to-subtree)
      (org-glance-headline:make-directory
       (org-glance:tag-file-name tag)
       (org-element-property :raw-value (org-element-at-point))))))

(cl-defun org-glance:create-tag (tag)
  (unless (and (symbolp tag) (org-glance--symbol-downcased? tag))
    (error "Expected downcased symbol for tag, bug got \"%s\" of type \"%s\"." tag (type-of tag)))

  (when (org-glance-tag:register tag org-glance-tags :namespace org-glance-directory)
    (org-glance-metadata:create (org-glance-metadata:location tag))
    (org-glance-overview:create tag)))

(cl-defun org-glance-materialized-headline:preserve-history-before-auto-repeat (&rest _)
  (when (and org-glance-clone-on-repeat-p
             (or org-glance-material-mode org-glance-overview-mode)
             (member (org-get-todo-state) org-done-keywords)
             (org-glance-headline:repeated-p))
    (let ((contents (org-glance-headline:contents (org-glance-headline:at-point))))
      (run-with-idle-timer 1 nil #'(lambda () (save-window-excursion
                                           (with-temp-buffer
                                             (insert contents)
                                             (goto-char (point-min))

                                             (org-glance-datetime-reset-buffer-timestamps-except-earliest)

                                             (cl-loop
                                              for class in (org-glance-headline:tags (org-glance-headline:at-point))
                                              do (let ((headline (org-glance-capture-headline-at-point class)))
                                                   (org-glance-overview:register-headline-in-archive headline class))))))))))

(cl-defun org-glance-materialized-headline:cleanup-after-auto-repeat (&rest _)
  "Do only if headline has been cloned before auto repeat.
Cleanup new headline considering auto-repeat ARGS.

- Remove all data but PINNED of cloned headline."
  (when (and (or org-glance-material-mode org-glance-overview-mode)
             org-glance-clone-on-repeat-p
             (org-glance-headline:repeated-p))
    (let ((contents (org-glance-headline:with-headline-at-point
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

;; TODO refactor
(cl-defmacro org-glance-choose-and-apply (&key filter action)
  "If HEADLINE specified, apply ACTION on it.

If HEADLINE is not specified, ask user to choose HEADLINE from
existing headlines filtered by FILTER.

If user chooses unexisting headline, capture it and apply ACTION
after capture process has been finished."
  `(condition-case default
       (cond (,filter (funcall ,action (org-glance-metadata:completing-read :filter ,filter)))
             (t (funcall ,action (org-glance-metadata:completing-read))))
     (org-glance-headline:not-found!
      (let ((<buffer> (current-buffer))
            (<point> (point))
            (tag (org-glance-tags:completing-read "Unknown headline. Please, specify it's tag to capture: ")))
        (org-glance-capture tag
          :title (cadr default)
          :callback (lambda ()
                      (let ((<hl> (org-glance-overview:original-headline)))
                        (switch-to-buffer <buffer>)
                        (goto-char <point>)
                        (funcall ,action <hl>))))))))

(cl-defun org-glance-init (&optional (directory org-glance-directory))
  "Update all changed entities from `org-glance-directory'."

  (unless (f-exists? directory)
    (mkdir directory t))

  (org-glance-overview-init)

  (add-hook 'org-glance-material-mode-hook #'org-glance-datetime-mode)
  (advice-add 'org-auto-repeat-maybe :before #'org-glance-materialized-headline:preserve-history-before-auto-repeat (list :depth -90))
  (advice-add 'org-auto-repeat-maybe :after #'org-glance-materialized-headline:cleanup-after-auto-repeat)

  (cl-loop for dir in (org-glance--list-directories directory)
           for tag = (org-glance-tag:read dir)
           do (org-glance:create-tag tag))

  (cl-loop for tag being the hash-keys of org-glance-tags
           unless (f-exists? (f-join directory (org-glance-tag:to-string tag)))
           do (org-glance-tag:remove tag org-glance-tags))

  (setq org-agenda-files (mapcar 'org-glance-overview:file-name (org-glance:tags-sorted))))

(cl-defun org-glance:@ ()
  "Choose headline to refer. Insert link to it at point."
  (interactive)
  (let ((active-region? (and (not (org-in-src-block-p)) (region-active-p)))
        (mention? (and (not (org-in-src-block-p)) (or (looking-back "^" 1) (looking-back "[[:space:]]" 1)))))
    (condition-case nil
        (cond (active-region? (error "Not implemented")
                              ;; (let* ((buffer (current-buffer))
                              ;;        (region-beginning (region-beginning))
                              ;;        (region-end (region-end))
                              ;;        (tag (org-glance-tags:completing-read (format "Specify tag for \"%s\": " (buffer-substring-no-properties region-beginning region-end))))
                              ;;        (callback (lambda () (let ((headline (org-glance-overview:original-headline)))
                              ;;                          (switch-to-buffer buffer)
                              ;;                          (goto-char region-beginning)
                              ;;                          (delete-region region-beginning region-end)
                              ;;                          (insert (org-glance-headline:with-narrowed-headline headline
                              ;;                                    (org-glance-headline-reference)))))))
                              ;;   (org-glance-capture tag
                              ;;     :default (buffer-substring-no-properties <region-beginning> <region-end>)
                              ;;     :finalize t
                              ;;     :callback callback))
                              )

              (mention? (org-glance-choose-and-apply
                         :action (lambda (headline)
                                   (insert
                                    (org-glance-headline:with-narrowed-headline headline
                                      (org-glance-headline-reference))))))

              ;; simple @
              (t (keyboard-quit)))
      (quit (self-insert-command 1 64)))))

(cl-defun org-glance:materialize (headline)
  "Materialize HEADLINE in a new buffer."
  (interactive (list (org-glance-metadata:completing-read)))
  (cl-check-type headline org-glance-headline)
  (switch-to-buffer (org-glance-headline:materialize headline))
  headline)

(cl-defun org-glance:open (headline)
  "Run `org-open-at-point' on any `org-link' inside HEADLINE's contents.
If there is only one link, open it.
If there is more than one link, prompt user to choose which one to open.
If headline doesn't contain links, role `can-be-opened' should be revoked."
  (interactive (list (org-glance-metadata:completing-read :filter (lambda (headline)
                                                                    (and
                                                                     (org-glance-headline:active? headline)
                                                                     (org-glance-headline:linked? headline))))))
  (org-glance-headline:with-narrowed-headline headline
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
  "Materialize HEADLINE and retrieve key-value pairs from its contents."
  (interactive (list (org-glance-metadata:completing-read :filter (lambda (headline) (and (org-glance-headline:active? headline)
                                                                                     (or (org-glance-headline:propertized? headline)
                                                                                         (org-glance-headline:encrypted? headline)))))))
  (cl-check-type headline org-glance-headline)
  (org-glance-headline:with-narrowed-headline headline
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

;; (cl-defun org-glance:prototype ()
;;   (interactive)
;;   "Capture headline based on chosen prototype."
;;   (org-glance-choose-and-apply
;;    :action (lambda (headline)
;;              (org-glance-capture
;;               :class (org-element-property :class headline)
;;               :template (org-glance-headline:contents headline)))))

(cl-defun org-glance-capture (tag &key
                                  (title (cond ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                                               (t "")))
                                  (callback nil)
                                  (finalize nil)
                                  (template (org-glance:capture-template tag :default title)))
  (declare (indent 1))
  (interactive)
  (let ((id (org-glance-tag:generate-id tag))
        (file (make-temp-file "org-glance-" nil ".org")))
    (find-file file)
    (add-hook 'org-capture-prepare-finalize-hook (lambda () (org-glance-capture:prepare-finalize-hook id tag)) 0 t)
    (add-hook 'org-capture-after-finalize-hook (lambda () (org-glance-capture:after-finalize-hook id tag)) 0 t)
    (when callback (add-hook 'org-capture-after-finalize-hook callback 1 t))

    (let ((org-capture-templates (list (list "_" "_" 'entry (list 'file file) template))))
      (org-capture nil "_")
      (when finalize (org-capture-finalize)))

    id))

(cl-defun org-glance:insert-pin-block ()
  (interactive)
  (insert "#+begin_pin" "\n\n" "#+end_pin")
  (forward-line -1))

(cl-defun org-glance:headline-alias (&optional (headline (org-glance-headline:at-point)))
  "Get title of HEADLINE considering alias property."
  (with-temp-buffer
    (save-excursion
      (insert (org-glance-headline:alias headline)))
    (org-glance--remove-links 'org-glance-overview 'org-glance-state)
    (org-glance--substitute-links)
    (s-trim (buffer-substring-no-properties (point-min) (point-max)))))

;; Relations

(cl-defun org-glance-headline:repeated-p ()
  (org-glance-datetime-headline-repeated-p))

(cl-defun org-glance-headline:make-directory (location title)
  (abbreviate-file-name
   (make-temp-file
    (org-glance--make-file-directory
     (f-join location
             (concat (format-time-string "%Y-%m-%d_")
                     (->> title
                          (replace-regexp-in-string "[^a-z0-9A-Z_]" "-")
                          (replace-regexp-in-string "\\-+" "-")
                          (replace-regexp-in-string "\\-+$" "")
                          (s-truncate 30))
                     "-")))
    'directory)))

(cl-defun org-glance-headlines (&key db (scope '(agenda)) (filter #'(lambda (_) t)) (db-init nil))
  "Deprecated method, refactor it."
  (let* ((create-db? (or (and db db-init) (and db (not (file-exists-p db)))))
         (load-db? (and (not (null db)) (file-exists-p db)))
         (skip-db? (null db)))
    (cond (create-db? (let ((headlines (org-glance-scope-headlines scope filter)))
                        (org-glance-metadata:create db headlines)
                        headlines))
          (load-db?   (org-glance-metadata:headlines (org-glance-metadata:read db)))
          (skip-db?   (org-glance-scope-headlines scope filter))
          (t          (user-error "Nothing to glance at (scope: %s)" scope)))))

;; TODO refactor, slow
(cl-defun org-glance-metadata:completing-read-options (&optional filter)
  (cl-loop for tag being the hash-keys of org-glance-tags
           append (cl-loop for headline-metadata in (org-glance:tag-headlines tag)
                           when (or (null filter) (funcall filter headline-metadata))
                           collect (cons (format "[%s] %s" tag (org-glance-headline:plain-title headline-metadata)) headline-metadata))))

(defvar org-glance-scope:extensions
  '("org" "org_archive"))

(defvar org-glance-scope--default-scope-alist
  '((file-with-archives . org-glance--file-with-archives)
    (agenda . org-agenda-files)
    (agenda-with-archives . org-glance--agenda-with-archives)))

(cl-defgeneric org-glance-scope (_)
  "Convert input to list of files if possible.")

(cl-defmethod org-glance-scope ((file string))
  "Return list of file S if exists."
  (let ((files (cond
                ((not (file-exists-p file)) (message "File \"%s\" does not exist" file) nil)
                ((not (file-readable-p file)) (message "File \"%s\" is not readable" file) nil)
                ((f-directory? file) (org-glance-scope (directory-files-recursively file "\\.*.org\\.*")))
                ((with-temp-buffer
                   (insert-file-contents file)
                   (hack-local-variables)
                   (alist-get 'org-glance-overview-mode (buffer-local-variables))) (message "File \"%s\" is in `org-glance-overview' mode" file) nil)
                (t (list file)))))
    (cl-loop
     for file in files
     when (member (file-name-extension file) org-glance-scope:extensions)
     collect file)))

(cl-defmethod org-glance-scope ((l sequence))
  "Convert L to flattened list of files."
  (-some->> l
    (-keep #'org-glance-scope)
    -flatten
    seq-uniq))

(cl-defmethod org-glance-scope ((s symbol))
  "Return extracted S from `org-glance-scope--default-scope-alist'."
  (if-let (reserved-scope (assoc s org-glance-scope--default-scope-alist))
      (funcall (cdr reserved-scope))
    (org-glance-scope (symbol-name s))))

(cl-defmethod org-glance-scope ((b buffer))
  "Return list of files from buffer B."
  (list (condition-case nil (get-file-buffer b) (error b))))

(cl-defmethod org-glance-scope ((f function))
  "Adapt result of F."
  (-some->> f funcall org-glance-scope))

(cl-defun org-glance-scope-headlines (scope &optional (filter (lambda (headline) headline)))
  (cl-loop for file in (org-glance-scope scope)
           append (with-temp-buffer
                    (org-mode)
                    (insert-file-contents file)
                    (-non-nil (mapcar filter (org-glance-headline:buffer-headlines (current-buffer)))))))

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

(cl-defun org-glance-link:choose-thing-for-materialization ()
  (concat "org-glance-visit:" (org-glance-headline:id (org-glance-metadata:completing-read))))

(cl-defun org-glance-link:choose-thing-for-opening ()
  (concat "org-glance-open:" (org-glance-headline:id (org-glance-metadata:completing-read
                                                      :filter #'(lambda (headline)
                                                                  (and
                                                                   (org-glance-headline:active? headline)
                                                                   (org-glance-headline:linked? headline)))))))


(org-link-set-parameters
 "org-glance-visit"
 :follow #'org-glance-link:materialize
 :face 'org-glance-link-materialize-face
 :complete 'org-glance-link:choose-thing-for-materialization
 ;; :export #'org-glance-link:export
 ;; :store #'org-glance-link:store-link
 )

(org-link-set-parameters
 "org-glance-open"
 :follow #'org-glance-link:open
 :complete 'org-glance-link:choose-thing-for-opening
 ;; :export #'org-glance-link:export
 ;; :store #'org-glance-link:store-link
 )

(org-link-set-parameters
 "org-glance-overview"
 :follow #'org-glance-link:overview
 :face 'org-glance-link-overview-face)

(org-link-set-parameters
 "org-glance-state"
 :follow #'org-glance-link:state
 :face 'org-glance-link-state-face)

;; (defcustom org-glance-link:command 'man
;;   "The Emacs command to be used to display a man page."
;;   :group 'org-link
;;   :type '(choice (const man) (const woman)))

(defun org-glance-link:materialize (id &optional _)
  "Materialize org-glance headline identified by ID."
  (org-glance:materialize (org-glance-metadata:headline-metadata id)))

(defun org-glance-link:open (id &optional _)
  "Open org-glance headline identified by ID."
  (org-glance:open (org-glance-metadata:headline-metadata id)))

(defun org-glance-link:overview (tag &optional _)
  "Open org-glance headline identified by ID."
  (org-glance-overview (intern (downcase tag))))

(defun org-glance-link:state (_state &optional _)
  "Get all headlines with todo state equal STATE."
  (user-error "Not implemented."))

;; (defun org-glance-link:store-link ()
;;   "Store a link to a man page."
;;   (when (memq major-mode '(Man-mode woman-mode))
;;     ;; This is a man page, we do make this link.
;;     (let* ((page (org-glance-link:get-page-name))
;;            (link (concat "man:" page))
;;            (description (format "Man page for %s" page)))
;;       (org-link-store-props
;;        :type "man"
;;        :link link
;;        :description description))))

;; (defun org-glance-link:get-page-name ()
;;   "Extract the page name from the buffer name."
;;   ;; This works for both `Man-mode' and `woman-mode'.
;;   (if (string-match " \\(\\S-+\\)\\*" (buffer-name))
;;       (match-string 1 (buffer-name))
;;     (error "Cannot create link to this man page")))

;; (defun org-glance-link:export (link description format _)
;;   "Export a man page link from Org files."
;;   (let ((path (format "http://man.he.net/?topic=%s&section=all" link))
;;         (desc (or description link)))
;;     (pcase format
;;       (`html (format "<a target=\"_blank\" href=\"%s\">%s</a>" path desc))
;;       (`latex (format "\\href{%s}{%s}" path desc))
;;       (`texinfo (format "@uref{%s,%s}" path desc))
;;       (`ascii (format "%s (%s)" desc path))
;;       (t path))))

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
                              (completing-read prompt (mapcar #'org-glance-headline:plain-title headlines) nil t)))
          (if-let (headline (org-glance-headline:select-by-title choice headlines))
              (condition-case nil
                  (funcall action headline)
                (DB-OUTDATED (message "Metadata %s is outdated, actualizing..." db)
                             (redisplay)
                             (org-glance :scope scope
                                         :filter filter
                                         :action action
                                         :db db
                                         :db-init t
                                         :default choice
                                         :prompt prompt)))
            (error "Headline not found"))))))

(provide 'org-glance)
;;; org-glance.el ends here
