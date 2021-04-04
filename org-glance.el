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

(defvar -org-glance-pwd nil)
(defvar -org-glance-src nil)
(defvar -org-glance-beg nil)
(defvar -org-glance-end nil)
(defvar -org-glance-hash nil)
(defvar -org-glance-indent nil)

(defgroup org-glance nil
  "Options concerning glancing entries."
  :tag "Org Glance"
  :group 'org)

(defvar org-glance-org-scope-extensions '("org" "org_archive"))
(defvar org-glance-scope--default-scope-alist
  '((file-with-archives . -org-glance-list-archives)
    (agenda . org-agenda-files)
    (agenda-with-archives . -org-glance-agenda-with-archives)))

(defcustom org-glance-after-materialize-hook nil
  "Normal hook that is run after a buffer is materialized in separate buffer."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'org-glance)

(defcustom org-glance-after-materialize-sync-hook nil
  "Hook that is run after a materialized buffer is synchronized to its source file."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'org-glance)

(defcustom org-glance-before-materialize-sync-hook nil
  "Normal hook that is run before a materialized buffer is synchronized to its source file."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'org-glance)

(defcustom org-glance-default-scope '(agenda-with-archives)
  "Default scope for glancing views."
  :group 'org-glance
  :type 'list)

(define-error 'org-glance-db-outdated "Material view database is outdated" 'user-error)

(defun org-glance-db-outdated (format &rest args)
  "Raise `org-glance-db-outdated' exception formatted with FORMAT ARGS."
  (signal 'org-glance-db-outdated
          (list (apply #'format-message format args))))

(define-error 'org-glance-view-not-modified "No changes made in materialized view" 'user-error)
(cl-defun org-glance-view-not-modified (format &rest args) (signal 'org-glance-view-not-modified (list (apply #'format-message format args))))

(define-error 'org-glance-source-file-corrupted "Source file corrupted, please reread" 'user-error)
(cl-defun org-glance-source-file-corrupted (format &rest args) (signal 'org-glance-source-file-corrupted (list (apply #'format-message format args))))

(define-error 'org-glance-properties-corrupted "Materialized view properties corrupted, please reread" 'user-error)
(cl-defun org-glance-properties-corrupted (format &rest args) (signal 'org-glance-properties-corrupted (list (apply #'format-message format args))))

(defun org-glance-format (headline)
  (or (org-element-property :TITLE headline)
      (org-element-property :raw-value headline)))

(defun org-glance-read-file-headlines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (->> (buffer-string)
         substring-no-properties
         read
         eval)))

(defun org-glance-choose-headline (choice headlines)
  (--first (string= (org-glance-format it) choice) headlines))

(defun org-glance-prompt-headlines (prompt headlines)
  (org-completing-read prompt (mapcar #'org-glance-format headlines)))

(defun org-glance-list-files-recursively (dir)
  (directory-files-recursively dir "\\.*.org\\.*"))

(defun org-glance-list-file-archives (filename)
  "Return list of org-mode files for FILENAME."
  (let* ((dir (file-name-directory filename))
         (base-filename (-some->> filename
                          file-name-nondirectory
                          file-name-sans-extension)))
    (directory-files-recursively dir (format "%s.org\\.*" base-filename))))

(defun -org-glance-list-archives ()
  (append (list (buffer-file-name))
          (org-glance-list-file-archives (buffer-file-name))))

(defun -org-glance-agenda-with-archives ()
  (cl-loop for filename in (org-agenda-files)
           append (list filename)
           append (org-glance-list-file-archives filename)))

(cl-defun org-glance-headlines
    (&key db
          (scope '(agenda))
          (filter #'(lambda (_) t))
          (db-init nil))
  (let* ((create-db? (or (and db db-init) (and db (not (file-exists-p db)))))
         (load-db? (and (not (null db)) (file-exists-p db)))
         (skip-db? (null db)))
    (cond (create-db? (org-glance-db-init db (org-glance-scope-headlines scope filter)))
          (load-db?   (org-glance-db-load db))
          (skip-db?   (org-glance-scope-headlines scope filter))
          (t         (user-error "Nothing to glance at (scope: %s)" scope)))))

(cl-defun org-glance-filter-apply (filter headline)
  (or (null filter) (and filter (funcall filter headline))))

(cl-defmethod org-glance-scope-headlines (scope &optional filter)
  (cl-loop
   for file in (org-glance-scope scope)
   when (member (file-name-extension file) org-glance-org-scope-extensions)
   do (message "Run org-glance on headlines in file %s" file)
   append (org-glance-read-headlines-from-file file filter)
   into result
   do (redisplay)
   finally (cl-return result)))

(cl-defmethod org-glance-read-headlines-from-file ((file string) &optional filter)
  (with-temp-buffer
    (insert-file-contents file)
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (headline)
        (when (org-glance-filter-apply filter headline)
          (plist-put (cadr headline) :file file)
          headline)))))

(cl-defmacro org-glance-with-headline-narrowed (headline &rest forms)
  "Visit HEADLINE, narrow to its subtree and execute FORMS on it."
  (declare (indent defun))
  `(let* ((file (org-element-property :file ,headline))
          (file-buffer (get-file-buffer file))
          (visited-buffer (current-buffer)))
     (org-glance-action-call 'visit :on ,headline)
     (widen)
     (org-narrow-to-subtree)
     (unwind-protect
          (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
            ,@forms)
       (widen))
     (cond ((and file-buffer (not (eq file-buffer (current-buffer)))) (bury-buffer file-buffer))
           ((and file-buffer (eq file-buffer (current-buffer))) (progn (switch-to-buffer visited-buffer)
                                                                       (bury-buffer file-buffer)))
           (t (kill-buffer (get-file-buffer file))))))

(cl-defmacro org-glance-with-headline-materialized (headline &rest forms)
  "Materialize HEADLINE, execute FORMS in materialized buffer."
  (declare (indent defun))
  `(let* ((file (org-element-property :file ,headline))
          (file-buffer (get-file-buffer file)))
     (org-glance-action-call 'materialize :on ,headline)
     (unwind-protect
          (let ((org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
            ,@forms)
       (kill-buffer org-glance-materialized-view-buffer))
     (cond (file-buffer (bury-buffer file-buffer))
           (t (kill-buffer (get-file-buffer file))))))

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

(defun -org-glance-promote-subtree ()
  (let ((promote-level 0))
    (cl-loop while (condition-case nil
                       (org-with-limited-levels (org-map-tree 'org-promote) t)
                     (error nil))
       do (cl-incf promote-level))
    promote-level))

(defun -org-glance-demote-subtree (level)
  (cl-loop repeat level
     do (org-with-limited-levels
         (org-map-tree 'org-demote))))

(defun -org-glance-first-level-heading ()
  (save-excursion
    (unless (org-at-heading-p) (org-back-to-heading))
    (beginning-of-line)
    (point)))

(defun -org-glance-end-of-meta-data ()
  (save-excursion
    (org-end-of-meta-data)
    (point)))

(defun -element-at-point-equals-headline (headline)
  (message "Element at point equals headline?")
  (let ((element-title (org-element-property :raw-value (org-element-at-point)))
        (headline-title (org-element-property :raw-value headline)))
    (message "Requested headline: %s" headline-title)
    (message "Visited headline: %s" element-title)
    (condition-case nil
        (s-contains? element-title headline-title)
      (error nil))))

(cl-defun org-glance-db-init (db headlines)
  (unless (file-exists-p (file-name-directory db))
    (make-directory (file-name-directory db) t))
  (with-temp-file db
    (insert "`(")
    (dolist (headline headlines)
      (insert (org-glance-db--serialize headline) "\n"))
    (insert ")"))
  (message "Database has been initialized: %s" db)
  headlines)

(defun org-glance-db-load (file)
  (-some->> file
    org-glance-read-file-headlines
    (mapcar 'org-glance-db--deserialize)))

(cl-defun org-glance-db--serialize (headline)
  (prin1-to-string
   (list (org-element-property :TITLE headline)
         (org-element-property :raw-value headline)
         (org-element-property :begin headline)
         (org-element-property :file headline))))

(cl-defun org-glance-db--deserialize (input)
  (cl-destructuring-bind (alias title begin file) input
    (org-element-create
     'headline
     `(:TITLE ,alias
       :raw-value ,title
       :begin ,begin
       :file ,file))))

(cl-defgeneric org-glance-scope (lfob)
  "Adapt list-file-or-buffer to list of files.")

(cl-defmethod org-glance-scope ((lfob string))
  "Return list of file LFOB if exists."
  (let ((file (expand-file-name lfob)))
    (cond
     ((not (file-exists-p file)) (warn "File %s does not exist" file) nil)
     ((not (file-readable-p file)) (warn "File %s is not readable" file) nil)
     ((f-directory? file) (org-glance-list-files-recursively file))
     (t file))))

(cl-defmethod org-glance-scope ((lfob sequence))
  "Adapt each element of LFOB."
  (-some->> lfob
    (-keep #'org-glance-scope)
    -flatten
    seq-uniq))

(cl-defmethod org-glance-scope ((lfob symbol))
  "Return extracted LFOB from `org-glance-scope--default-scope-alist'."
  (funcall (cdr (assoc lfob org-glance-scope--default-scope-alist))))

(cl-defmethod org-glance-scope ((lfob buffer))
  "Return list of files from LFOB buffer."
  (list
   (condition-case nil
       (get-file-buffer lfob)
     (error lfob))))

(cl-defmethod org-glance-scope ((lfob function))
  "Adapt result of LFOB."
  (-some->> lfob
    funcall
    org-glance-scope))

(defvar org-glance-view-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with sync abilities.")

(define-minor-mode org-glance-view-mode
    "A minor mode to be activated only in materialized view editor."
  nil nil org-glance-view-mode-map)

(defvar org-glance-view-default-type '(all)
  "Default type for all views.")

(defvar org-glance-properties-ignore-patterns
  (append
   org-special-properties
   '("^ARCHIVE_" "^TITLE$")))

(define-key org-glance-view-mode-map (kbd "C-x C-s") #'org-glance-view-sync-subtree)
(define-key org-glance-view-mode-map (kbd "C-c C-v") #'org-glance-view-visit-original-heading)
(define-key org-glance-view-mode-map (kbd "C-c C-q") #'kill-current-buffer)

(cl-defstruct org-glance-view
  id
  (type org-glance-view-default-type)
  (scope org-glance-default-scope))

(eval-and-compile
  (defvar org-glance-views (make-hash-table :test 'equal))
  (defvar org-glance-view-actions (make-hash-table :test 'equal))
  (defvar org-glance-db-directory (f-join user-emacs-directory "org-glance" "compiled-views"))
  (defvar org-glance-export-directory (f-join user-emacs-directory "org-glance" "materialized-views"))
  (defvar org-glance-materialized-view-buffer "*org-glance materialized view*"))

  (defun org-glance-exports ()
    (org-glance-list-files-recursively org-glance-export-directory))

  (cl-defmethod org-glance-view ((view-id symbol)) (gethash view-id org-glance-views))
  (cl-defmethod org-glance-view ((view-id string)) (org-glance-view (intern view-id)))

  (cl-defmethod org-glance-view-db ((view org-glance-view))
    (->> view
      (org-glance-view-id)
      (format "org-glance-%s.el")
      (downcase)
      (format "%s/%s" org-glance-db-directory)))

  (cl-defmethod org-glance-view-filter ((view org-glance-view))
    (-partial
     #'(lambda (view headline)
         (-contains?
          (mapcar #'downcase (org-element-property :tags headline))
          (downcase (symbol-name (org-glance-view-id view)))))
     view))

  (cl-defun org-glance-view-reread (&optional (view-id (org-glance-read-view)))
    (interactive)
    (message "Reread view %s" view-id)
    (let* ((view (gethash view-id org-glance-views))
           (db (org-glance-view-db view))
           (filter (org-glance-view-filter view))
           (scope (org-glance-view-scope view)))
      (org-glance-db-init db (org-glance-scope-headlines scope filter))
      view))

  (cl-defmethod org-glance-view-headlines ((view org-glance-view))
    "List headlines as org-elements for VIEW."
    (org-glance-headlines
     :db (org-glance-view-db view)
     :scope (org-glance-view-scope view)
     :filter (org-glance-view-filter view)))

  (cl-defmethod org-glance-view-headlines/formatted ((view org-glance-view))
    "List headlines as formatted strings for VIEW."
    (->> view
      org-glance-view-headlines
      (mapcar #'org-glance-format)
      (mapcar #'(lambda (hl) (format "[%s] %s" (org-glance-view-id view) hl)))))

  (cl-defmethod org-glance-view-prompt ((view org-glance-view) (action symbol))
    (s-titleize (format "%s %s: " action (org-glance-view-id view))))

  (cl-defmethod org-glance-view-action-resolve ((view org-glance-view) (action symbol))
    (let* ((action-types (->> org-glance-view-actions
                           (gethash action)
                           (-sort (lambda (lhs rhs) (> (length lhs) (length rhs))))))
           (view-actions (cl-loop for action-type in action-types
                            with view-type = (org-glance-view-type view)
                            when (cl-subsetp action-type view-type)
                            return action-type)))
      (or view-actions
          (car (member org-glance-view-default-type (gethash action org-glance-view-actions))))))

  (defun org-glance-act-arguments nil
    (transient-args 'org-glance-act))

  (defun org-glance-list-views ()
    "List registered views."
    (sort (hash-table-keys org-glance-views) #'s-less?))

  (cl-defun org-glance-export-all-views
      (&optional (destination
                  (or org-glance-export-directory
                      (read-directory-name "Export destination: "))))
    (interactive)
    (cl-loop for view-id being the hash-keys of org-glance-views
       do (org-glance-view-export view-id destination)))

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

  (cl-defun org-glance-def-view (view-id &key type scope)
    (unless (eq nil (gethash view-id org-glance-views))
      (user-error "View %s is already registered." view-id))
    (let ((view (make-org-glance-view :id view-id)))
      (when scope (setf (org-glance-view-scope view) scope))
      (when type  (setf (org-glance-view-type view) type))
      (puthash view-id view org-glance-views)
      (message "%s view of type %s is now ready to glance scope %s"
               view-id (or type "default") scope)
      view))

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

(cl-defun org-glance-view-export-filename
    (&optional
       (view-id (org-glance-read-view))
       (dir org-glance-export-directory))
  (f-join dir (s-downcase (format "%s.org" view-id))))

(cl-defun org-glance-view-export
    (&optional (view-id (org-glance-read-view))
       (destination (or org-glance-export-directory
                        (read-directory-name "Export destination: "))))
  (interactive)
  (let ((dest-file-name (org-glance-view-export-filename view-id destination)))
    (when (file-exists-p dest-file-name)
      (delete-file dest-file-name t))
    (cl-loop for headline in (->> view-id
                                  org-glance-view-reread
                                  org-glance-view-headlines)
       do (org-glance-with-headline-materialized headline
              (append-to-file (point-min) (point-max) dest-file-name)
            (append-to-file "\n" nil dest-file-name)))
    (progn  ;; sort headlines by TODO order
      (find-file dest-file-name)
      (goto-char (point-min))
      (set-mark (point-max))
      (org-sort-entries nil ?o)
      (org-overview)
      (save-buffer)
      (bury-buffer))
    dest-file-name))

(cl-defun org-glance-view-agenda
    (&optional
       (view-id (org-glance-read-view)))
  (interactive)
  (let ((org-agenda-files (list (org-glance-view-export-filename view-id))))
    (org-agenda-list)))

(cl-defun org-glance-view-visit
    (&optional
       (view-id (org-glance-read-view)))
  (interactive)
  (find-file (org-glance-view-export-filename view-id)))

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

(cl-defun org-glance-read-view (&optional (prompt "Choose view: "))
  "Run completing read PROMPT on registered views filtered by TYPE."
  (let ((views (org-glance-list-views)))
    (if (> (length views) 1)
        (intern (org-completing-read prompt views))
      (car views))))

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
            (setq-local -org-glance-src file)
            (setq-local -org-glance-beg beg)
            (setq-local -org-glance-end end)
            ;; extract hash from promoted subtree
            (setq-local -org-glance-hash (org-glance-view-subtree-hash))
            ;; run hooks on original subtree
            (with-demoted-errors (run-hooks 'org-glance-after-materialize-hook))
            ;; then promote it saving original level
            (setq-local -org-glance-indent (-org-glance-promote-subtree)))
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
              (setq-local -org-glance-pwd (read-passwd "Password: "))
              (org-glance-decrypt-subtree -org-glance-pwd)))
    (add-hook 'org-glance-after-materialize-hook #'decrypt t)
    (unwind-protect
         (progn
           (org-glance-action-call 'materialize :on headline)
           (org-cycle-hide-drawers 'all))
      (remove-hook 'org-glance-after-materialize-hook #'decrypt)))
  (add-hook 'org-glance-before-materialize-sync-hook
            (lambda ()
              (-org-glance-demote-subtree -org-glance-indent)
              (org-glance-encrypt-subtree -org-glance-pwd)
              (-org-glance-promote-subtree))
            'append 'local)
  (add-hook 'org-glance-after-materialize-sync-hook
            (lambda ()
              (-org-glance-demote-subtree -org-glance-indent)
              (org-glance-decrypt-subtree -org-glance-pwd)
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

(defun org-glance-view-visit-original-heading ()
  (interactive)
  (save-excursion
    (cl-loop while (org-up-heading-safe))
    (let* ((heading (list :file -org-glance-src
                          :begin -org-glance-beg
                          :raw-value (org-element-property :raw-value (org-element-at-point))))
           (virtual-element (org-element-create 'headline heading)))
      (org-glance-action-call 'visit :on virtual-element))))

(defun org-glance-view-sync-subtree ()
  (interactive)
  (save-excursion
    (cl-loop while (org-up-heading-safe))
    (let* ((source -org-glance-src)
           (beg -org-glance-beg)
           (end -org-glance-end)
           (promote-level -org-glance-indent)
           (glance-hash -org-glance-hash)
           (mat-hash (org-glance-view-subtree-hash))
           (src-hash (org-glance-view-source-hash)))

      (unless (string= glance-hash src-hash)
        (org-glance-source-file-corrupted source))

      (when (string= glance-hash mat-hash)
        (org-glance-view-not-modified source))

      (when (y-or-n-p "Subtree has been modified. Apply changes?")
        (with-demoted-errors (run-hooks 'org-glance-before-materialize-sync-hook))

        (let ((new-contents
               (save-restriction
                 (org-narrow-to-subtree)
                 (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
                   (with-temp-buffer
                     (org-mode)
                     (insert buffer-contents)
                     (goto-char (point-min))
                     (-org-glance-demote-subtree promote-level)
                     (buffer-substring-no-properties (point-min) (point-max)))))))

          (with-temp-file source
            (org-mode)
            (insert-file-contents source)
            (delete-region beg end)
            (goto-char beg)
            (insert new-contents)
            (setq end (point)))

          (setq-local -org-glance-beg beg)
          (setq-local -org-glance-end end)
          (setq-local -org-glance-hash (org-glance-view-source-hash))

          (with-demoted-errors (run-hooks 'org-glance-after-materialize-sync-hook)))))))

(defun org-glance-view-subtree-hash ()
  (save-restriction
    (org-narrow-to-subtree)
    (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
      (with-temp-buffer
        (org-mode)
        (insert buffer-contents)
        (goto-char (point-min))
        (-org-glance-promote-subtree)
        (buffer-hash)))))

(defun org-glance-view-source-hash ()
  (let ((src -org-glance-src)
        (beg -org-glance-beg)
        (end -org-glance-end))
    (with-temp-buffer
      (insert-file-contents src)
      (let ((subtree (condition-case nil
                         (buffer-substring-no-properties beg end)
                       (error (org-glance-properties-corrupted "Materialized properties corrupted, please reread")))))
        (with-temp-buffer
          (org-mode)
          (insert (s-trim subtree))
          (cl-loop while (org-up-heading-safe))
          (-org-glance-promote-subtree)
          (buffer-hash))))))

(cl-defmethod org-glance-remove-view ((view-id symbol))
  (remhash view-id org-glance-views))

(defun org-glance-capture-subtree-at-point ()
  (interactive)
  (unless (org-at-heading-p) (org-back-to-heading))
  ;; (let* ((other-views (seq-difference
  ;;                      (org-glance-list-views)
  ;;                      (mapcar #'intern (org-get-tags))))
  ;;        (view-id (org-completing-read "View: " other-views))
  ;;        (view (org-glance-view view-id)))
  ;;   (org-toggle-tag view-id)
  ;;   ;; (loop for type in (org-glance-view-type view)
  ;;   ;;       do (pp type))
  ;;   )
  )

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
