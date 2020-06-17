;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl))

(eval-and-compile
  (require 'aes)
  (require 'load-relative)
  (require 'org)
  (require 'org-element)
  (require 'org-glance)
  (require 'org-glance-db)
  (require 'transient))

;; buffer-locals for materialized views

(defvar --og-transient--current-view nil
  "Local scoped current-view variable for transient forms.")

(defcustom after-materialize-hook nil
  "Normal hook that is run after a buffer is materialized in separate buffer."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'org-glance)

(defcustom after-materialize-sync-hook nil
  "Hook that is run after a materialized buffer is synchronized to its source file."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'org-glance)

(defcustom before-materialize-sync-hook nil
  "Normal hook that is run before a materialized buffer is synchronized to its source file."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'org-glance)

(eval-and-compile
  (defvar org-glance-db-directory (concat user-emacs-directory "org-glance"))
  (defvar org-glance-views '())
  (defvar org-glance-materialized-view-buffer "*org-glance materialized view*")
  (defvar org-glance-view-scopes (make-hash-table :test 'equal))
  (defvar org-glance-view-types (make-hash-table :test 'equal))
  (defvar org-glance-view-actions (make-hash-table :test 'equal))
  (defvar org-glance-view-files (make-hash-table :test 'equal))
  (defvar org-glance-default-scope '(agenda-with-archives)))

;; locals in materialized buffers
(defvar -org-glance-pwd nil)
(defvar -org-glance-src nil)
(defvar -org-glance-beg nil)
(defvar -org-glance-end nil)
(defvar -org-glance-hash nil)
(defvar -org-glance-indent nil)

(eval-and-compile
  (defvar org-glance-view-mode-map (make-sparse-keymap) "Extend `org-mode' map with sync abilities.")
  (define-key org-glance-view-mode-map (kbd "C-x C-s") #'org-glance-view-sync-subtree)
  (define-key org-glance-view-mode-map (kbd "C-c C-v") #'org-glance-view-visit-original-heading)
  (define-key org-glance-view-mode-map (kbd "C-c C-q") #'kill-current-buffer))

(define-error 'org-glance-view-not-modified "No changes made in materialized view" 'user-error)
(cl-defun org-glance-view-not-modified (format &rest args) (signal 'org-glance-view-not-modified (list (apply #'format-message format args))))

(define-error 'org-glance-view-corrupted "Materialized view source corrupted" 'user-error)
(cl-defun org-glance-view-corrupted (format &rest args) (signal 'org-glance-view-corrupted (list (apply #'format-message format args))))

;;;###autoload
(define-minor-mode org-glance-view-mode
  "A minor mode to be activated only in materialized view editor."
  nil nil org-glance-view-mode-map)

(defun org-glance-act-arguments nil
  (transient-args 'org-glance-act))

(cl-defun org-glance-view-filter (view headline)
  (-contains?
   (mapcar #'s-downcase (org-element-property :tags headline))
   (s-downcase view)))

(defun org-glance-list-views (&optional type)
  "List views mathing TYPE."
  (cond ((or (null type) (seq-set-equal-p (cl-intersection type '(any all _ *)) type))
         org-glance-views)
        (t (cl-loop for view in org-glance-views
                    if (let ((types (gethash view org-glance-view-types)))
                         (seq-set-equal-p (cl-intersection type types) type))
                    collect view))))

;; some private helpers

(defun -org-glance-encrypt-subtree (&optional password)
  "Encrypt subtree at point with PASSWORD."
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

(defun -org-glance-decrypt-subtree (&optional password)
  "Decrypt subtree at point with PASSWORD."
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
             do (incf promote-level))
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

(defun -org-glance-end-of-subtree ()
  (save-excursion
    (org-end-of-subtree t)))

(defun -element-at-point-equals-headline (headline)
  (condition-case nil
      (s-contains? (org-element-property :raw-value (org-element-at-point))
                   (org-element-property :raw-value headline))
    (error nil)))

(defun -org-glance-filter-for (view)
  (-partial #'org-glance-view-filter view))

(defun -org-glance-db-for (view)
  (format "%s/org-glance-%s.el" org-glance-db-directory view))

(defun -org-glance-prompt-for (action view)
  (s-titleize (format "%s %s: " action view)))

(defun org-glance-read-view (prompt &optional type)
  "Run completing read PROMPT on registered views filtered by TYPE."
  (let ((views (org-glance-list-views type)))
    (if (> (length views) 1)
        (org-completing-read prompt views)
      (symbol-name (car views)))))

(cl-defun org-glance-call-action (name &key (on 'current-headline) (for "all"))
  (when (eq on 'current-headline)
    (setq on (org-element-at-point)))
  (let ((fn (intern (format "org-glance--%s--%s" name for))))
    (unless (fboundp fn)
      (user-error "Unbound function %s" fn))
    (funcall fn on)))

(cl-defmacro org-glance-def-action (name args _ type &rest body)
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
  ;; register view action
  (puthash type
           (cl-pushnew name (gethash type org-glance-view-actions))
           org-glance-view-actions)
  (let* ((res (cl--transform-lambda (cons args body) name))
	 (form `(progn

                  (defun ,(intern (format "org-glance-action-%s" name)) (&optional args view)
                    (interactive (list (org-glance-act-arguments)))

                    (setq view
                          (or view
                              --og-transient--current-view
                              (org-glance-read-view
                               (format "%s view: " ,(s-titleize (format "%s" name)))
                               (list (quote ,type)))))

                    (org-glance
                     :scope (gethash (intern view) org-glance-view-scopes org-glance-default-scope)
                     :prompt (-org-glance-prompt-for (quote ,name) view)
                     :db (gethash (intern view) org-glance-view-files (-org-glance-db-for view))
                     :db-init (member "--reread" args)
                     :filter (-org-glance-filter-for view)
                     :action (function ,(intern (format "org-glance--%s--%s" name type)))))

                  (defun ,(intern (format "org-glance--%s--%s" name type))
                      ,@(cdr res)))))

    (if (car res) `(progn ,(car res) ,form) form)))

(org-glance-def-action visit (headline) :for all
  "Visit HEADLINE."
  (let* ((file (org-element-property :file headline))
         (point (org-element-property :begin headline))
         (file-buffer (get-file-buffer file)))

    (cond ((file-exists-p file) (find-file file))
          (t (org-glance-db-outdated "File not found: %s" file)))

    (widen)
    (goto-char point)

    (cond ((-element-at-point-equals-headline headline)
           (cl-loop while (org-up-heading-safe)) ;; expand parents
           (org-narrow-to-subtree)
           (outline-show-branches)
           (widen)
           (goto-char point)
           (outline-show-subtree))
          (t (unless file-buffer
               (kill-buffer))
             (org-glance-db-outdated "Cache file is outdated")))))

(org-glance-def-action materialize (headline) :for all
  "Materialize HEADLINE in separate buffer."
  (save-window-excursion
    (org-glance-call-action 'visit :on headline)
    (let* ((file (org-element-property :file headline))
           (beg (-org-glance-first-level-heading))
           (end-of-subtree (-org-glance-end-of-subtree))
           (contents (s-trim-right (buffer-substring-no-properties beg end-of-subtree))))
      (when (get-buffer org-glance-materialized-view-buffer)
        (kill-buffer org-glance-materialized-view-buffer))
      (with-current-buffer (get-buffer-create org-glance-materialized-view-buffer)
        (delete-region (point-min) (point-max))
        (org-mode)
        (org-glance-view-mode)
        (insert
         (with-temp-buffer
           (insert contents)
           (s-trim (buffer-substring-no-properties (point-min) (point-max)))))
        (goto-char (point-min))
        (setq-local -org-glance-src file)
        (setq-local -org-glance-beg beg)
        (setq-local -org-glance-end end-of-subtree)
        ;; extract hash from promoted subtree
        (setq-local -org-glance-hash (org-glance-view-subtree-hash))
        ;; run hooks on original subtree
        (with-demoted-errors (run-hooks 'after-materialize-hook))
        ;; then promote it saving original level
        (setq-local -org-glance-indent (-org-glance-promote-subtree)))))
  (switch-to-buffer org-glance-materialized-view-buffer))

(org-glance-def-action open (headline) :for link
  "Search for `org-any-link-re' under the HEADLINE
then run `org-completing-read' to open it."
  (let* ((file (org-element-property :file headline))
         (file-buffer (get-file-buffer file))
         (org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))

    (save-window-excursion
      (org-glance-call-action 'materialize :on headline))

    (unwind-protect
        (with-current-buffer org-glance-materialized-view-buffer
          (let* ((links (org-element-map (org-element-parse-buffer) 'link
                          (lambda (link)
                            (cons
                             (substring-no-properties
                              (or (nth 2 link) ;; link alias
                                  (org-element-property :raw-link link)) ;; full link if alias is none
                              )
                             (org-element-property :begin link)))))
                 (point (cond
                         ((> (length links) 1) (cdr (assoc (org-completing-read "Open link: " links) links)))
                         ((= (length links) 1) (cdar links))
                         (t (user-error "Unable to find links in %s" file)))))
            (goto-char point)
            (org-open-at-point)))
      (kill-buffer org-glance-materialized-view-buffer))

    (if file-buffer
        (bury-buffer file-buffer)
      (kill-buffer (get-file-buffer file)))))

;;; Actions for CRYPT views

(org-glance-def-action decrypt (headline) :for crypt
  "Decrypt encrypted HEADLINE, then call MATERIALIZE action on it."
  (cl-flet ((decrypt ()
                     (setq-local -org-glance-pwd (read-passwd "Password: "))
                     (-org-glance-decrypt-subtree -org-glance-pwd)))
    (add-hook 'after-materialize-hook #'decrypt t)
    (unwind-protect
        (org-glance-call-action 'materialize :on headline)
      (remove-hook 'after-materialize-hook #'decrypt)))
  (add-hook 'before-materialize-sync-hook
            (lambda ()
              (-org-glance-demote-subtree -org-glance-indent)
              (-org-glance-encrypt-subtree -org-glance-pwd)
              (-org-glance-promote-subtree))
            'append 'local)
  (add-hook 'after-materialize-sync-hook
            (lambda ()
              (-org-glance-demote-subtree -org-glance-indent)
              (-org-glance-decrypt-subtree -org-glance-pwd)
              (-org-glance-promote-subtree))
            'append 'local))

(defun org-glance-view-visit-original-heading ()
  (interactive)
  (save-excursion
    (cl-loop while (org-up-heading-safe))
    (let* ((heading (list :file -org-glance-src
                          :begin -org-glance-beg
                          :raw-value (org-element-property :raw-value (org-element-at-point))))
           (virtual-element (org-element-create 'headline heading)))
      (org-glance-call-action 'visit :on virtual-element))))

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
        (user-error "Source file modified, please reread"))

      (when (string= glance-hash mat-hash)
        (org-glance-view-not-modified "No changes made in subtree"))

      (when (y-or-n-p "Subtree has been modified. Apply changes?")
        (with-demoted-errors (run-hooks 'before-materialize-sync-hook))

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

          (with-demoted-errors (run-hooks 'after-materialize-sync-hook)))))))

(defun org-glance-view-subtree-hash ()
  (save-restriction
    (org-narrow-to-subtree)
    (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
      (with-temp-buffer
        (org-mode)
        (insert buffer-contents)
        (goto-char (point-min))
        (-org-glance-promote-subtree)
        (message "Subtree string:\n\"%s\"" (buffer-substring-no-properties (point-min) (point-max)))
        (message "Subtree hash: \"%s\"" (buffer-hash))
        (buffer-hash)))))

(defun org-glance-view-source-hash (&optional src beg end)
  (let ((src (or src -org-glance-src))
        (beg (or beg -org-glance-beg))
        (end (or end -org-glance-end)))
    (with-temp-buffer
      (insert-file-contents src)
      (let ((subtree (condition-case nil
                         (buffer-substring-no-properties beg end)
                       (error (org-glance-view-corrupted "Materialized properties corrupted, please reread")))))
        (with-temp-buffer
          (org-mode)
          (insert subtree)
          (cl-loop while (org-up-heading-safe))
          (-org-glance-promote-subtree)
          (message "Source string:\n\"%s\"" (buffer-substring-no-properties (point-min) (point-max)))
          (message "Source hash: \"%s\"" (buffer-hash))
          (buffer-hash))))))

(cl-defmacro org-glance-def-view (view &key bind type scope file &allow-other-keys)
  (declare
   (debug (stringp listp listp listp))
   (indent 1))
  `(progn
     (cl-pushnew (intern ,view) org-glance-views)

     (when ,scope
       (puthash (intern ,view) ,scope org-glance-view-scopes))

     (when ,file
       (puthash (intern ,view) ,file org-glance-view-files))

     (when ,type
       (puthash (intern ,view) ,type org-glance-view-types))

     (when (quote ,bind)
       (cl-loop for (binding . cmd) in (quote ,bind)
                do (lexical-let ((command-name (intern (format "org-glance-action-%s" cmd)))
                                 (view ,view))
                     (global-set-key (kbd binding)
                                     (lambda () (interactive)
                                       (funcall command-name view))))))

     (message "Create view %s" ,view)))

(cl-defun org-glance-remove-view (tag)
  (setq org-glance-views (cl-remove (intern tag) org-glance-views))
  (remhash (intern tag) org-glance-view-scopes)
  (remhash (intern tag) org-glance-view-types))

(defun org-glance-capture-subtree-at-point ()
  (interactive)
  (unless (org-at-heading-p) (org-back-to-heading))
  (let ((view (org-completing-read "View: " (seq-difference (org-glance-list-views)
                                                            (mapcar #'intern (org-get-tags)))))
        (headline (org-element-at-point)))
    (when (org-glance-view-filter view headline)
      (user-error "Subtree is already captured"))
    (org-toggle-tag view)
    ;; capture action priority list
    ))

(provide-me)
;;; org-glance-views.el ends here
