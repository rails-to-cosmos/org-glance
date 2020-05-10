;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'aes)
  (require 'cl)
  (require 'load-relative)
  (require 'org)
  (require 'org-element))

(require 'org-glance-cache)
(require 'org-glance-sec)

;; buffer-locals for mv sync

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

(defvar org-glance-views '())
(defvar org-glance-view-scopes (make-hash-table :test 'equal))
(defvar org-glance-view-types (make-hash-table :test 'equal))
(defvar org-glance-view-actions (make-hash-table :test 'equal))

;; locals in materialized buffers
(defvar -org-glance-pwd nil)
(defvar -org-glance-src nil)
(defvar -org-glance-beg nil)
(defvar -org-glance-end nil)
(defvar -org-glance-hash nil)
(defvar -org-glance-indent nil)

(defvar org-glance-view-mode-map (make-sparse-keymap) "Extend `org-mode' map with sync abilities.")
(define-key org-glance-view-mode-map (kbd "C-x C-s") #'org-glance-view-sync-subtree)
(define-key org-glance-view-mode-map (kbd "C-c C-v") #'org-glance-view-visit-original-heading)
(define-key org-glance-view-mode-map (kbd "C-c C-q") #'quit-window)

(define-error 'org-glance-view-not-modified "No changes found in materialized view." 'user-error)
(cl-defun org-glance-view-not-modified (format &rest args) (signal 'org-glance-view-not-modified (list (apply #'format-message format args))))

(define-error 'org-glance-view-corrupted "Materialized view source corrupted." 'user-error)
(cl-defun org-glance-view-corrupted (format &rest args) (signal 'org-glance-view-corrupted (list (apply #'format-message format args))))

;;;###autoload
(define-minor-mode org-glance-view-mode
  "A minor mode to be activated only in materialized view editor."
  nil nil org-glance-view-mode-map)

(cl-defun org-glance-view-filter (view headline)
  (-contains?
   (mapcar #'s-downcase (org-element-property :tags headline))
   (s-downcase view)))

(cl-defun org-glance-list-views (&key type &allow-other-keys)
  "List views mathing TYPE."
  (cl-loop for view being the hash-keys in org-glance-view-types
           using (hash-value types)
           if (or (seq-set-equal-p (cl-intersection type types) type)
                  (seq-set-equal-p (cl-intersection type '(any all _ *)) type))
           collect view))

;; some private helpers

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
    (unless (org-at-heading-p)
      (org-back-to-heading))
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

(defun -org-glance-cache-for (view)
  (format "~/.emacs.d/org-glance/org-glance-%s.el" view))

(defun -org-glance-fallback-for (view)
  (-partial #'user-error "%s not found: %s" view))

(defun -org-glance-prompt-for (action view)
  (format "%s %s: " action view))

(defun -org-glance-view-completing-read (view &optional type)
  (or view (org-completing-read "View: " (org-glance-list-views :type type))))

;; common interactives

;; (defun org-glance-materialize (&optional view minify)
;;   (interactive)
;;   (let ((view (-org-glance-view-completing-read view)))
;;     (org-glance-reread view)
;;     (org-glance-mv--materialize-cache (-org-glance-cache-for view) minify)))

;; action factory

(cl-defun org-glance-call-action (name &key (on 'current-headline) (for "all"))
  (when (eq on 'current-headline)
    (setq on (org-element-at-point)))
  (let ((fn (intern (format "org-glance--%s--%s" name for))))
    (unless (fboundp fn)
      (user-error "Unbound function %s" fn))
    (funcall fn on)))

(defmacro org-glance-def-action (name args _ type &rest body)
  "Defun method NAME (ARGS) BODY.
Make it accessible for views of TYPE in `org-glance-view-actions'."
  (declare (debug
            ;; Same as defun but use cl-lambda-list.
            (&define [&or name ("setf" :name setf name)]
                     cl-lambda-list
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

                  (defun ,(intern (format "org-glance-action-%s" name)) (&optional view reread-p)
                    (interactive)
                    (let* ((view (-org-glance-view-completing-read view (list (quote ,type)))))
                      (org-glance :scope (gethash (intern view) org-glance-view-scopes '(agenda))
                                  :prompt (-org-glance-prompt-for (quote ,name) view)
                                  :cache-file (-org-glance-cache-for view)
                                  :reread-p reread-p
                                  :filter (-org-glance-filter-for view)
                                  :fallback (-org-glance-fallback-for view)
                                  :action (function ,(intern (format "org-glance--%s--%s" name type))))))

                  (defun ,(intern (format "org-glance--%s--%s" name type))
                      ,@(cdr res)))))

    (if (car res) `(progn ,(car res) ,form) form)))

;;
;;; Actions available for all types of views
;;

(org-glance-def-action visit (headline) :for all
  "Visit HEADLINE."
  (let* ((file (org-element-property :file headline))
         (point (org-element-property :begin headline))
         (file-buffer (get-file-buffer file)))

    (cond ((file-exists-p file)
           (find-file file))
          (t (org-glance-cache-outdated "File not found: %s" file)))

    (widen)
    (goto-char point)

    (cond ((-element-at-point-equals-headline headline)
           (cl-loop while (org-up-heading-safe))  ;; expand parents
           (org-narrow-to-subtree)
           (org-show-all)
           (widen)
           (goto-char point))
          (t (unless file-buffer
               (kill-buffer))
             (org-glance-cache-outdated "Cache file is outdated")))))

(org-glance-def-action materialize (headline) :for all
  "Materialize HEADLINE in separate buffer."
  (org-glance-call-action 'visit :on headline)
  (let* ((file (buffer-file-name))
         (output-buffer "*org-glance materialized view*")
         (beg (-org-glance-first-level-heading))
         (end-of-meta-data (-org-glance-end-of-meta-data))
         (end-of-subtree (-org-glance-end-of-subtree))
         (headline (s-trim (buffer-substring-no-properties beg end-of-meta-data)))
         (contents (s-trim (buffer-substring-no-properties end-of-meta-data end-of-subtree))))
    (when (get-buffer output-buffer)
      (kill-buffer output-buffer))
    (with-current-buffer (get-buffer-create output-buffer)
      (delete-region (point-min) (point-max))
      (org-mode)
      (org-glance-view-mode)
      (insert headline)
      (insert contents)
      (goto-char (point-min))
      (let ((hash (org-glance-view-subtree-hash)))
        (setq-local -org-glance-src file)
        (setq-local -org-glance-beg beg)
        (setq-local -org-glance-end end-of-subtree)
        (setq-local -org-glance-hash hash)
        ;; run hooks on original subtree
        (with-demoted-errors (run-hooks 'after-materialize-hook))
        ;; then promote it saving original level
        (setq-local -org-glance-indent (-org-glance-promote-subtree))))
    (switch-to-buffer-other-window output-buffer)))

;;; Actions available for LINK views
;;
(org-glance-def-action open (headline) :for link
  "Search for `org-any-link-re' under the HEADLINE
then run `org-completing-read' to open it."
  (let* ((file (org-element-property :file headline))
         (file-buffer (get-file-buffer file))
         (org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
    (org-glance-call-action 'visit :on headline)
    (org-open-at-point)
    (if file-buffer (bury-buffer file-buffer)
      (kill-buffer (get-file-buffer file)))))


;;; Actions for CRYPT views

(org-glance-def-action decrypt (headline) :for crypt
  "Decrypt encrypted HEADLINE, then call MATERIALIZE action on it."
  (cl-flet ((decrypt ()
                  (setq-local -org-glance-pwd (read-passwd "Password: "))
                  (org-glance-sec-decrypt-subtree -org-glance-pwd)))
    (add-hook 'after-materialize-hook #'decrypt t)
    (org-glance-call-action 'materialize :on headline)
    (remove-hook 'after-materialize-hook #'decrypt))
  (add-hook 'before-materialize-sync-hook
            (lambda ()
              (-org-glance-demote-subtree -org-glance-indent)
              (org-glance-sec-encrypt-subtree -org-glance-pwd)
              (-org-glance-promote-subtree))
            'append 'local)
  (add-hook 'after-materialize-sync-hook
            (lambda ()
              (-org-glance-demote-subtree -org-glance-indent)
              (org-glance-sec-decrypt-subtree -org-glance-pwd)
              (-org-glance-promote-subtree))
            'append 'local))

(defun org-glance-mv--safe-extract-property (property)
  (condition-case nil
      (org-entry-get (point) property)
    (error (org-glance-view-corrupted "Materialized properties corrupted, please reread"))))

(defun org-glance-mv--safe-extract-num-property (property)
  (string-to-number (org-glance-mv--safe-extract-property property)))

;; (defun org-glance-mv--materialize-cache (filename &optional interact)
;;   (let ((headlines (org-glance-load filename))
;;         (file-entries (make-hash-table))
;;         (output-filename (make-temp-file "org-glance-materialized-" nil ".org")))

;;     (loop for hl in headlines
;;           do (let ((fn (intern (org-element-property :file hl)))
;;                    (pos (org-element-property :begin hl)))
;;                (puthash fn (cons pos (gethash fn file-entries)) file-entries)))

;;     (maphash (lambda (file entries)
;;                (with-temp-buffer
;;                  (org-mode)
;;                  (insert-file-contents (symbol-name file))
;;                  (loop for pos in entries
;;                        do (let* ((beg (save-excursion
;;                                         (goto-char pos)
;;                                         (beginning-of-line)
;;                                         (point)))
;;                                  (end (save-excursion
;;                                         (goto-char pos)
;;                                         (org-end-of-subtree)
;;                                         (point)))
;;                                  (contents (buffer-substring-no-properties beg end)))
;;                             (with-temp-buffer
;;                               (org-mode)
;;                               (insert contents)
;;                               (goto-char (point-min))
;;                               (let ((promote-level 0))
;;                                 (while
;;                                     (condition-case nil
;;                                         (org-with-limited-levels (org-map-tree 'org-promote) t)
;;                                       (error nil))
;;                                   (incf promote-level))

;;                                 (let ((hash (buffer-hash)))
;;                                   (goto-char (point-min))
;;                                   (org-set-property "ORG_GLANCE_SOURCE" (symbol-name file))
;;                                   (org-set-property "ORG_GLANCE_INDENT" (number-to-string promote-level))
;;                                   (org-set-property "ORG_GLANCE_BEG" (number-to-string beg))
;;                                   (org-set-property "ORG_GLANCE_END" (number-to-string end))
;;                                   (org-set-property "ORG_GLANCE_HASH" hash)
;;                                   (goto-char (point-max))
;;                                   (insert "\n")
;;                                   (append-to-file (point-min) (point-max) output-filename))))))))
;;              file-entries)

;;     (unless interact
;;       (with-current-buffer (find-file-other-window output-filename)
;;         (org-mode)
;;         (org-overview)
;;         (org-glance-view-mode)))

;;     output-filename))

(defun org-glance-view-visit-original-heading ()
  (interactive)
  (let* ((beg -org-glance-beg))
    (find-file-other-window -org-glance-src)
    (widen)
    (goto-char beg)
    (cl-loop while (org-up-heading-safe))
    (org-narrow-to-subtree)
    (org-show-all)
    (widen)
    (goto-char beg)))

(defun org-glance-mv--backup (&optional view dir)
  (interactive)
  (let* ((view (or view (org-completing-read "View: " org-glance-views)))
         (dir (or dir (read-directory-name "Backup directory: ")))
         (vf (funcall (intern (format "org-glance--%s-materialize" (s-downcase view))) 'interact))
         (new-file (concat (s-downcase view) ".org"))
         (new-file-path (f-join dir new-file)))

    (condition-case nil
        (mkdir dir)
      (error nil))

    (if (file-exists-p new-file-path)
        (let ((existed-buffer-hash (with-temp-buffer
                                     (insert-file-contents new-file-path)
                                     (buffer-hash)))
              (new-buffer-hash (with-temp-buffer
                                 (insert-file-contents vf)
                                 (buffer-hash))))
          (if (not (string= existed-buffer-hash new-buffer-hash))
              (copy-file vf new-file-path t)
            (message "View %s backup is up to date" view)))
      (copy-file vf new-file-path t))))

;; (defun org-glance-mv--sync-buffer ()
;;   (interactive)
;;   (org-map-entries #'org-glance-view-sync-subtree))

(defun org-glance-view-sync-subtree ()
  (interactive)
  (save-excursion
    (cl-loop while (org-up-heading-safe))
    (with-demoted-errors (run-hooks 'before-materialize-sync-hook))
    (let* ((source -org-glance-src)
           (beg -org-glance-beg)
           (end -org-glance-end)
           (promote-level -org-glance-indent)
           (glance-hash -org-glance-hash)
           ;; (end-old end)
           (mat-hash (org-glance-view-subtree-hash))
           (src-hash (org-glance-view-source-hash)))

      (unless (string= glance-hash src-hash)
        (user-error "Source file modified, please reread"))

      (when (string= glance-hash mat-hash)
        (org-glance-view-not-modified "No changes made in subtree"))

      (when (y-or-n-p "Subtree has been modified. Apply changes?")
        (let ((new-contents (save-restriction
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

          ;; (let ((end-diff (- end end-old)))
          ;;   (org-map-entries
          ;;    (lambda ()
          ;;      (condition-case nil
          ;;          (when (and (> (org-glance-mv--safe-extract-num-property "ORG_GLANCE_BEG") beg)
          ;;                     (string= source (org-glance-mv--safe-extract-property "ORG_GLANCE_SOURCE")))
          ;;            (org-set-property "ORG_GLANCE_BEG" (number-to-string (+ end-diff (org-glance-mv--safe-extract-num-property "ORG_GLANCE_BEG"))))
          ;;            (org-set-property "ORG_GLANCE_END" (number-to-string (+ end-diff (org-glance-mv--safe-extract-num-property "ORG_GLANCE_END"))))
          ;;            (message "Update indentation for headline %s" (org-entry-get (point) "ITEM")))
          ;;        (error (message "Skip headline %s" (org-entry-get (point) "ITEM")))))))

          (with-demoted-errors (run-hooks 'after-materialize-sync-hook)))))))

(defun org-glance-view-subtree-hash ()
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
      (with-temp-buffer
        (org-mode)
        (insert buffer-contents)
        (goto-char (point-min))
        (buffer-hash)))))

(defun org-glance-view-source-hash (&optional src beg end)
  (interactive)
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
          (buffer-hash))))))

(defun org-glance-backup-views (&optional dir)
  (interactive)
  (let ((dir (or dir (read-directory-name "Backup directory: "))))
    (cl-loop for view in org-glance-views
             do (org-glance-mv--backup (symbol-name view) dir))))

(cl-defmacro org-glance-def-view (tag &key bind type
                                  (scope '(agenda-with-archives))
                                  &allow-other-keys)
  (declare (indent 1))
  `(progn
     (cl-pushnew (intern ,tag) org-glance-views)
     (puthash (intern ,tag) (quote ,scope) org-glance-view-scopes)
     (puthash (intern ,tag) ,type org-glance-view-types)
     (cl-assert (listp ,type) nil "Type must be instance of list.")
     (when (quote ,bind)
       (cl-loop for (binding . cmd) in (quote ,bind)
                do (lexical-let ((command-name (intern (format "org-glance-action-%s" cmd)))
                                 (tag ,tag))
                     (global-set-key (kbd binding)
                                     (lambda () (interactive)
                                       (funcall command-name tag))))))))

(cl-defun org-glance-remove-view (tag)
  (setq org-glance-views (cl-remove (intern tag) org-glance-views))
  (remhash (intern tag) org-glance-view-scopes)
  (remhash (intern tag) org-glance-view-types))

(provide-me)
;;; org-glance-views.el ends here
