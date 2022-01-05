(require 'org-glance-module)

(org-glance:require
  ol

  lib.core.headline

  ;; remove these dependencies:
  lib.core.metastore
  lib.core.view
  lib.modes.overview-mode)

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with sync abilities.")

(define-minor-mode org-glance-material-mode
    "A minor mode to be activated only in materialized view editor."
  nil nil org-glance-material-mode-map)

(defvar org-glance-materialized-buffers (make-hash-table))

(defvar-local --org-glance-materialized-headline:classes nil)
(defvar-local --org-glance-materialized-headline:begin nil)
(defvar-local --org-glance-materialized-headline:file nil)
(defvar-local --org-glance-materialized-headline:buffer nil)
(defvar-local --org-glance-materialized-headline:hash nil)
(defvar-local --org-glance-materialized-headline:id nil)
(defvar-local --org-glance-materialized-headline:indent nil)
(defvar-local --org-glance-materialized-headline:password nil)
(defvar-local --org-glance-materialized-headline:read-only nil)

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

(define-key org-glance-material-mode-map (kbd "C-x C-s") #'org-glance-materialized-headline-apply)
(define-key org-glance-material-mode-map (kbd "C-c C-q") #'kill-current-buffer)
(define-key org-glance-material-mode-map (kbd "C-c C-v") #'org-glance-overview)

(define-error 'org-glance-exception:HEADLINE-NOT-MODIFIED "No changes made in materialized view" 'user-error)
(cl-defun org-glance-exception:HEADLINE-NOT-MODIFIED (format &rest args)
  (signal 'org-glance-exception:HEADLINE-NOT-MODIFIED (list (apply #'format-message format args))))

(cl-defun org-glance-materialized-headline-apply (&optional without-relations)
  "Push material buffer changes to all headline origins and views."
  (interactive)
  (unless --org-glance-materialized-headline:read-only
    (save-excursion
      (org-glance-headline:search-buffer-by-id --org-glance-materialized-headline:id)
      (let* ((id --org-glance-materialized-headline:id)
             (source-file --org-glance-materialized-headline:file)
             (source-buffer --org-glance-materialized-headline:buffer)
             (source-hash (cond (source-file (with-temp-buffer
                                               (org-mode)
                                               (insert-file-contents source-file)
                                               (org-glance-headline:search-buffer-by-id id)
                                               (org-glance-headline:hash)))
                                (source-buffer (with-current-buffer source-buffer
                                                 (org-glance-headline:search-buffer-by-id id)
                                                 (org-glance-headline:hash)))))
             (indent-level --org-glance-materialized-headline:indent)
             (glance-hash --org-glance-materialized-headline:hash)
             (current-hash (org-glance-headline:hash))
             (source-headline (org-glance-headline:enrich (org-glance-headline:at-point)
                                :begin --org-glance-materialized-headline:begin
                                :file --org-glance-materialized-headline:file
                                :buffer --org-glance-materialized-headline:buffer))
             (source-classes (org-glance-headline:classes)))

        (unless (string= glance-hash source-hash)
          (org-glance-exception:SOURCE-CORRUPTED (or source-file source-buffer)))

        (when (string= glance-hash current-hash)
          (org-glance-exception:HEADLINE-NOT-MODIFIED (or source-file source-buffer)))

        (save-excursion
          (with-demoted-errors "Hook error: %s"
            (run-hooks 'org-glance-before-materialize-sync-hook)))

        (unless without-relations
          (cl-loop
             for relation in (org-glance-headline-relations)
             for relation-id = (org-element-property :id relation)
             for headline-link = (org-glance-headline-ref)
             for state = (intern (or (org-get-todo-state) ""))
             for done-kws = (mapcar #'intern org-done-keywords)
             for relation-headline = (org-glance-metastore:get-headline (symbol-name relation-id))
             do (save-window-excursion
                  (save-excursion
                    (condition-case nil
                        (org-glance:with-headline-materialized relation-headline
                          (unless (cl-loop
                                     for rr in (org-glance-headline-relations)
                                     if (eq (org-element-property :id rr) (intern id))
                                     return t)
                            (cond ((memq (org-element-property :type relation) '(subtask subtask-done))
                                   (if (memq state done-kws)
                                       (org-glance-headline:add-log-note "- [X] Part of a project %s from %s" headline-link (org-glance-now))
                                     (org-glance-headline:add-log-note "- [ ] Part of a project %s from %s" headline-link (org-glance-now))))
                                  (t (org-glance-headline:add-log-note "- Mentioned in %s on %s" headline-link (org-glance-now))))))
                      (org-glance-exception:HEADLINE-NOT-FOUND (message "Relation not found: %s" relation-id)))))))

        (let ((new-contents (org-glance-headline:with-headline-at-point
                             (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
                               (with-temp-buffer
                                 (org-mode)
                                 (insert buffer-contents)
                                 (goto-char (point-min))
                                 (org-glance-headline:demote indent-level)
                                 (buffer-substring-no-properties (point-min) (point-max)))))))

          (cond (source-file (with-temp-file source-file
                               (org-mode)
                               (insert-file-contents source-file)
                               (org-glance-headline:search-buffer-by-id id)
                               (org-glance-headline:replace-headline-at-point new-contents)))
                (source-buffer (with-current-buffer source-buffer
                                 (org-glance-headline:search-buffer-by-id id)
                                 (org-glance-headline:replace-headline-at-point new-contents))))

          (setq-local --org-glance-materialized-headline:hash (org-glance-materialized-headline:source-hash))

          (cl-loop
             for class in source-classes
             do
               (org-glance-overview:register-headline-in-overview source-headline class)
               (org-glance-overview:register-headline-in-metastore source-headline class))

          (cl-loop
             for class in (seq-difference --org-glance-materialized-headline:classes source-classes)
             do
               (org-glance-overview:remove-headline-from-overview source-headline class)
               (org-glance-overview:remove-headline-from-metastore source-headline class))

          (with-demoted-errors "Hook error: %s"
            (run-hooks 'org-glance-after-materialize-sync-hook))

          (org-glance:log-info "Materialized headline successfully synchronized"))))))

(defun org-glance-materialized-headline:source-hash ()
  (org-glance-headline:hash (org-glance-metastore:get-headline --org-glance-materialized-headline:id)))

(cl-defun org-glance:material-buffer-default-view ()
  "Default restriction of material buffer."
  (org-display-inline-images)

  ;; hide all blocks but pins
  (org-hide-block-all)
  (org-element-map (org-element-parse-buffer) 'special-block
    (lambda (special-block) (when (string= "pin" (org-element-property :type special-block))
                         (save-excursion
                           (goto-char (org-element-property :begin special-block))
                           (org-hide-block-toggle 'off)))))

  (org-cycle-hide-drawers 'all))

(cl-defun org-glance-headline:generate-materialized-buffer (&optional (headline (org-glance-headline:at-point)))
  (generate-new-buffer (concat "org-glance:<" (org-glance-headline:title headline) ">")))

(cl-defun org-glance-headline:materialize (headline &optional (read-only nil))
  "Materialize HEADLINE, prevent synchronization by enabling READ-ONLY."
  (switch-to-buffer
   (with-current-buffer (org-glance-headline:generate-materialized-buffer headline)
     (let ((id (org-glance-headline:id headline))
           (file (org-glance-headline:file headline))
           (buffer (org-glance-headline:buffer headline))
           (begin (org-glance-headline:begin headline))
           (contents (org-glance-headline-contents headline)))

       (when file
         (setq-local default-directory (file-name-directory file))
         (org-glance:log-debug "Set default directory in materialized buffer to %s"
                               default-directory))

       (org-glance:log-debug "Enable `org-mode' and `org-glance-material-mode'")
       (org-mode)
       (org-glance-material-mode +1)

       (save-excursion
         (insert contents))

       (cl-loop for relation in (org-glance-headline-relations)
            )

       (org-glance:material-buffer-default-view)

       (org-glance:log-info "Set local variables")
       (setq --org-glance-materialized-headline:id id
             --org-glance-materialized-headline:classes (org-glance-headline:classes)
             --org-glance-materialized-headline:file file
             --org-glance-materialized-headline:buffer buffer
             --org-glance-materialized-headline:begin begin
             --org-glance-materialized-headline:hash (org-glance-headline:hash)
             --org-glance-materialized-headline:read-only read-only)

       ;; run hooks on original subtree
       (org-glance:log-info "Run `org-glance-after-materialize-hook' on original subtree")
       (run-hooks 'org-glance-after-materialize-hook)

       (org-glance:log-info "Promote subtree to the first level")
       (set (make-local-variable '--org-glance-materialized-headline:indent) (1- (org-glance-headline:level)))
       (org-glance-headline:promote-to-the-first-level)

       (puthash (intern id) (current-buffer) org-glance-materialized-buffers)
       (current-buffer)))))

(cl-defun org-glance-headline:decrypt-headline-at-point-with-local-password ()
  (setq-local --org-glance-materialized-headline:password (read-passwd "Password: "))
  (org-glance-headline:decrypt --org-glance-materialized-headline:password))

(cl-defun org-glance-materialized-headline:support-encrypted-headlines (fn headline &rest args)
  (cond ((org-glance-headline:encrypted-p headline)
         (progn
           (org-glance:log-info "The headline is encrypted")
           (org-glance:log-info "Add `org-glance-after-materialize-hook' to decrypt it")
           (add-hook 'org-glance-after-materialize-hook #'org-glance-headline:decrypt-headline-at-point-with-local-password)

           (unwind-protect
                (apply fn headline args)
             (remove-hook 'org-glance-after-materialize-hook #'org-glance-headline:decrypt-headline-at-point-with-local-password)
             (add-hook 'org-glance-before-materialize-sync-hook
                       (lambda ()
                         (org-glance-headline:demote --org-glance-materialized-headline:indent)
                         (org-glance-headline:encrypt --org-glance-materialized-headline:password)
                         (org-glance-headline:promote-to-the-first-level))
                       0 'local)
             (add-hook 'org-glance-after-materialize-sync-hook
                       (lambda ()
                         (org-glance-headline:demote --org-glance-materialized-headline:indent)
                         (org-glance-headline:decrypt --org-glance-materialized-headline:password)
                         (org-glance-headline:promote-to-the-first-level))
                       0 'local))))
        (t (apply fn headline args))))

(cl-defmacro org-glance:with-headline-materialized (headline &rest forms)
  "Materialize HEADLINE and run FORMS on it."
  (declare (indent 1) (debug t))
  `(let ((materialized-buffer (org-glance-headline:materialize ,headline))
         (org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
     (unwind-protect
          (with-current-buffer materialized-buffer
            (org-glance-headline:search-parents)
            ,@forms)
       (when (buffer-live-p materialized-buffer)
         (with-current-buffer materialized-buffer
           (condition-case nil
               (org-glance-materialized-headline-apply 'without-relations)
             (error nil)))
         (with-demoted-errors "Unable to kill buffer: %s"
           (kill-buffer materialized-buffer))))))

(cl-defun org-glance-materialized-headline-buffer (headline)
  (gethash (intern (org-glance-headline:id headline)) org-glance-materialized-buffers))

(org-glance:provide)
