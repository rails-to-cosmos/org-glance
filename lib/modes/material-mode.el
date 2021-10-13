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

(defvar --org-glance-materialized-headline:classes nil)
(defvar --org-glance-materialized-headline:begin nil)
(defvar --org-glance-materialized-headline:end nil)
(defvar --org-glance-materialized-headline:file nil)
(defvar --org-glance-materialized-headline:hash nil)
(defvar --org-glance-materialized-headline:id nil)
(defvar --org-glance-materialized-headline:indent nil)
(defvar --org-glance-materialized-headline:password nil)
(defvar --org-glance-materialized-headline:clock-marker-position nil)

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

(cl-defun org-glance-metastore:get-headlines (id)
  "Get full headline by ID."
  (cl-loop
     for class being the hash-keys of org-glance:classes
     for metastore = (->> class
                          org-glance:get-class
                          org-glance-view:metastore-location
                          org-glance-metastore:read)
     for headline = (gethash id metastore)
     when headline
     collect (-> headline
                 (org-glance-headline:deserialize)
                 (org-glance-headline:enrich :ORG_GLANCE_ID id))))

(add-hook 'org-glance-before-materialize-sync-hook
          #'(lambda () (when (eql (marker-buffer org-clock-marker) (current-buffer))
                    (setq-local --org-glance-materialized-headline:clock-marker-position (marker-position org-clock-marker))
                    (let ((org-log-note-clock-out nil)
                          (org-clock-out-switch-to-state nil))
                      (org-clock-out)))))

(add-hook 'org-glance-after-materialize-sync-hook
          #'(lambda () (when --org-glance-materialized-headline:clock-marker-position
                    (goto-char --org-glance-materialized-headline:clock-marker-position)
                    (org-clock-in)
                    (setq-local --org-glance-materialized-headline:clock-marker-position nil))))

(add-hook 'org-glance-after-materialize-sync-hook
          #'(lambda () (let ((headline (org-glance-headline:enrich
                                      (org-glance-headline:search-buffer-by-id --org-glance-materialized-headline:id)
                                    :begin --org-glance-materialized-headline:begin
                                    :file --org-glance-materialized-headline:file)))
                    (org-glance:log-debug "Updated headline: %s" headline)
                    (cl-loop
                       for class in --org-glance-materialized-headline:classes
                       when (org-glance:get-class class)
                       do
                         (org-glance:log-debug "Update overview %s" class)
                         (org-glance-overview:register-headline-in-overview headline class)
                         (org-glance:log-debug "Update metastore %s" class)
                         (org-glance-overview:register-headline-in-metastore headline class)
                         (redisplay))

                    (cl-loop
                       for class in (seq-difference --org-glance-materialized-headline:classes
                                                    (org-glance-headline:classes))
                       do
                         (org-glance-overview:remove-headline-from-overview headline class)
                         (org-glance-overview:remove-headline-from-metastore headline class)))))

(define-key org-glance-material-mode-map (kbd "C-x C-s") #'org-glance-materialized-headline:sync)
(define-key org-glance-material-mode-map (kbd "C-c C-q") #'kill-current-buffer)
(define-key org-glance-material-mode-map (kbd "C-c C-v") #'org-glance-overview)

(define-error 'org-glance-exception:HEADLINE-NOT-MODIFIED "No changes made in materialized view" 'user-error)
(cl-defun org-glance-exception:HEADLINE-NOT-MODIFIED (format &rest args)
  (signal 'org-glance-exception:HEADLINE-NOT-MODIFIED (list (apply #'format-message format args))))

(cl-defun org-glance-materialized-headline:sync ()
  (interactive)
  (save-excursion
    (org-glance-headline:search-parents)
    (let* ((source --org-glance-materialized-headline:file)
           (id --org-glance-materialized-headline:id)
           (indent-level --org-glance-materialized-headline:indent)
           (glance-hash --org-glance-materialized-headline:hash)
           (current-hash (org-glance-headline:hash))
           (source-hash (with-temp-buffer
                          (insert-file-contents source)
                          (org-glance-headline:search-buffer-by-id id)
                          (org-glance-headline:hash))))

      (unless (string= glance-hash source-hash)
        (org-glance-exception:SOURCE-FILE-CORRUPTED source))

      (when (string= glance-hash current-hash)
        (org-glance-exception:HEADLINE-NOT-MODIFIED source))

      (with-demoted-errors "Hook error: %s" (run-hooks 'org-glance-before-materialize-sync-hook))
      (let ((new-contents (save-restriction
                            (org-narrow-to-subtree)
                            (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
                              (with-temp-buffer
                                (org-mode)
                                (insert buffer-contents)
                                (goto-char (point-min))
                                (org-glance-headline:demote indent-level)
                                (buffer-substring-no-properties (point-min) (point-max)))))))

        (with-temp-file source
          (org-mode)
          (insert-file-contents source)
          (org-glance-headline:search-buffer-by-id id)
          (let ((beg (org-glance-headline:begin))
                (end (org-glance-headline:end)))
            (delete-region beg end)
            (goto-char beg)
            (insert new-contents)))

        ;; TODO: get rid of metastore knowledge here
        (setq-local --org-glance-materialized-headline:hash (org-glance-materialized-headline:source-hash))

        (with-demoted-errors "Hook error: %s" (run-hooks 'org-glance-after-materialize-sync-hook))
        (org-glance:log-info "Materialized headline successfully synchronized")))))

(defun org-glance-materialized-headline:source-hash ()
  (org-glance-headline:hash (org-glance-metastore:get-headline --org-glance-materialized-headline:id)))

(cl-defun org-glance-headline:generate-materialized-buffer (&optional (headline (org-glance-headline:at-point)))
  (generate-new-buffer (concat "org-glance:<" (org-glance-headline:title headline) ">")))

(cl-defun org-glance-headline:materialize (&optional (headline (org-glance-headline:at-point)))
  (org-glance:log-info "Materialize headline %s" headline)
  (switch-to-buffer
   (with-current-buffer (org-glance-headline:generate-materialized-buffer headline)
     (let* ((id (org-glance-headline:id headline))
            (file (org-glance-headline:file headline))
            (beg (org-glance-headline:begin headline))
            (end (org-glance-headline:end headline))
            (contents (org-glance-headline:contents headline)))
       (setq-local default-directory (file-name-directory file))
       (org-glance:log-info "Headline ID: \"%s\"" id)
       (org-glance:log-info "Headline FILE: \"%s\"" file)
       (org-glance:log-info "Headline BEG: \"%s\"" beg)
       (org-glance:log-info "Headline END: \"%s\"" end)

       (org-glance:log-info "Enable `org-mode' and `org-glance-material-mode'")
       (org-mode)
       (org-glance-material-mode +1)

       (org-glance:log-info "Insert headline contents")
       (insert contents)
       (goto-char (point-min))
       (org-content 1)
       (org-cycle-hide-drawers 'all)

       (org-glance:log-info "Set local variables")
       (set (make-local-variable '--org-glance-materialized-headline:id) id)
       (set (make-local-variable '--org-glance-materialized-headline:classes) (org-glance-headline:classes))
       (set (make-local-variable '--org-glance-materialized-headline:file) file)
       (set (make-local-variable '--org-glance-materialized-headline:begin) beg)
       (set (make-local-variable '--org-glance-materialized-headline:end) end)
       (set (make-local-variable '--org-glance-materialized-headline:hash) (org-glance-headline:hash))

       ;; run hooks on original subtree
       (org-glance:log-info "Run `org-glance-after-materialize-hook' on original subtree")
       (run-hooks 'org-glance-after-materialize-hook)

       (org-glance:log-info "Promote subtree to the first level")
       (set (make-local-variable '--org-glance-materialized-headline:indent) (1- (org-glance-headline:level)))
       (org-glance-headline:promote-to-the-first-level)

       (org-cycle 'contents)
       (puthash (intern id) (current-buffer) org-glance-materialized-buffers)
       (current-buffer)))))

(advice-add 'org-glance-headline:materialize :around
            (lambda (fn headline)
              (cond ((org-glance-headline:encrypted? headline)
                     (cl-flet ((decrypt ()
                                 (setq-local --org-glance-materialized-headline:password (read-passwd "Password: "))
                                 (org-glance-headline:decrypt --org-glance-materialized-headline:password)))

                       (org-glance:log-info "The headline is encrypted")
                       (org-glance:log-info "Add `org-glance-after-materialize-hook' to decrypt it")
                       (add-hook 'org-glance-after-materialize-hook #'decrypt)

                       (unwind-protect
                            (funcall fn headline)
                         (remove-hook 'org-glance-after-materialize-hook #'decrypt)
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
                    (t (funcall fn headline)))))

(cl-defmacro org-glance-headline:with-materialized-headline (headline &rest forms)
  "Materialize HEADLINE and run FORMS on it."
  (declare (indent 1) (debug t))
  `(let ((materialized-buffer (org-glance-headline:materialize ,headline))
         (org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
     (unwind-protect
          (with-current-buffer materialized-buffer
            ,@forms)
       (when (buffer-live-p materialized-buffer)
         (with-current-buffer materialized-buffer
           (condition-case nil
               (org-glance-materialized-headline:sync)
             (error nil)))
         (with-demoted-errors "Unable to kill buffer: %s"
           (kill-buffer materialized-buffer))))))

(cl-defun org-glance-headline:materialized-buffer (headline)
  (gethash (intern (org-glance-headline:id headline)) org-glance-materialized-buffers))

(org-glance:provide)
