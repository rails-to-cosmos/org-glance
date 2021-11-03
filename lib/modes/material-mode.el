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
(defvar --org-glance-materialized-headline:file nil)
(defvar --org-glance-materialized-headline:buffer nil)
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
                                    :file --org-glance-materialized-headline:file
                                    :buffer --org-glance-materialized-headline:buffer)))
                    (org-glance:log-debug "Updated headline: %s" headline)

                    (cl-loop
                       for class in (org-glance-headline:classes)
                       do
                         (org-glance:log-debug "Update overview %s" class)
                         (org-glance-overview:register-headline-in-overview headline class)
                         (org-glance:log-debug "Update metastore %s" class)
                         (org-glance-overview:register-headline-in-metastore headline class)
                         (redisplay))

                    (cl-loop
                       for class in (seq-difference --org-glance-materialized-headline:classes (org-glance-headline:classes))
                       do
                         (org-glance:log-debug "Remove from overview %s" class)
                         (org-glance-overview:remove-headline-from-overview headline class)
                         (org-glance:log-debug "Remove from metastore %s" class)
                         (org-glance-overview:remove-headline-from-metastore headline class)))))

(define-key org-glance-material-mode-map (kbd "C-x C-s") #'org-glance-materialized-headline:sync)
(define-key org-glance-material-mode-map (kbd "C-c C-q") #'kill-current-buffer)
(define-key org-glance-material-mode-map (kbd "C-c C-v") #'org-glance-overview)

(define-error 'org-glance-exception:HEADLINE-NOT-MODIFIED "No changes made in materialized view" 'user-error)
(cl-defun org-glance-exception:HEADLINE-NOT-MODIFIED (format &rest args)
  (signal 'org-glance-exception:HEADLINE-NOT-MODIFIED (list (apply #'format-message format args))))

(cl-defun org-glance-materialized-headline:sync ()
  "Apply material buffer changes to metadata and all headline views."
  (interactive)
  (save-excursion
    (org-glance-headline:search-parents)
    (let* ((id --org-glance-materialized-headline:id)
           (source-file --org-glance-materialized-headline:file)
           (source-buffer --org-glance-materialized-headline:buffer)
           (source-hash (cond (source-file (with-temp-buffer
                                             (insert-file-contents source-file)
                                             (org-glance-headline:search-buffer-by-id id)
                                             (org-glance-headline:hash)))
                              (source-buffer (with-current-buffer source-buffer
                                               (org-glance-headline:search-buffer-by-id id)
                                               (org-glance-headline:hash)))))
           (indent-level --org-glance-materialized-headline:indent)
           (glance-hash --org-glance-materialized-headline:hash)
           (current-hash (org-glance-headline:hash)))

      (unless (string= glance-hash source-hash)
        (org-glance-exception:SOURCE-CORRUPTED (or source-file source-buffer)))

      (when (string= glance-hash current-hash)
        (org-glance-exception:HEADLINE-NOT-MODIFIED (or source-file source-buffer)))

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

        (cond (source-file (with-temp-file source-file
                             (org-mode)
                             (insert-file-contents source-file)
                             (org-glance-headline:search-buffer-by-id id)
                             (let ((beg (org-glance-headline:begin))
                                   (end (save-excursion (org-end-of-subtree t))))
                               (delete-region beg end)
                               (goto-char beg)
                               (insert new-contents))))
              (source-buffer (with-current-buffer source-buffer
                               (org-glance-headline:search-buffer-by-id id)
                               (let ((beg (org-glance-headline:begin))
                                     (end (save-excursion (org-end-of-subtree t))))
                                 (delete-region beg end)
                                 (goto-char beg)
                                 (insert new-contents)))))

        ;; TODO: get rid of metastore knowledge here
        (setq-local --org-glance-materialized-headline:hash (org-glance-materialized-headline:source-hash))

        (with-demoted-errors "Hook error: %s" (run-hooks 'org-glance-after-materialize-sync-hook))
        (org-glance:log-info "Materialized headline successfully synchronized")))))

(defun org-glance-materialized-headline:source-hash ()
  (org-glance-headline:hash (org-glance-metastore:get-headline --org-glance-materialized-headline:id)))

(cl-defun org-glance-headline:generate-materialized-buffer (&optional (headline (org-glance-headline:at-point)))
  (generate-new-buffer (concat "org-glance:<" (org-glance-headline:title headline) ">")))

(cl-defun org-glance-headline:materialize (headline &optional (read-only nil))
  "Materialize HEADLINE.
READ-ONLY materialization means side-effect-free behaviour: `org-blocker-hook' will be deactivated."
  (org-glance:log-info "Materialize headline %s" headline)
  (switch-to-buffer
   (with-current-buffer (org-glance-headline:generate-materialized-buffer headline)
     (let* ((id (org-glance-headline:id headline))
            (file (org-glance-headline:file headline))
            (buffer (org-glance-headline:buffer headline))
            (begin (org-glance-headline:begin headline))
            (contents (org-glance-headline:contents headline)))

       (org-glance:log-debug "Headline ID: \"%s\"" id)
       (org-glance:log-debug "Headline FILE: \"%s\"" file)
       (org-glance:log-debug "Headline BUFFER: \"%s\"" buffer)
       (org-glance:log-debug "Headline BEGIN: %d" begin)

       (when file
         (setq-local default-directory (file-name-directory file)))

       (org-glance:log-debug "Enable `org-mode' and `org-glance-material-mode'")
       (org-mode)
       (org-glance-material-mode +1)

       (org-glance:log-debug "Insert headline contents")
       (insert contents)
       (goto-char (point-min))
       (org-content 1)
       (org-cycle-hide-drawers 'all)

       (org-glance:log-info "Set local variables")
       (set (make-local-variable '--org-glance-materialized-headline:id) id)
       (set (make-local-variable '--org-glance-materialized-headline:classes) (org-glance-headline:classes))
       (set (make-local-variable '--org-glance-materialized-headline:file) file)
       (set (make-local-variable '--org-glance-materialized-headline:buffer) buffer)
       (set (make-local-variable '--org-glance-materialized-headline:begin) begin)
       (set (make-local-variable '--org-glance-materialized-headline:hash) (org-glance-headline:hash))

       (unless read-only
         (add-hook 'org-blocker-hook #'org-glance-headline:material-blocker-hook 0 'local))

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
            (lambda (fn headline &rest args)
              (cond ((org-glance-headline:encrypted? headline)
                     (cl-flet ((decrypt ()
                                 (setq-local --org-glance-materialized-headline:password (read-passwd "Password: "))
                                 (org-glance-headline:decrypt --org-glance-materialized-headline:password)))

                       (org-glance:log-info "The headline is encrypted")
                       (org-glance:log-info "Add `org-glance-after-materialize-hook' to decrypt it")
                       (add-hook 'org-glance-after-materialize-hook #'decrypt)

                       (unwind-protect
                            (apply fn headline args)
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
                    (t (apply fn headline args)))))

(cl-defmacro org-glance-headline:with-materialized-headline (headline &rest forms)
  "Materialize HEADLINE and run FORMS on it."
  (declare (indent 1) (debug t))
  `(let ((materialized-buffer (org-glance-headline:materialize ,headline 'read-only))
         (org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
     (unwind-protect
          (with-current-buffer materialized-buffer
            (org-glance-headline:search-parents)
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

(cl-defun org-glance-headline:material-blocker-hook (change-plist)
  (if (and
       org-glance-clone-on-repeat-p
       (eql 'todo-state-change (plist-get change-plist :type))
       (member (plist-get change-plist :to) org-done-keywords)
       (org-glance-headline:repeated-p))
      (let* ((headline (org-glance-headline:at-point))
             (classes (org-glance-headline:classes))
             (captured-headline (org-glance:capture-headline-at-point (car classes) :remove-original nil))
             (from-state (plist-get change-plist :from))
             (to-state (plist-get change-plist :to)))

        (cl-loop
           for class in classes
           do
             (org-glance-overview:register-headline-in-metastore captured-headline class)
             (org-glance-overview:register-headline-in-overview captured-headline class))

        (if (null from-state)
            (progn
              (goto-char (org-glance-headline:begin))
              (while (looking-at "[* ]") (forward-char))
              (insert (substring-no-properties to-state) " "))
          (replace-string (substring-no-properties from-state)
                          (substring-no-properties to-state)
                          nil
                          (org-glance-headline:begin)
                          (save-excursion (end-of-line) (point))))

        (org-glance-materialized-headline:sync)
        (bury-buffer)
        (lexical-let ((buffer (current-buffer))) ;; blocker should not modify buffer, but idle timer could
          (run-with-idle-timer 1 nil #'(lambda () (kill-buffer buffer))))

        ;; post-process headline here
        ;; remove unnecessary data
        (org-glance-headline:with-materialized-headline captured-headline
          (org-todo to-state)
          (org-end-of-meta-data)
          (kill-region (point) (point-max)))

        (org-glance-headline:materialize captured-headline)
        nil)
    t))

(org-glance:provide)
