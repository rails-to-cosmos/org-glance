;; -*- lexical-binding: t -*-

(require 's)
(require 'dash)
(require 'org-glance-utils)
(require 'org-glance-headline)
(require 'org-glance-overview)

(declare-function org-glance--now "org-glance-utils.el")
(declare-function org-glance-headline-reference "org-glance-metadata.el" (&optional (type 'org-glance-visit)))
(declare-function org-glance-headline-relations "org-glance-metadata.el" ())
(declare-function org-glance-headline:add-log-note "org-glance-headline.el" (string &rest objects))
(declare-function org-glance-headline:at-point "org-glance-headline.el" ())
(declare-function org-glance-headline:contents "org-glance-headline.el" (headline))
(declare-function org-glance-headline:decrypt "org-glance-headline.el" (&optional password))
(declare-function org-glance-headline:demote "org-glance-headline.el" (level))
(declare-function org-glance-headline:encrypt "org-glance-headline.el" (&optional password))
(declare-function org-glance-headline:hash "org-glance-headline.el" (headline))
(declare-function org-glance-headline:plain-title "org-glance-headline.el" (headline))
(declare-function org-glance-headline:promote-to-the-first-level "org-glance-headline.el" (&optional password))
(declare-function org-glance-headline:replace-headline-at-point "org-glance-headline.el" (contents))
(declare-function org-glance-headline:search-buffer-by-id "org-glance-headline.el" (id))
(declare-function org-glance-headline:search-parents "org-glance-headline.el" ())
(declare-function org-glance-headline:with-headline-at-point "org-glance-headline.el" (&rest forms))
(declare-function org-glance-headline:with-narrowed-headline "org-glance-headline.el" (headline &rest forms))
(declare-function org-glance-metadata:headline-metadata "org-glance-metadata.el" (id))
(declare-function s-split-up-to "s.el" (separator s n &optional omit-nulls))

(defvar org-glance-material-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with sync abilities.")

(define-minor-mode org-glance-material-mode
  "A minor mode to be activated only in materialized view editor."
  :lighter nil
  :global nil
  :group 'glance
  :keymap org-glance-material-mode-map)

(defvar org-glance-materialized-buffers (make-hash-table))

(defvar-local --org-glance-materialized-headline:tags nil)
(defvar-local --org-glance-materialized-headline:begin nil)
(defvar-local --org-glance-materialized-headline:file nil)
(defvar-local --org-glance-materialized-headline:buffer nil)
(defvar-local --org-glance-materialized-headline:hash nil)
(defvar-local --org-glance-materialized-headline:id nil)
(defvar-local --org-glance-materialized-headline:indent nil)
(defvar-local --org-glance-materialized-headline:password nil)

(defcustom org-glance-after-materialize-sync-hook nil
  "Runs after a materialized buffer has been synchronized with its source file."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'org-glance)

(defcustom org-glance-before-materialize-sync-hook nil
  "Runs before a materialized buffer has been synchronized with its source file."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'org-glance)

(define-key org-glance-material-mode-map (kbd "C-x C-s") #'org-glance-materialized-headline-apply)
(define-key org-glance-material-mode-map (kbd "C-c C-q") #'kill-current-buffer)
(define-key org-glance-material-mode-map (kbd "C-c C-v") #'org-glance-overview)

(define-error 'org-glance-exception:source-corrupted "No changes made in materialized view" 'user-error)
(cl-defun org-glance-exception:source-corrupted (format &rest args)
  (signal 'org-glance-exception:source-corrupted (list (apply #'format-message format args))))

(define-error 'org-glance-exception:headline-not-modified "No changes made in materialized view" 'user-error)
(cl-defun org-glance-exception:headline-not-modified (format &rest args)
  (signal 'org-glance-exception:headline-not-modified (list (apply #'format-message format args))))

(cl-defmacro org-glance:with-headline-materialized (headline &rest forms)
  "Materialize HEADLINE and run FORMS on it. Then change all related overviews."
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
               (org-glance-materialized-headline-apply)
             (error nil)))
         (with-demoted-errors "Unable to kill buffer: %s"
           (kill-buffer materialized-buffer))))
     ))

(cl-defun org-glance-materialized-headline-apply ()
  "Sync material buffer changes across all headline origins and views."
  (interactive)
  (save-excursion

    (let* ((headline (org-glance-headline:search-buffer-by-id --org-glance-materialized-headline:id))
           (id --org-glance-materialized-headline:id)
           (source-file --org-glance-materialized-headline:file)
           (source-hash (with-temp-buffer
                          (org-mode)
                          (insert-file-contents source-file)
                          (-> (org-glance-headline:search-buffer-by-id id)
                              (org-glance-headline:hash))))
           (indent-level --org-glance-materialized-headline:indent)
           (glance-hash --org-glance-materialized-headline:hash)
           (current-hash (org-glance-headline:hash headline))
           (source-headline (org-glance-headline:update headline
                              :begin --org-glance-materialized-headline:begin
                              :file --org-glance-materialized-headline:file
                              :buffer --org-glance-materialized-headline:buffer))
           (source-tags (org-glance-headline:tags source-headline))
           (source-active? (org-glance-headline:active? source-headline)))

      (unless (string= glance-hash source-hash)
        (error "Source file corrupted, please reread: %s" source-file))

      (when (string= glance-hash current-hash)
        (user-error "Headline has not been modified: %s" source-file))

      (with-demoted-errors "Hook error: %s" (run-hooks 'org-glance-before-materialize-sync-hook))

      ;; (unless without-relations
      ;;   (cl-loop
      ;;    with relations = (org-glance-headline-relations)
      ;;    with progress-reporter = (make-progress-reporter "Updating relations... " 0 (length relations))
      ;;    for relation in relations
      ;;    for progress from 0
      ;;    for relation-id = (org-element-property :id relation)
      ;;    for headline-ref = (org-glance-headline-reference)
      ;;    for state = (intern (or (org-get-todo-state) ""))
      ;;    for done-kws = (mapcar #'intern org-done-keywords)
      ;;    for relation-headline = (org-glance-metadata:headline-metadata relation-id)
      ;;    do (save-window-excursion
      ;;         (save-excursion
      ;;           (progress-reporter-update progress-reporter progress)
      ;;           (condition-case nil
      ;;               (org-glance:with-headline-materialized relation-headline
      ;;                 (unless (cl-loop
      ;;                          for rr in (org-glance-headline-relations)
      ;;                          if (eq (org-element-property :id rr) (intern id))
      ;;                          return t)
      ;;                   (org-glance-headline:add-log-note "- Mentioned in %s on %s" headline-ref (org-glance--now))))
      ;;             (org-glance-headline:not-found! (message "Relation not found: %s" relation-id)))
      ;;           (redisplay)))
      ;;    finally (progress-reporter-done progress-reporter)))

      (let ((new-contents (org-glance-headline:with-headline-at-point
                           (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
                             (with-temp-buffer
                               (org-mode)
                               (insert buffer-contents)
                               (goto-char (point-min))
                               (org-glance-headline:demote indent-level)
                               (s-trim (buffer-substring-no-properties (point-min) (point-max))))))))
        ;; FIXME: headline mutation!
        (cond (source-file (with-temp-file source-file
                             (org-mode)
                             (insert-file-contents source-file)
                             (org-glance-headline:search-buffer-by-id id)
                             (org-glance-headline:replace-headline-at-point new-contents)))
              (source-buffer (with-current-buffer source-buffer
                               (org-glance-headline:search-buffer-by-id id)
                               (org-glance-headline:replace-headline-at-point new-contents))))

        (setq-local --org-glance-materialized-headline:hash (org-glance-materialized-headline:source-hash))

        (cond ((memq 'archive source-tags)
               (cl-loop for tag in source-tags
                        if (eq tag 'archive)
                        do (progn (org-glance-overview:register-headline-in-overview source-headline tag)
                                  (org-glance-overview:register-headline-in-metadata source-headline tag))
                        else
                        do (progn (org-glance-overview:remove-headline-from-overview source-headline tag)
                                  (org-glance-overview:remove-headline-from-metadata source-headline tag))))

              ((not source-active?) (cl-loop for tag in source-tags
                                             do (progn (org-glance-overview:remove-headline-from-overview source-headline tag)
                                                       (org-glance-overview:remove-headline-from-metadata source-headline tag)
                                                       (org-glance-overview:register-headline-in-archive source-headline tag))))
              (t (cl-loop for tag in source-tags
                          do (progn (org-glance-overview:register-headline-in-overview source-headline tag)
                                    (org-glance-overview:register-headline-in-metadata source-headline tag)))
                 (cl-loop for tag in (seq-difference --org-glance-materialized-headline:tags source-tags)
                          do (progn (org-glance-overview:remove-headline-from-overview source-headline tag)
                                    (org-glance-overview:remove-headline-from-metadata source-headline tag)))))

        (with-demoted-errors "Hook error: %s" (run-hooks 'org-glance-after-materialize-sync-hook))

        (message "Materialized headline successfully synchronized")))))

(defun org-glance-materialized-headline:source-hash ()
  (-> (org-glance-metadata:headline --org-glance-materialized-headline:id)
      (org-glance-headline:hash)))

(cl-defun org-glance:material-buffer-default-view ()
  "Default restriction of material buffer."
  (org-display-inline-images)
  (org-cycle-hide-drawers 'all))

(cl-defun org-glance-headline:generate-materialized-buffer (headline)
  (cl-check-type headline (or org-glance-headline org-glance-headline-metadata))
  (generate-new-buffer (concat "org-glance:<" (org-glance-headline:plain-title headline) ">")))

(cl-defun org-glance-headline:materialize (headline)
  (cl-check-type headline org-glance-headline)
  (let* ((id (org-glance-headline:id headline))
         (file (org-glance-headline:file-name headline))
         (buffer (org-glance-headline:buffer headline))
         (begin (org-glance-headline:begin headline))
         (tags (org-glance-headline:tags headline))
         (hash (org-glance-headline:hash headline))
         (encrypted? (org-glance-headline:encrypted? headline))
         (indent (1- (org-glance-headline:level headline)))
         (password (when encrypted?
                     (read-passwd "Password: ")))
         (contents (if encrypted?
                       (org-glance-headline:with-narrowed-headline headline
                         (org-glance-headline:decrypt password)
                         (buffer-string))
                     (org-glance-headline:contents headline)))

         (materialized-buffer (org-glance-headline:generate-materialized-buffer headline)))

    (puthash (intern id) materialized-buffer org-glance-materialized-buffers)

    (with-current-buffer materialized-buffer
      (insert contents)

      (org-mode)
      (org-glance-material-mode +1)
      (org-glance:material-buffer-default-view)
      (goto-char (point-min))
      (org-glance-headline:promote-to-the-first-level)
      (org-content)
      (setq-local default-directory (file-name-directory file))

      (setq --org-glance-materialized-headline:id id
            --org-glance-materialized-headline:tags tags
            --org-glance-materialized-headline:file file
            --org-glance-materialized-headline:buffer buffer
            --org-glance-materialized-headline:begin begin
            --org-glance-materialized-headline:hash hash
            --org-glance-materialized-headline:indent indent)

      (when encrypted?
        (add-hook 'org-glance-before-materialize-sync-hook
                  `(lambda ()
                     (org-glance-headline:demote ,indent)
                     (org-glance-headline:encrypt ,password)
                     (org-glance-headline:promote-to-the-first-level))
                  0 'local)

        (add-hook 'org-glance-after-materialize-sync-hook
                  `(lambda ()
                     (org-glance-headline:demote ,indent)
                     (org-glance-headline:decrypt ,password)
                     (org-glance-headline:promote-to-the-first-level))
                  0 'local)))

    materialized-buffer

    ;; (when update-relations
    ;;   (cl-loop while (re-search-forward (concat "[[:blank:]]?" org-link-any-re) nil t)
    ;;            collect (let* ((standard-output 'ignore)
    ;;                           (link (s-split-up-to ":" (substring-no-properties (or (match-string 2) "")) 1))
    ;;                           (type (intern (car link)))
    ;;                           (id (cadr link)))

    ;;                      (when (memq type '(org-glance-visit
    ;;                                         org-glance-open
    ;;                                         org-glance-overview
    ;;                                         org-glance-state))
    ;;                        (delete-region (match-beginning 0) (match-end 0)))

    ;;                      (when (memq type '(org-glance-visit org-glance-open))
    ;;                        (when-let (headline (org-glance-metadata:headline-metadata id))
    ;;                          (goto-char (match-beginning 0))
    ;;                          (insert
    ;;                           (if (or (bolp) (looking-back "[[:blank:]]" 1))
    ;;                               ""
    ;;                             " ")
    ;;                           (org-glance-headline:with-narrowed-headline headline
    ;;                             (org-glance-headline-reference type))))))))

    ))

(provide 'org-glance-material-mode)
