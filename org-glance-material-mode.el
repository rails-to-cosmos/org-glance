;; -*- lexical-binding: t -*-

(require 's)
(require 'dash)
(require 'org-glance-utils)
(require 'org-glance-headline)
(require 'org-glance-graph)

(declare-function org-glance-overview "org-glance-overview.el")
(declare-function org-glance-overview:register "org-glance-overview.el")
(declare-function org-glance-overview:remove "org-glance-overview.el")
(declare-function org-glance-overview:register-in-archive "org-glance-overview.el")

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
(defvar-local --org-glance-materialized-headline:file nil)
(defvar-local --org-glance-materialized-headline:hash nil)
(defvar-local --org-glance-materialized-headline:id nil)
(defvar-local --org-glance-materialized-headline:indent nil)

(defcustom org-glance-after-materialize-sync-hook nil
  "Runs after a materialized buffer has been synchronized with its source."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'org-glance)

(defcustom org-glance-before-materialize-sync-hook nil
  "Runs before a materialized buffer has been synchronized with its source."
  :options '(copyright-update time-stamp)
  :type 'hook
  :group 'org-glance)

(define-key org-glance-material-mode-map (kbd "C-x C-s") #'org-glance-materialized-headline-apply)
(define-key org-glance-material-mode-map (kbd "C-c C-q") #'kill-current-buffer)
(define-key org-glance-material-mode-map (kbd "C-c C-v") #'org-glance-overview)

(define-error 'org-glance-exception:headline-not-modified "No changes made in materialized view" 'user-error)

(cl-defmacro org-glance:with-headline-materialized (headline &rest forms)
  "Materialize HEADLINE and run FORMS on it. Then sync changes."
  (declare (indent 1) (debug t))
  `(let ((materialized-buffer (org-glance-headline:materialize ,headline))
         (org-link-frame-setup (cl-acons 'file 'find-file org-link-frame-setup)))
     (unwind-protect
         (with-current-buffer materialized-buffer
           ,@forms)
       (when (buffer-live-p materialized-buffer)
         (with-current-buffer materialized-buffer
           (condition-case nil
               (org-glance-materialized-headline-apply)
             (error nil)))
         (with-demoted-errors "Unable to kill buffer: %s"
           (kill-buffer materialized-buffer))))))

(cl-defun org-glance-materialized-headline-apply ()
  "Sync material buffer changes back to graph and overview."
  (interactive)
  (let* ((graph (org-glance-graph))
         (id --org-glance-materialized-headline:id)
         (source-file --org-glance-materialized-headline:file)
         (glance-hash --org-glance-materialized-headline:hash)
         (indent-level --org-glance-materialized-headline:indent))

    ;; Get current contents from buffer
    (save-excursion
      (goto-char (point-min))
      (let* ((current-headline (org-glance-headline:at-point))
             (current-hash (org-glance-headline:hash current-headline)))

        (when (string= glance-hash current-hash)
          (user-error "Headline has not been modified"))

        (with-demoted-errors "Hook error: %s" (run-hooks 'org-glance-before-materialize-sync-hook))

        ;; Build new contents with proper indentation
        (let* ((buffer-contents (buffer-substring-no-properties (point-min) (point-max)))
               (new-contents (with-temp-buffer
                               (org-mode)
                               (insert buffer-contents)
                               (goto-char (point-min))
                               ;; Demote back to original indent
                               (cl-loop repeat indent-level
                                        do (org-with-limited-levels
                                            (org-map-tree 'org-demote)))
                               (s-trim (buffer-substring-no-properties (point-min) (point-max))))))

          ;; Write to source file
          (when source-file
            (with-temp-file source-file
              (org-mode)
              (insert-file-contents source-file)
              (goto-char (point-min))
              (when (org-glance-headline:search-forward id)
                (let ((beg (org-entry-beginning-position))
                      (end (save-excursion (org-end-of-subtree t t))))
                  (delete-region beg end)
                  (goto-char beg)
                  (insert new-contents "\n")))))

          ;; Re-read the headline from source and update graph
          (let ((updated-headline (if source-file
                                      (org-glance--with-file-visited source-file
                                        (goto-char (point-min))
                                        (org-glance-headline:search-forward id)
                                        (org-glance-headline:at-point))
                                    (org-glance-headline--from-string new-contents))))
            (org-glance-graph:add graph updated-headline)
            (org-glance-graph:store-headline graph updated-headline)

            ;; Update overview for each tag
            (let ((tags (org-glance-headline:tags updated-headline)))
              (dolist (tag tags)
                (condition-case nil
                    (if (org-glance-headline:active? updated-headline)
                        (org-glance-overview:register updated-headline tag)
                      (progn
                        (org-glance-overview:remove updated-headline tag)
                        (org-glance-overview:register-in-archive updated-headline tag)))
                  (error nil))))

            (setq-local --org-glance-materialized-headline:hash
                        (org-glance-headline:hash updated-headline)))

          (with-demoted-errors "Hook error: %s" (run-hooks 'org-glance-after-materialize-sync-hook))

          (message "Materialized headline successfully synchronized"))))))

(cl-defun org-glance:material-buffer-default-view ()
  "Default restriction of material buffer."
  (org-display-inline-images)
  (org-cycle-hide-drawers 'all))

(cl-defun org-glance-headline:materialize (headline)
  (cl-check-type headline org-glance-headline)
  (let* ((id (org-glance-headline:id headline))
         (contents (org-glance-headline:contents headline))
         (tags (org-glance-headline:tags headline))
         (hash (org-glance-headline:hash headline))
         (encrypted? (org-glance-headline:encrypted? headline))
         (indent (1- (org-glance-headline:indent headline)))
         (password (when encrypted?
                     (read-passwd "Password: ")))
         (graph (org-glance-graph))
         (source-file (let ((data-path (org-glance-graph:headline-data-path graph id)))
                        (f-join data-path "headline.org")))
         (actual-contents (if encrypted?
                              (org-glance-headline:contents
                               (org-glance-headline:decrypt headline password))
                            contents))
         (materialized-buffer (generate-new-buffer (concat "org-glance:<" (org-glance-headline:title-clean headline) ">"))))

    (puthash (intern id) materialized-buffer org-glance-materialized-buffers)

    (with-current-buffer materialized-buffer
      (add-hook 'kill-buffer-hook
                (let ((id-sym (intern id)))
                  (lambda () (remhash id-sym org-glance-materialized-buffers)))
                nil t)
      (insert actual-contents)

      (org-mode)
      (org-glance-material-mode +1)
      (org-glance:material-buffer-default-view)
      (goto-char (point-min))

      ;; Promote to first level
      (while (looking-at "^\\*\\*")
        (org-promote-subtree))

      (org-content)
      (setq-local default-directory (file-name-directory source-file))

      (setq --org-glance-materialized-headline:id id
            --org-glance-materialized-headline:tags tags
            --org-glance-materialized-headline:file source-file
            --org-glance-materialized-headline:hash hash
            --org-glance-materialized-headline:indent indent)

      (when encrypted?
        (add-hook 'org-glance-before-materialize-sync-hook
                  `(lambda ()
                     (goto-char (point-min))
                     (cl-loop repeat ,indent
                              do (org-with-limited-levels
                                  (org-map-tree 'org-demote)))
                     (save-excursion
                       (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
                             (end (save-excursion (org-end-of-subtree t) (point))))
                         (org-glance--encrypt-region beg end ,password)))
                     (goto-char (point-min))
                     (while (looking-at "^\\*\\*")
                       (org-promote-subtree)))
                  0 'local)

        (add-hook 'org-glance-after-materialize-sync-hook
                  `(lambda ()
                     (goto-char (point-min))
                     (cl-loop repeat ,indent
                              do (org-with-limited-levels
                                  (org-map-tree 'org-demote)))
                     (save-excursion
                       (let ((beg (save-excursion (org-end-of-meta-data t) (point)))
                             (end (save-excursion (org-end-of-subtree t) (point))))
                         (org-glance--decrypt-region beg end ,password)))
                     (goto-char (point-min))
                     (while (looking-at "^\\*\\*")
                       (org-promote-subtree)))
                  0 'local)))

    materialized-buffer))

(provide 'org-glance-material-mode)
