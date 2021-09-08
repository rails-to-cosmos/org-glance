(require 'org-glance-module)

(defvar org-glance-materialized-view-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with sync abilities.")

(define-minor-mode org-glance-materialized-view-mode
    "A minor mode to be activated only in materialized view editor."
  nil nil org-glance-materialized-view-mode-map)

(defvar --org-glance-view-pwd nil)
(defvar --org-glance-view-src nil)
(defvar --org-glance-view-beg nil)
(defvar --org-glance-view-end nil)
(defvar --org-glance-view-hash nil)
(defvar --org-glance-view-indent nil)

(defcustom org-glance-materialized-view-buffer "*org-glance materialized view*"
  "Default buffer name for materialized view."
  :group 'org-glance
  :type 'string)

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

(define-key org-glance-materialized-view-mode-map (kbd "C-x C-s") #'org-glance-materialized-view:sync)
(define-key org-glance-materialized-view-mode-map (kbd "C-c C-q") #'kill-current-buffer)
(define-key org-glance-materialized-view-mode-map (kbd "C-c C-v") #'org-glance-overview)

(define-error 'org-glance-view-not-modified "No changes made in materialized view" 'user-error)
(cl-defun org-glance-view-not-modified (format &rest args)
  (signal 'org-glance-view-not-modified (list (apply #'format-message format args))))

(cl-defun org-glance-materialized-view:sync ()
  (interactive)
  (save-excursion
    (org-glance:first-level-headline)
    (let* ((source --org-glance-view-src)
           (beg --org-glance-view-beg)
           (end --org-glance-view-end)
           (promote-level --org-glance-view-indent)
           (glance-hash --org-glance-view-hash)
           (mat-hash (org-glance-materialized-view:hash))
           (src-hash (org-glance-materialized-view:source-hash)))

      (unless (string= glance-hash src-hash)
        (message "Source: %s" source)
        (message "Source hash: %s" src-hash)
        (message "Glance hash: %s" glance-hash)
        (org-glance-source-file-corrupted source))

      (when (string= glance-hash mat-hash)
        (org-glance-view-not-modified source))

      (when t ;; (y-or-n-p "Subtree has been modified. Apply changes?")
        (with-demoted-errors (run-hooks 'org-glance-before-materialize-sync-hook))

        (let ((new-contents
               (save-restriction
                 (org-narrow-to-subtree)
                 (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
                   (with-temp-buffer
                     (org-mode)
                     (insert buffer-contents)
                     (goto-char (point-min))
                     (org-glance-headline:demote promote-level)
                     (buffer-substring-no-properties (point-min) (point-max)))))))

          (with-temp-file source
            (org-mode)
            (insert-file-contents source)
            (delete-region beg end)
            (goto-char beg)
            (insert new-contents)
            (setq end (point)))

          (setq-local --org-glance-view-beg beg)
          (setq-local --org-glance-view-end end)
          (setq-local --org-glance-view-hash (org-glance-materialized-view:source-hash))

          (with-demoted-errors (run-hooks 'org-glance-after-materialize-sync-hook))
          (message "Materialized view synchronized successfully"))))))

(defun org-glance-materialized-view:hash ()
  (save-restriction
    (org-narrow-to-subtree)
    (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
      (with-temp-buffer
        (org-mode)
        (insert buffer-contents)
        (goto-char (point-min))
        (--org-glance-headline:promote.deprecated)
        (buffer-hash)))))

(defun org-glance-materialized-view:source-hash ()
  (let ((src --org-glance-view-src)
        (beg --org-glance-view-beg)
        (end --org-glance-view-end))
    (with-temp-buffer
      (insert-file-contents src)
      (let ((subtree (condition-case nil
                         (buffer-substring-no-properties beg end)
                       (error (org-glance-properties-corrupted "Materialized properties corrupted, please reread")))))
        (with-temp-buffer
          (org-mode)
          (insert (s-trim subtree))
          (org-glance:first-level-headline)
          (--org-glance-headline:promote.deprecated)
          (buffer-hash))))))

(org-glance:provide)
