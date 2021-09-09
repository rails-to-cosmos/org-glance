(require 'org-glance-module)

(defvar org-glance-materialized-headline-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with sync abilities.")

(define-minor-mode org-glance-materialized-headline-mode
    "A minor mode to be activated only in materialized view editor."
  nil nil org-glance-materialized-headline-mode-map)

(defvar --org-glance-materialized-headline:begin nil)
(defvar --org-glance-materialized-headline:end nil)
(defvar --org-glance-materialized-headline:file nil)
(defvar --org-glance-materialized-headline:hash nil)
(defvar --org-glance-materialized-headline:id nil)
(defvar --org-glance-materialized-headline:indent nil)
(defvar --org-glance-materialized-headline:password nil)

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

(cl-defun --sync-overviews ()
  (when-let (headlines (org-glance-metastore:get-headlines --org-glance-materialized-headline:id))
    (cl-loop
       for headline in headlines
       for class = (org-glance-headline:class headline)
       do
         (message "Update %s overview" class)
         (org-glance-overview:register-headline-in-overview headline class)
         (redisplay))))

(add-hook 'org-glance-after-materialize-sync-hook '--sync-overviews)

(define-key org-glance-materialized-headline-mode-map (kbd "C-x C-s") #'org-glance-materialized-headline:sync)
(define-key org-glance-materialized-headline-mode-map (kbd "C-c C-q") #'kill-current-buffer)
(define-key org-glance-materialized-headline-mode-map (kbd "C-c C-v") #'org-glance-overview)

(define-error 'org-glance-exception:headline-not-modified "No changes made in materialized view" 'user-error)
(cl-defun org-glance-exception:headline-not-modified (format &rest args)
  (signal 'org-glance-exception:headline-not-modified (list (apply #'format-message format args))))

(cl-defun org-glance-materialized-headline:sync ()
  (interactive)
  (save-excursion
    (org-glance-headline:goto-beginning-of-current-headline)
    (let* ((source --org-glance-materialized-headline:file)
           (beg --org-glance-materialized-headline:begin)
           (end --org-glance-materialized-headline:end)
           (indent-level --org-glance-materialized-headline:indent)
           (glance-hash --org-glance-materialized-headline:hash)
           (current-hash (org-glance-headline:hash))
           (source-hash (org-glance-materialized-headline:source-hash)))

      (unless (string= glance-hash source-hash)
        (org-glance-exception:source-file-corrupted source))

      (when (string= glance-hash current-hash)
        (org-glance-exception:headline-not-modified source))

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
          (delete-region beg end)
          (goto-char beg)
          (insert new-contents)
          (setq end (point)))

        (setq-local --org-glance-materialized-headline:begin beg)
        (setq-local --org-glance-materialized-headline:end end)
        (setq-local --org-glance-materialized-headline:hash (org-glance-materialized-headline:source-hash))

        (with-demoted-errors "Hook error: %s" (run-hooks 'org-glance-after-materialize-sync-hook))
        (message "Materialized headline successfully synchronized")))))

(defun org-glance-materialized-headline:source-hash ()
  (let ((src --org-glance-materialized-headline:file)
        (beg --org-glance-materialized-headline:begin))
    (with-temp-buffer
      (org-mode)
      (insert-file-contents src)
      (goto-char beg)
      (org-glance-headline:hash))))

(org-glance:provide)
