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

(add-hook 'org-glance-before-materialize-sync-hook #'(lambda () (when (eql (marker-buffer org-clock-marker) (current-buffer))
                                                             (setq-local --org-glance-materialized-headline:clock-marker-position (marker-position org-clock-marker))
                                                             (let ((org-log-note-clock-out nil)
                                                                   (org-clock-out-switch-to-state nil))
                                                               (org-clock-out)))))

(add-hook 'org-glance-after-materialize-sync-hook #'(lambda () (when --org-glance-materialized-headline:clock-marker-position
                                                            (goto-char --org-glance-materialized-headline:clock-marker-position)
                                                            (org-clock-in)
                                                            (setq-local --org-glance-materialized-headline:clock-marker-position nil))))

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
           (id --org-glance-materialized-headline:id)
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
          (org-glance-headline:search-buffer-by-id id)
          (let ((beg (org-glance-headline:begin))
                (end (org-glance-headline:end)))
            (delete-region beg end)
            (goto-char beg)
            (insert new-contents)))

        (setq-local --org-glance-materialized-headline:hash (org-glance-materialized-headline:source-hash))

        (with-demoted-errors "Hook error: %s" (run-hooks 'org-glance-after-materialize-sync-hook))
        (message "Materialized headline successfully synchronized")))))

(defun org-glance-materialized-headline:source-hash ()
  (org-glance-headline:hash (org-glance-metastore:get-headline --org-glance-materialized-headline:id)))

(cl-defun org-glance-headline:materialized-buffer-name (&optional (headline (org-glance-headline:at-point)))
  (concat "org-glance:<" (org-glance-headline:title headline) ">"))

(cl-defun org-glance-headline:materialize (&optional (headline (org-glance-headline:at-point)))
  (switch-to-buffer
   (with-current-buffer (generate-new-buffer (org-glance-headline:materialized-buffer-name headline))
     (let* ((id (org-glance-headline:id headline))
            (file (org-glance-headline:file headline))
            (beg (org-glance-headline:begin headline))
            (end (org-glance-headline:end headline))
            (contents (org-glance-headline:contents headline)))
       (org-mode)
       (org-glance-materialized-headline-mode +1)
       (insert contents)
       (goto-char (point-min))
       (org-content 1)
       (org-cycle-hide-drawers 'all)
       (set (make-local-variable '--org-glance-materialized-headline:id) id)
       (set (make-local-variable '--org-glance-materialized-headline:file) file)
       (set (make-local-variable '--org-glance-materialized-headline:begin) beg)
       (set (make-local-variable '--org-glance-materialized-headline:end) end)
       (set (make-local-variable '--org-glance-materialized-headline:hash) (org-glance-headline:hash))
       ;; run hooks on original subtree
       (run-hooks 'org-glance-after-materialize-hook)
       ;; then promote it saving original level
       (set (make-local-variable '--org-glance-materialized-headline:indent) (1- (org-glance-headline:level)))
       (org-glance-headline:promote-to-the-first-level)
       (org-cycle 'contents)
       (current-buffer)))))

(advice-add 'org-glance-headline:materialize :around
            (lambda (fn headline)
              (cond ((org-glance-headline:encrypted? headline)
                     (cl-flet ((decrypt ()
                                 (setq-local --org-glance-materialized-headline:password (read-passwd "Password: "))
                                 (org-glance-headline:decrypt --org-glance-materialized-headline:password)))
                       (add-hook 'org-glance-after-materialize-hook #'decrypt)

                       (unwind-protect (funcall fn headline)
                         (remove-hook 'org-glance-after-materialize-hook #'decrypt))

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
                                 0 'local)))
                    (t (funcall fn headline)))))

(org-glance:provide)
