;; -*- lexical-binding: t -*-

(require 'highlight)
(require 'org-attach)
(require 'org-capture)
(require 'ol)
(require 'org-agenda)
(require 'org-element)
(require 'org-macs)

(require 'org-glance-headline)
(require 'org-glance-graph)
(require 'org-glance-tag)
(require 'org-glance-utils)

(declare-function org-glance:create-tag "org-glance.el")
(declare-function org-glance:tags-sorted "org-glance.el")
(declare-function org-glance:open "org-glance.el")
(declare-function org-glance-tags:completing-read "org-glance.el")
(declare-function org-glance-scope "org-glance.el")
(defvar org-glance-tags)

(defcustom org-glance-clocktable-properties
  (list :maxlevel 2
        :properties '("CLOSED" "SCHEDULED")
        :link t)
  "Default clocktable properties for glance overview")

(defface org-glance-headline-changed-face
  '((((background dark)) (:background "#013220"))
    (((background light)) (:background "honeydew")))
  "Face used to highlight evaluated paragraph."
  :group 'org-glance :group 'faces)

(cl-defun org-glance-overview-init ()
  "Init overview mode."
  (set-face-extend 'org-glance-headline-changed-face t))

(defconst org-glance-overview:header
  "#    -*- mode: org; mode: org-glance-overview -*-

${category}
${todo-states}
${todo-order}

")

(defvar org-glance-overview-mode-map (make-sparse-keymap)
  "Manipulate `org-mode' entries in `org-glance-overview-mode'.")

(defvar org-glance-overview-deferred-import-timer nil)
(defvar org-glance-overview-deferred-import-hash-table (make-hash-table))

;;; heavy methods applied to all headlines from current view's scope
;;; convention is to bind such methods to UPPERCASE KEYS

;; rebuild view and reread all files from view's scope
(define-key org-glance-overview-mode-map (kbd "G") 'org-glance-overview:reread)

;;; medium methods applied for all first-level headlines in current file

(cl-defun org-glance-overview:choose-headline ()
  "Choose `org-glance-headline' from current overview buffer and move cursor to it."
  (let* ((headlines (org-glance-overview:buffer-headlines))
         (headlines-active (--filter (org-glance-headline:active? it) headlines))
         (titles (mapcar #'org-glance-headline:title-clean headlines-active))
         (title (completing-read "Specify headline: " titles nil t))
         (headline (--first (string= (org-glance-headline:title-clean it) title) headlines))
         (id (org-glance-headline:id headline)))
    (goto-char (point-min))
    (org-glance-headline:search-forward id)))

(cl-defun org-glance-overview:buffer-headlines ()
  "Extract headlines from current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (headlines)
      (while (re-search-forward org-heading-regexp nil t)
        (beginning-of-line)
        (condition-case nil
            (push (org-glance-headline:at-point) headlines)
          (error nil))
        (forward-line 1))
      (nreverse headlines))))

(cl-defmacro org-glance-overview:apply-on-headline (&rest forms)
  "Eval FORMS on headline at point. If point is not at the headline, prompt user to choose headline and eval forms on it."
  (declare (indent 0) (debug t))
  `(org-glance:interactive-lambda
     (when (org-before-first-heading-p)
       (org-glance-overview:choose-headline))
     ,@forms))

(define-key org-glance-overview-mode-map (kbd ";") #'org-glance-overview:archive)
(define-key org-glance-overview-mode-map (kbd ",") #'beginning-of-buffer)
(define-key org-glance-overview-mode-map (kbd "<") #'beginning-of-buffer)
(define-key org-glance-overview-mode-map (kbd ".") #'end-of-buffer)
(define-key org-glance-overview-mode-map (kbd ">") #'end-of-buffer)
(define-key org-glance-overview-mode-map (kbd "^") #'org-glance-overview:order)

(define-key org-glance-overview-mode-map (kbd "RET")
            (org-glance-overview:apply-on-headline
              (org-glance-overview:materialize-headline)))

(define-key org-glance-overview-mode-map (kbd "#")
            (org-glance-overview:apply-on-headline
              (let* ((graph (org-glance-graph))
                     (original (org-glance-overview--load-original-at-point)))
                (if (org-glance-headline:encrypted? original)
                    (let* ((password (read-passwd "Password: "))
                           (decrypted (org-glance-headline:decrypt original password)))
                      (org-glance-graph:store-headline graph decrypted)
                      (org-glance-graph:add graph decrypted)
                      (org-glance-overview:pull))
                  (let* ((password (read-passwd "Password: " t))
                         (encrypted (org-glance-headline:encrypt original password)))
                    (org-glance-graph:store-headline graph encrypted)
                    (org-glance-graph:add graph encrypted)
                    (org-glance-overview:pull))))))

(define-key org-glance-overview-mode-map (kbd "/")
            (org-glance:interactive-lambda
              (org-glance-overview:choose-headline)))

(define-key org-glance-overview-mode-map (kbd "F")
            (org-glance-overview:apply-on-headline
              (org-attach-reveal-in-emacs)))

(define-key org-glance-overview-mode-map (kbd "g")
            (org-glance:interactive-lambda
              (if (org-before-first-heading-p)
                  (progn
                    (org-glance-overview:refresh-widgets (org-glance-overview:tag))
                    (org-glance-overview:order)
                    (pulse-momentary-highlight-region
                     (point-min)
                     (save-excursion
                       (org-next-visible-heading 1)
                       (point))
                     'region))
                (org-glance-overview:pull)
                (outline-show-subtree)
                (org-cycle-hide-drawers 'org-cycle-hide-drawers))
              (save-buffer)))

(define-key org-glance-overview-mode-map (kbd "j")
            (org-glance-overview:apply-on-headline
              (org-glance-overview:jump-headline)))

(define-key org-glance-overview-mode-map (kbd "a") #'org-glance-overview:agenda)
(define-key org-glance-overview-mode-map (kbd "n") #'outline-next-heading)
(define-key org-glance-overview-mode-map (kbd "p") #'outline-previous-heading)
(define-key org-glance-overview-mode-map (kbd "q") #'bury-buffer)

(define-key org-glance-overview-mode-map (kbd "k")
            (org-glance-overview:apply-on-headline
              (org-glance-overview:kill-headline)))

(define-key org-glance-overview-mode-map (kbd "R")
            (org-glance-overview:apply-on-headline
              (org-glance-overview:move)))

(define-key org-glance-overview-mode-map (kbd "r") #'org-glance-overview:move-headline)

(define-key org-glance-overview-mode-map (kbd "+")
            (org-glance:interactive-lambda
              (org-glance-capture (org-glance-overview:tag) "")))

(defcustom org-glance-overview:default-state-ordering
  (list
   "started"
   "pending"
   "todo"
   ""
   "done"
   "cancelled")
  "State-related ordering.")

(cl-defun org-glance-overview:state-ordering (tag)
  (cl-check-type tag org-glance-tag)

  (let ((config (f-join (org-glance-overview:directory tag) "task-states.el")))
    (if (and (file-exists-p config) (file-readable-p config))
        (with-temp-buffer
          (insert-file-contents config)
          (read (buffer-substring-no-properties (point-min) (point-max))))
      org-glance-overview:default-state-ordering)))

(cl-defun org-glance-overview:partition-mapper (tag)
  "Main method for partitioning headlines in overview."
  (cl-check-type tag org-glance-tag)

  (let ((ordering (org-glance-overview:state-ordering tag)))
    (list (not (org-in-archived-heading-p))
          (not (org-in-commented-heading-p))
          (or (-elem-index (downcase (or (org-element-property :todo-keyword (org-element-at-point)) "")) ordering) 0)
          (or (org-element-property :priority (org-element-at-point)) ?B))))

(cl-defun org-glance-overview:partition-comparator (lhs rhs)
  "Main method to compare LHS with RHS."
  (cl-loop
   for (i j) in (-zip-lists lhs rhs)
   when (cond ((not (eql (type-of i) (type-of j))) nil)
              ((stringp i) (not (string= i j)))
              (t (not (eql i j))))
   return (cond ((stringp i) (string< i j))
                ((numberp i) (< i j))
                ((booleanp i) i)
                (t nil))))

(cl-defun org-glance-overview:partition (&key using (test #'equal) (comparator #'<))
  (declare (indent 0))
  (let ((buffers (make-hash-table :test test)))
    (save-excursion
      (goto-char (point-min))
      (outline-next-heading)
      (while (< (point) (point-max))
        (let* ((group-state (funcall using))
               (group-buffer (get-buffer-create (concat "org-glance-overview-group:" (prin1-to-string group-state))))
               (contents (s-trim (buffer-substring-no-properties (point) (save-excursion (org-end-of-subtree t t))))))
          (with-current-buffer group-buffer
            (org-mode)
            (unless (gethash group-state buffers)
              (delete-region (point-min) (point-max)))
            (insert contents "\n"))
          (puthash group-state group-buffer buffers)
          (outline-next-heading))))
    (cl-loop for key in (sort (hash-table-keys buffers) comparator)
             collect (gethash key buffers))))

(cl-defun org-glance-overview:register (headline tag)
  "Add HEADLINE overview to TAG file."
  (cl-check-type headline org-glance-headline)
  (cl-check-type tag org-glance-tag)

  ;; Update graph
  (let ((graph (org-glance-graph)))
    (org-glance-graph:add graph headline)
    (org-glance-graph:store-headline graph headline))

  ;; Update overview file
  (org-glance--with-file-visited (org-glance-overview:file-name tag)
    (save-restriction
      (widen)

      (condition-case nil
          (org-glance-overview:remove headline tag)
        (org-glance-headline:not-found! nil))

      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n" (org-glance-headline:overview headline) "\n")
        (save-buffer))))
  headline)

(cl-defun org-glance-overview:remove (headline tag)
  "Remove HEADLINE from overview for TAG."
  (cl-check-type headline org-glance-headline)
  (cl-check-type tag org-glance-tag)

  (save-window-excursion
    (org-glance-overview tag)
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (when (org-glance-headline:search-forward (org-glance-headline:id headline))
          (let ((inhibit-read-only t))
            (delete-region (org-entry-beginning-position) (save-excursion (org-end-of-subtree t t)))
            (save-buffer)))))))

(cl-defun org-glance-overview:register-in-archive (headline tag)
  "Add HEADLINE overview to TAG archive."
  (cl-check-type headline org-glance-headline)
  (cl-check-type tag org-glance-tag)
  (org-glance-overview:ensure-archive tag)
  (org-glance--with-file-visited (org-glance-overview:archive-location tag)
    (save-restriction
      (widen)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n" (org-glance-headline:overview headline) "\n")
        (save-buffer))))
  headline)

(cl-defun org-glance-overview:import-headlines-from-files (tag files &optional (initial-progress 0))
  "Read each org-file from FILES, visit each headline of TAG and add it to overview."
  (let ((graph (org-glance-graph))
        (overviews '())
        (archives '()))
    (cl-labels ((-register (headline) (let ((tags (mapcar #'symbol-name (org-glance-headline:tags headline))))
                                        (when (member (symbol-name tag) tags)
                                          (org-glance-graph:add graph headline)
                                          (org-glance-graph:store-headline graph headline)
                                          (cond ((org-glance-headline:active? headline)
                                                 (push (org-glance-headline:overview headline) overviews))
                                                (t
                                                 (push (org-glance-headline:overview headline) archives)))))))
      (cl-loop with progress-label = (format "Collecting %s... " tag)
               with progress-reporter = (make-progress-reporter progress-label 0 (length files))
               for file in (-take-last (- (length files) initial-progress) files)
               for progress from initial-progress
               if (sit-for 0)
               do (progn
                    (progress-reporter-update progress-reporter progress)
                    (org-glance--with-file-visited file
                      (goto-char (point-min))
                      (while (re-search-forward org-heading-regexp nil t)
                        (beginning-of-line)
                        (condition-case nil
                            (-register (org-glance-headline:at-point))
                          (error nil))
                        (forward-line 1))))
               else do (progn
                         (org-glance--with-file-visited (org-glance-overview:file-name tag)
                           (goto-char (point-max))
                           (let ((inhibit-read-only t))
                             (cl-loop for overview in overviews
                                      do (insert overview "\n"))
                             (org-overview)))

                         (org-glance--with-file-visited (org-glance-overview:archive-location tag)
                           (goto-char (point-max))
                           (let ((inhibit-read-only t))
                             (cl-loop for archive in archives
                                      do (insert archive "\n"))
                             (org-overview)))

                         (puthash tag (list :progress progress :files files) org-glance-overview-deferred-import-hash-table)
                         (when (or (null org-glance-overview-deferred-import-timer)
                                   (not (memq org-glance-overview-deferred-import-timer timer-idle-list)))
                           (setq org-glance-overview-deferred-import-timer
                                 (run-with-idle-timer 1 t #'org-glance-overview:deferred-import-daemon)))

                         (message (format "%s import has been deferred: %d files processed of %d"
                                          tag progress (length files)))
                         (cl-return nil))

               finally do (progn
                            (org-glance--with-file-visited (org-glance-overview:file-name tag)
                              (goto-char (point-max))
                              (let ((inhibit-read-only t))
                                (cl-loop for overview in overviews
                                         do (insert overview "\n"))
                                (org-glance-overview:order)
                                (org-overview)
                                (org-align-tags 'all)))

                            (org-glance--with-file-visited (org-glance-overview:archive-location tag)
                              (goto-char (point-max))
                              (let ((inhibit-read-only t))
                                (cl-loop for archive in archives
                                         do (insert archive "\n"))
                                (org-glance-overview:order)
                                (org-overview)
                                (org-align-tags 'all)
                                (save-buffer)))

                            (remhash tag org-glance-overview-deferred-import-hash-table)
                            (progress-reporter-done progress-reporter))))))

(cl-defun org-glance-overview:deferred-import-daemon ()
  (if (hash-table-empty-p org-glance-overview-deferred-import-hash-table)
      (cancel-timer org-glance-overview-deferred-import-timer)
    (let* ((tag (cl-first (hash-table-keys org-glance-overview-deferred-import-hash-table)))
           (config (gethash tag org-glance-overview-deferred-import-hash-table))
           (files (plist-get config :files))
           (progress (plist-get config :progress)))
      (org-glance-overview:import-headlines-from-files tag files progress))))

(define-minor-mode org-glance-overview-mode
  "A minor read-only mode to use in overview files."
  :global nil
  :init-value nil
  :keymap org-glance-overview-mode-map
  :after-hook (progn
                (org-overview)
                (read-only-mode +1)))

(cl-defun org-glance-overview:directory (&optional (tag (org-glance-tags:completing-read)))
  "Path to file where TAG headlines are stored."
  (cl-check-type tag org-glance-tag)
  (abbreviate-file-name (f-join org-glance-directory (org-glance-tag:to-string tag))))

(cl-defun org-glance-overview:file-name (&optional (tag (org-glance-tags:completing-read)))
  "Path to file where TAG headlines are stored."
  (cl-check-type tag org-glance-tag)
  (let ((tag-str (org-glance-tag:to-string tag)))
    (f-join org-glance-directory tag-str (concat tag-str ".org"))))

(cl-defun org-glance-overview:archive-location (&optional (tag (org-glance-tags:completing-read)))
  "Path to file where TAG archived headlines are stored."
  (cl-check-type tag org-glance-tag)
  (let ((tag-str (org-glance-tag:to-string tag)))
    (f-join org-glance-directory tag-str (concat tag-str ".org_archive"))))

(cl-defun org-glance-overview:refresh-widgets (tag)
  (cl-check-type tag org-glance-tag)
  (save-excursion
    (let* ((inhibit-read-only t)
           (inhibit-message t)
           (category (format "#+CATEGORY: %s" tag))
           (todo-states (cl-loop for todo-seq in org-todo-keywords
                                 concat (concat "#+TODO: " (s-join " " (cdr todo-seq)) "\n")
                                 into result
                                 finally return (s-trim result)))

           (todo-order (concat "#+TODO_ORDER: " (cl-loop for state in (org-glance-overview:state-ordering tag)
                                                         if (string-empty-p state)
                                                         concat "_ " into result
                                                         else
                                                         concat (concat (upcase state) " ") into result
                                                         finally return (s-trim result))))
           (header (s-replace-all `(("${category}" . ,category)
                                    ("${todo-states}" . ,todo-states)
                                    ("${todo-order}" . ,todo-order))
                                  org-glance-overview:header)))

      (goto-char (point-min))
      (org-next-visible-heading 1)
      (delete-region (point-min) (point))
      (insert header)
      (goto-char (point-min))
      (org-next-visible-heading 1)
      (backward-char)
      (let ((org-clock-clocktable-default-properties org-glance-clocktable-properties))
        (org-clock-report)))))

(cl-defun org-glance-overview:create-archive (tag)
  (cl-check-type tag org-glance-tag)
  (let ((filename (org-glance--make-file-directory (org-glance-overview:archive-location tag))))
    (with-temp-file filename
      (org-glance-overview:refresh-widgets tag))
    filename))

(cl-defun org-glance-overview:ensure-archive (tag)
  (cl-check-type tag org-glance-tag)
  (let ((filename (org-glance--make-file-directory (org-glance-overview:archive-location tag))))
    (unless (file-exists-p filename)
      (with-temp-file filename
        (org-glance-overview:refresh-widgets tag)))
    filename))

(cl-defun org-glance-overview:create (tag)
  (cl-check-type tag org-glance-tag)
  (let ((overview-file-name (org-glance-overview:file-name tag)))
    (unless (f-exists? overview-file-name)
      (org-glance--make-file-directory overview-file-name)
      (org-glance-overview:create-archive tag)
      (with-temp-file overview-file-name
        (org-glance-overview:refresh-widgets tag)))
    overview-file-name))

(cl-defun org-glance-overview (tag)
  (interactive (list (org-glance-tags:completing-read "Overview: " nil)))
  (cl-check-type tag org-glance-tag)
  (when-let (location (org-glance-overview:file-name tag))
    (if (file-exists-p location)
        (find-file location)
      (org-glance:create-tag tag)
      (org-glance-overview:create tag))))

(cl-defun org-glance-overview:agenda ()
  (interactive)
  (let ((org-agenda-start-on-weekday nil)
        (org-agenda-overriding-header "org-glance agenda")
        (org-agenda-files (list (buffer-file-name))))
    (save-window-excursion
      (org-agenda-list nil "-7d" 21))
    (switch-to-buffer org-agenda-buffer)))

(cl-defun org-glance-overview:agenda* ()
  (interactive)
  (let ((org-agenda-overriding-header "org-glance agenda")
        (org-agenda-start-on-weekday nil)
        (org-agenda-files (mapcar 'org-glance-overview:file-name (org-glance:tags-sorted))))
    (save-window-excursion
      (org-agenda-list nil "-2d" 7))
    (switch-to-buffer org-agenda-buffer)))

(cl-defun org-glance-overview--load-original-at-point ()
  "Load the original headline from graph for the headline at point."
  (let* ((graph (org-glance-graph))
         (id (org-glance-headline:id (org-glance-headline:at-point))))
    (org-glance-graph:load-headline graph id)))

(cl-defun org-glance-overview:materialize-headline ()
  (switch-to-buffer (org-glance-headline:materialize (org-glance-overview--load-original-at-point))))

(cl-defun org-glance-overview:jump-headline ()
  (interactive)
  (org-glance:open (org-glance-overview--load-original-at-point)))

(cl-defun org-glance-overview:category ()
  (org-entry-get-with-inheritance "CATEGORY" nil 0))

(cl-defun org-glance-overview:tag ()
  "Return tag name of current overview."
  (let ((tag (org-glance-tag:from-string (org-glance-overview:category))))
    (when (gethash tag org-glance-tags)
      tag)))

(cl-defun org-glance-overview:original-headline ()
  "Get the original headline for the headline at point via graph."
  (org-glance-overview--load-original-at-point))

(cl-defun org-glance-overview:reread ()
  "Completely rebuild current overview file."
  (interactive)
  (let ((tag (org-glance-overview:tag)))
    (when (gethash tag org-glance-overview-deferred-import-hash-table)
      (if (not (memq org-glance-overview-deferred-import-timer timer-idle-list))
          (timer-activate org-glance-overview-deferred-import-timer)
        (when (y-or-n-p (format "%s is being rebuilt. Stop it?" tag))
          (remhash tag org-glance-overview-deferred-import-hash-table))))

    (when (y-or-n-p (format "Rebuild %s overview?" tag))
      (progn
        (save-buffer)
        (kill-buffer)
        (org-glance-overview:create tag)
        (when (y-or-n-p (format "Import headlines from %s?" (org-glance-overview:directory tag)))
          (let ((files (--filter (not (s-contains? "sync-conflict" it))
                                 (org-glance-scope (org-glance-overview:directory tag)))))
            (org-glance-overview:import-headlines-from-files tag files)
            (org-glance-overview:order)
            (let ((inhibit-read-only t))
              (org-align-tags))))))))

(cl-defun org-glance-overview:kill-headline (&key (force nil))
  "Remove `org-glance-headline' from overview."
  (interactive)
  (let* ((headline-at-point (org-glance-headline:at-point))
         (title (org-glance-headline:title-clean headline-at-point))
         (tag (org-glance-overview:tag))
         (graph (org-glance-graph))
         (id (org-glance-headline:id headline-at-point)))
    (when (or force (y-or-n-p (format "Revoke the tag \"%s\" from \"%s\"?" tag title)))
      (org-glance-graph:delete graph id)
      (org-glance-overview:remove headline-at-point tag))))

(cl-defun org-glance-overview:pull ()
  "Pull any modifications from original headline to it's overview at point."
  (interactive)
  (let* ((inhibit-read-only t)
         (initial-point (point))
         (current-headline (org-glance-headline:at-point))
         (current-headline-title (org-glance-headline:title-clean current-headline))
         (graph (org-glance-graph))
         (id (org-glance-headline:id current-headline)))
    (condition-case nil
        (let* ((original-headline (org-glance-graph:load-headline graph id))
               (overview-contents (org-glance-headline:overview original-headline)))
          (cond ((string= (org-glance-headline:contents current-headline) overview-contents)
                 (message (format "Headline \"%s\" is up to date" current-headline-title))
                 t)
                (t
                 (save-excursion
                   (goto-char (point-min))
                   (when (org-glance-headline:search-forward id)
                     (let ((beg (org-entry-beginning-position))
                           (end (save-excursion (org-end-of-subtree t t))))
                       (delete-region beg end)
                       (goto-char beg)
                       (insert overview-contents "\n"))))
                 (org-overview)
                 (goto-char initial-point)
                 (org-align-tags t)
                 (condition-case nil
                     (org-update-checkbox-count-maybe)
                   (error nil))
                 (save-buffer)
                 (message (format "Headline \"%s\" is now up to date" current-headline-title))
                 t)))
      (error
       (if (y-or-n-p (format "Original headline for \"%s\" not found. Remove it from overview?" current-headline-title))
           (org-glance-overview:kill-headline :force t)
         (org-glance-headline:not-found! "Original headline not found"))
       nil))))

(cl-defun org-glance-overview:archive ()
  "Archive headline at point."
  (interactive)
  (let* ((original (org-glance-overview--load-original-at-point))
         (tag (org-glance-overview:tag)))
    (org-glance-overview:remove original tag)
    (org-glance-overview:register-in-archive original tag)))

(cl-defun org-glance-overview:move ()
  "Move headline to another tag."
  (interactive)
  (let* ((old-tag (org-glance-overview:tag))
         (new-tag (let ((tages (--filter (not (eql old-tag it)) (org-glance:tags-sorted))))
                    (intern (completing-read "Move headline to: " tages nil t))))
         (original (org-glance-overview--load-original-at-point)))
    (org-glance:create-tag new-tag)
    (org-glance-overview:remove original old-tag)
    (org-glance-overview:register original new-tag)))

(defalias 'org-glance-overview:move-headline #'org-glance-overview:move)

(cl-defun org-glance-overview:beginning-of-headlines ()
  (save-excursion
    (goto-char (point-min))
    (outline-next-heading)
    (point)))

(cl-defun org-glance-overview:order (&optional (tag (org-glance-overview:tag)))
  (interactive)
  (cl-check-type tag org-glance-tag)

  (save-excursion
    (let ((inhibit-read-only t)
          (beginning-of-headlines (org-glance-overview:beginning-of-headlines))
          (end-of-headlines (point-max)))

      ;; Cache state ordering to avoid re-reading config file per headline
      (let ((cached-ordering (org-glance-overview:state-ordering tag)))
        (cl-flet ((mapper () (list (not (org-in-archived-heading-p))
                                   (not (org-in-commented-heading-p))
                                   (or (-elem-index (downcase (or (org-element-property :todo-keyword (org-element-at-point)) "")) cached-ordering) 0)
                                   (or (org-element-property :priority (org-element-at-point)) ?B))))
          (cl-loop for buffer in (org-glance-overview:partition
                                   :using #'mapper
                                   :comparator #'org-glance-overview:partition-comparator)
                   do
                   (goto-char (point-max))
                   (insert (let ((standard-output 'ignore))
                             (with-current-buffer buffer
                               (set-mark (point-min))
                               (goto-char (point-max))
                               (org-sort-entries nil ?a)
                               (buffer-substring-no-properties (point-min) (point-max)))))
                   (kill-buffer buffer))
          (delete-region beginning-of-headlines end-of-headlines)
          (org-overview)
          (save-buffer))))))

(provide 'org-glance-overview)
