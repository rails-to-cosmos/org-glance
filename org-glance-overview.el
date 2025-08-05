;; -*- lexical-binding: t -*-

(require 'highlight)
(require 'org-attach)
(require 'org-capture)
(require 'ol)
(require 'org-agenda)

(require 'org-glance-headline)
(require 'org-glance-metadata)
(require 'org-glance-tag)
(require 'org-glance-utils)

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
  (let* ((headlines (org-glance-headline:buffer-headlines (current-buffer)))
         (headlines-active (--filter (org-glance-headline:active? it) headlines))
         (titles (mapcar #'org-glance-headline:plain-title headlines-active))
         (title (completing-read "Specify headline: " titles nil t))
         (headline (org-glance-headline:select-by-title title headlines))
         (id (org-glance-headline:id headline)))
    (org-glance-headline:search-buffer-by-id id)))

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
              (let ((headline (org-glance-overview:original-headline)))
                (if (org-glance-headline:encrypted? headline)
                    (progn
                      (org-glance-headline:with-narrowed-headline headline
                        (org-glance-headline:decrypt)
                        (save-buffer))
                      (org-glance-overview:pull))
                  (org-glance:with-headline-materialized headline
                    (org-glance-headline:encrypt))))))

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
;; (define-key org-glance-overview-mode-map (kbd "z") #'org-glance-overview:vizualize)

(define-key org-glance-overview-mode-map (kbd "+")
            (org-glance:interactive-lambda
              (org-glance-capture (org-glance-overview:tag))))

;; (define-key org-glance-overview-mode-map (kbd "*") #'org-glance-overview:import-headlines-from-directory)

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
    (list (not (org-in-archived-heading-p)) ;; partition by ARCHIVED. "not" means archived headlines should be in a bottom
          (not (org-in-commented-heading-p)) ;; partition by COMMENTED. "not" means commented headlines should be in a bottom
          (or (-elem-index (downcase (or (org-element-property :todo-keyword (org-element-at-point)) "")) ordering) 0) ;; partition by state
          ;; (downcase (s-join ":" (sort (org-get-tags) #'string<))) ;; partition by tag string.
          (or (org-element-property :priority (org-element-at-point)) ?B))))

(cl-defun org-glance-overview:partition-comparator (lhs rhs)
  "Main method to compare LHS with RHS."
  (cl-check-type lhs org-glance-headline)
  (cl-check-type rhs org-glance-headline)

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

(cl-defun org-glance-overview:register-headline-in-metadata (headline tag)
  (cl-check-type headline org-glance-headline)
  (cl-check-type tag org-glance-tag)

  ;; TODO implement explicit metadata model
  (let ((metadata-file-name (org-glance-metadata:location tag))
        (metadata (org-glance-metadata:read-tag-metadata tag)))
    (org-glance-metadata:add-headline headline metadata)
    (org-glance-metadata:save metadata metadata-file-name)))

(cl-defun org-glance-overview:remove-headline-from-metadata (headline tag)
  (cl-check-type headline org-glance-headline)
  (cl-check-type tag org-glance-tag)

  ;; TODO implement explicit model for metadata
  (let ((metadata-location (org-glance-metadata:location tag))
        (metadata (org-glance-metadata:read-tag-metadata tag)))
    (org-glance-metadata:remove-headline headline metadata)
    (org-glance-metadata:save metadata metadata-location)))

(cl-defun org-glance-overview:register-headline-in-overview (headline tag)
  "Add HEADLINE overview to TAG file."
  (cl-check-type headline org-glance-headline)
  (cl-check-type tag org-glance-tag)

  (org-glance--with-file-visited (org-glance-overview:file-name tag)
    (save-restriction
      (widen)

      (condition-case nil
          (org-glance-overview:remove-headline-from-overview headline tag)
        (org-glance-headline:not-found! nil))

      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n" (org-glance-headline:overview headline) "\n")
        (save-buffer))))
  headline)

(cl-defun org-glance-overview:remove-headline-from-overview (headline tag)
  "Add HEADLINE clone in overview VIEW-ID file."
  (cl-check-type headline org-glance-headline)
  (cl-check-type tag org-glance-tag)

  (save-window-excursion
    (org-glance-overview tag)
    (save-restriction
      (widen)
      (save-excursion
        (when (condition-case nil
                  (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline))
                (org-glance-headline:not-found! nil))
          (let ((inhibit-read-only t))
            (delete-region (org-entry-beginning-position) (save-excursion (org-end-of-subtree t t)))
            (save-buffer)))))))

(cl-defun org-glance-overview:register-headline-in-archive (headline tag)
  "Add HEADLINE overview to CLASS archive."
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

(cl-defun org-glance-capture-headline-at-point (&optional (tag (org-glance-tags:completing-read "Tag: ")))
  "Extract all possible information from headline at point."
  (declare (indent 1))
  (interactive)
  (save-window-excursion
    (save-excursion
      (org-glance--back-to-heading)
      (let* ((id (org-glance-tag:generate-id tag))
             (dir (org-glance:make-tag-directory tag))
             (output-file (f-join dir (format "%s.org" (org-glance-tag:to-string tag)))))

        (mkdir dir 'parents)

        (save-restriction
          (org-narrow-to-subtree)
          (let* ((contents (buffer-substring-no-properties (point-min) (point-max)))
                 (result (save-window-excursion
                           (find-file output-file)
                           (save-restriction
                             (widen)
                             (end-of-buffer)
                             (save-excursion
                               (insert contents))
                             (org-glance-headline:promote-to-the-first-level)
                             (org-set-property "ORG_GLANCE_ID" id)
                             (org-set-property "DIR" dir)
                             (org-set-property "CATEGORY" (org-glance-tag:to-string tag))
                             (org-set-property "ORG_GLANCE_CREATION_TIME" (with-temp-buffer
                                                                            (let ((current-prefix-arg '(16)))
                                                                              (call-interactively #'org-time-stamp-inactive)
                                                                              (buffer-substring-no-properties (point-min) (point-max)))))
                             (unless (member tag (org-glance-tag:from-headline-at-point))
                               (org-toggle-tag (org-glance-tag:to-string tag)))
                             (save-buffer)
                             (org-glance-headline:at-point)))))
            result))))))

(cl-defun org-glance-capture:prepare-finalize-hook (id tag)
  "Preprocess headline before capturing"
  (goto-char (point-min))
  (or (org-at-heading-p) (org-next-visible-heading 0))
  (org-set-property "ORG_GLANCE_ID" id)
  (org-glance:create-tag tag)
  (org-toggle-tag (format "%s" tag) t))

(cl-defun org-glance-capture:after-finalize-hook (id tag)
  "Register captured headline ID in the system tagged by TAG."
  (when-let (headline (org-glance-headline:search-buffer-by-id id))
    (let* ((title (org-glance-headline:plain-title headline))
           (tag-file (org-glance:tag-file-name tag))
           (refile-dir (org-glance-headline:make-directory tag-file title))
           (tmp-file (org-glance-headline:file-name headline))
           (new-file (org-glance--make-file-directory (f-join refile-dir (format "%s.org" tag)))))
      ;; (org-set-property "DIR" (abbreviate-file-name refile-dir))
      ;; (save-buffer)
      ;; (kill-buffer)

      (f-move tmp-file new-file)
      (org-glance-headline:update headline
        :file new-file
        :dir (abbreviate-file-name refile-dir))
      (org-glance-overview tag)
      (org-glance-overview:register-headline-in-metadata headline tag)
      (org-glance-overview:register-headline-in-overview headline tag)
      ;; (org-overview)
      ;; (org-glance-headline:search-buffer-by-id id)
      )))

(cl-defun org-glance-overview:import-headlines-from-files (tag files &optional (initial-progress 0))
  "Read each org-file from PATH, visit each headline of current overview tag and add it to overview."
  (let* ((metadata-location (org-glance-metadata:location tag))
         (metadata (org-glance-metadata:read metadata-location))
         (overviews '())
         (archives '()))
    (cl-labels ((-register (headline) (let ((tags (mapcar #'downcase (org-element-property :tags headline))))
                                        (when (and (member tag tags) (not (member 'archive tags)))
                                          (cond ((org-glance-headline:active? headline)
                                                 (org-glance-metadata:add-headline headline metadata)
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
                      (when-let (headline (org-glance-headline:at-point))
                        (-register headline))

                      (while-let ((headline (org-glance-headline:search-forward)))
                        (-register headline))))
               else do (progn
                         (org-glance-metadata:save metadata metadata-location)

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
                            (org-glance-metadata:save metadata metadata-location)

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

(cl-defun org-glance-overview:sync-headlines ()
  (when (and org-glance-overview:changed-headlines
             (y-or-n-p (format "%d headline%s has been changed. Syncronize with origins?"
                               (length org-glance-overview:changed-headlines)
                               (if (> (length org-glance-overview:changed-headlines) 1) "s" ""))))

    (cl-loop
     for id in org-glance-overview:changed-headlines
     do (save-excursion
          (org-glance-headline:search-buffer-by-id id)
          (let ((contents (buffer-substring-no-properties (point) (save-excursion (org-end-of-subtree t)))))
            (save-window-excursion
              (save-excursion
                (->> (org-glance-headline:at-point)
                     org-glance-headline:id
                     org-glance-metadata:headline-metadata
                     org-glance-headline:visit)
                (save-restriction
                  (org-narrow-to-subtree)
                  (unless (string= (buffer-substring-no-properties (point-min) (point-max)) contents)
                    (delete-region (point-min) (point-max))
                    (insert contents)
                    (save-buffer)
                    (kill-buffer))))
              (hlt-unhighlight-region (point) (save-excursion (org-end-of-subtree t t))))))))

  (setq-local org-glance-overview:changed-headlines '()))

(cl-defun org-glance-overview:track-changes (start end old-len)
  (save-match-data
    (let ((diff (buffer-substring-no-properties start end)))
      (when (and (not (org-before-first-heading-p))
                 (not (and (eobp) (string= diff "\n"))))
        (when-let (id (org-glance-headline:id (org-glance-headline:at-point)))
          (cl-pushnew id org-glance-overview:changed-headlines :test #'string=)
          (hlt-highlight-region (org-glance-headline:begin)
                                (save-excursion (org-end-of-subtree t t))
                                'org-glance-headline-changed-face))))))

(define-minor-mode org-glance-overview-mode
  "A minor read-only mode to use in overview files."
  :global nil
  :init-value nil
  :keymap org-glance-overview-mode-map
  :after-hook (progn
                (org-overview)
                (read-only-mode +1)))

(defvar org-glance-edit-mode-map (make-sparse-keymap)
  "Edit entries in `org-glance-edit-mode'.")

;; (define-key org-glance-overview-mode-map (kbd "C-c C-p") #'org-glance-edit-mode:start)
;; (define-key org-glance-edit-mode-map (kbd "C-c C-c") 'org-glance-edit-mode:apply)

(define-minor-mode org-glance-edit-mode
  "A minor mode to edit and sync overview files."
  :global nil
  :init-value nil
  :keymap org-glance-edit-mode-map)

;; (cl-defun org-glance-edit-mode:start ()
;;   (interactive)
;;   (org-glance-edit-mode +1)
;;   (org-glance-overview-mode -1)
;;   (defvar-local org-glance-overview:changed-headlines '())
;;   (cl-pushnew 'org-glance-overview:track-changes after-change-functions)
;;   ;; (add-hook 'before-save-hook #'org-glance-overview:sync-headlines t t)
;;   (message "Edit mode is now enabled."))

;; (cl-defun org-glance-edit-mode:apply ()
;;   (interactive)
;;   (org-glance-overview:sync-headlines)
;;   (setq-local after-change-functions (cl-remove 'org-glance-overview:track-changes after-change-functions))
;;   (org-glance-edit-mode -1)
;;   (org-glance-overview-mode +1)
;;   (message "All changes have been applied."))

(cl-defun org-glance-overview:directory (&optional (tag (org-glance-tags:completing-read)))
  "Path to file where CLASS headlines are stored."
  (cl-check-type tag org-glance-tag)
  (abbreviate-file-name (f-join org-glance-directory (org-glance-tag:to-string tag))))

(cl-defun org-glance-overview:file-name (&optional (tag (org-glance-tags:completing-read)))
  "Path to file where VIEW-ID headlines are stored."
  (cl-check-type tag org-glance-tag)
  (format "%s/%s/%s.org" org-glance-directory tag tag))

(cl-defun org-glance-overview:archive-location (&optional (tag (org-glance-tags:completing-read)))
  "Path to file where VIEW-ID headlines are stored."
  (cl-check-type tag org-glance-tag)
  (format "%s/%s/%s.org_archive" org-glance-directory tag tag))

(cl-defun org-glance:capture-template (tag &key (default ""))
  (cl-check-type tag org-glance-tag)
  (let ((capture-template-config-file (f-join (org-glance-overview:directory tag) "template.org")))
    (s-replace "%?" (concat default "%?")
               (cond ((f-exists-p capture-template-config-file) (with-temp-buffer
                                                                  (insert-file-contents capture-template-config-file)
                                                                  (buffer-substring-no-properties (point-min) (point-max))))
                     (t "* %?")))))

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
      (let ((metadata-file-name (org-glance-metadata:location tag))
            (overview-file-name (org-glance-overview:file-name tag)))
        (org-glance--make-file-directory overview-file-name)
        (org-glance-metadata:create metadata-file-name)
        (org-glance-overview:create-archive tag)
        (with-temp-file overview-file-name
          (org-glance-overview:refresh-widgets tag))))
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

(cl-defun org-glance-overview:materialize-headline ()
  (let ((headline (org-glance-overview:original-headline)))
    (switch-to-buffer (org-glance-headline:materialize headline))))

(cl-defun org-glance-overview:jump-headline ()
  (interactive)
  (let ((offset (- (point) (save-excursion
                             (org-glance-headline:search-parents)
                             (point)))))
    (-some->> (org-glance-headline:at-point)
      org-glance-headline:id
      org-glance-metadata:headline-metadata
      org-glance:open)
    (forward-char offset)))

(cl-defun org-glance-overview:category ()
  (org-entry-get-with-inheritance "CATEGORY" nil 0))

(cl-defun org-glance-overview:tag ()
  "Return tag name of current overview."
  (save-excursion
    (goto-char (point-min))
    (let ((tag (org-glance-tag:from-string (org-glance-overview:category))))
      (when (gethash tag org-glance-tags)
        tag))))

(cl-defun org-glance-overview:reread ()
  "Completely rebuild current overview file."
  (interactive)
  (let ((tag (org-glance-overview:tag)))
    (when (gethash tag org-glance-overview-deferred-import-hash-table)
      (if (not (memq org-glance-overview-deferred-import-timer timer-idle-list))
          (timer-activate org-glance-overview-deferred-import-timer)
        ;; timer is already running
        (when (y-or-n-p (format "%s is being rebuilt. Stop it?" tag))
          (remhash tag org-glance-overview-deferred-import-hash-table))))

    (when (y-or-n-p (format "Clear %s metadata?" tag))
      (progn
        (save-buffer)
        (kill-buffer)
        (org-glance-overview:create tag)
        (when (y-or-n-p (format "Import headlines from %s?" (f-parent (org-glance-metadata:location tag))))
          (let ((files (--filter (not (s-contains? "sync-conflict" it))
                                 (org-glance-scope (f-parent (org-glance-metadata:location tag))))))
            (org-glance-overview:import-headlines-from-files tag files)
            (org-glance-overview:order)
            (let ((inhibit-read-only t))
              (org-align-all-tags))))))))

(cl-defun org-glance-overview:kill-headline (&key (force nil))
  "Remove `org-glance-headline' from overview, don't ask to confirm if FORCE is t."
  (interactive)
  (org-glance-headline:search-parents)
  (let ((title (org-glance-headline:plain-title))
        (tag (org-glance-overview:tag))
        (original-headline (org-glance-overview:original-headline)))
    (when (or force (y-or-n-p (format "Revoke the tag \"%s\" from \"%s\"?" tag title)))
      (save-window-excursion
        (org-glance:with-headline-materialized original-headline
          (cl-loop with tags = (org-glance-tag:from-headline-at-point)
                   with indices = (--find-indices (eq tag it) tags)
                   for index in indices
                   do (org-toggle-tag (nth index tags) 'off)))))))

(cl-defun org-glance-overview:pull ()
  "Pull any modifications from original headline to it's overview at point."
  (interactive)
  (let* ((inhibit-read-only t)
         (initial-point (point))
         (current-headline (org-glance-headline:at-point))
         (current-headline-title (org-glance-headline:plain-title current-headline))
         (current-headline-contents (org-glance-headline:contents current-headline))
         (original-headline (org-glance-overview:original-headline))
         (overview-contents (org-glance-headline:overview original-headline)))
    (cond ((null overview-contents)
           (if (y-or-n-p (format "Original headline for \"%s\" not found. Remove it from overview?" current-headline-title))
               (org-glance-overview:kill-headline :force t)
             (org-glance-headline:not-found! "Original headline not found"))
           nil)
          ((string= current-headline-contents overview-contents)
           (message (format "Headline \"%s\" is up to date" current-headline-title))
           t)
          (t (org-glance-headline:replace-headline-at-point overview-contents)
             (org-overview)
             (goto-char initial-point)
             (org-align-tags t)
             (condition-case nil
                 (org-update-checkbox-count-maybe)
               (error nil))
             (save-buffer)
             (message (format "Headline \"%s\" is now up to date" current-headline-title))
             t))))

(cl-defun org-glance-overview:archive ()
  "Archive headline at point."
  (interactive)
  (save-window-excursion
    (org-glance:with-headline-materialized (org-glance-overview:original-headline)
      (org-toggle-archive-tag))))

(cl-defun org-glance-overview:move ()
  "Move headline to another tag."
  (interactive)
  (let* ((old-tag (org-glance-overview:tag))
         (new-tag (let ((tages (--filter (not (eql old-tag it)) (org-glance:tags-sorted))))
                    (intern (completing-read "Move headline to: " tages nil t))))
         (original-headline (org-glance-overview:original-headline)))
    (org-glance:create-tag new-tag)
    (save-window-excursion
      (org-glance:with-headline-materialized original-headline
        (cl-loop with tags = (org-get-tags)
                 with indices = (--find-indices (eql old-tag (org-glance-tag:from-string it)) tags)
                 for index in indices
                 do (org-toggle-tag (nth index tags) 'off)
                 finally (org-toggle-tag (symbol-name new-tag) 'on))))))

(cl-defun org-glance-overview:add-tag ()
  "Add tag to headline."
  (interactive)
  (let* ((original-headline (org-glance-overview:original-headline))
         (old-tages (org-glance-headline:tags original-headline))
         (new-tag (let ((views (--filter (not (member it old-tages)) (org-glance:tags-sorted))))
                      (intern (completing-read "Add tag: " views)))))
    (save-window-excursion
      (org-glance:with-headline-materialized original-headline
        (org-toggle-tag (symbol-name new-tag) 'on)))))

(cl-defun org-glance-overview:original-headline ()
  (->> (org-glance-headline:at-point)
       org-glance-headline:id
       org-glance-metadata:headline))

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

      (cl-loop for buffer in (org-glance-overview:partition
                               :using (-partial org-glance-overview:partition-mapper tag)
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
      (save-buffer))))

;; (cl-defun org-glance-overview:vizualize ()
;;   (interactive)
;;   (org-glance-overview:apply-to-buffer-headlines
;;       (error "not implemented yet")
;;     (let ((relations (org-glance-headline-relations)))
;;       (with-temp-file "relations.js"
;;         (insert "var relations = ["
;;                 (s-join "," (cl-loop
;;                                for rel in relations
;;                                for name = (car rel)
;;                                for relations = (s-join "," (mapcar (-rpartial #'s-wrap "\"") (cdr rel)))
;;                                collect (org-glance:format "{\"name\":\"${name}\",\"relations\":[${relations}]}")))
;;                 "];")))))

;; (cl-defun -og-calw-d (day)
;;   (let ((org-agenda-files (mapcar 'org-glance-overview:file-name (org-glance:tags-sorted))))
;;     (org-agenda-list)
;;     (org-agenda-day-view day)
;;     (switch-to-buffer org-agenda-buffer)
;;     (delete-other-windows)))

(provide 'org-glance-overview)
