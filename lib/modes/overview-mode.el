(require 'org-glance-module)

(org-glance:require
  highlight
  org-attach
  org-capture)

(defface org-glance-headline-changed-face
    '((((background dark)) (:background "#013220"))
      (((background light)) (:background "honeydew")))
  "*Face used to highlight evaluated paragraph."
  :group 'org-glance :group 'faces)

(cl-defun org-glance-overview-init ()
  "Init overview mode."
  (set-face-extend 'org-glance-headline-changed-face t))

(defconst org-glance-overview:header "#    -*- mode: org; mode: org-glance-overview -*-

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
  "Choose `org-glance-headline' from current overview buffer and goto it."
  (let ((headlines (org-glance-headline:extract-from (current-buffer))))
    (org-glance-headline:search-buffer-by-id
     (org-glance-headline:id
      (org-glance-scope--choose-headline
       (org-completing-read "Specify headline: "
                            (mapcar #'org-glance-headline:title (--filter (org-glance-headline:active? it) headlines)))
       headlines)))))

(cl-defmacro org-glance-overview:apply-on-headline (&rest forms)
  "Eval FORMS on headline at point.
If point is before the first heading, prompt for headline and eval forms on it."
  (declare (indent 0) (debug t))
  `(org-glance:interactive-lambda
     (when (org-before-first-heading-p)
       (org-glance-overview:choose-headline))
     ,@forms))

;; lightweight methods applied for current headline
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
      (if (org-glance-headline:encrypted-p headline)
          (progn
            (org-glance:with-headline-narrowed headline
              (org-glance-headline:decrypt)
              (save-buffer))
            (org-glance-overview:pull))
        (org-glance:with-headline-materialized headline
          (org-glance-headline:encrypt))))))

(define-key org-glance-overview-mode-map (kbd "/")
  (org-glance:interactive-lambda
    (org-glance-overview:choose-headline)))

;; (define-key org-glance-overview-mode-map (kbd "j")
;;   (org-glance:interactive-lambda
;;     (org-glance:open (org-glance-overview:choose-headline))))

(define-key org-glance-overview-mode-map (kbd "F")
  (org-glance-overview:apply-on-headline
    (org-attach-reveal-in-emacs)))

(define-key org-glance-overview-mode-map (kbd "g")
  (org-glance:interactive-lambda
    (if (org-before-first-heading-p)
        (progn
          (org-glance-overview:refresh-widgets)
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

(define-key org-glance-overview-mode-map (kbd "v")
  (org-glance-overview:apply-on-headline
    (org-glance-overview:visit-headline)))

(define-key org-glance-overview-mode-map (kbd "a") #'org-glance-overview:agenda)

(define-key org-glance-overview-mode-map (kbd "n")
  (org-glance:interactive-lambda
    (when (org-glance-headline:at-point)
      (outline-hide-subtree))
    (org-glance-headline:search-forward)
    ;; (when (org-glance-headline:at-point)
    ;;   (outline-show-subtree)
    ;;   (org-cycle-hide-drawers 'org-cycle-hide-drawers))
    ))

(define-key org-glance-overview-mode-map (kbd "p")
  (org-glance:interactive-lambda
    (when (org-glance-headline:at-point)
      (outline-hide-subtree))
    (org-glance-headline:search-backward)
    ;; (when (org-glance-headline:at-point)
    ;;   (outline-show-subtree)
    ;;   (org-cycle-hide-drawers 'org-cycle-hide-drawers))
    ))

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
    (org-glance-capture :class (org-glance-overview:class))))

;; (define-key org-glance-overview-mode-map (kbd "*") #'org-glance-overview:import-headlines-from-directory)

(defcustom org-glance-overview:state-ordering
  (list
   "started"
   "pending"
   "todo"
   ""
   "done"
   "cancelled")
  "State-related ordering.")

(cl-defun org-glance-overview:partition-mapper ()
  "Main method for partitioning headlines in overview."
  (let* ((class (org-glance-overview:class))
         (state-ordering-config (f-join (org-glance-overview:directory class) "task-states.el"))
         (state-ordering (if (and (file-exists-p state-ordering-config)
                                  (file-readable-p state-ordering-config))
                             (with-temp-buffer
                               (insert-file-contents state-ordering-config)
                               (read (buffer-substring-no-properties (point-min) (point-max))))
                           org-glance-overview:state-ordering)))
    (list
     (not (org-in-archived-heading-p)) ;; partition by ARCHIVED. "not" means archived headlines should be in a bottom
     (not (org-in-commented-heading-p)) ;; partition by COMMENTED. "not" means commented headlines should be in a bottom
     (or (-elem-index (downcase (or (org-element-property :todo-keyword (org-element-at-point)) "")) state-ordering) 0) ;; partition by state
     ;; (downcase (s-join ":" (sort (org-get-tags) #'string<))) ;; partition by tag string.
     (or (org-element-property :priority (org-element-at-point)) ?B))))

(cl-defun org-glance-overview:partition-comparator (headline1 headline2)
  "Main method to compare HEADLINE1 with HEADLINE2."
  (cl-loop
     for (i j) in (-zip-lists headline1 headline2)
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

(cl-defun org-glance-overview:register-headline-in-metastore (headline class)
  (org-glance:log-debug "Update metastore %s" class)
  (let* ((metastore-location (-some->> class
                               org-glance:get-class
                               org-glance-view:metastore-location))
         (metastore (org-glance-metastore:read metastore-location)))
    (org-glance-metastore:add-headline headline metastore)
    (org-glance-metastore:save metastore metastore-location)))

(cl-defun org-glance-overview:remove-headline-from-metastore (headline class)
  (org-glance:log-debug "Remove from metastore %s" class)
  (let* ((metastore-location (-some->> class
                               org-glance:get-class
                               org-glance-view:metastore-location))
         (metastore (org-glance-metastore:read metastore-location)))
    (org-glance-metastore:rem-headline headline metastore)
    (org-glance-metastore:save metastore metastore-location)))

(cl-defun org-glance-overview:register-headline-in-overview (headline class)
  "Add HEADLINE overview to CLASS file."
  (org-glance:with-file-visited (org-glance-overview:location class)
    (seq-let (id contents partition) (org-glance:with-headline-narrowed headline
                                       (list (org-glance-headline:id)
                                             (org-glance-headline:overview)
                                             (org-glance-overview:partition-mapper)))
      (save-restriction
        (widen)
        (condition-case nil
            (org-glance-overview:remove-headline-from-overview headline class)
          (org-glance-exception:HEADLINE-NOT-FOUND nil))
        (let ((inhibit-read-only t)
              (headline-seen-p nil))
          (unless (or (string-empty-p contents)
                      (and (memq 'archive (org-glance-headline:classes headline))
                           (not (eql 'archive class))))
            (goto-char (point-min))

            (while (and (outline-next-heading)
                        (org-glance-headline-p)
                        (org-glance-overview:partition-comparator (org-glance-overview:partition-mapper) partition))
              (when (org-glance-headline-p)
                (setq headline-seen-p t)))

            (when (and (not headline-seen-p)
                       (not (org-glance-headline-p)))
              (insert "\n"))

            (insert (s-trim contents) "\n")

            (save-buffer))))))
  headline)

(cl-defun org-glance-overview:remove-headline-from-overview (headline class)
  "Add HEADLINE clone in overview VIEW-ID file."
  (org-glance:log-debug "Remove from overview %s" class)
  (save-window-excursion
    (org-glance-overview class)
    (save-restriction
      (widen)
      (save-excursion
        (when (condition-case nil
                  (org-glance-headline:search-buffer-by-id (org-glance-headline:id headline))
                (org-glance-exception:HEADLINE-NOT-FOUND nil))
          (let ((inhibit-read-only t))
            (kill-region (org-entry-beginning-position) (save-excursion
                                                          (org-end-of-subtree t t)))
            (save-buffer)))))))

;; (cl-defun org-glance-overview:register-headline-in-write-ahead-log (headline class)
;;   (org-glance:with-headline-materialized headline
;;     (let ((id (intern (org-glance-headline:id headline)))
;;           (class (if (symbolp class) class (intern class))))
;;       (org-glance-posit:write
;;        (org-glance-posit (list class 'is-class))
;;        (org-glance-posit (list id 'thing) (list class 'class))
;;        (org-glance-posit (list id 'origin) :value (list (org-glance-headline:file headline) (org-glance-headline:begin headline)))
;;        (org-glance-posit (list id 'title) :value (org-glance-headline:title headline))
;;        (org-glance-posit (list id 'contents)
;;                          :value (save-excursion
;;                                   (org-end-of-meta-data t)
;;                                   ""
;;                                   ;; (base64-encode-string
;;                                   ;;  (buffer-substring-no-properties (point) (point-max))
;;                                   ;;  t)
;;                                   ))
;;        (org-glance-posit (list id 'extractable)
;;                          :value (save-excursion
;;                                   (org-end-of-meta-data t)
;;                                   (when (re-search-forward org-glance:key-value-pair-re nil t)
;;                                     t)))
;;        (org-glance-posit (list id 'openable)
;;                          :value (save-excursion
;;                                   (org-end-of-meta-data t)
;;                                   (when (re-search-forward org-any-link-re nil t)
;;                                     t)))
;;        (org-glance-posit (list id 'decryptable)
;;                          :value (save-excursion
;;                                   (org-end-of-meta-data t)
;;                                   (looking-at "aes-encrypted V [0-9]+.[0-9]+-.+\n")))))))

(cl-defun org-glance-capture-headline-at-point
    (&optional (class (org-completing-read "Specify class: " (org-glance-classes))))
  "Extract all possible information from headline at point."
  (declare (indent 1))
  (interactive)
  (save-window-excursion
    (save-excursion
      (org-glance-ensure-at-heading)
      (let* ((class (cond ((symbolp class) (symbol-name class))
                          ((stringp class) class)))
             (id (org-glance-generate-id class))
             (dir (org-glance-generate-directory class))
             (output-file (f-join dir (org-glance:format "${class}.org"))))

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
                             (org-set-property "CATEGORY" class)
                             (org-set-property "ORG_GLANCE_CREATION_TIME" (with-temp-buffer
                                                                            (let ((current-prefix-arg '(16)))
                                                                              (call-interactively #'org-time-stamp-inactive)
                                                                              (buffer-substring-no-properties (point-min) (point-max)))))
                             (unless (member (downcase class) (-org-glance:collect-tags))
                               (org-toggle-tag class))
                             (save-buffer)
                             (org-glance-headline:at-point)))))
            result))))))

(cl-defun org-glance-capture:prepare-finalize-hook ()
  "Preprocess headline before capturing.

Available buffer local variables: `org-glance-capture:id', `org-glance-capture:class', `org-glance-capture:default'."
  (goto-char (point-min))
  (or (org-at-heading-p) (org-next-visible-heading 0))
  (org-set-property "ORG_GLANCE_ID" org-glance-capture:id)

  (unless (org-glance:get-class org-glance-capture:class)
    (when (y-or-n-p (format "Create class %s?" org-glance-capture:class))
      (save-window-excursion
        (org-glance-class-create org-glance-capture:class))))

  (org-toggle-tag (format "%s" org-glance-capture:class) t))

(cl-defun org-glance-capture:after-finalize-hook ()
  "Register captured headline in metastore.

Buffer local variables: `org-glance-capture:id', `org-glance-capture:class', `org-glance-capture:default'."

  (org-glance:log-debug
   "Finalize capture (id: %s, class: %s)"
   org-glance-capture:id
   org-glance-capture:class)

  (when-let (headline (org-glance-headline:search-buffer-by-id org-glance-capture:id))
    (let* ((id org-glance-capture:id)
           (class org-glance-capture:class)
           (refile-dir (org-glance-headline:generate-directory
                        (org-glance-class-location class)
                        (org-glance-headline:title headline)))
           (tmp-file (org-glance-headline:file headline))
           (new-file (-org-glance:make-file-directory (f-join refile-dir (format "%s.org" class)))))
      (org-glance:log-debug "Generate headline directory: %s" refile-dir)
      (org-set-property "DIR" (abbreviate-file-name refile-dir))
      (save-buffer)
      (kill-buffer)

      (f-move tmp-file new-file)
      (org-glance-headline:enrich headline :file new-file)

      (org-glance-overview class)

      (org-glance:log-debug "Register headline of class %s in metastore: %s"
                            (pp-to-string class)
                            (pp-to-string headline))

      (org-glance-overview:register-headline-in-metastore headline class)
      (org-glance-overview:register-headline-in-overview headline class)

      (org-overview)
      (org-glance-headline:search-buffer-by-id id))))

(cl-defun org-glance-overview:import-headlines-from-files (class files &optional (initial-progress 0))
  "Read each org-file from PATH, visit each headline of currents' overview class and add it to overview."
  (let* ((tag (downcase (symbol-name class)))
         (metastore-location (-some->> class
                               org-glance:get-class
                               org-glance-view:metastore-location))
         (metastore (org-glance-metastore:read metastore-location))
         (overviews '()))
    (cl-loop
       with progress-label = (format "Collecting %s... " class)
       with progress-reporter = (make-progress-reporter progress-label 0 (length files))
       for file in (-take-last (- (length files) initial-progress) files)
       for current-progress from initial-progress
       if (sit-for 0)
       do
         (progress-reporter-update progress-reporter current-progress)
         (org-glance:with-file-visited file
           (org-element-map (org-element-parse-buffer 'headline) 'headline
             (lambda (element)
               (save-excursion
                 (goto-char (org-element-property :begin element))
                 (let* ((headline (org-glance-headline:at-point))
                        (tags (mapcar #'downcase (org-element-property :tags headline))))
                   (when (and (member tag tags) (not (member 'archive tags)))
                     (org-glance-metastore:add-headline headline metastore)
                     (push (org-glance-headline:overview) overviews)))))))
       else
       do
         (org-glance-with-debug-msg "Persist metastore changes..."
           (org-glance-metastore:save metastore metastore-location))

         (org-glance:with-file-visited (org-glance-overview:location class)
           (goto-char (point-max))
           (let ((inhibit-read-only t))
             (cl-loop for overview in overviews
                do (insert overview "\n"))
             (org-overview)))

         (puthash class (list :progress current-progress :files files) org-glance-overview-deferred-import-hash-table)

         (when (or (null org-glance-overview-deferred-import-timer)
                   (not (memq org-glance-overview-deferred-import-timer timer-idle-list)))
           (setq org-glance-overview-deferred-import-timer
                 (run-with-idle-timer 1 t #'org-glance-overview:deferred-import-daemon)))

         (org-glance:log-info (format "%s import has been deferred: %d files processed of %d"
                                      class current-progress (length files)))
         (return nil)
       finally
       do
         (org-glance-with-debug-msg "Persist metastore changes..."
           (org-glance-metastore:save metastore metastore-location))

         (org-glance:with-file-visited (org-glance-overview:location class)
           (goto-char (point-max))
           (let ((inhibit-read-only t))
             (cl-loop for overview in overviews
                do (insert overview "\n"))
             (org-glance-overview:order)
             (org-overview)
             (org-align-tags 'all)))

         (remhash class org-glance-overview-deferred-import-hash-table)
         (progress-reporter-done progress-reporter))))

(cl-defun org-glance-overview:deferred-import-daemon ()
  (if (hash-table-empty-p org-glance-overview-deferred-import-hash-table)
      (cancel-timer org-glance-overview-deferred-import-timer)
    (let* ((class (first (hash-table-keys org-glance-overview-deferred-import-hash-table)))
           (config (gethash class org-glance-overview-deferred-import-hash-table))
           (files (plist-get config :files))
           (progress (plist-get config :progress)))
      (org-glance-overview:import-headlines-from-files class files progress))))

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
                       org-glance-metastore:get-headline
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
        (when-let (id (org-glance-headline:id))
          (cl-pushnew id org-glance-overview:changed-headlines :test #'string=)
          (hlt-highlight-region (org-glance-headline:begin)
                                (save-excursion (org-end-of-subtree t t))
                                'org-glance-headline-changed-face))))))

(define-minor-mode org-glance-overview-mode
    "A minor read-only mode to use in overview files."
  nil nil org-glance-overview-mode-map
  (org-overview)
  (read-only-mode +1))

(defvar org-glance-edit-mode-map (make-sparse-keymap)
  "Edit entries in `org-glance-edit-mode'.")

;; (define-key org-glance-overview-mode-map (kbd "C-c C-p") #'org-glance-edit-mode:start)
;; (define-key org-glance-edit-mode-map (kbd "C-c C-c") 'org-glance-edit-mode:apply)

(define-minor-mode org-glance-edit-mode
    "A minor mode to edit and sync overview files."
  nil nil org-glance-edit-mode-map)

(cl-defun org-glance-edit-mode:start ()
  (interactive)
  (org-glance-edit-mode +1)
  (org-glance-overview-mode -1)
  (defvar-local org-glance-overview:changed-headlines '())
  (cl-pushnew 'org-glance-overview:track-changes after-change-functions)
  ;; (add-hook 'before-save-hook #'org-glance-overview:sync-headlines t t)

  (org-glance:log-info "Edit mode is now enabled."))

(cl-defun org-glance-edit-mode:apply ()
  (interactive)
  (org-glance-overview:sync-headlines)
  (setq-local after-change-functions (cl-remove 'org-glance-overview:track-changes after-change-functions))
  (org-glance-edit-mode -1)
  (org-glance-overview-mode +1)
  (org-glance:log-info "All changes have been applied."))

(cl-defun org-glance-overview:directory (&optional (class (org-glance-view:completing-read)))
  "Path to file where CLASS headlines are stored."
  (let ((class-name (s-downcase (format "%s" class))))
    (abbreviate-file-name
     (f-join org-glance-directory class-name))))

(cl-defun org-glance-overview:location (&optional (view-id (org-glance-view:completing-read)))
  "Path to file where VIEW-ID headlines are stored."
  (when view-id
    (let ((view-name (s-downcase (format "%s" view-id))))
      (f-join org-glance-directory view-name (concat view-name ".org")))))

(cl-defun org-glance-headline:main-role (&optional (headline (org-glance-overview:original-headline)))
  "Assume main role of HEADLINE as role directory where it is stored."
  (cl-loop
     for view-id in (org-glance-headline:tags headline)
     for overview-directory = (org-glance-overview:directory view-id)
     for original-directory = (org-glance-headline:file headline)
     for common-parent = (abbreviate-file-name (f-common-parent (list overview-directory original-directory)))
     when (f-equal? common-parent overview-directory)
     do (return view-id)))

(cl-defun org-glance-capture-template (class &key (default ""))
  (let ((class (if (symbolp class) class (intern class)))
        (capture-template-config-file (f-join (org-glance-overview:directory class) "template.org")))
    (s-replace "%?" (concat default "%?")
               (cond ((f-exists-p capture-template-config-file) (with-temp-buffer
                                                                  (insert-file-contents capture-template-config-file)
                                                                  (buffer-substring-no-properties (point-min) (point-max))))
                     (t "* %?")))))

(cl-defun org-glance-overview:refresh-widgets (&optional (class (org-glance-overview:class)))
  (interactive)
  (let* ((inhibit-read-only t)
         (point (point)))
    (goto-char (point-min))
    (org-next-visible-heading 1)
    (kill-region (point-min) (point))
    (insert (let ((category (format "#+CATEGORY: %s" class))
                  (todo-states (cl-loop
                                  for todo-seq in org-todo-keywords
                                  concat (concat "#+TODO: " (s-join " " (cdr todo-seq)) "\n")
                                  into result
                                  finally return (s-trim result)))
                  (todo-order (concat "#+TODO_ORDER: " (cl-loop
                                                          for state in org-glance-overview:state-ordering
                                                          if (string-empty-p state)
                                                          concat "_ " into result
                                                          else
                                                          concat (concat (upcase state) " ") into result
                                                          finally return (s-trim result)))))
              (org-glance:format org-glance-overview:header)))
    (goto-char point)))

(cl-defun org-glance-overview:create (&optional (class (org-glance-view:completing-read)))
  (interactive)
  (let ((filename (-org-glance:make-file-directory
                   (org-glance-overview:location class))))
    (with-temp-file filename
      (org-glance-overview:refresh-widgets class))
    (find-file filename)))

(cl-defun org-glance-overview (&optional (class (org-glance-view:completing-read)))
  (interactive)
  (when class
    (when-let (location (org-glance-overview:location class))
      (if (file-exists-p location)
          (find-file location)
        (org-glance-class-create class)
        (org-glance-overview:create class)))))

(cl-defun org-glance-overview:agenda ()
  (interactive)
  (let ((org-agenda-files (list (buffer-file-name)))
        (org-agenda-overriding-header "org-glance agenda")
        (org-agenda-start-on-weekday nil)
        (org-agenda-span 21)
        (org-agenda-start-day "-7d"))
    (org-agenda-list)
    ;; (switch-to-buffer org-agenda-buffer)
    ;; (delete-other-windows)
    ))

(cl-defun org-glance-overview:agenda* ()
  (interactive)
  (let ((org-agenda-files (mapcar 'org-glance-overview:location (org-glance-classes)))
        (org-agenda-overriding-header "org-glance agenda")
        (org-agenda-start-on-weekday nil)
        (org-agenda-span 21)
        (org-agenda-start-day "-7d"))
    (org-agenda-list)
    (switch-to-buffer org-agenda-buffer)
    (delete-other-windows)))

(cl-defun org-glance-overview:materialize-headline ()
  (interactive)
  (let* ((headline (org-glance-overview:original-headline))
         (buffer (org-glance-materialized-headline-buffer headline)))
    (switch-to-buffer
     (if (buffer-live-p buffer)
         buffer
       (org-glance-headline:materialize headline)))))

(cl-defun org-glance-overview:visit-headline ()
  (interactive)
  (org-glance-overview:for-all
      nil
    (let ((offset (- (point) (save-excursion
                               (org-glance-headline:search-parents)
                               (point)))))
      (-some->> (org-glance-headline:at-point)
        org-glance-headline:id
        org-glance-metastore:get-headline
        org-glance-headline:visit)
      (forward-char offset))))

(cl-defun org-glance-overview:class ()
  "Return class name of current overview."
  (save-excursion
    (goto-char (point-min))
    (let ((class (org-glance-headline:string-to-class (org-get-category))))
      (when (gethash class org-glance:classes)
        class))))

(cl-defmacro org-glance-overview:for-all (then &rest else)
  (declare (indent 1) (debug t))
  `(if (org-before-first-heading-p)
       ,then
     ,@else))

(cl-defun org-glance-overview:reread ()
  "Completely rebuild current overview file."
  (interactive)
  (let ((class-name (org-glance-overview:class)))
    (when (gethash class-name org-glance-overview-deferred-import-hash-table)
      (if (not (memq org-glance-overview-deferred-import-timer timer-idle-list))
          (timer-activate org-glance-overview-deferred-import-timer)
        ;; timer is already running
        (when (y-or-n-p (format "%s is being rebuilt. Stop it?" class-name))
          (remhash class-name org-glance-overview-deferred-import-hash-table))))

    (when (y-or-n-p (format "Clear %s metadata?" class-name))
      (let ((class (org-glance:get-class class-name)))
        (save-buffer)
        (kill-buffer)
        (org-glance-metastore:create (org-glance-view:metastore-location class))
        (org-glance-overview:create class-name)
        (when (y-or-n-p (format "Import headlines from %s?" (f-parent (org-glance-view:metastore-location class))))
          (let ((files (--filter (not (s-contains? "sync-conflict" it))
                                 (org-glance-scope (f-parent (org-glance-view:metastore-location class))))))
            (org-glance-overview:import-headlines-from-files class-name files)
            (org-glance-overview:order)
            (let ((inhibit-read-only t))
              (org-align-all-tags))))))))

(cl-defun org-glance-overview:kill-headline (&key (force nil))
  "Remove `org-glance-headline' from overview, don't ask to confirm if FORCE is t."
  (interactive)
  (org-glance-headline:search-parents)
  (let ((title (org-glance-headline:title))
        (class (org-glance-overview:class))
        (original-headline (org-glance-overview:original-headline)))
    (when (or force (y-or-n-p (org-glance:format "Revoke the class \"${class}\" from \"${title}\"?")))
      (save-window-excursion
        (org-glance:with-headline-materialized original-headline
          (cl-loop
             with tags = (org-get-tags)
             with indices = (--find-indices (string= class (org-glance-headline:string-to-class it)) tags)
             for index in indices
             do (org-toggle-tag (nth index tags) 'off)))))))

(cl-defun org-glance-overview:pull ()
  "Pull any modifications from original headline to it's overview at point."
  (interactive)
  (org-glance:log-debug "Pull modifications for headline overview.")
  (let* ((inhibit-read-only t)
         (initial-point (point))
         (current-headline (org-glance-headline:at-point))
         (current-headline-title (org-glance-headline:title current-headline))
         (current-headline-contents (org-glance-headline-contents current-headline))
         (original-headline (org-glance-overview:original-headline))
         (overview-contents (org-glance:with-headline-narrowed original-headline
                              (org-glance-headline:overview))))
    (cond ((null overview-contents)
           (if (y-or-n-p (org-glance:format "Original headline for \"${current-headline-title}\" not found. Remove it from overview?"))
               (org-glance-overview:kill-headline :force t)
             (org-glance-exception:HEADLINE-NOT-FOUND "Original headline not found"))
           nil)
          ((string= current-headline-contents overview-contents)
           (org-glance:log-info (org-glance:format "Headline \"${current-headline-title}\" is up to date"))
           t)
          (t (org-glance-headline:replace-headline-at-point overview-contents)
             (org-overview)
             (goto-char initial-point)
             (org-align-tags t)
             (condition-case nil
                 (org-update-checkbox-count-maybe)
               (error nil))
             (save-buffer)
             (org-glance:log-info (org-glance:format "Headline \"${current-headline-title}\" is now up to date"))
             t))))

(cl-defun org-glance-overview:archive ()
  "Archive headline at point."
  (interactive)
  (save-window-excursion
    (org-glance:with-headline-materialized (org-glance-overview:original-headline)
      (org-toggle-archive-tag))))

(cl-defun org-glance-overview:move ()
  "Move headline to another class."
  (interactive)
  (let* ((old-class (org-glance-overview:class))
         (new-class (let ((classes (--filter (not (eql old-class it)) (org-glance-classes))))
                      (intern (org-completing-read "Move headline to: " classes))))
         (original-headline (org-glance-overview:original-headline)))
    (unless (member new-class (org-glance-classes))
      (org-glance-class-create new-class))
    (save-window-excursion
      (org-glance:with-headline-materialized original-headline
        (cl-loop
           with tags = (org-get-tags)
           with indices = (--find-indices (eql old-class (org-glance-headline:string-to-class it)) tags)
           for index in indices
           do (org-toggle-tag (nth index tags) 'off)
           finally (org-toggle-tag (symbol-name new-class) 'on))))))

(cl-defun org-glance-overview:add-class ()
  "Add class to headline."
  (interactive)
  (let* ((original-headline (org-glance-overview:original-headline))
         (old-classes (org-glance-headline:classes original-headline))
         (new-class (let ((views (--filter (not (member it old-classes)) (org-glance-classes))))
                      (intern (org-completing-read "Add class: " views)))))
    (save-window-excursion
      (org-glance:with-headline-materialized original-headline
        (org-toggle-tag (symbol-name new-class) 'on)))))

(cl-defun org-glance-overview:original-headline ()
  (org-glance:with-headline-narrowed
      (->> (org-glance-headline:at-point)
           org-glance-headline:id
           org-glance-metastore:get-headline)
    (org-glance-headline:at-point)))

(cl-defun org-glance-overview:order ()
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
          (beginning-of-headlines (save-excursion
                                    (goto-char (point-min))
                                    (outline-next-heading)
                                    (point)))
          (end-of-headlines (point-max)))

      (cl-loop
         for buffer in (org-glance-overview:partition
                         :using #'org-glance-overview:partition-mapper
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
;;   (org-glance-overview:for-all
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
;;   (let ((org-agenda-files (mapcar 'org-glance-overview:location (org-glance-classes))))
;;     (org-agenda-list)
;;     (org-agenda-day-view day)
;;     (switch-to-buffer org-agenda-buffer)
;;     (delete-other-windows)))

(org-glance:provide)
