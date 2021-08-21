;; org-glance view means a set of headlines in scope of org-files

(require 'org-glance-module)

(require 'org)
(require 'org-element)
(require 'subr-x)

(org-glance-module-import lib.core.scope)
(org-glance-module-import lib.core.metastore)
(org-glance-module-import lib.core.exceptions)
(org-glance-module-import lib.core.headline)

(org-glance-module-import lib.utils.helpers)

(defconst org-glance-view:all "All views")  ;; do not apply any filtering to views/headlines datasets

(defvar org-glance-form:view org-glance-view:all)

(defvar org-glance-view-default-type '(all)
  "Default type for all views.")

(defvar org-glance-view-mode-map (make-sparse-keymap)
  "Extend `org-mode' map with sync abilities.")

(define-minor-mode org-glance-view-mode
    "A minor mode to be activated only in materialized view editor."
  nil nil org-glance-view-mode-map)

(defvar --org-glance-view-pwd nil)
(defvar --org-glance-view-src nil)
(defvar --org-glance-view-beg nil)
(defvar --org-glance-view-end nil)
(defvar --org-glance-view-hash nil)
(defvar --org-glance-view-indent nil)

(defvar org-glance-views (make-hash-table :test 'equal))
(defvar org-glance-view-actions (make-hash-table :test 'equal))

(defcustom org-glance-view-location (f-join user-emacs-directory "org-glance" "views")
  "The location where view metadata should be stored."
  :group 'org-glance
  :type 'string)

(defcustom org-glance-materialized-view-buffer "*org-glance materialized view*"
  "Default buffer name for materialized view."
  :group 'org-glance
  :type 'string)

(define-key org-glance-view-mode-map (kbd "C-x C-s") #'org-glance-view-sync-subtree)
(define-key org-glance-view-mode-map (kbd "C-c C-q") #'kill-current-buffer)
(define-key org-glance-view-mode-map (kbd "C-c C-v") #'org-glance-overview:visit)

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

(cl-defstruct (org-glance-view
                ;; (:constructor org-glance-def-view (id
                ;;                                    &key
                ;;                                    (type org-glance-view-default-type)
                ;;                                    (scope nil)))
                )
  id type scope)

(cl-defun org-glance-view-resource-location (&optional (view-id (org-glance-view:completing-read)))
  "Path to directory where VIEW-ID resources and metadata are stored."
  (f-join org-glance-view-location
          (s-downcase (format "%s" view-id))
          "resources"))

(defun org-glance-exports ()
  (org-glance--list-files-recursively org-glance-view-location))

(define-error 'org-glance-view-not-modified "No changes made in materialized view" 'user-error)
(cl-defun org-glance-view-not-modified (format &rest args)
  (signal 'org-glance-view-not-modified (list (apply #'format-message format args))))

(defun org-glance-view:ids ()
  "List registered views."
  (sort (hash-table-keys org-glance-views) #'s-less?))

(cl-defmethod org-glance-view ((view-id symbol)) (gethash view-id org-glance-views))
(cl-defmethod org-glance-view ((view-id string)) (org-glance-view (intern view-id)))

(cl-defmethod org-glance-view-metastore-location ((view org-glance-view))
  (let ((view-id (downcase (symbol-name (org-glance-view-id view)))))
    (f-join org-glance-view-location
            view-id
            (format "%s.metadata.el" view-id))))

(cl-defmethod org-glance-view-filter ((view org-glance-view))
  (-partial
   #'(lambda (view headline)
       (when (-contains?
              (mapcar #'downcase (org-element-property :tags headline))
              (downcase (symbol-name (org-glance-view-id view))))
         headline))
   view))

(cl-defun org-glance-view:update (&optional (vid org-glance-form:view))
  (interactive)

  (org-glance-view:if-all? vid
      (message "Update all views")
    (message "Update view %s" vid))

  (org-glance-view:if-all? vid
      (cl-loop for id in (org-glance-view:ids)
         append (org-glance-view:update id))
    (let* ((view (org-glance-view:get-view-by-id vid))
           (db (org-glance-view-metastore-location view))
           (filter (org-glance-view-filter view))
           (scope (or (org-glance-view-scope view) org-glance-default-scope)))
      (org-glance-metastore:create db (org-glance-scope-headlines scope filter))
      (list view))))

(cl-defgeneric org-glance-view:headlines (view))

(cl-defmethod org-glance-view:headlines ((view symbol))
  "When VIEW is a symbol, extract org-glance-view from `org-glance-view` hashmap by key."
  (org-glance-view:headlines (org-glance-view:get-view-by-id view)))

(cl-defmethod org-glance-view:headlines ((view string))
  "When VIEW is a string, extract org-glance-view from `org-glance-view` hashmap by key intern."
  (org-glance-view:headlines (org-glance-view:get-view-by-id (intern view))))

(cl-defmethod org-glance-view:headlines ((view list))
  "When VIEW is a list, apply org-glance-view:headlines for each element of it."
  (cl-loop for v in view
     append (org-glance-view:headlines v)))

(cl-defmethod org-glance-view:headlines ((view org-glance-view))
  "Browse each file of a VIEW scope, run org-element-map and collect headlines as org-elements."
  (org-glance-headlines
   :db (org-glance-view-metastore-location view)
   :scope (or (org-glance-view-scope view) org-glance-default-scope)
   :filter (org-glance-view-filter view)))

(cl-defmethod org-glance-view:headlines* ((view org-glance-view))
  "List headlines as formatted strings for VIEW."
  (->> view
    org-glance-view:headlines
    (mapcar #'org-glance-headline:title)
    (mapcar #'(lambda (hl) (format "[%s] %s" (org-glance-view-id view) hl)))))

(cl-defgeneric org-glance-view-prompt (view action)
  "Generate prompt for VIEW. Assume ACTION context.")

(cl-defmethod org-glance-view-prompt ((view org-glance-view) (action symbol))
  (s-titleize (format "%s %s: " action (org-glance-view-id view))))

(cl-defgeneric org-glance-view-action-resolve (view action))

(cl-defmethod org-glance-view-action-resolve ((view org-glance-view) (action symbol))
  (let* ((action-types (->> org-glance-view-actions
                         (gethash action)
                         (-sort (lambda (lhs rhs) (> (length lhs) (length rhs))))))
         (view-actions (cl-loop for action-type in action-types
                          with view-type = (org-glance-view-type view)
                          when (cl-subsetp action-type view-type)
                          return action-type)))
    (or view-actions
        (car (member org-glance-view-default-type (gethash action org-glance-view-actions))))))

(cl-defmethod org-glance-view-action-resolve ((view null) (action symbol))
  (user-error "Assertion error: unable to resolve action when view is null"))

(defun org-glance-view-sync-subtree ()
  (interactive)
  (save-excursion
    (org-glance:first-level-headline)
    (let* ((source --org-glance-view-src)
           (beg --org-glance-view-beg)
           (end --org-glance-view-end)
           (promote-level --org-glance-view-indent)
           (glance-hash --org-glance-view-hash)
           (mat-hash (org-glance-view-subtree-hash))
           (src-hash (org-glance-view-source-hash)))

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
          (setq-local --org-glance-view-hash (org-glance-view-source-hash))

          (with-demoted-errors (run-hooks 'org-glance-after-materialize-sync-hook))
          (message "Materialized view synchronized successfully"))))))

(defun org-glance-view-subtree-hash ()
  (save-restriction
    (org-narrow-to-subtree)
    (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
      (with-temp-buffer
        (org-mode)
        (insert buffer-contents)
        (goto-char (point-min))
        (--org-glance-headline:promote.deprecated)
        (buffer-hash)))))

(defun org-glance-view-source-hash ()
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

(cl-defmethod org-glance-view-delete ((view-id symbol))
  (remhash view-id org-glance-views))

(cl-defun org-glance-view:completing-read (&optional (prompt "Choose view: "))
  "Run completing read PROMPT on registered views filtered by TYPE."
  (let ((views (org-glance-view:ids)))
    (if (> (length views) 1)
        (intern (org-completing-read prompt views))
      (car views))))

(cl-defun org-glance-read-view (&optional (prompt "Choose view: "))
  "Run completing read PROMPT on registered views filtered by TYPE."
  (gethash (org-glance-view:completing-read prompt) org-glance-views))

(cl-defmacro org-glance-view:if-all? (vid do-for-all-forms &rest do-for-specific-view-forms)
  (declare (indent 2) (debug t))
  `(cond
     ((string= ,vid org-glance-view:all) ,do-for-all-forms)
     (t ,@do-for-specific-view-forms)))

(defun org-glance-view:get-view-by-id (view-id)
  (gethash view-id org-glance-views))

(cl-defun org-glance-def-view (view-id &key type scope &allow-other-keys)
  (when (org-glance-view:get-view-by-id view-id)
    (user-error "View %s is already registered." view-id))
  (let ((view (make-org-glance-view :id view-id)))
    (when scope (setf (org-glance-view-scope view) scope))
    (when type  (setf (org-glance-view-type view) type))
    (puthash view-id view org-glance-views)
    (unless scope
      (message "Default scope is: %s" org-glance-default-scope))
    (message "View \"%s\"%s is now ready to glance %s"
             view-id
             (if type (concat " of type \"" (s-trim (pp-to-string type)) "\"") "")
             (if scope (concat " over scope \"" (s-trim (pp-to-string scope)) "\"") ""))
    view))

(cl-defun org-glance-view:capture-headline-at-point (&optional view-id)
  (interactive)
  (save-excursion
    (org-glance:ensure-at-heading)
    (let* ((view-id (cond ((symbolp view-id) (symbol-name view-id))
                          ((stringp view-id) view-id)
                          (t (org-completing-read "Capture subtree for view: "
                                                  (seq-difference
                                                   (org-glance-view:ids)
                                                   (mapcar #'intern (org-get-tags)))))))
           (view (org-glance-view view-id))
           (id (org-glance:generate-id-for-subtree-at-point view-id))
           (dir (org-glance:generate-dir-for-subtree-at-point view-id))
           (output-file (f-join dir (org-glance:format "${view-id}.org"))))
      ;; (y-or-n-p (org-glance:format "Refile ${view-id} to ${dir}?"))
      (org-set-property "DIR" dir)
      (org-set-property "CATEGORY" view-id)
      (org-set-property "ORG_GLANCE_CREATION_TIME" (with-temp-buffer
                                                     (let ((current-prefix-arg '(16)))
                                                       (call-interactively #'org-time-stamp-inactive)
                                                       (buffer-substring-no-properties (point-min) (point-max)))))
      (unless (member (downcase view-id) (org-glance--collect-tags))
        (org-toggle-tag view-id))
      (save-restriction
        (org-narrow-to-subtree)
        (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
          (delete-region (point-min) (point-max))
          (mkdir dir 'parents)
          (find-file output-file)
          (widen)
          (end-of-buffer)
          (insert contents)
          (org-glance-headline:goto-beginning-of-current-headline)
          (save-buffer)
          (org-glance-headline:at-point))))))

(cl-defun org-glance-view:capture-headline
    (&optional
       (view-id (org-completing-read "Capture new entry for view: " (org-glance-view:ids)))
       (title (read-string (format "Title for new %s: " view-id))))
  (interactive)
  (with-temp-buffer
    (insert "* " title)
    (org-glance-view:capture-headline-at-point view-id)))

(org-glance-module-provide)
